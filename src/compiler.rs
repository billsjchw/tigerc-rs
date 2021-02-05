use crate::{
    ast::{BinOp, Expr},
    error::Error,
    parser,
    symtab::SymbolTable,
};
use cranelift::codegen::ir::types::Type as IRType;
use cranelift::codegen::ir::types::*;
use cranelift::{
    codegen::{binemit::NullTrapSink, isa},
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::*,
};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::str::FromStr;
use target_lexicon::triple;

pub struct Compiler {
    module: ObjectModule,
    funcs: SymbolTable<String, Func>,
}

impl Compiler {
    pub fn new() -> Self {
        let flags_builder = settings::builder();
        let flags = settings::Flags::new(flags_builder);
        let isa_builder = isa::lookup(triple!("x86_64-unknown-unknown-elf")).unwrap();
        let isa = isa_builder.finish(flags);

        let object_builder = ObjectBuilder::new(
            isa,
            String::from("tiger"),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let module = ObjectModule::new(object_builder);

        let mut funcs = SymbolTable::new();
        funcs.insert(
            String::from("printi"),
            Func {
                name: String::from("printi"),
                params: vec![Type::Integer],
                ret: Type::Unit,
            },
        );

        Self { module, funcs }
    }

    pub fn compile(mut self, prog: &str) -> Result<Vec<u8>, Error> {
        let expr = parser::parse(prog)?;

        let mut context = self.module.make_context();
        context.func.signature.returns.push(AbiParam::new(I64));
        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        self.handle_expr(&*expr, &mut builder)?;
        let ret = builder.ins().iconst(I64, 0);
        builder.ins().return_(&[ret]);
        builder.finalize();
        let id = self
            .module
            .declare_function("tigermain", Linkage::Export, &context.func.signature)
            .unwrap();
        self.module
            .define_function(id, &mut context, &mut NullTrapSink {})
            .unwrap();

        let product = self.module.finish();
        let bytes = product.emit().unwrap();
        Ok(bytes)
    }

    fn handle_expr(
        &mut self,
        expr: &Expr,
        builder: &mut FunctionBuilder,
    ) -> Result<(Value, Type), Error> {
        match *expr {
            Expr::Integer { value, .. } => Ok((builder.ins().iconst(I64, value), Type::Integer)),
            Expr::BinOp {
                ref lhs,
                ref op,
                ref rhs,
                ..
            } => {
                let (lhs_value, lhs_type) = self.handle_expr(&**lhs, builder)?;
                let (rhs_value, rhs_type) = self.handle_expr(&**rhs, builder)?;
                match (lhs_type, op, rhs_type) {
                    (Type::Integer, BinOp::Add, Type::Integer) => {
                        Ok((builder.ins().iadd(lhs_value, rhs_value), Type::Integer))
                    }
                    (Type::Integer, BinOp::Sub, Type::Integer) => {
                        Ok((builder.ins().isub(lhs_value, rhs_value), Type::Integer))
                    }
                    (Type::Integer, BinOp::Mul, Type::Integer) => {
                        Ok((builder.ins().imul(lhs_value, rhs_value), Type::Integer))
                    }
                    (Type::Integer, BinOp::Div, Type::Integer) => {
                        Ok((builder.ins().udiv(lhs_value, rhs_value), Type::Integer))
                    }
                    (Type::Integer, BinOp::And, Type::Integer) => {
                        Ok((builder.ins().band(lhs_value, rhs_value), Type::Integer))
                    }
                    (Type::Integer, BinOp::Or, Type::Integer) => {
                        Ok((builder.ins().bor(lhs_value, rhs_value), Type::Integer))
                    }
                    (Type::Integer, BinOp::Eq, Type::Integer) => {
                        let tmp = builder.ins().icmp(IntCC::Equal, lhs_value, rhs_value);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    (Type::Integer, BinOp::NE, Type::Integer) => {
                        let tmp = builder.ins().icmp(IntCC::NotEqual, lhs_value, rhs_value);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    (Type::Integer, BinOp::LT, Type::Integer) => {
                        let tmp = builder
                            .ins()
                            .icmp(IntCC::SignedLessThan, lhs_value, rhs_value);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    (Type::Integer, BinOp::LE, Type::Integer) => {
                        let tmp =
                            builder
                                .ins()
                                .icmp(IntCC::SignedLessThanOrEqual, lhs_value, rhs_value);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    (Type::Integer, BinOp::GT, Type::Integer) => {
                        let tmp =
                            builder
                                .ins()
                                .icmp(IntCC::SignedGreaterThan, lhs_value, rhs_value);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    (Type::Integer, BinOp::GE, Type::Integer) => {
                        let tmp = builder.ins().icmp(
                            IntCC::SignedGreaterThanOrEqual,
                            lhs_value,
                            rhs_value,
                        );
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    _ => todo!(),
                }
            }
            Expr::Call {
                loc,
                ref callee,
                ref args,
            } => {
                let func = self.funcs.get(callee).ok_or(Error::UndefFunc(loc))?.clone();

                if func.params.len() != args.len() {
                    return Err(Error::ParamMismatch(loc));
                }

                let mut arg_values = vec![];
                let mut arg_types = vec![];
                for arg in args.iter() {
                    let (arg_value, arg_type) = self.handle_expr(arg, builder)?;
                    arg_values.push(arg_value);
                    arg_types.push(arg_type);
                }

                for (param, arg_type) in func.params.iter().zip(arg_types.iter()) {
                    if param != arg_type {
                        return Err(Error::ParamMismatch(loc));
                    }
                }

                let mut sig = self.module.make_signature();
                func.translate(&mut sig);
                let callee = self
                    .module
                    .declare_function(&func.name[..], Linkage::Import, &sig)
                    .unwrap();
                let local_callee = self.module.declare_func_in_func(callee, &mut builder.func);

                let call = builder.ins().call(local_callee, &arg_values);
                Ok((builder.inst_results(call)[0], func.ret))
            }
            _ => todo!(),
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
enum Type {
    Integer,
    Unit,
}

impl Type {
    fn translate(&self) -> IRType {
        match *self {
            Type::Integer => I64,
            Type::Unit => I64,
        }
    }
}

#[derive(Clone)]
struct Func {
    name: String,
    params: Vec<Type>,
    ret: Type,
}

impl Func {
    fn translate(&self, sig: &mut Signature) {
        for param in self.params.iter() {
            sig.params.push(AbiParam::new(param.translate()));
        }
        sig.returns.push(AbiParam::new(self.ret.translate()));
    }
}
