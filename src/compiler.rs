use crate::{
    ast::{BinOp, Def, Expr},
    error::Error,
    parser,
    symtab::SymbolTable,
};
use codegen::ir::StackSlot;
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
    types: SymbolTable<String, Type>,
    vars: SymbolTable<String, Var>,
    seq: i32,
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
                depth: 0,
            },
        );

        let mut types = SymbolTable::new();
        types.insert(String::from("int"), Type::Integer);

        let vars = SymbolTable::new();

        Self {
            module,
            funcs,
            types,
            vars,
            seq: 0,
        }
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
        self.handle_expr(&*expr, 0, &mut builder)?;
        let ret_value = builder.ins().iconst(I64, 0);
        builder.ins().return_(&[ret_value]);
        builder.seal_all_blocks();
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
        depth: i32,
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
                let (lhs_value, lhs_type) = self.handle_expr(&**lhs, depth, builder)?;
                let (rhs_value, rhs_type) = self.handle_expr(&**rhs, depth, builder)?;
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
                if func.depth > 0 {
                    todo!();
                }
                for arg in args.iter() {
                    let (arg_value, arg_type) = self.handle_expr(arg, depth, builder)?;
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
            Expr::Let {
                ref defs, ref body, ..
            } => {
                self.funcs.enter_scope();
                self.types.enter_scope();
                self.vars.enter_scope();
                self.handle_defs(&defs[..], depth, builder)?;
                let (value, type_) = self.handle_expr(&**body, depth, builder)?;
                self.funcs.exit_scope();
                self.types.exit_scope();
                self.vars.exit_scope();
                Ok((value, type_))
            }
            Expr::Seq {
                ref exprs,
                ref expr,
                ..
            } => {
                for expr in exprs.iter() {
                    self.handle_expr(expr, depth, builder)?;
                }
                self.handle_expr(expr, depth, builder)
            }
            Expr::Empty { .. } => {
                let value = builder.ins().iconst(I64, 0);
                Ok((value, Type::Unit))
            }
            _ => todo!(),
        }
    }

    fn handle_defs(
        &mut self,
        defs: &[Def],
        depth: i32,
        builder: &mut FunctionBuilder,
    ) -> Result<(), Error> {
        if defs.len() == 0 {
            return Ok(());
        }

        let mut tail = 0;
        let mut funcs = vec![];
        while let Def::Func {
            loc,
            ref ident,
            ref params,
            ref ret,
            ..
        } = defs[tail]
        {
            let mut func: Func = Default::default();
            self.seq += 1;
            func.name = format!("{}{}", ident, self.seq);
            for param in params.iter() {
                func.params
                    .push(*self.types.get(&param.1).ok_or(Error::UndefType(loc))?);
            }
            func.ret = match *ret {
                Some(ref ret) => *self.types.get(ret).ok_or(Error::UndefType(loc))?,
                None => Type::Unit,
            };
            func.depth = depth + 1;
            self.funcs.insert(func.name.clone(), func.clone());
            funcs.push(func);
            tail += 1;
            if tail == defs.len() {
                break;
            }
        }

        let mut tail = 0;
        while let Def::Func {
            loc,
            ref params,
            ref escs,
            ref body,
            ..
        } = defs[tail]
        {
            let func = &funcs[tail];

            let mut context = self.module.make_context();
            func.translate(&mut context.func.signature);
            let mut builder_context = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);

            let entry_block = builder.create_block();
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);
            builder.append_block_params_for_function_params(entry_block);

            let static_link =
                builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
            let arg0 = builder.block_params(entry_block)[0];
            builder.ins().stack_store(arg0, static_link, 0);

            self.vars.enter_scope();

            for (i, (((ident, _), type_), esc)) in params
                .iter()
                .zip(func.params.iter())
                .zip(escs.iter())
                .enumerate()
            {
                let arg = builder.block_params(entry_block)[i + 1];
                let access = if *esc {
                    let stack_slot = builder
                        .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
                    builder.ins().stack_store(arg, stack_slot, 0);
                    Access::Memory(stack_slot)
                } else {
                    self.seq += 1;
                    let var = Variable::with_u32(self.seq as u32);
                    builder.declare_var(var, I64);
                    builder.def_var(var, arg);
                    Access::Temporary(var)
                };
                self.vars.insert(
                    ident.clone(),
                    Var {
                        type_: *type_,
                        depth: depth + 1,
                        access,
                    },
                );
            }

            let (ret_value, ret_type) = self.handle_expr(&**body, depth + 1, &mut builder)?;

            self.vars.exit_scope();

            if ret_type != func.ret {
                return Err(Error::RetMismatch(loc));
            }

            builder.ins().return_(&[ret_value]);
            builder.seal_all_blocks();
            builder.finalize();
            let id = self
                .module
                .declare_function(&func.name[..], Linkage::Export, &context.func.signature)
                .unwrap();
            self.module
                .define_function(id, &mut context, &mut NullTrapSink {})
                .unwrap();

            tail += 1;
            if tail == defs.len() {
                break;
            }
        }

        if tail > 0 {
            return self.handle_defs(&defs[tail..], depth, builder);
        }

        todo!()
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

impl Default for Type {
    fn default() -> Self {
        Type::Unit
    }
}

#[derive(Clone, Default)]
struct Func {
    name: String,
    params: Vec<Type>,
    ret: Type,
    depth: i32,
}

impl Func {
    fn translate(&self, sig: &mut Signature) {
        if self.depth > 0 {
            sig.params.push(AbiParam::new(I64));
        }
        for param in self.params.iter() {
            sig.params.push(AbiParam::new(param.translate()));
        }
        sig.returns.push(AbiParam::new(self.ret.translate()));
    }
}

enum Access {
    Temporary(Variable),
    Memory(StackSlot),
}

#[allow(dead_code)]
struct Var {
    type_: Type,
    depth: i32,
    access: Access,
}
