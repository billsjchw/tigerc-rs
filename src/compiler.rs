use crate::{
    ast::{BinOp, Expr},
    error::Error,
    parser,
};
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

        Self { module }
    }

    pub fn compile(mut self, prog: &str) -> Result<Vec<u8>, Error> {
        let expr = parser::parse(prog)?;

        let mut context = self.module.make_context();
        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        Compiler::handle_expr(&*expr, &mut builder)?;
        builder.ins().return_(&[]);
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

    fn handle_expr(expr: &Expr, builder: &mut FunctionBuilder) -> Result<(Value, Type), Error> {
        match *expr {
            Expr::Integer { value, .. } => Ok((builder.ins().iconst(I64, value), Type::Integer)),
            Expr::BinOp {
                ref lhs,
                ref op,
                ref rhs,
                ..
            } => {
                let (lhs_value, lhs_type) = Compiler::handle_expr(&**lhs, builder)?;
                let (rhs_value, rhs_type) = Compiler::handle_expr(&**rhs, builder)?;
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
                }
            }
            _ => todo!(),
        }
    }
}

enum Type {
    Integer,
}
