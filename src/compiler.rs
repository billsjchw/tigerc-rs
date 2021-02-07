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
use std::{str::FromStr, todo};
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
        let link = builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
        self.handle_rvalue(&*expr, 0, &mut builder, link)?;
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

    fn handle_rvalue(
        &mut self,
        rvalue: &Expr,
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
    ) -> Result<(Value, Type), Error> {
        match *rvalue {
            Expr::Integer { value, .. } => Ok((builder.ins().iconst(I64, value), Type::Integer)),
            Expr::BinOp {
                ref lhs,
                ref op,
                ref rhs,
                ..
            } => {
                let (lhs_value, lhs_type) = self.handle_rvalue(&**lhs, depth, builder, link)?;
                let (rhs_value, rhs_type) = self.handle_rvalue(&**rhs, depth, builder, link)?;
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
                    arg_values.push(walk_link(depth - func.depth + 1, builder, link));
                }
                for arg in args.iter() {
                    let (arg_value, arg_type) = self.handle_rvalue(arg, depth, builder, link)?;
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
                self.handle_defs(&defs[..], depth, builder, link)?;
                let (value, type_) = self.handle_rvalue(&**body, depth, builder, link)?;
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
                    self.handle_rvalue(expr, depth, builder, link)?;
                }
                self.handle_rvalue(expr, depth, builder, link)
            }
            Expr::Empty { .. } => {
                let value = builder.ins().iconst(I64, 0);
                Ok((value, Type::Unit))
            }
            Expr::Ident { loc, ref ident } => {
                let var = self.vars.get(ident).ok_or(Error::UndefVar(loc))?;
                match var.access {
                    Access::Temporary(irvar) => Ok((builder.use_var(irvar), var.type_)),
                    Access::Stack(access_depth, slot) if access_depth == depth => {
                        Ok((builder.ins().stack_load(I64, slot, 0), var.type_))
                    }
                    Access::Stack(access_depth, slot) => {
                        let fp = walk_link(depth - access_depth, builder, link);
                        let offset = -(slot.as_u32() as i32 + 1) * 8;
                        Ok((
                            builder.ins().load(I64, MemFlags::new(), fp, offset),
                            var.type_,
                        ))
                    }
                }
            }
            Expr::Assign {
                loc,
                ref lvalue,
                ref rvalue,
                ..
            } => {
                let (lvalue_access, lvalue_type) =
                    self.handle_lvalue(&**lvalue, depth, builder, link)?;
                let (rvalue_value, rvalue_type) =
                    self.handle_rvalue(&**rvalue, depth, builder, link)?;

                if lvalue_type != rvalue_type {
                    return Err(Error::AssignMismatch(loc));
                }

                match lvalue_access {
                    Access::Temporary(var) => {
                        builder.def_var(var, rvalue_value);
                    }
                    Access::Stack(access_depth, slot) if access_depth == depth => {
                        builder.ins().stack_store(rvalue_value, slot, 0);
                    }
                    Access::Stack(access_depth, slot) => {
                        let fp = walk_link(depth - access_depth, builder, link);
                        let offset = -(slot.as_u32() as i32 + 1) * 8;
                        builder
                            .ins()
                            .store(MemFlags::new(), rvalue_value, fp, offset);
                    }
                }

                Ok((builder.ins().iconst(I64, 0), Type::Unit))
            }
            _ => todo!(),
        }
    }

    #[allow(unused_variables)]
    fn handle_lvalue(
        &mut self,
        lvalue: &Expr,
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
    ) -> Result<(Access, Type), Error> {
        match *lvalue {
            Expr::Ident { loc, ref ident } => {
                let var = self.vars.get(ident).ok_or(Error::UndefVar(loc))?;
                Ok((var.access, var.type_))
            }
            _ => todo!(),
        }
    }

    fn handle_defs(
        &mut self,
        defs: &[Def],
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
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
            self.funcs.insert(ident.clone(), func.clone());
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

            let link =
                builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
            let arg0 = builder.block_params(entry_block)[0];
            builder.ins().stack_store(arg0, link, 0);

            self.vars.enter_scope();

            for (i, (((ident, _), type_), esc)) in params
                .iter()
                .zip(func.params.iter())
                .zip(escs.iter())
                .enumerate()
            {
                let arg = builder.block_params(entry_block)[i + 1];
                let access = if *esc {
                    let slot = builder
                        .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
                    builder.ins().stack_store(arg, slot, 0);
                    Access::Stack(depth + 1, slot)
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
                        access,
                    },
                );
            }

            let (ret_value, ret_type) =
                self.handle_rvalue(&**body, depth + 1, &mut builder, link)?;

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
            return self.handle_defs(&defs[tail..], depth, builder, link);
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

#[derive(Clone, Copy)]
enum Access {
    Temporary(Variable),
    Stack(i32, StackSlot),
}

struct Var {
    type_: Type,
    access: Access,
}

fn walk_link(delta: i32, builder: &mut FunctionBuilder, link: StackSlot) -> Value {
    let link_addr = builder.ins().stack_addr(I64, link, 0);
    let mut fp = builder.ins().iadd_imm(link_addr, 8);
    for _ in 0..delta {
        fp = builder.ins().load(I64, MemFlags::new(), fp, -8);
    }
    fp
}
