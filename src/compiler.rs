use crate::{
    ast::{BinOp, Def, Expr, Type as ASTType, UnOp},
    error::Error,
    esc, parser,
    symtab::SymbolTable,
};
use cranelift::{
    codegen::{
        binemit::NullTrapSink,
        ir::{types::*, StackSlot},
        isa,
    },
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::*,
};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
    todo,
};
use target_lexicon::triple;

pub struct Compiler {
    module: ObjectModule,
    funcs: SymbolTable<String, Func>,
    types: SymbolTable<String, Type>,
    vars: SymbolTable<String, Var>,
    array_types: HashMap<i32, ArrayType>,
    record_types: HashMap<i32, RecordType>,
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
                name: String::from("tiger_printi"),
                param_types: vec![Type::Integer],
                ret_type: Type::Unit,
                depth: 0,
            },
        );
        funcs.insert(
            String::from("print"),
            Func {
                name: String::from("tiger_print"),
                param_types: vec![Type::String],
                ret_type: Type::Unit,
                depth: 0,
            },
        );

        let mut types = SymbolTable::new();
        types.insert(String::from("int"), Type::Integer);
        types.insert(String::from("string"), Type::String);

        let vars = SymbolTable::new();
        let arrays = HashMap::new();
        let records = HashMap::new();

        Self {
            module,
            funcs,
            types,
            vars,
            array_types: arrays,
            record_types: records,
            seq: 0,
        }
    }

    pub fn compile(mut self, prog: &str) -> Result<Vec<u8>, Error> {
        let mut expr = parser::parse(prog)?;
        esc::find_esc(&mut *expr);

        let mut context = self.module.make_context();
        context.func.signature.returns.push(AbiParam::new(I64));
        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        let link = builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
        self.handle_rvalue(&*expr, 0, &mut builder, link, None, None)?;
        let ret = builder.ins().iconst(I64, 0);
        builder.ins().return_(&[ret]);
        builder.finalize();
        let id = self
            .module
            .declare_function("tiger_main", Linkage::Export, &context.func.signature)
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
        break_block: Option<Block>,
        continue_block: Option<Block>,
    ) -> Result<(Value, Type), Error> {
        match *rvalue {
            Expr::Integer { value, .. } => Ok((builder.ins().iconst(I64, value), Type::Integer)),
            Expr::BinOp {
                loc,
                ref lhs,
                op,
                ref rhs,
                ..
            } => {
                let (lhs_value, lhs_type) =
                    self.handle_rvalue(&**lhs, depth, builder, link, break_block, continue_block)?;
                let (rhs_value, rhs_type) =
                    self.handle_rvalue(&**rhs, depth, builder, link, break_block, continue_block)?;

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
                    (Type::Integer, _, Type::Integer) => {
                        let cc = self.translate_rel_op(op);
                        let tmp = builder.ins().icmp(cc, lhs_value, rhs_value);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    (Type::String, BinOp::Add, Type::String) => todo!(),
                    (Type::String, BinOp::Sub, Type::String) => Err(Error::WrongOperandType(loc)),
                    (Type::String, BinOp::Mul, Type::String) => Err(Error::WrongOperandType(loc)),
                    (Type::String, BinOp::Div, Type::String) => Err(Error::WrongOperandType(loc)),
                    (Type::String, BinOp::And, Type::String) => Err(Error::WrongOperandType(loc)),
                    (Type::String, BinOp::Or, Type::String) => Err(Error::WrongOperandType(loc)),
                    (Type::String, _, Type::String) => {
                        let cc = self.translate_rel_op(op);
                        let mut sig = self.module.make_signature();
                        sig.params.push(AbiParam::new(I64));
                        sig.params.push(AbiParam::new(I64));
                        sig.returns.push(AbiParam::new(I64));
                        let cmp = self.translate_call(
                            "tiger_strcmp",
                            &sig,
                            &[lhs_value, rhs_value],
                            builder,
                        );
                        let zero = builder.ins().iconst(I64, 0);
                        let tmp = builder.ins().icmp(cc, cmp, zero);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    (Type::String, BinOp::Add, Type::Integer) => todo!(),
                    (Type::Integer, BinOp::Add, Type::String) => todo!(),
                    (Type::Integer, _, _) => Err(Error::WrongOperandType(loc)),
                    (_, _, Type::Integer) => Err(Error::WrongOperandType(loc)),
                    (Type::String, _, _) => Err(Error::WrongOperandType(loc)),
                    (_, _, Type::String) => Err(Error::WrongOperandType(loc)),
                    (Type::Unit, _, _) => Err(Error::WrongOperandType(loc)),
                    (_, _, Type::Unit) => Err(Error::WrongOperandType(loc)),
                    (_, BinOp::Eq, _) if lhs_type == rhs_type => {
                        let tmp = builder.ins().icmp(IntCC::Equal, lhs_value, rhs_value);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    (_, BinOp::NE, _) if lhs_type == rhs_type => {
                        let tmp = builder.ins().icmp(IntCC::NotEqual, lhs_value, rhs_value);
                        Ok((builder.ins().bint(I64, tmp), Type::Integer))
                    }
                    _ => Err(Error::WrongOperandType(loc)),
                }
            }
            Expr::UnOp { loc, op, ref expr } => {
                let (expr_value, expr_type) =
                    self.handle_rvalue(&**expr, depth, builder, link, break_block, continue_block)?;
                match (op, expr_type) {
                    (UnOp::Pos, Type::Integer) => Ok((expr_value, Type::Integer)),
                    (UnOp::Neg, Type::Integer) => {
                        Ok((builder.ins().ineg(expr_value), Type::Integer))
                    }
                    (UnOp::Not, Type::Integer) => {
                        Ok((builder.ins().bnot(expr_value), Type::Integer))
                    }
                    _ => Err(Error::WrongOperandType(loc)),
                }
            }
            Expr::Call {
                loc,
                ref callee,
                ref args,
            } => {
                let func = self.funcs.get(callee).ok_or(Error::UndefFunc(loc))?.clone();

                if func.param_types.len() != args.len() {
                    return Err(Error::ParamMismatch(loc));
                }

                let mut arg_values = vec![];
                let mut arg_types = vec![];
                if func.depth > 0 {
                    arg_values.push(self.walk_link(depth - func.depth + 1, builder, link));
                }
                for arg in args.iter() {
                    let (arg_value, arg_type) =
                        self.handle_rvalue(arg, depth, builder, link, break_block, continue_block)?;
                    arg_values.push(arg_value);
                    arg_types.push(arg_type);
                }

                for (param_type, arg_type) in func.param_types.iter().zip(arg_types.iter()) {
                    if param_type != arg_type {
                        return Err(Error::ParamMismatch(loc));
                    }
                }

                let mut sig = self.module.make_signature();
                func.translate(&mut sig);

                Ok((
                    self.translate_call(&func.name[..], &sig, &arg_values, builder),
                    func.ret_type,
                ))
            }
            Expr::Let {
                ref defs, ref body, ..
            } => {
                self.funcs.enter_scope();
                self.types.enter_scope();
                self.vars.enter_scope();
                self.handle_defs(&defs[..], depth, builder, link, break_block, continue_block)?;
                let (value, type_) =
                    self.handle_rvalue(&**body, depth, builder, link, break_block, continue_block)?;
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
                    self.handle_rvalue(expr, depth, builder, link, break_block, continue_block)?;
                }
                self.handle_rvalue(expr, depth, builder, link, break_block, continue_block)
            }
            Expr::Empty { .. } => {
                let value = builder.ins().iconst(I64, 0);
                Ok((value, Type::Unit))
            }
            Expr::Ident { loc, ref ident } => {
                let var = self.vars.get(ident).ok_or(Error::UndefVar(loc))?;
                Ok((
                    self.translate_load(var.access, depth, builder, link),
                    var.type_,
                ))
            }
            Expr::Assign {
                loc,
                ref lvalue,
                ref rvalue,
                ..
            } => {
                let (lvalue_access, lvalue_type) = self.handle_lvalue(
                    &**lvalue,
                    depth,
                    builder,
                    link,
                    break_block,
                    continue_block,
                )?;
                let (rvalue_value, rvalue_type) = self.handle_rvalue(
                    &**rvalue,
                    depth,
                    builder,
                    link,
                    break_block,
                    continue_block,
                )?;

                if lvalue_type != rvalue_type {
                    return Err(Error::AssignMismatch(loc));
                }

                self.translate_store(lvalue_access, rvalue_value, depth, builder, link);

                Ok((builder.ins().iconst(I64, 0), Type::Unit))
            }
            Expr::Nil { .. } => Ok((builder.ins().iconst(I64, 0), Type::Nil)),
            Expr::Array {
                loc,
                ref type_,
                ref size,
                ref init,
            } => {
                let type_ = *self.types.get(type_).ok_or(Error::UndefType(loc))?;
                let elem_type = match type_ {
                    Type::Array(id) => Ok(self.array_types.get(&id).unwrap().0),
                    _ => Err(Error::TypeNotArray(loc)),
                }?;

                let (size_value, size_type) =
                    self.handle_rvalue(&**size, depth, builder, link, break_block, continue_block)?;
                let (init_value, init_type) =
                    self.handle_rvalue(&**init, depth, builder, link, break_block, continue_block)?;

                if init_type != elem_type {
                    return Err(Error::ArrayInitMismatch(loc));
                }
                if size_type != Type::Integer {
                    return Err(Error::ArraySizeNotInteger(loc));
                }

                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));

                Ok((
                    self.translate_call(
                        "tiger_make_array",
                        &sig,
                        &[size_value, init_value],
                        builder,
                    ),
                    type_,
                ))
            }
            Expr::Index {
                loc,
                ref array,
                ref index,
            } => {
                let (addr, elem_type) = self.translate_index(
                    &**array,
                    &**index,
                    loc,
                    depth,
                    builder,
                    link,
                    break_block,
                    continue_block,
                )?;
                Ok((
                    self.translate_load(Access::Heap(addr), depth, builder, link),
                    elem_type,
                ))
            }
            Expr::Record {
                loc,
                ref type_,
                ref elems,
            } => {
                let type_ = *self.types.get(type_).ok_or(Error::UndefType(loc))?;
                let record_type = match type_ {
                    Type::Record(id) => Ok(self.record_types.get(&id).unwrap()),
                    _ => Err(Error::TypeNotRecord(loc)),
                }?
                .clone();

                let size = builder
                    .ins()
                    .iconst(I64, record_type.attr_types.len() as i64 * 8);
                let mut sig = self.module.make_signature();
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
                let base = self.translate_call("tiger_malloc", &sig, &[size], builder);

                let mut remained_attr_names = record_type
                    .attr_types
                    .keys()
                    .cloned()
                    .collect::<HashSet<String>>();
                for (attr, init) in elems.iter() {
                    let attr_type = *record_type
                        .attr_types
                        .get(attr)
                        .ok_or(Error::RecordInitMismatch(loc))?;
                    let attr_offset = *record_type.attr_offsets.get(attr).unwrap();
                    let (init_value, init_type) = self.handle_rvalue(
                        init,
                        depth,
                        builder,
                        link,
                        break_block,
                        continue_block,
                    )?;
                    if init_type != attr_type {
                        return Err(Error::RecordInitMismatch(loc));
                    }
                    let addr = builder.ins().iadd_imm(base, attr_offset);
                    self.translate_store(Access::Heap(addr), init_value, depth, builder, link);
                    remained_attr_names.remove(attr);
                }

                if !remained_attr_names.is_empty() {
                    return Err(Error::RecordInitMismatch(loc));
                }

                Ok((base, type_))
            }
            Expr::Attr {
                loc,
                ref record,
                ref attr,
            } => {
                let (addr, attr_type) = self.translate_attr(
                    &**record,
                    &attr[..],
                    loc,
                    depth,
                    builder,
                    link,
                    break_block,
                    continue_block,
                )?;
                Ok((
                    self.translate_load(Access::Heap(addr), depth, builder, link),
                    attr_type,
                ))
            }
            Expr::String { ref value, .. } => {
                let len = value.len() as u64;
                let mut contents = len.to_le_bytes().to_vec();
                contents.append(&mut value.as_bytes().to_vec());

                self.seq += 1;
                let name = format!("tiger_string{}", self.seq);

                let mut context = DataContext::new();
                context.define(contents.into_boxed_slice());
                let id = self
                    .module
                    .declare_data(&name[..], Linkage::Export, false, false)
                    .unwrap();
                self.module.define_data(id, &context).unwrap();

                let local_id = self.module.declare_data_in_func(id, &mut builder.func);

                Ok((builder.ins().symbol_value(I64, local_id), Type::String))
            }
            Expr::For {
                loc,
                ref cnt,
                esc,
                ref low,
                ref high,
                ref body,
            } => {
                let (low_value, low_type) =
                    self.handle_rvalue(&**low, depth, builder, link, break_block, continue_block)?;
                let (high_value, high_type) =
                    self.handle_rvalue(&**high, depth, builder, link, break_block, continue_block)?;

                if low_type != Type::Integer || high_type != Type::Integer {
                    return Err(Error::ForBoundNotInteger(loc));
                }

                let access = self.new_var(esc, depth, builder);

                let inc_block = builder.create_block();
                let body_block = builder.create_block();
                let check_block = builder.create_block();
                let exit_block = builder.create_block();

                self.vars.enter_scope();

                self.vars.insert(
                    cnt.clone(),
                    Var {
                        type_: Type::Integer,
                        access,
                        readonly: true,
                    },
                );

                self.translate_store(access, low_value, depth, builder, link);
                let tmp = builder
                    .ins()
                    .icmp(IntCC::SignedLessThanOrEqual, low_value, high_value);
                builder.ins().brz(tmp, exit_block, &[]);
                builder.ins().jump(body_block, &[]);
                builder.switch_to_block(body_block);
                self.handle_rvalue(
                    &**body,
                    depth,
                    builder,
                    link,
                    Some(exit_block),
                    Some(check_block),
                )?;
                builder.ins().jump(check_block, &[]);
                builder.switch_to_block(check_block);
                let cnt = self.translate_load(access, depth, builder, link);
                let tmp = builder.ins().icmp(IntCC::SignedLessThan, cnt, high_value);
                builder.ins().brnz(tmp, inc_block, &[]);
                builder.ins().jump(exit_block, &[]);
                builder.switch_to_block(inc_block);
                let cnt = self.translate_load(access, depth, builder, link);
                let new_cnt = builder.ins().iadd_imm(cnt, 1);
                self.translate_store(access, new_cnt, depth, builder, link);
                builder.ins().jump(body_block, &[]);
                builder.switch_to_block(exit_block);

                self.vars.exit_scope();

                builder.seal_block(inc_block);
                builder.seal_block(body_block);
                builder.seal_block(check_block);
                builder.seal_block(exit_block);

                Ok((builder.ins().iconst(I64, 0), Type::Unit))
            }
            Expr::While {
                loc,
                ref test,
                ref body,
            } => {
                let check_block = builder.create_block();
                let body_block = builder.create_block();
                let exit_block = builder.create_block();

                builder.ins().jump(check_block, &[]);
                builder.switch_to_block(check_block);
                let (test_value, test_type) =
                    self.handle_rvalue(&**test, depth, builder, link, break_block, continue_block)?;
                builder.ins().brz(test_value, exit_block, &[]);
                builder.ins().jump(body_block, &[]);
                builder.switch_to_block(body_block);
                self.handle_rvalue(
                    &**body,
                    depth,
                    builder,
                    link,
                    Some(exit_block),
                    Some(check_block),
                )?;
                builder.ins().jump(check_block, &[]);
                builder.switch_to_block(exit_block);

                builder.seal_block(check_block);
                builder.seal_block(body_block);
                builder.seal_block(exit_block);

                if test_type != Type::Integer {
                    return Err(Error::WhileTestNotInteger(loc));
                }

                Ok((builder.ins().iconst(I64, 0), Type::Unit))
            }
            Expr::If {
                loc,
                ref test,
                ref then,
                ref orelse,
            } => {
                let (test_value, test_type) =
                    self.handle_rvalue(&**test, depth, builder, link, break_block, continue_block)?;

                if test_type != Type::Integer {
                    return Err(Error::IfTestNotInteger(loc));
                }

                let then_block = builder.create_block();
                let orelse_block = builder.create_block();
                let merge_block = builder.create_block();

                builder.append_block_param(merge_block, I64);

                builder.ins().brnz(test_value, then_block, &[]);
                builder.ins().jump(orelse_block, &[]);
                builder.switch_to_block(then_block);
                let (then_value, then_type) =
                    self.handle_rvalue(&**then, depth, builder, link, break_block, continue_block)?;
                builder.ins().jump(merge_block, &[then_value]);

                builder.switch_to_block(orelse_block);
                let (orelse_value, orelse_type) = self.handle_rvalue(
                    &**orelse,
                    depth,
                    builder,
                    link,
                    break_block,
                    continue_block,
                )?;
                builder.ins().jump(merge_block, &[orelse_value]);
                builder.switch_to_block(merge_block);

                builder.seal_block(then_block);
                builder.seal_block(orelse_block);
                builder.seal_block(merge_block);

                if then_type != orelse_type {
                    return Err(Error::IfMismatch(loc));
                }

                Ok((builder.block_params(merge_block)[0], then_type))
            }
            Expr::Break { loc } => match break_block {
                Some(break_block) => {
                    builder.ins().jump(break_block, &[]);
                    let unreachable_block = builder.create_block();
                    builder.seal_block(unreachable_block);
                    builder.switch_to_block(unreachable_block);
                    Ok((builder.ins().iconst(I64, 0), Type::Unit))
                }
                None => Err(Error::BreakOutsideLoop(loc)),
            },
            Expr::Continue { loc } => match continue_block {
                Some(continue_block) => {
                    builder.ins().jump(continue_block, &[]);
                    let unreachable_block = builder.create_block();
                    builder.seal_block(unreachable_block);
                    builder.switch_to_block(unreachable_block);
                    Ok((builder.ins().iconst(I64, 0), Type::Unit))
                }
                None => Err(Error::ContinueOutsideLoop(loc)),
            },
        }
    }

    fn handle_lvalue(
        &mut self,
        lvalue: &Expr,
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
        break_block: Option<Block>,
        continue_block: Option<Block>,
    ) -> Result<(Access, Type), Error> {
        match *lvalue {
            Expr::Ident { loc, ref ident } => {
                let var = self.vars.get(ident).ok_or(Error::UndefVar(loc))?;

                if var.readonly {
                    return Err(Error::AssignToReadonly(loc));
                }

                Ok((var.access, var.type_))
            }
            Expr::Index {
                loc,
                ref array,
                ref index,
            } => {
                let (addr, elem_type) = self.translate_index(
                    &**array,
                    &**index,
                    loc,
                    depth,
                    builder,
                    link,
                    break_block,
                    continue_block,
                )?;
                Ok((Access::Heap(addr), elem_type))
            }
            Expr::Attr {
                loc,
                ref record,
                ref attr,
            } => {
                let (addr, attr_type) = self.translate_attr(
                    &**record,
                    &attr[..],
                    loc,
                    depth,
                    builder,
                    link,
                    break_block,
                    continue_block,
                )?;
                Ok((Access::Heap(addr), attr_type))
            }
            Expr::Array { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Assign { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::BinOp { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Break { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Call { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Continue { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Empty { loc } => Err(Error::ExprNotLvalue(loc)),
            Expr::For { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::If { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Integer { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Let { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Nil { loc } => Err(Error::ExprNotLvalue(loc)),
            Expr::Record { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::Seq { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::String { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::UnOp { loc, .. } => Err(Error::ExprNotLvalue(loc)),
            Expr::While { loc, .. } => Err(Error::ExprNotLvalue(loc)),
        }
    }

    fn handle_defs(
        &mut self,
        defs: &[Def],
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
        break_block: Option<Block>,
        continue_block: Option<Block>,
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
            ref ret_type,
            ..
        } = defs[tail]
        {
            let mut func: Func = Default::default();
            self.seq += 1;
            func.name = format!("tiger_{}{}", ident, self.seq);
            for (_, param_type) in params.iter() {
                func.param_types
                    .push(*self.types.get(param_type).ok_or(Error::UndefType(loc))?);
            }
            func.ret_type = match *ret_type {
                Some(ref ret_type) => *self.types.get(ret_type).ok_or(Error::UndefType(loc))?,
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
                .zip(func.param_types.iter())
                .zip(escs.iter())
                .enumerate()
            {
                let arg = builder.block_params(entry_block)[i + 1];
                let access = self.new_var(*esc, depth + 1, &mut builder);
                self.translate_store(access, arg, depth + 1, &mut builder, link);
                self.vars.insert(
                    ident.clone(),
                    Var {
                        type_: *type_,
                        access,
                        readonly: false,
                    },
                );
            }

            let (ret_value, ret_type) =
                self.handle_rvalue(&**body, depth + 1, &mut builder, link, None, None)?;

            self.vars.exit_scope();

            if ret_type != func.ret_type {
                return Err(Error::RetMismatch(loc));
            }

            builder.ins().return_(&[ret_value]);
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
            return self.handle_defs(
                &defs[tail..],
                depth,
                builder,
                link,
                break_block,
                continue_block,
            );
        }

        let mut tail = 0;
        let mut aliases = HashMap::new();
        let mut alias_locs = HashMap::new();
        let mut arrays = HashMap::new();
        let mut array_locs = HashMap::new();
        let mut records = HashMap::new();
        let mut record_locs = HashMap::new();
        while let Def::Type {
            ref ident,
            ref type_,
            ..
        } = defs[tail]
        {
            match **type_ {
                ASTType::Alias { loc, ref type_, .. } => {
                    aliases.insert(ident.clone(), type_.clone());
                    alias_locs.insert(ident.clone(), loc);
                }
                ASTType::Array {
                    loc, ref elem_type, ..
                } => {
                    self.seq += 1;
                    arrays.insert(self.seq, elem_type.clone());
                    array_locs.insert(self.seq, loc);
                    self.types.insert(ident.clone(), Type::Array(self.seq));
                    aliases.remove(ident);
                }
                ASTType::Record { loc, ref attrs, .. } => {
                    self.seq += 1;
                    records.insert(
                        self.seq,
                        attrs.iter().cloned().collect::<HashMap<String, String>>(),
                    );
                    record_locs.insert(self.seq, loc);
                    self.types.insert(ident.clone(), Type::Record(self.seq));
                    aliases.remove(ident);
                }
            }
            tail += 1;
            if tail == defs.len() {
                break;
            }
        }

        let mut actuals = HashMap::new();
        for (ident, alias) in aliases.iter() {
            let loc = *alias_locs.get(ident).unwrap();
            let mut p = ident.clone();
            let mut q = alias.clone();
            let type_ = loop {
                p = aliases.get(&p).unwrap_or(&p).clone();
                q = aliases.get(&q).unwrap_or(&q).clone();
                q = aliases.get(&q).unwrap_or(&q).clone();
                if let Some(type_) = self.types.get(&q) {
                    if !aliases.contains_key(&q) {
                        break Ok(*type_);
                    }
                }
                if p == q {
                    break Err(Error::AliasCycle(loc));
                }
            }?;
            actuals.insert(ident.clone(), type_);
        }
        for (ident, type_) in actuals {
            self.types.insert(ident, type_);
        }

        for (&id, elem_type) in arrays.iter() {
            let loc = *array_locs.get(&id).unwrap();
            let elem_type = *self.types.get(elem_type).ok_or(Error::UndefType(loc))?;
            self.array_types.insert(id, ArrayType(elem_type));
        }

        for (&id, attr_types) in records.iter() {
            let loc = *record_locs.get(&id).unwrap();
            let mut record_type = RecordType {
                attr_types: HashMap::new(),
                attr_offsets: HashMap::new(),
            };
            let mut offset = 0;
            for (attr, attr_type) in attr_types.iter() {
                let attr_type = self.types.get(attr_type).ok_or(Error::UndefType(loc))?;
                record_type.attr_types.insert(attr.clone(), *attr_type);
                if !record_type.attr_offsets.contains_key(attr) {
                    record_type.attr_offsets.insert(attr.clone(), offset);
                    offset += 8;
                }
            }
            self.record_types.insert(id, record_type);
        }

        if tail > 0 {
            return self.handle_defs(
                &defs[tail..],
                depth,
                builder,
                link,
                break_block,
                continue_block,
            );
        }

        if let Def::Var {
            loc,
            ref ident,
            esc,
            ref type_,
            ref init,
        } = defs[0]
        {
            let (init_value, init_type) =
                self.handle_rvalue(&**init, depth, builder, link, break_block, continue_block)?;

            match *type_ {
                Some(ref type_) => {
                    let type_ = *self.types.get(type_).ok_or(Error::UndefType(loc))?;
                    if type_ != init_type {
                        return Err(Error::VarInitMismatch(loc));
                    }
                }
                None => {
                    if let Type::Nil = init_type {
                        return Err(Error::NilVarInitWithoutType(loc));
                    }
                }
            }

            let access = self.new_var(esc, depth, builder);
            self.translate_store(access, init_value, depth, builder, link);

            self.vars.insert(
                ident.clone(),
                Var {
                    type_: init_type,
                    access,
                    readonly: false,
                },
            );
        }

        self.handle_defs(
            &defs[1..],
            depth,
            builder,
            link,
            break_block,
            continue_block,
        )
    }

    fn translate_call(
        &mut self,
        name: &str,
        sig: &Signature,
        args: &[Value],
        builder: &mut FunctionBuilder,
    ) -> Value {
        let callee = self
            .module
            .declare_function(name, Linkage::Import, sig)
            .unwrap();
        let local_callee = self.module.declare_func_in_func(callee, &mut builder.func);

        let call = builder.ins().call(local_callee, args);
        builder.inst_results(call)[0]
    }

    fn translate_index(
        &mut self,
        array: &Expr,
        index: &Expr,
        loc: (usize, usize),
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
        break_block: Option<Block>,
        continue_block: Option<Block>,
    ) -> Result<(Value, Type), Error> {
        let (array_value, array_type) =
            self.handle_rvalue(array, depth, builder, link, break_block, continue_block)?;
        let (index_value, index_type) =
            self.handle_rvalue(index, depth, builder, link, break_block, continue_block)?;

        let elem_type = match array_type {
            Type::Array(id) => Ok(self.array_types.get(&id).unwrap().0),
            _ => Err(Error::ExprNotArray(loc)),
        }?;
        if index_type != Type::Integer {
            return Err(Error::IndexNotInteger(loc));
        }

        let offset = builder.ins().imul_imm(index_value, 8);
        Ok((builder.ins().iadd(array_value, offset), elem_type))
    }

    fn translate_attr(
        &mut self,
        record: &Expr,
        attr: &str,
        loc: (usize, usize),
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
        break_block: Option<Block>,
        continue_block: Option<Block>,
    ) -> Result<(Value, Type), Error> {
        let (record_value, record_type) =
            self.handle_rvalue(record, depth, builder, link, break_block, continue_block)?;

        let record_type = match record_type {
            Type::Record(id) => Ok(self.record_types.get(&id).unwrap()),
            _ => Err(Error::ExprNotRecord(loc)),
        }?;

        let attr_type = *record_type
            .attr_types
            .get(attr)
            .ok_or(Error::AttrNotFound(loc))?;
        let attr_offset = *record_type.attr_offsets.get(attr).unwrap();
        Ok((builder.ins().iadd_imm(record_value, attr_offset), attr_type))
    }

    fn new_var(&mut self, esc: bool, depth: i32, builder: &mut FunctionBuilder) -> Access {
        if esc {
            let slot =
                builder.create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
            Access::Stack(depth, slot)
        } else {
            self.seq += 1;
            let var = Variable::with_u32(self.seq as u32);
            builder.declare_var(var, I64);
            Access::Temporary(var)
        }
    }

    fn walk_link(&self, delta: i32, builder: &mut FunctionBuilder, link: StackSlot) -> Value {
        let link_addr = builder.ins().stack_addr(I64, link, 0);
        let mut fp = builder.ins().iadd_imm(link_addr, 8);
        for _ in 0..delta {
            fp = builder.ins().load(I64, MemFlags::new(), fp, -8);
        }
        fp
    }

    fn translate_load(
        &self,
        access: Access,
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
    ) -> Value {
        match access {
            Access::Temporary(var) => builder.use_var(var),
            Access::Stack(access_depth, slot) if access_depth == depth => {
                builder.ins().stack_load(I64, slot, 0)
            }
            Access::Stack(access_depth, slot) => {
                let fp = self.walk_link(depth - access_depth, builder, link);
                let offset = -(slot.as_u32() as i32 + 1) * 8;
                builder.ins().load(I64, MemFlags::new(), fp, offset)
            }
            Access::Heap(addr) => builder.ins().load(I64, MemFlags::new(), addr, 0),
        }
    }

    fn translate_store(
        &self,
        access: Access,
        value: Value,
        depth: i32,
        builder: &mut FunctionBuilder,
        link: StackSlot,
    ) {
        match access {
            Access::Temporary(var) => {
                builder.def_var(var, value);
            }
            Access::Stack(access_depth, slot) if access_depth == depth => {
                builder.ins().stack_store(value, slot, 0);
            }
            Access::Stack(access_depth, slot) => {
                let fp = self.walk_link(depth - access_depth, builder, link);
                let offset = -(slot.as_u32() as i32 + 1) * 8;
                builder.ins().store(MemFlags::new(), value, fp, offset);
            }
            Access::Heap(addr) => {
                builder.ins().store(MemFlags::new(), value, addr, 0);
            }
        }
    }

    fn translate_rel_op(&self, op: BinOp) -> IntCC {
        match op {
            BinOp::Eq => IntCC::Equal,
            BinOp::NE => IntCC::NotEqual,
            BinOp::LT => IntCC::SignedLessThan,
            BinOp::LE => IntCC::SignedLessThanOrEqual,
            BinOp::GT => IntCC::SignedGreaterThan,
            BinOp::GE => IntCC::SignedGreaterThanOrEqual,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy)]
enum Type {
    Integer,
    String,
    Unit,
    Nil,
    Array(i32),
    Record(i32),
}

impl Default for Type {
    fn default() -> Self {
        Type::Unit
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Type::Integer, Type::Integer) => true,
            (Type::String, Type::String) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Nil, Type::Nil) => false,
            (Type::Nil, Type::Array(_)) => true,
            (Type::Array(_), Type::Nil) => true,
            (Type::Array(id), Type::Array(other_id)) => id == other_id,
            (Type::Nil, Type::Record(_)) => true,
            (Type::Record(_), Type::Nil) => true,
            (Type::Record(id), Type::Record(other_id)) => id == other_id,
            _ => false,
        }
    }
}

struct ArrayType(Type);

#[derive(Clone)]
struct RecordType {
    attr_types: HashMap<String, Type>,
    attr_offsets: HashMap<String, i64>,
}

#[derive(Clone, Default)]
struct Func {
    name: String,
    param_types: Vec<Type>,
    ret_type: Type,
    depth: i32,
}

impl Func {
    fn translate(&self, sig: &mut Signature) {
        if self.depth > 0 {
            sig.params.push(AbiParam::new(I64));
        }
        for _ in self.param_types.iter() {
            sig.params.push(AbiParam::new(I64));
        }
        sig.returns.push(AbiParam::new(I64));
    }
}

#[derive(Clone, Copy)]
enum Access {
    Temporary(Variable),
    Stack(i32, StackSlot),
    Heap(Value),
}

struct Var {
    type_: Type,
    access: Access,
    readonly: bool,
}
