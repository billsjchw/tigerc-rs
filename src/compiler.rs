use crate::{
    ast::{BinOp, Def, Expr, Type as ASTType},
    error::Error,
    parser,
    symtab::SymbolTable,
};
use cranelift::{
    codegen::{
        binemit::NullTrapSink,
        ir::{
            types::{Type as IRType, *},
            StackSlot,
        },
        isa,
    },
    frontend::{FunctionBuilder, FunctionBuilderContext},
    prelude::*,
};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::{collections::HashMap, str::FromStr, todo};
use target_lexicon::triple;

pub struct Compiler {
    module: ObjectModule,
    funcs: SymbolTable<String, Func>,
    types: SymbolTable<String, Type>,
    vars: SymbolTable<String, Var>,
    arrays: HashMap<i32, Type>,
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
        funcs.insert(
            String::from("print"),
            Func {
                name: String::from("print"),
                params: vec![Type::String],
                ret: Type::Unit,
                depth: 0,
            },
        );

        let mut types = SymbolTable::new();
        types.insert(String::from("int"), Type::Integer);
        types.insert(String::from("string"), Type::String);

        let vars = SymbolTable::new();
        let arrays = HashMap::new();

        Self {
            module,
            funcs,
            types,
            vars,
            arrays,
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
        let ret = builder.ins().iconst(I64, 0);
        builder.ins().return_(&[ret]);
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
                    arg_values.push(self.walk_link(depth - func.depth + 1, builder, link));
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

                Ok((
                    self.translate_call(&func.name[..], &sig, &arg_values, builder),
                    func.ret,
                ))
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
                let (lvalue_access, lvalue_type) =
                    self.handle_lvalue(&**lvalue, depth, builder, link)?;
                let (rvalue_value, rvalue_type) =
                    self.handle_rvalue(&**rvalue, depth, builder, link)?;

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
                let elem = match type_ {
                    Type::Array(id) => Ok(*self.arrays.get(&id).unwrap()),
                    _ => Err(Error::TypeNotArray(loc)),
                }?;

                let (size_value, size_type) = self.handle_rvalue(&**size, depth, builder, link)?;
                let (init_value, init_type) = self.handle_rvalue(&**init, depth, builder, link)?;

                if init_type != elem {
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
                    self.translate_call("make_array", &sig, &[size_value, init_value], builder),
                    type_,
                ))
            }
            Expr::Index {
                loc,
                ref array,
                ref index,
            } => {
                let (addr, elem) =
                    self.translate_index(&**array, &**index, loc, depth, builder, link)?;
                Ok((
                    self.translate_load(Access::Heap(addr), depth, builder, link),
                    elem,
                ))
            }
            Expr::String { ref value, .. } => {
                let len = value.len() as u64;
                let mut contents = len.to_le_bytes().to_vec();
                contents.append(&mut value.as_bytes().to_vec());

                self.seq += 1;
                let name = format!("string{}", self.seq);

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
            _ => todo!(),
        }
    }

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
            Expr::Index {
                loc,
                ref array,
                ref index,
            } => {
                let (addr, elem) =
                    self.translate_index(&**array, &**index, loc, depth, builder, link)?;
                Ok((Access::Heap(addr), elem))
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
                let access = self.new_var(*esc, depth + 1, &mut builder);
                self.translate_store(access, arg, depth + 1, &mut builder, link);
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

        let mut tail = 0;
        let mut aliases = HashMap::new();
        let mut alias_locs = HashMap::new();
        let mut arrays = HashMap::new();
        let mut array_locs = HashMap::new();
        while let Def::Type {
            loc,
            ref ident,
            ref type_,
            ..
        } = defs[tail]
        {
            match **type_ {
                ASTType::Alias { ref type_, .. } => {
                    aliases.insert(ident.clone(), type_.clone());
                    alias_locs.insert(ident.clone(), loc);
                }
                ASTType::Array { ref elem, .. } => {
                    self.seq += 1;
                    arrays.insert(self.seq, elem.clone());
                    array_locs.insert(self.seq, loc);
                    self.types.insert(ident.clone(), Type::Array(self.seq));
                    aliases.remove(ident);
                }
                _ => todo!(),
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

        for (id, elem) in arrays.iter() {
            let loc = *array_locs.get(id).unwrap();
            let elem = self.types.get(elem).ok_or(Error::UndefType(loc))?;
            self.arrays.insert(*id, *elem);
        }

        if tail > 0 {
            return self.handle_defs(&defs[tail..], depth, builder, link);
        }

        if let Def::Var {
            loc,
            ref ident,
            esc,
            ref type_,
            ref init,
        } = defs[0]
        {
            let (init_value, init_type) = self.handle_rvalue(&**init, depth, builder, link)?;

            if let Some(ref type_) = *type_ {
                let type_ = *self.types.get(type_).ok_or(Error::UndefType(loc))?;
                if type_ != init_type {
                    return Err(Error::VarInitMismatch(loc));
                }
            }

            let access = self.new_var(esc, depth, builder);
            self.translate_store(access, init_value, depth, builder, link);

            self.vars.insert(
                ident.clone(),
                Var {
                    type_: init_type,
                    access,
                },
            );
        }

        self.handle_defs(&defs[1..], depth, builder, link)
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
    ) -> Result<(Value, Type), Error> {
        let (array_value, array_type) = self.handle_rvalue(array, depth, builder, link)?;
        let (index_value, index_type) = self.handle_rvalue(index, depth, builder, link)?;

        let elem = match array_type {
            Type::Array(id) => Ok(*self.arrays.get(&id).unwrap()),
            _ => Err(Error::ArrayNotArray(loc)),
        }?;
        if index_type != Type::Integer {
            return Err(Error::IndexNotInteger(loc));
        }

        let offset = builder.ins().imul_imm(index_value, 8);
        Ok((builder.ins().iadd(array_value, offset), elem))
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
}

#[derive(Clone, Copy)]
enum Type {
    Integer,
    String,
    Unit,
    Nil,
    Array(i32),
}

impl Type {
    fn translate(&self) -> IRType {
        match *self {
            Type::Integer => I64,
            Type::String => I64,
            Type::Unit => I64,
            Type::Nil => I64,
            Type::Array(_) => I64,
        }
    }
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
            _ => false,
        }
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
    Heap(Value),
}

struct Var {
    type_: Type,
    access: Access,
}
