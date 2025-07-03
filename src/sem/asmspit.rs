use clap::error::ContextKind;

use crate::{
    common::ArchInt,
    sem::typechk::Operator,
    syn::tree::{AstNode, NodeKind, Virtual},
};

use super::{
    symtab::{Symbol, SymbolTable},
    typechk::{operator_check, BuiltinTypes, SymbolMajorType, ValueKind},
};

pub fn mangle_symbol(sym: &Symbol) -> String {
    let scope = sym.scope;
    let gen = sym.generation;
    let ident = &sym.ident.clone();

    if sym.stype.is_lambda() == false {
        format!("_Z{}{}_{scope}_{gen}", ident.len(), *ident)
    } else {
        sym.ident.clone()
    }
}

pub struct AssemblySpitter<'a> {
    program: String,
    syms: &'a SymbolTable,
    branchgen: &'a mut usize,
    //    tree: &'a Box<AstNode>,
}

impl<'a> AssemblySpitter<'a> {
    pub fn new(syms: &'a SymbolTable, branchgen: &'a mut usize) -> Self {
        AssemblySpitter {
            syms,
            program: String::default(),
            branchgen,
            //          tree,
        }
    }
    pub fn spit_defines(mut self) -> Self {
        self.program += "SYS_EXIT equ 0x3c\n";
        return self;
    }
    pub fn spit_globals(mut self) -> Self {
        self.program += "section .bss\n";
        for symbol in self.syms.get_all_syms() {
            let mut size = ArchInt::BITS / 8; // TODO
            if symbol.stype.is_array() {
                size *= symbol.stype.get_array_quant().unwrap() as u32;
            }
            if symbol.stype.is_lambda() {
                self.program += format!("\t ; {} is a function\n", symbol.ident).as_str();
            } else {
                self.program += format!("\t{}:\tresb\t{size}\n", mangle_symbol(symbol)).as_str();
            }
        }
        for i in 0..8 {
            self.program += format!("\t_funcall_arg{i}\tresb\t8\n").as_str();
        }
        for i in 0..8 {
            self.program += format!("\tglobal _funcall_arg{i}\n").as_str();
        }
        return self;
    }
    pub fn next_branchgen(&mut self) -> usize {
        let res = *self.branchgen;
        *self.branchgen += 1;
        return res;
    }
    pub fn venture_ifexpr(&mut self, node: &Box<AstNode>) {
        let condition = node.follow_line2(1, 0);
        let block = node.follow_line2(1, 1);

        let cvk = self.venture_expression(condition).unwrap();

        if matches!(cvk, ValueKind::LvalueRef(_)) {
            self.program += "\n\t\tmov rax, qword [rax]\n";
        }

        let brn = self.next_branchgen();
        let bre = self.next_branchgen();

        self.program += format!("\n\t\tcmp rax, 0\n\t\tje _if_next{}\n\n", brn).as_str();

        self.venture_expression(block);

        self.program += format!("\n\t\tjmp _if_end{}\n\n", bre).as_str();

        self.program += format!("\n_if_next{}:\t\t\n\n", brn).as_str();

        if node.get_children().len() == 3 {
            let nextpart = node.follow_line2(1, 2).follow_line(1);
            match nextpart.get_kind().to_owned().some_virt().unwrap() {
                Virtual::Scope => {
                    self.venture_expression(nextpart);
                }
                Virtual::IfExpr => {
                    self.venture_ifexpr(nextpart);
                }
                _ => panic!(),
            }
        }

        self.program += format!("_if_end{}:\t\t\n", bre).as_str();
    }
    pub fn venture_whilexpr(&mut self, node: &Box<AstNode>) {
        let condition = node.follow_line2(1, 0);
        let block = node.follow_line2(1, 1);

        let wlo = self.next_branchgen();
        let wen = self.next_branchgen();

        self.program += format!("\t_while_loop{}:\n", wlo).as_str();

        let cvk = self.venture_expression(condition).unwrap();

        if matches!(cvk, ValueKind::LvalueRef(_)) {
            self.program += "\n\t\tmov rax, qword [rax]\n";
        }

        self.program += format!("\n\t\tcmp rax, 0\n\t\tje _while_end{}\n", wen).as_str();

        self.venture_expression(block);

        self.program += format!("\t\tjmp _while_loop{}\n", wlo).as_str();

        self.program += format!("\t_while_end{}:\n", wen).as_str();
    }
    pub fn venture_wrapped_term(
        &mut self,
        node: &Box<AstNode>,
        vk: &ValueKind,
    ) -> Option<ValueKind> {
        match vk {
            ValueKind::Rvalue(SymbolMajorType::Builtin(BuiltinTypes::Unoperable)) => {
                if matches!(
                    node.follow_line(1).get_kind(),
                    NodeKind::Virt(Virtual::IfExpr)
                ) {
                    let ifroot = node.follow_line(1);
                    self.venture_ifexpr(ifroot);
                } else if matches!(
                    node.follow_line(1).get_kind(),
                    NodeKind::Virt(Virtual::WhileExpr)
                ) {
                    let whileroot = node.follow_line(1);
                    self.venture_whilexpr(whileroot);
                } else {
                    self.program += "\n\t\t; Expr() -> Unoperable\n\n";
                }
                return Some(vk.clone());
            }
            ValueKind::Rvalue(_) => {
                let grandkid = node
                    .follow_line(1)
                    .get_kind()
                    .clone()
                    .some_lex()
                    .unwrap()
                    .get_token()
                    .some_number()
                    .or(Some(&0i64)) // NOTE: Workaround for unit type
                    .unwrap()
                    .clone();
                self.program += format!("\t\tmov rax, {}\n", grandkid).as_str();
                return Some(vk.clone());
            }
            ValueKind::Lvalue(tp, gen) => {
                self.program += format!(
                    "\t\tlea rax, qword [{}]\n",
                    mangle_symbol(self.syms.get_by_gen(*gen).unwrap())
                )
                .as_str();
                return Some(ValueKind::LvalueRef(tp.clone()));
            }
            _ => panic!("c"),
        }
    }
    pub fn venture_expression(&mut self, node: &Box<AstNode>) -> Option<ValueKind> {
        if matches!(
            node.get_kind(),
            NodeKind::TypedVirt(Virtual::GenericExpression, _)
        ) {
            /*
            if node.get_children().len() == 2 {
                panic!("internal compiler error: only binary operators supported.");
            }
            */

            match node.get_children().len() {
                3 => {
                    let opr = node.follow_line2(1, 1);

                    let opr = Operator::from(
                        opr.follow_line(1)
                            .get_kind()
                            .to_owned()
                            .some_lex()
                            .unwrap()
                            .get_token(),
                    );

                    let (lhs, rhs) = {
                        let lhs = node.follow_line2(1, 0);
                        let rhs = node.follow_line2(1, 2);
                        if matches!(opr, Operator::AssignOptr) {
                            (rhs, lhs)
                        } else {
                            (lhs, rhs)
                        }
                    };

                    let lhstype = self.venture_expression(lhs).unwrap();
                    self.program += "\t\tpush rax\n\n";

                    let rhstype = self.venture_expression(rhs).unwrap();
                    self.program += "\t\tmov rbx, rax\n\n\t\tpop rax\n\n";

                    if matches!(lhstype, ValueKind::LvalueRef(_)) {
                        self.program += "\n\t\tmov rax, qword [rax]\n";
                    } else if matches!(lhstype, ValueKind::Lvalue(_, _)) {
                        eprintln!("warning: impossible lvalue.");
                        self.program += "\n\t\tmov rax, qword [rax]\n";
                    }

                    if !matches!(opr, Operator::AssignOptr) {
                        if matches!(rhstype, ValueKind::LvalueRef(_)) {
                            self.program += "\n\t\tmov rbx, qword [rbx]\n";
                        } else if matches!(rhstype, ValueKind::Lvalue(_, _)) {
                            eprintln!("warning: impossible lvalue.");
                            self.program += "\n\t\tmov rbx, qword [rbx]\n";
                        }
                    }

                    match opr {
                        Operator::AddOptr => {
                            self.program += "\t\tadd rax, rbx\n\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::SubOptr => {
                            self.program += "\t\tsub rax, rbx\n\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::BitAndOptr => {
                            self.program += "\t\tand rax, rbx\n\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::BitOrOptr => {
                            self.program += "\t\tor  rax, rbx\n\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::BitXorOptr => {
                            self.program += "\t\txor rax, rbx\n\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::RShiftOptr => {
                            self.program += "\t\tshr rax, rbx\n\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::LShiftOptr => {
                            self.program += "\t\tshl rax, rbx\n\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::AssignOptr => {
                            // The assign operator swaps operand order
                            self.program += "\t\tmov qword [rbx], rax\n\n";
                            Some(operator_check(&rhstype, &lhstype, &opr).unwrap())
                        }
                        Operator::Brack => {
                            self.program += "\t\tlea rax, qword [rax + rbx]\n\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::EqualsOptr => {
                            /*
                            cmp     edi, esi         ; compare a (edi) to b (esi)
                            sete    al               ; al = 1 if edi == esi
                            movzx   eax, al          ; zero-extend al into eax
                            */
                            self.program += "\t\tcmp rax, rbx\n\t\tsete al\n\t\tmovzx rax, al\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::NeqOptr => {
                            self.program += "\t\tcmp rax, rbx\n\t\tsetne al\n\t\tmovzx rax, al\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                        Operator::LtOptr => {
                            self.program += "\t\tcmp rax, rbx\n\t\tsetl al\n\t\tmovzx rax, al\n";
                            Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                        }
                    }
                }
                2 => {
                    let oprn = node.follow_line2(1, 1);

                    if !matches!(oprn.get_kind(), NodeKind::Virt(Virtual::Brack)) {
                        panic!("internal compiler error: non-brack operator");
                    }

                    let opr = Operator::Brack;

                    let lhs = node.follow_line2(1, 0);
                    let rhs = oprn.follow_line(1);

                    let lhstype = self.venture_expression(lhs).unwrap();
                    self.program += "\t\tpush rax\n\n";

                    let rhstype = self.venture_expression(rhs).unwrap();
                    self.program += "\t\tmov rbx, rax\n\n\t\tpop rax\n\n";

                    if matches!(rhstype, ValueKind::LvalueRef(_)) {
                        self.program += "\n\t\tmov rbx, qword [rbx]\n";
                    } else if matches!(rhstype, ValueKind::Lvalue(_, _)) {
                        eprintln!("warning: impossible lvalue.");
                        self.program += "\n\t\tmov rbx, qword [rbx]\n";
                    }

                    self.program += "\t\tlea rax, qword [rax + rbx * 8]\n\n";
                    Some(operator_check(&lhstype, &rhstype, &opr).unwrap())
                }
                1 => {
                    let kid = node.follow_line(1);
                    match kid.get_kind() {
                        NodeKind::TypedVirt(Virtual::WrappedTerm, vk) => {
                            self.venture_wrapped_term(kid, vk)
                        }
                        _ => {
                            /*
                             * At this point I have no idea wher the control flow is going,
                             * but this is where the function call stuff happens, apparently.
                             */

                            let Some((v, vk)) = kid.get_kind().to_owned().some_typed() else {
                                todo!(
                                    "internal compiler error: expression lists not yet implemented"
                                )
                            };

                            if !matches!(v, Virtual::Application) {
                                todo!(
                                    "internal compiler error: expression lists not yet implemented"
                                )
                            }

                            eprintln!("warning: function call");
                            self.program += "\t\t; funcall \n";

                            for (i, todd) in kid.get_children().iter().skip(1).enumerate() {
                                let todd_vk = &todd.get_kind().to_owned().some_typed().unwrap().1;
                                let todd_vk = match todd.get_kind() {
                                    NodeKind::TypedVirt(Virtual::GenericExpression, _) => {
                                        self.venture_expression(todd).unwrap()
                                    }
                                    NodeKind::TypedVirt(Virtual::WrappedTerm, _) => {
                                        self.venture_wrapped_term(todd, todd_vk).unwrap()
                                    }
                                    _ => panic!(),
                                };

                                if matches!(todd_vk, ValueKind::LvalueRef(_)) {
                                    self.program += "\n\t\tmov rax, qword [rax]\n";
                                }
                                self.program +=
                                    format!("\n\t\tmov qword [_funcall_arg{i}], rax\n\n").as_str();
                            }

                            let func_gen = &kid.get_children()[0];

                            let func_gen_vk =
                                &func_gen.get_kind().to_owned().some_typed().unwrap().1;

                            assert!(!matches!(func_gen_vk, ValueKind::Rvalue(_)));
                            self.venture_wrapped_term(func_gen, func_gen_vk).unwrap();

                            self.program += format!("\t\tcall rax\n").as_str();

                            self.program += "\t\t;\n";

                            return Some(vk);
                        }
                    }
                }
                _ => panic!("a"),
            }
        } else {
            for c in node.get_children() {
                if c.get_kind()
                    .to_owned()
                    .some_virt()
                    .is_some_and(|v| matches!(v, Virtual::Return))
                {
                    let evk = self.venture_expression(c.follow_line(1)).unwrap();

                    if matches!(evk, ValueKind::LvalueRef(_)) {
                        self.program += "\n\t\tmov rax, qword [rax]\n";
                    } else if matches!(evk, ValueKind::Lvalue(_, _)) {
                        eprintln!("warning: impossible lvalue.");
                        self.program += "\n\t\tmov rax, qword [rax]\n";
                    }

                    self.program += "\t\tret\n";
                } else {
                    self.venture_expression(c);
                }
            }
            None
        }
    }
    /*
        pub fn spit_global_routine(mut self) -> Self {
            self.program += "
    section .text
        global _start

    _start:
    ";

            self.venture_expression(self.tree);

            self.program += "
                    xor rdi, rdi
                    mov rax, SYS_EXIT
                    syscall
            ";
            return self;
        }
        */
    pub fn get_program(self) -> String {
        self.program
    }
}

pub struct Compiler<'a> {
    syms: &'a SymbolTable,
    preamble: String,
    functions_s: Vec<String>,
    functions_t: Vec<(String, &'a Box<AstNode>, usize)>,
    branchgen: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(syms: &'a SymbolTable, functions_t: Vec<(String, &'a Box<AstNode>, usize)>) -> Self {
        Compiler {
            syms,
            preamble: String::new(),
            functions_s: Vec::new(),
            functions_t,
            branchgen: 0usize,
        }
    }
    pub fn compile(&mut self) {
        self.preamble = AssemblySpitter::new(self.syms, &mut self.branchgen)
            .spit_defines()
            .spit_globals()
            .get_program();
        for (name, code, gen) in &self.functions_t {
            let mut res = String::new();

            let mut asmspit = AssemblySpitter::new(self.syms, &mut self.branchgen);
            asmspit.venture_expression(code);

            res += format!("\n{name}:").as_str();

            let SymbolMajorType::Lambda {
                args: _,
                ret: _,
                argsym,
            } = self.syms.get_by_gen(*gen).unwrap().stype.clone()
            else {
                panic!();
            };

            for (i, arg) in argsym.iter().enumerate() {
                let actual_argsym = self.syms.get_by_gen(*arg).unwrap();
                res += format!(
                    "\t\tmov rcx, qword [_funcall_arg{i}]\n\t\tmov qword [{}], rcx\n",
                    mangle_symbol(actual_argsym)
                )
                .as_str();
                if i >= 8 {
                    panic!("Functions are limited to 8 arguments only.");
                }
            }

            res += "\n";
            res += asmspit.get_program().as_str();

            self.functions_s.push(res);
        }
    }
    pub fn get_program(&self) -> String {
        let mut res = String::new();
        res += format!("\n{}\n", self.preamble).as_str();

        res += "section .text\n\n\tglobal _start\n\textern out_int\n\n_start:\t\tcall main\n\t\tmov rax, SYS_EXIT\n\t\txor rdi, rdi\n\t\tsyscall\n\n";
        for f in &self.functions_s {
            res += format!("\n{}\n", f).as_str();
        }
        return res;
    }
}
