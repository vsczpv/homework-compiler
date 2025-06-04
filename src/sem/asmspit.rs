use std::any::Any;

use crate::{
    common::ArchInt,
    sem::typechk::Operator,
    syn::tree::{AstNode, NodeKind, Virtual},
};

use super::{
    symtab::SymbolTable,
    typechk::{operator_check, ValueKind},
};

pub struct AssemblySpitter<'a> {
    program: String,
    syms: &'a SymbolTable,
    tree: &'a Box<AstNode>,
}

impl<'a> AssemblySpitter<'a> {
    pub fn new(syms: &'a SymbolTable, tree: &'a Box<AstNode>) -> Self {
        AssemblySpitter {
            syms,
            program: String::default(),
            tree,
        }
    }
    pub fn spit_defines(mut self) -> Self {
        self.program += "SYS_EXIT equ 0x3c\n";
        return self;
    }
    pub fn spit_globals(mut self) -> Self {
        self.program += "section .bss\n";
        for symbol in self.syms.get_all_syms() {
            let name = &symbol.ident;
            let size = ArchInt::BITS / 8; // TODO
            self.program += format!("\t{name}:\tresb\t{size}\n").as_str();
        }
        return self;
    }
    pub fn venture_expression(&mut self, node: &Box<AstNode>) -> Option<ValueKind> {
        if matches!(
            node.get_kind(),
            NodeKind::TypedVirt(Virtual::GenericExpression, _)
        ) {
            if node.get_children().len() == 2 {
                panic!("internal compiler error: only binary operators supported.");
            }

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

                    if !matches!(opr, Operator::AssignOptr) {
                        if matches!(lhstype, ValueKind::LvalueRef(_)) {
                            self.program += "\n\t\tmov rax, qword [rax]\n";
                        }
                        if matches!(rhstype, ValueKind::LvalueRef(_)) {
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
                        Operator::AssignOptr => {
                            // The assign operator swaps operand order
                            self.program += "\t\tmov qword [rbx], rax\n\n";
                            Some(operator_check(&rhstype, &lhstype, &opr).unwrap())
                        }
                    }
                }
                1 => {
                    let kid = node.follow_line(1);
                    match kid.get_kind() {
                        NodeKind::TypedVirt(Virtual::WrappedTerm, vk) => match vk {
                            ValueKind::Rvalue(_) => {
                                let grandkid = kid
                                    .follow_line(1)
                                    .get_kind()
                                    .clone()
                                    .some_lex()
                                    .unwrap()
                                    .get_token()
                                    .some_number()
                                    .unwrap()
                                    .clone();
                                self.program += format!("\t\tmov rax, {}\n", grandkid).as_str();
                                return Some(vk.clone());
                            }
                            ValueKind::Lvalue(tp, gen) => {
                                self.program += format!(
                                    "\t\tlea rax, qword [{}]\n",
                                    self.syms.get_by_gen(*gen).unwrap().ident
                                )
                                .as_str();
                                return Some(ValueKind::LvalueRef(tp.clone()));
                            }
                            _ => panic!("c"),
                        },
                        _ => todo!("internal compiler error: expression lists not yet implemented"),
                    }
                }
                _ => panic!("a"),
            }
        } else {
            for c in node.get_children() {
                self.venture_expression(c);
            }
            None
        }
    }
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
    pub fn get_program(self) -> String {
        self.program
    }
}
