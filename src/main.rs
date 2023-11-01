use clap::Parser;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
    ptr,
    rc::Rc,
};

type Integer = i32;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Priority {
    Low,
    High,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Term {
    priority: Priority,
    exprs: BTreeMap<Expr, usize>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Atom {
    Integer(Integer),
    Term(Rc<Term>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Expr {
    symmetrical: bool,
    atom: Atom,
}

impl From<Term> for Atom {
    fn from(value: Term) -> Self {
        Self::Term(value.into())
    }
}

impl Expr {
    fn sym(atom: Atom) -> Self {
        Self {
            symmetrical: true,
            atom,
        }
    }

    fn asy(atom: Atom) -> Self {
        Self {
            symmetrical: false,
            atom,
        }
    }
}

impl Term {
    fn new(priority: Priority) -> Self {
        Self {
            priority,
            exprs: BTreeMap::new(),
        }
    }

    fn insert(mut self, expr: Expr) -> Self {
        match expr.atom {
            Atom::Term(term) if term.priority == self.priority => {
                for (key, value) in &term.exprs {
                    *self
                        .exprs
                        .entry(Expr {
                            symmetrical: key.symmetrical == expr.symmetrical,
                            atom: key.atom.clone(),
                        })
                        .or_default() += value;
                }
            }
            _ => *self.exprs.entry(expr).or_default() += 1,
        }
        self
    }
}

impl Add for Atom {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Term::new(Priority::Low)
            .insert(Expr::sym(self))
            .insert(Expr::sym(rhs))
            .into()
    }
}

impl Sub for Atom {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Term::new(Priority::Low)
            .insert(Expr::sym(self))
            .insert(Expr::asy(rhs))
            .into()
    }
}

impl Mul for Atom {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Term::new(Priority::High)
            .insert(Expr::sym(self))
            .insert(Expr::sym(rhs))
            .into()
    }
}

impl Div for Atom {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Term::new(Priority::High)
            .insert(Expr::sym(self))
            .insert(Expr::asy(rhs))
            .into()
    }
}

#[derive(Clone, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Add => '+',
                Op::Sub => '-',
                Op::Mul => '×',
                Op::Div => '÷',
            }
        )
    }
}

struct Child<'a> {
    op: Op,
    lhs: &'a Node<'a>,
    rhs: &'a Node<'a>,
}

struct Node<'a> {
    value: f64,
    id: Atom,
    child: Option<Child<'a>>,
}

impl<'a> Display for Node<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.child {
            Some(node) => {
                if node
                    .lhs
                    .child
                    .as_ref()
                    .map(|lhs| matches!((lhs.op, node.op), (Op::Add | Op::Sub, Op::Mul | Op::Div)))
                    .unwrap_or_default()
                {
                    write!(f, "({})", node.lhs)
                } else {
                    write!(f, "{}", node.lhs)
                }?;
                write!(f, "{}", node.op)?;
                if node
                    .rhs
                    .child
                    .as_ref()
                    .map(|rhs| {
                        matches!(
                            (node.op, rhs.op),
                            (Op::Mul | Op::Sub, Op::Add | Op::Sub) | (Op::Div, _)
                        )
                    })
                    .unwrap_or_default()
                {
                    write!(f, "({})", node.rhs)
                } else {
                    write!(f, "{}", node.rhs)
                }
            }
            None => {
                if self.value < 0. {
                    write!(f, "({:.0})", self.value)
                } else {
                    write!(f, "{:.0}", self.value)
                }
            }
        }
    }
}

fn solve(exprs: Vec<&Node>, target: Integer, ids: &mut BTreeSet<Atom>) {
    if exprs.len() == 1 {
        let expr = exprs[0];
        if (expr.value - f64::from(target)).abs() < f64::EPSILON && ids.insert(expr.id.clone()) {
            println!("{expr}={target}");
        }
        return;
    }

    for i in 0..exprs.len() {
        for j in i + 1..exprs.len() {
            for (op, reverse) in [
                (Op::Add, false),
                (Op::Sub, false),
                (Op::Mul, false),
                (Op::Div, false),
                (Op::Sub, true),
                (Op::Div, true),
            ] {
                let (lhs, rhs) = if reverse {
                    (exprs[j], exprs[i])
                } else {
                    (exprs[i], exprs[j])
                };
                if let (Op::Sub, Some(Op::Add | Op::Sub)) | (Op::Div, Some(Op::Mul | Op::Div)) =
                    (op, rhs.child.as_ref().map(|node| node.op))
                {
                    continue;
                }
                let mut exprs = exprs
                    .iter()
                    .filter(|&&expr| !ptr::eq(expr, lhs) && !ptr::eq(expr, rhs))
                    .copied()
                    .collect::<Vec<_>>();
                let new = Node {
                    value: match op {
                        Op::Add => lhs.value + rhs.value,
                        Op::Sub => lhs.value - rhs.value,
                        Op::Mul => lhs.value * rhs.value,
                        Op::Div => lhs.value / rhs.value,
                    },
                    id: match op {
                        Op::Add => lhs.id.clone() + rhs.id.clone(),
                        Op::Sub => lhs.id.clone() - rhs.id.clone(),
                        Op::Mul => lhs.id.clone() * rhs.id.clone(),
                        Op::Div => lhs.id.clone() / rhs.id.clone(),
                    },
                    child: Some(Child { op, lhs, rhs }),
                };
                exprs.push(&new);
                solve(exprs, target, ids);
            }
        }
    }
}

#[derive(Parser)]
struct Args {
    #[arg(short, long, default_value_t = 24)]
    target: Integer,
    integers: Vec<Integer>,
}

fn main() {
    let args = Args::parse();
    let exprs = args
        .integers
        .iter()
        .map(|&n| Node {
            value: f64::from(n),
            id: Atom::Integer(n),
            child: None,
        })
        .collect::<Vec<_>>();
    let mut ids = BTreeSet::new();
    solve(exprs.iter().collect(), args.target, &mut ids);
    println!(
        "[{} solution{}]",
        ids.len(),
        if ids.len() > 1 { "s" } else { "" }
    );
}
