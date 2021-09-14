use swc_common::source_map::DUMMY_SP;
use swc_ecma_ast as es;

pub static NULL: es::Null = es::Null { span: DUMMY_SP };
pub static THIS: es::ThisExpr = es::ThisExpr { span: DUMMY_SP };

fn binding(id: es::Ident) -> es::Pat {
    es::Pat::Ident(es::BindingIdent { id, type_ann: None })
}

fn spread(expr: es::Expr) -> es::ExprOrSpread {
    es::ExprOrSpread {
        spread: None,
        expr: Box::new(expr),
    }
}

pub fn ident(ident: String) -> es::Ident {
    es::Ident::new(ident.into(), DUMMY_SP)
}

pub fn number(value: f64) -> es::Expr {
    if value.is_sign_negative() {
        unary(es::UnaryOp::Minus, (-value).into())
    } else {
        value.into()
    }
}

pub fn long(value: i64) -> es::Lit {
    es::BigInt {
        span: DUMMY_SP,
        value: value.into(),
    }
    .into()
}

pub fn fnexpr(function: es::Function) -> es::Expr {
    es::FnExpr { ident: None, function }.into()
}

pub fn unary(op: es::UnaryOp, arg: es::Expr) -> es::Expr {
    es::UnaryExpr {
        span: DUMMY_SP,
        op,
        arg: Box::new(arg),
    }
    .into()
}

pub fn bin(op: es::BinaryOp, left: es::Expr, right: es::Expr) -> es::Expr {
    es::BinExpr {
        span: DUMMY_SP,
        op,
        left: Box::new(left),
        right: Box::new(right),
    }
    .into()
}

pub fn eassign(op: es::AssignOp, left: es::Expr, right: es::Expr) -> es::Expr {
    es::AssignExpr {
        span: DUMMY_SP,
        op,
        left: Box::new(left).into(),
        right: Box::new(right),
    }
    .into()
}

pub fn passign(op: es::AssignOp, left: es::Ident, right: es::Expr) -> es::Expr {
    es::AssignExpr {
        span: DUMMY_SP,
        op,
        left: Box::new(binding(left)).into(),
        right: Box::new(right),
    }
    .into()
}

pub fn emember(obj: es::Expr, prop: es::Expr) -> es::Expr {
    es::MemberExpr {
        span: DUMMY_SP,
        obj: Box::new(obj).into(),
        prop: Box::new(prop),
        computed: true,
    }
    .into()
}

pub fn imember(obj: es::Expr, prop: es::Ident) -> es::Expr {
    es::MemberExpr {
        span: DUMMY_SP,
        obj: Box::new(obj).into(),
        prop: Box::new(prop.into()),
        computed: false,
    }
    .into()
}

pub fn call(callee: es::Expr, args: Vec<es::Expr>) -> es::Expr {
    es::CallExpr {
        span: DUMMY_SP,
        callee: Box::new(callee).into(),
        args: args.into_iter().map(spread).collect(),
        type_args: None,
    }
    .into()
}

pub fn new(callee: es::Expr, args: Vec<es::Expr>) -> es::Expr {
    es::NewExpr {
        span: DUMMY_SP,
        callee: Box::new(callee),
        args: Some(args.into_iter().map(spread).collect()),
        type_args: None,
    }
    .into()
}

pub fn maybeawait(isasync: bool, arg: es::Expr) -> es::Expr {
    if isasync {
        es::AwaitExpr {
            span: DUMMY_SP,
            arg: Box::new(arg),
        }
        .into()
    } else {
        arg
    }
}

pub fn wrap(expr: es::Expr) -> es::Stmt {
    es::ExprStmt {
        span: DUMMY_SP,
        expr: Box::new(expr),
    }
    .into()
}

pub fn block(stmts: Vec<es::Stmt>) -> es::Stmt {
    if stmts.len() == 1 {
        return stmts.into_iter().next().unwrap();
    }
    es::BlockStmt { span: DUMMY_SP, stmts }.into()
}

pub fn returns(arg: Option<es::Expr>) -> es::Stmt {
    es::ReturnStmt {
        span: DUMMY_SP,
        arg: arg.map(Box::new),
    }
    .into()
}

pub fn labeled(label: es::Ident, body: es::Stmt) -> es::Stmt {
    es::LabeledStmt {
        span: DUMMY_SP,
        label,
        body: Box::new(body),
    }
    .into()
}

pub fn lbreak(label: Option<es::Ident>) -> es::Stmt {
    es::BreakStmt { span: DUMMY_SP, label }.into()
}

pub fn lcontinue(label: Option<es::Ident>) -> es::Stmt {
    es::ContinueStmt { span: DUMMY_SP, label }.into()
}

pub fn cond(test: es::Expr, cons: es::Stmt, alt: Option<es::Stmt>) -> es::Stmt {
    es::IfStmt {
        span: DUMMY_SP,
        test: Box::new(test),
        cons: Box::new(cons),
        alt: alt.map(Box::new),
    }
    .into()
}

pub fn switch(discriminant: es::Expr, cases: Vec<es::SwitchCase>) -> es::Stmt {
    es::SwitchStmt {
        span: DUMMY_SP,
        discriminant: Box::new(discriminant),
        cases,
    }
    .into()
}

pub fn switchcase(test: Option<es::Expr>, cons: Vec<es::Stmt>) -> es::SwitchCase {
    es::SwitchCase {
        span: DUMMY_SP,
        test: test.map(Box::new),
        cons,
    }
}

pub fn throw(arg: es::Expr) -> es::Stmt {
    es::ThrowStmt {
        span: DUMMY_SP,
        arg: Box::new(arg),
    }
    .into()
}

pub fn loops(body: es::Stmt) -> es::Stmt {
    es::ForStmt {
        span: DUMMY_SP,
        init: None,
        test: None,
        update: None,
        body: Box::new(body),
    }
    .into()
}

pub fn function_decl(ident: es::Ident, function: es::Function) -> es::Decl {
    es::FnDecl {
        ident,
        declare: false,
        function,
    }
    .into()
}

pub fn class_decl(ident: es::Ident, class: es::Class) -> es::Decl {
    es::ClassDecl {
        ident,
        declare: false,
        class,
    }
    .into()
}

pub fn var(decls: Vec<es::VarDeclarator>) -> es::Decl {
    es::VarDecl {
        span: DUMMY_SP,
        kind: es::VarDeclKind::Var,
        declare: false,
        decls,
    }
    .into()
}

pub fn vari(ident: es::Ident, init: Option<es::Expr>) -> es::VarDeclarator {
    es::VarDeclarator {
        span: DUMMY_SP,
        name: ident.into(),
        init: init.map(Box::new),
        definite: false,
    }
}

pub fn class(body: Vec<es::ClassMember>, super_class: Option<es::Expr>) -> es::Class {
    es::Class {
        span: DUMMY_SP,
        decorators: vec![],
        body,
        super_class: super_class.map(Box::new),
        is_abstract: false,
        type_params: None,
        super_type_params: None,
        implements: vec![],
    }
}

pub fn field(key: es::Ident, value: es::Expr) -> es::ClassProp {
    es::ClassProp {
        span: DUMMY_SP,
        key: Box::new(key.into()),
        value: Some(Box::new(value)),
        type_ann: None,
        is_static: false,
        decorators: vec![],
        computed: false,
        accessibility: None,
        is_abstract: false,
        is_optional: false,
        is_override: false,
        readonly: false,
        declare: false,
        definite: false,
    }
}

pub fn method(key: es::Ident, function: es::Function) -> es::ClassMethod {
    es::ClassMethod {
        span: DUMMY_SP,
        key: key.into(),
        function,
        kind: es::MethodKind::Method,
        is_static: false,
        accessibility: None,
        is_abstract: false,
        is_optional: false,
        is_override: false,
    }
}

pub fn function(params: Vec<es::Param>, stmts: Vec<es::Stmt>, is_async: bool) -> es::Function {
    es::Function {
        params,
        decorators: vec![],
        span: DUMMY_SP,
        body: Some(es::BlockStmt { span: DUMMY_SP, stmts }),
        is_generator: false,
        is_async,
        type_params: None,
        return_type: None,
    }
}

pub fn param(ident: es::Ident) -> es::Param {
    es::Param {
        span: DUMMY_SP,
        decorators: vec![],
        pat: binding(ident),
    }
}
