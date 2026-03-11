use std::collections::{HashMap, HashSet};

use goby_core::{Expr, Module, Stmt, ast::InterpolatedPart, stdlib::EmbeddedRuntimeHandlerKind};

use crate::{
    RuntimeLocals, RuntimeValue,
    runtime_eval::{EvaluatedFunctions, IntCallable, IntEvaluator, ListIntEvaluator},
};

pub(crate) type WithId = u64;

#[derive(Clone)]
pub(crate) struct Continuation {
    pub(crate) consumed: bool,
}

#[derive(Clone)]
pub(crate) struct ResumeToken {
    pub(crate) continuation: Continuation,
    pub(crate) state: HandlerContinuationState,
    pub(crate) cont: Option<Cont>,
}

#[derive(Clone)]
pub(crate) struct OptimizedResumeToken {
    pub(crate) consumed: bool,
    pub(crate) state: HandlerContinuationState,
    pub(crate) cont: Option<Cont>,
}

#[derive(Clone)]
pub(crate) enum HandlerContinuationState {
    Pending,
    Resumed(Box<RuntimeValue>),
    Suspended,
}

#[allow(clippy::large_enum_variant)]
pub(crate) enum HandlerCompletion {
    Aborted,
    Escaped(Escape),
    Resumed(Box<RuntimeValue>),
    Suspended,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub(crate) enum RuntimeError {
    Abort { kind: String },
    Unsupported,
}

#[allow(dead_code)]
#[allow(clippy::large_enum_variant)]
pub(crate) enum Out<T> {
    Done(T),
    Suspend(Cont),
    Escape(Escape),
    Err(RuntimeError),
}

#[allow(dead_code)]
#[derive(Clone)]
pub(crate) enum Escape {
    WithScope {
        with_id: WithId,
        value: RuntimeValue,
    },
}

#[allow(dead_code)]
#[derive(Clone)]
pub(crate) enum FinishKind {
    Block,
    WithBody {
        with_id: WithId,
    },
    HandlerBody {
        token_idx: usize,
        produce_value: bool,
        with_id: WithId,
    },
    Ingest,
}

#[allow(dead_code)]
#[derive(Clone)]
#[allow(clippy::large_enum_variant)]
pub(crate) enum Cont {
    StmtSeq {
        store: Option<StoreOp>,
        remaining: Vec<Stmt>,
        locals: RuntimeLocals,
        callables: HashMap<String, IntCallable>,
        depth: usize,
        handler_stack: Vec<InlineHandlerValue>,
        finish: FinishKind,
    },
    Apply {
        step: ApplyStep,
        locals: RuntimeLocals,
        callables: HashMap<String, IntCallable>,
        depth: usize,
        handler_stack: Vec<InlineHandlerValue>,
    },
    Resume,
}

#[allow(dead_code)]
#[derive(Clone)]
pub(crate) enum StoreOp {
    Bind { name: String },
    Assign { name: String },
}

#[allow(dead_code)]
#[derive(Clone)]
pub(crate) enum ApplyStep {
    WithBody {
        body: Vec<Stmt>,
    },
    Pipeline {
        callee: String,
    },
    SingleArgCall {
        fn_name: String,
    },
    ReceiverMethod {
        receiver: String,
        member: String,
    },
    MultiArgCall {
        fn_name: String,
        evaluated: Vec<RuntimeValue>,
        remaining: Vec<Expr>,
    },
    CaseSelect {
        arms: Vec<goby_core::CaseArm>,
    },
    IfBranch {
        then_expr: Expr,
        else_expr: Expr,
    },
    BinOpLeft {
        op: goby_core::BinOpKind,
        right: Expr,
    },
    BinOpRight {
        op: goby_core::BinOpKind,
        left: RuntimeValue,
    },
    ListLitElement {
        evaluated: Vec<RuntimeValue>,
        remaining: Vec<Expr>,
        spread: Option<Expr>,
        resuming_spread: bool,
    },
    TupleLitElement {
        evaluated: Vec<RuntimeValue>,
        remaining: Vec<Expr>,
    },
    RecordField {
        constructor: String,
        evaluated: Vec<(String, RuntimeValue)>,
        pending_field: String,
        remaining: Vec<(String, Expr)>,
    },
    InterpolatedPart {
        accumulated: String,
        remaining: Vec<InterpolatedPart>,
    },
}

#[allow(dead_code)]
#[derive(Clone)]
#[allow(clippy::large_enum_variant)]
pub(crate) enum TokenState {
    Pending,
    Done(RuntimeValue),
    Suspended(Cont),
}

#[derive(Clone)]
pub(crate) struct ResolvedHandlerMethod {
    pub(crate) method: RuntimeHandlerMethod,
    pub(crate) with_id: Option<WithId>,
}

#[derive(Clone)]
pub(crate) enum ResolvedEffectHandler {
    Explicit(ResolvedHandlerMethod),
    EmbeddedDefault {
        handler_kind: EmbeddedRuntimeHandlerKind,
        method_name: String,
    },
}

#[derive(Clone)]
pub(crate) struct InlineHandlerMethod {
    pub(crate) effect_name: Option<String>,
    pub(crate) method: RuntimeHandlerMethod,
}

#[derive(Clone)]
pub(crate) struct InlineHandlerValue {
    pub(crate) methods: Vec<InlineHandlerMethod>,
    pub(crate) captured_locals: RuntimeLocals,
    pub(crate) changed_outer_names: HashSet<String>,
    pub(crate) captured_callables: HashMap<String, IntCallable>,
    pub(crate) with_id: Option<WithId>,
}

#[derive(Clone)]
pub(crate) struct RuntimeHandlerMethod {
    pub(crate) name: String,
    pub(crate) params: Vec<String>,
    pub(crate) body: String,
    pub(crate) parsed_body: Option<Vec<Stmt>>,
}

pub(crate) struct RuntimeEvaluators<'a, 'b> {
    pub(crate) int: &'b IntEvaluator<'a>,
    pub(crate) list: &'b ListIntEvaluator<'a>,
    pub(crate) unit: &'b EvaluatedFunctions<'a>,
}

#[derive(Clone)]
pub(crate) struct RuntimeDeclInfo {
    pub(crate) name: String,
    pub(crate) owner_module: Option<String>,
    pub(crate) params: Vec<String>,
    pub(crate) callable_param_mask: Vec<bool>,
    pub(crate) stmts: Vec<Stmt>,
}

#[derive(Clone)]
pub(crate) enum DirectCallHead {
    Bare(String),
    Qualified { receiver: String, member: String },
}

#[allow(dead_code)]
fn _module_marker(_: &Module) {}
