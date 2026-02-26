#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub type_annotation: Option<String>,
    pub body: String,
}
