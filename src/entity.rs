use ra_ap_hir::{Semantics, Struct};
use ra_ap_ide::RootDatabase;
use ra_ap_syntax::ast;

pub struct Entity {
    pub name: ast::Name,
    pub fields: Vec<EntityField>,
    pub subsets: Vec<Subset>,
}

pub struct EntityField {
    pub path: FieldPath,
    pub kind: EnitityFieldKind,
}

pub enum EnitityFieldKind {
    SystemId,
    BizId,
    Scalar,
    Foreign,
    FieldGroup { fields: Vec<EntityField> },
}

#[derive(Debug, Clone)]
pub struct FieldPath {
    pub qualifier: Option<Box<FieldPath>>,
    pub name: String,
}

impl PartialEq for FieldPath {
    fn eq(&self, other: &Self) -> bool {
        self.qualifier == other.qualifier && self.name == other.name
    }
}

#[derive(Debug)]
pub struct Subset {
    pub name: ast::Name,
    pub fields: Vec<SubsetField>,
}

#[derive(Debug)]
pub struct SubsetField {
    pub path: FieldPath,
}

impl Entity {
    pub fn parse(sema: &Semantics<RootDatabase>, struct_def: Struct) -> anyhow::Result<Self> {
        todo!()
    }
}

impl Subset {
    pub fn contains(&self, field_path: &FieldPath) -> bool {
        self.fields.iter().any(|f| f.path == *field_path)
    }
}
