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

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FieldPath {
    segments: Vec<String>,
}

impl FieldPath {
    pub fn new_scalar(name: String) -> Self {
        Self {
            segments: vec![name],
        }
    }

    pub fn join(&self, other: &Self) -> Self {
        let mut segments = self.segments.clone();
        segments.extend(other.segments.clone());
        Self { segments }
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
