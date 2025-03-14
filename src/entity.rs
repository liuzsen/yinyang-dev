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
    pub fn new_current() -> Self {
        Self { segments: vec![] }
    }
    pub fn new_segment(name: String) -> Self {
        Self {
            segments: vec![name],
        }
    }

    pub fn starts_with(&self, base: &Self) -> bool {
        self.segments.len() >= base.segments.len()
            && self.segments[..base.segments.len()] == base.segments
    }

    pub fn join(&self, other: &Self) -> Self {
        let mut segments = self.segments.clone();
        segments.extend(other.segments.clone());
        Self { segments }
    }

    pub fn push(&mut self, segment: String) {
        self.segments.push(segment);
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
