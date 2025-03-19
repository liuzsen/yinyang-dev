use std::ops::Deref;
use std::sync::Arc;

use ra_ap_hir::{Semantics, Struct};
use ra_ap_ide::RootDatabase;
use ra_ap_syntax::ast;

pub struct Entity {
    pub name: ast::Name,
    pub fields: Vec<EntityField>,
    pub subsets: Vec<Subset>,
}

pub struct EntityField {
    pub path: NamedRefPath,
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
pub struct NamedRefPath {
    segments: Vec<Arc<FieldName>>,
}

// #[derive(Debug, Clone, PartialEq, Hash, Eq)]
// pub struct NamedRefPathBorrow<'a> {
//     segments: &'a [FieldName],
// }
// use std::path::Path;
// use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FieldName(pub String);

impl FieldName {
    pub fn to_string(self) -> String {
        self.0
    }
}

impl NamedRefPath {
    pub fn dot_string(&self) -> String {
        let mut out = String::new();

        for s in &self.segments {
            out += ".";
            out += s.0.as_str();
        }

        out
    }

    pub fn new_this() -> Self {
        Self { segments: vec![] }
    }

    pub fn is_this(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn new_segment(name: String) -> Self {
        Self {
            segments: vec![Arc::new(FieldName(name))],
        }
    }

    pub fn segment(&self) -> Option<&FieldName> {
        if self.segments.len() == 1 {
            self.segments.last().map(|s| &**s)
        } else {
            None
        }
    }

    pub fn split_at_1(&self) -> (&FieldName, Self) {
        let (first, remain) = self.segments.split_at(1);
        (
            &first[0],
            NamedRefPath {
                segments: Vec::from_iter(remain.iter().cloned()),
            },
        )
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
        self.segments.push(Arc::new(FieldName(segment)));
    }
}

#[derive(Debug)]
pub struct Subset {
    pub name: ast::Name,
    pub fields: Vec<SubsetField>,
}

#[derive(Debug)]
pub struct SubsetField {
    pub path: NamedRefPath,
}

impl Entity {
    pub fn parse(sema: &Semantics<RootDatabase>, struct_def: Struct) -> anyhow::Result<Self> {
        todo!()
    }
}

impl Subset {
    pub fn contains(&self, field_path: &NamedRefPath) -> bool {
        self.fields.iter().any(|f| f.path == *field_path)
    }
}
