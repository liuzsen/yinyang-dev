use ra_ap_syntax::ast;

pub struct UseCaseMod {
    pub input: UseCaseInput,
    pub output: UseCaseOutput,
    pub error: UseCaseError,
    pub impl_trait: UseCaseImpl,
}

pub struct UseCaseInput(ast::Struct);

pub struct UseCaseOutput(ast::Struct);

pub struct UseCaseError(ast::Enum);

pub struct UseCaseImpl(ast::Impl);

impl UseCaseMod {
    pub fn from_file(file: &ast::SourceFile) -> Option<Self> {
        todo!()
    }

    pub fn input(&self) -> &UseCaseInput {
        &self.input
    }
}

impl UseCaseInput {
    pub fn fields(&self) -> Vec<ast::RecordField> {
        todo!()
    }

    pub fn repo_field(&self) -> Option<ast::RecordField> {
        todo!()
    }
}
