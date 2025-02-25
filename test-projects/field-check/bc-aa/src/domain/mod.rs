use bagua::{flake_id, Entity, FieldGroup};

flake_id!(AAId, @serde);

#[Entity]
#[subset(AAWithOnlyId{id})]
pub struct AA {
    id: AAId,
    name1: String,
    name2: String,
    #[entity(group)]
    group: AAGroup,
}

#[FieldGroup]
pub struct AAGroup {
    g1: u32,
    g2: String,
}

impl AA {
    pub fn set_name1(&mut self, name: String) {
        self.name1.set(name);
    }

    pub fn set_name2_if_empty(&mut self, name: String) {
        if self.name2.is_empty() {
            self.name2.set(name);
        }
    }

    pub fn set_g1(&mut self, g1: u32) {
        self.group.set_g1(g1);
    }

    pub fn get_group(&self) -> &AAGroup {
        &self.group
    }
}

impl AAGroup {
    pub fn set_g1(&mut self, g1: u32) {
        self.g1.set(g1);
    }

    pub fn get_g2(&self) -> &String {
        &self.g2
    }
}
