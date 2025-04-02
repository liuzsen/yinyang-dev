use ra_ap_hir::{Enum, Trait};

pub struct Bagua {
    traits: BaguaTraits,
    adts: BaguaAdts,
}

pub struct BaguaAdts {
    field: Enum,
}

pub struct BaguaTraits {
    entity: Trait,
    usecase: Trait,
    repository: Trait,
}
