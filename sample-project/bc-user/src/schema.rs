// @generated automatically by Diesel CLI.

diesel::table! {
    users (id) {
        id -> Int8,
        email -> Text,
        name -> Text,
        password -> Text,
        role -> Int2,
        created_at -> Timestamptz,
        updated_at -> Timestamptz,
    }
}
