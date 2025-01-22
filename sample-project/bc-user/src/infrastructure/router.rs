pub mod user {
    use bagua::http_api;

    http_api!(create, user::create: HttpJsonBody);
    http_api!(update, user::update: HttpJsonBody + HttpCredential);
}
