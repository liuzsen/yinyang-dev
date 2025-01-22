-- Your SQL goes here
create table users (
    id bigint primary key,
    email text not null,
    name text not null,
    password text not null,
    role smallint not null,

    created_at timestamptz not null default now(),
    updated_at timestamptz not null default now()
);

SELECT diesel_manage_updated_at('users');