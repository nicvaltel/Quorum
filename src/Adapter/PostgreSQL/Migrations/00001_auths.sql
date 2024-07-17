create extension citext;
create extension pgcrypto;

create table IF NOT EXISTS auths (
  id bigserial primary key not null,
  pass text not null,
  email citext not null unique,
  email_verification_code text not null,
  is_email_verified boolean not null
);

CREATE TABLE IF NOT EXISTS articles (
id bigserial primary key not null,
user_id bigserial NOT null,
head text not null,
body text NOT NULL,
FOREIGN KEY (user_id) REFERENCES auths(id)
);