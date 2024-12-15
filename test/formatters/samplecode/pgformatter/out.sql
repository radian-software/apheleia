CREATE TABLE person (
        PRIMARY KEY id int,
        firstname varchar(500),
        lastname varchar(500),
        email varchar(500),
        password_sha256 varchar(200),
        password_seed varchar(200)
);

