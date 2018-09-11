/*******************************************************************************
SQL script for PIXEL_DB
09/2018
Thomas Denecker
*******************************************************************************/

/*******************************************************************************
EXTENSION
*******************************************************************************/
CREATE EXTENSION pgcrypto;

/*******************************************************************************
Tables
*******************************************************************************/

/*------------------------------------------------------------------------------
PIXELER
------------------------------------------------------------------------------*/

CREATE TABLE pixeler (
  id SERIAL PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  user_name TEXT NOT NULL UNIQUE,
  email TEXT NOT NULL UNIQUE,
  password TEXT NOT NULL,
  user_type TEXT NOT NULL,
  lab_country TEXT NOT NULL,
  creation_date timestamp default current_timestamp
);


INSERT INTO pixeler (first_name, last_name, user_name, email, user_type, password, lab_country) VALUES (
  'ad',
  'min',
  'admin',
  'admin@mail.com',
  'Admin',
  crypt('admin', gen_salt('bf')),
  'France'
);

INSERT INTO pixeler (first_name, last_name, user_name, email, user_type, password, lab_country) VALUES (
  'Us',
  'er',
  'User',
  'user@mail.com',
  'User',
  crypt('user', gen_salt('bf')),
  'France'
);
