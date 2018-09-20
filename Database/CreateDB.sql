/*******************************************************************************
SQL script for PIXEL_DB
09/2018
Thomas Denecker
*******************************************************************************/

/*******************************************************************************
EXTENSION
*******************************************************************************/
CREATE EXTENSION pgcrypto;
CREATE EXTENSION ltree;

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


/*------------------------------------------------------------------------------
OmicsUnitType
------------------------------------------------------------------------------*/

CREATE TABLE OmicsUnitType (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  description TEXT NOT NULL
);

/*------------------------------------------------------------------------------
CFSource
------------------------------------------------------------------------------*/

CREATE TABLE CFSource (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  abbreviation TEXT UNIQUE,
  description TEXT NOT NULL,
  url TEXT
);

INSERT INTO CFSource (name,abbreviation, description, url) VALUES ('Saccharomyces genome database','SGD', 'The Saccharomyces Genome Database (SGD) provides comprehensive integrated biological information for the budding yeast Saccharomyces cerevisiae along with search and analysis tools to explore these data, enabling the discovery of functional relationships between sequence and gene products in fungi and higher organisms.', 'https://www.yeastgenome.org/');
INSERT INTO CFSource (name,abbreviation, description, url) VALUES ('Candida genome database','CGD', 'This is the home of the Candida Genome Database, a resource for genomic sequence data and gene and protein information for Candida albicans and related species. CGD is based on the Saccharomyces Genome Database and is funded by the National Institute of Dental & Craniofacial Research at the US National Institutes of Health.', 'http://www.candidagenome.org/');

/*------------------------------------------------------------------------------
Species
------------------------------------------------------------------------------*/

CREATE TABLE Species (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  description TEXT NOT NULL,
  url TEXT
);

INSERT INTO Species (name, description, url) VALUES ('Saccharomyces cerevisiae', 'Saccharomyces cerevisiae is a species of yeast.', 'https://www.yeastgenome.org/');
INSERT INTO Species (name, description, url) VALUES ('Candida glabrata', 'Candida glabrata is a species of yeast.', 'http://www.candidagenome.org/');
INSERT INTO Species (name, description, url) VALUES ('Candida albicans', 'Candida glabrata is a species of yeast.', 'http://www.candidagenome.org/');

/*------------------------------------------------------------------------------
ChromosomalFeature
------------------------------------------------------------------------------*/

CREATE TABLE ChromosomalFeature (
  id SERIAL PRIMARY KEY,
  feature_name TEXT NOT NULL UNIQUE,
  gene_name TEXT,
  chromosome TEXT NOT NULL,
  start_coordinate INTEGER NOT NULL,
  stop_coordinate INTEGER NOT NULL,
  strand TEXT,
  species_name TEXT,
  url TEXT,
  default_db_name TEXT NOT NULL,
  CONSTRAINT fks FOREIGN KEY (species_name) REFERENCES Species (name),
  CONSTRAINT fkcfsource FOREIGN KEY (default_db_name) REFERENCES CFSource (name)
);

/*------------------------------------------------------------------------------
ChromosomalFeature -- CFSource
------------------------------------------------------------------------------*/

CREATE TABLE annotation (
  feature_name TEXT,
  annot_table TEXT,
  CONSTRAINT fkfeature_name FOREIGN KEY (feature_name) REFERENCES ChromosomalFeature (feature_name)
);

/*------------------------------------------------------------------------------
Strain
------------------------------------------------------------------------------*/

CREATE TABLE Strain (
  id SERIAL PRIMARY KEY,
  name TEXT,
  description TEXT,
  ref TEXT,
  species_name TEXT,
  CONSTRAINT fkspecies FOREIGN KEY (species_name) REFERENCES Species (name)
);


/*------------------------------------------------------------------------------
DataSource
------------------------------------------------------------------------------*/

CREATE TABLE DataSource (
  id SERIAL PRIMARY KEY,
  name TEXT,
  description TEXT,
  published BOOLEAN,
  url TEXT
);

/*------------------------------------------------------------------------------
OmicsArea
------------------------------------------------------------------------------*/

CREATE TABLE OmicsArea (
    id serial primary key,
    name TEXT NOT NULL UNIQUE,
    descritpion TEXT,
    path ltree
);

CREATE INDEX tree_path_idx ON OmicsArea using gist (path);
CREATE INDEX path_idx ON OmicsArea USING btree(path);

insert into OmicsArea (name, descritpion, path) values ('Area', 'Area', 'Area');
insert into OmicsArea (name, descritpion, path) values ('Proteomic', 'Proteomics is the large-scale study of proteins.', 'Area.Proteomic');
insert into OmicsArea (name, descritpion, path) values ('Mass spectrometry', 'Mass spectrometry (MS) is an analytical technique that ionizes chemical species and sorts the ions based on their mass-to-charge ratio.', 'Area.Proteomic.Massspectrometry');
insert into OmicsArea (name, descritpion, path) values ('Transcriptomic', E'Transcriptomics technologies are the techniques used to study an organism\'s transcriptome, the sum of all of its RNA transcripts.', 'Area.Transcriptomic');
insert into OmicsArea (name, descritpion, path) values ('Microarray', 'A DNA microarray is a collection of microscopic DNA spots attached to a solid surface.', 'Area.Transcriptomic.Microarray');
insert into OmicsArea (name, descritpion, path) values ('RNAseq', 'RNA-Seq (RNA sequencing), also called whole transcriptome shotgun sequencing (WTSS), uses next-generation sequencing (NGS) to reveal the presence and quantity of RNA in a biological sample at a given moment.', 'Area.Transcriptomic.RNAseq');

/*------------------------------------------------------------------------------
Experiment
------------------------------------------------------------------------------*/

CREATE TABLE Experiment (
    id serial primary key,
    omics_area_name TEXT NOT NULL,
    descritpion TEXT,
    completionDate DATE,
    CONSTRAINT fkomicsarea FOREIGN KEY (omics_area_name) REFERENCES OmicsArea (name)
);

/*------------------------------------------------------------------------------
Analysis
------------------------------------------------------------------------------*/

CREATE TABLE Analysis (
    id serial primary key,
    descritpion TEXT,
    completionDate DATE,
    notebook_file TEXT,
    secondary_data_file TEXT
);


/*------------------------------------------------------------------------------
Analysis -- Experiment
------------------------------------------------------------------------------*/

CREATE TABLE Analysis_Experiment (
  id_experiment INTEGER,
  id_analysis INTEGER,
  PRIMARY KEY (id_experiment, id_analysis),
  CONSTRAINT fkanalysis FOREIGN KEY (id_experiment) REFERENCES Experiment (id),
  CONSTRAINT fkexperiment FOREIGN KEY (id_analysis) REFERENCES Analysis (id)
);


/*------------------------------------------------------------------------------
Tag
------------------------------------------------------------------------------*/

CREATE TABLE Tag (
    id serial primary key,
    name TEXT,
    description TEXT
);


/*------------------------------------------------------------------------------
TAG -- Experiment
------------------------------------------------------------------------------*/

CREATE TABLE Tag_Experiment (
  id_tag INTEGER,
  id_experiment INTEGER,
  PRIMARY KEY (id_experiment, id_tag),
  CONSTRAINT fkexperiment FOREIGN KEY (id_experiment) REFERENCES Experiment (id),
  CONSTRAINT fktag FOREIGN KEY (id_tag) REFERENCES Tag (id)
);

/*------------------------------------------------------------------------------
TAG -- Analysis
------------------------------------------------------------------------------*/

CREATE TABLE Tag_Analysis (
  id_tag INTEGER,
  id_analysis INTEGER,
  PRIMARY KEY (id_analysis, id_tag),
  CONSTRAINT fkanalysis FOREIGN KEY (id_analysis) REFERENCES Analysis (id),
  CONSTRAINT fktag FOREIGN KEY (id_tag) REFERENCES Tag (id)
);


/*------------------------------------------------------------------------------
Submission
------------------------------------------------------------------------------*/

CREATE TABLE Submission (
  id serial primary key,
  submission_date Date,
  status BOOLEAN,
  pixeler_user_name TEXT,
  CONSTRAINT fkpixeler FOREIGN KEY (pixeler_user_name) REFERENCES pixeler (user_name)
);

/*------------------------------------------------------------------------------
PixelSet
------------------------------------------------------------------------------*/

CREATE TABLE PixelSet (
  id serial primary key,
  name TEXT UNIQUE,
  pixelSet_file TEXT,
  description TEXT,
  id_analysis INTEGER,
  id_submission INTEGER,
  CONSTRAINT fkanalysis FOREIGN KEY (id_analysis) REFERENCES Analysis (id),
  CONSTRAINT fksubmission FOREIGN KEY (id_submission) REFERENCES Submission (id)
);


/*------------------------------------------------------------------------------
Pixel
------------------------------------------------------------------------------*/

CREATE TABLE Pixel (
  id serial primary key,
  value NUMERIC,
  quality_score NUMERIc,
  pixelSet_name TEXT,
  cf_feature_name TEXT,
  OmicsUnitType_name TEXT,
  CONSTRAINT fkpixelerset FOREIGN KEY (pixelSet_name) REFERENCES PixelSet (name),
  CONSTRAINT fkcf FOREIGN KEY (cf_feature_name) REFERENCES ChromosomalFeature (feature_name),
  CONSTRAINT fkout FOREIGN KEY (OmicsUnitType_name) REFERENCES OmicsUnitType (name)
);
