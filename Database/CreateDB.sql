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
  'Pix',
  'Eler',
  'pixeler',
  'pixeler@mail.com',
  'Pixeler',
  crypt('pixeler', gen_salt('bf')),
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
  name TEXT UNIQUE NOT NULL,
  description TEXT NOT NULL
);

INSERT INTO OmicsUnitType (name, description) VALUES ('gene', 'Base unit');
INSERT INTO OmicsUnitType (name, description) VALUES ('mRNA', 'This is the (polyadenylated) mRNA transcribed from a gene');
INSERT INTO OmicsUnitType (name, description) VALUES ('protein', 'This is the protein encoded by a gene');


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
  description TEXT,
  species_id INTEGER,
  url TEXT,
  default_db_id INTEGER NOT NULL,
  CONSTRAINT fks FOREIGN KEY (species_id) REFERENCES Species (id),
  CONSTRAINT fkcfsource FOREIGN KEY (default_db_id) REFERENCES CFSource (id)
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
  name TEXT UNIQUE,
  description TEXT,
  ref TEXT,
  species_id INTEGER,
  CONSTRAINT fkspecies FOREIGN KEY (species_id) REFERENCES Species (id)
);


/*------------------------------------------------------------------------------
DataSource
------------------------------------------------------------------------------*/

CREATE TABLE DataSource (
  id SERIAL PRIMARY KEY,
  name TEXT UNIQUE,
  description TEXT,
  published BOOLEAN,
  url TEXT
);

/*------------------------------------------------------------------------------
OmicsArea
------------------------------------------------------------------------------*/

CREATE TABLE OmicsArea (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    description TEXT,
    path ltree
);

CREATE INDEX tree_path_idx ON OmicsArea using gist (path);
CREATE INDEX path_idx ON OmicsArea USING btree(path);

CREATE TABLE temporaire(
	id TEXT PRIMARY KEY,
	name TEXT,
	description TEXT,
	idOmicsAreaPere TEXT
);

insert into temporaire (id, name, description) values ('Area','Area', 'Area');
insert into temporaire (id, name, description, idOmicsAreaPere) values ('Proteomic', 'Proteomic', 'Proteomics is the large-scale study of proteins.', 'Area');
insert into temporaire (id, name, description, idOmicsAreaPere) values ('Massspectrometry','Mass spectrometry', 'Mass spectrometry (MS) is an analytical technique that ionizes chemical species and sorts the ions based on their mass-to-charge ratio.', 'Proteomic');
  insert into temporaire (id, name, description, idOmicsAreaPere) values ('Labelfree','Label free', 'Unlike other methods for protein quantification, label-free quantification does not use a stable isotope containing compound to chemically bind to and thus label the protein', 'Massspectrometry');
insert into temporaire (id, name, description, idOmicsAreaPere) values ('Transcriptomic','Transcriptomic', 'Transcriptomics technologies are the techniques used to study an organism s transcriptome, the sum of all of its RNA transcripts.', 'Area');
insert into temporaire (id, name, description, idOmicsAreaPere) values ('Microarray', 'Microarray', 'A DNA microarray is a collection of microscopic DNA spots attached to a solid surface.', 'Transcriptomic');
insert into temporaire (id, name, description, idOmicsAreaPere) values ('RNAseq', 'RNAseq', 'RNA-Seq (RNA sequencing), also called whole transcriptome shotgun sequencing (WTSS), uses next-generation sequencing (NGS) to reveal the presence and quantity of RNA in a biological sample at a given moment.', 'Transcriptomic');
insert into temporaire (id, name, description, idOmicsAreaPere) values ('Annotation', 'Annotation', 'Information about chromosomal feature.', 'Area');
insert into temporaire (id, name, description, idOmicsAreaPere) values ('GOTerms', 'GO Terms', 'The GO defines concepts/classes used to describe gene function, and relationships between these concepts.', 'Annotation');
insert into temporaire (id, name, description, idOmicsAreaPere) values ('Sequence', 'Sequence', 'Sequence of chromosomal feature', 'Annotation');

insert into OmicsArea WITH RECURSIVE nodes_cte(id, name, description, path) AS (
 SELECT tn.id, tn.name,tn.description, tn.id::TEXT AS path
 FROM temporaire AS tn
 WHERE tn.idOmicsAreaPere IS NULL
UNION ALL
 SELECT c.id, c.name, c.description, (p.path || '.' || c.id::TEXT)
 FROM nodes_cte AS p, temporaire AS c
 WHERE c.idOmicsAreaPere = p.id
)
SELECT id, name, description, text2ltree(path) FROM nodes_cte AS n ORDER BY n.id ASC;

DROP TABLE temporaire;


/*------------------------------------------------------------------------------
Experiment
------------------------------------------------------------------------------*/

CREATE TABLE Experiment (
    id TEXT primary key,
    omicsAreaid TEXT NOT NULL,
    description TEXT,
    completionDate DATE,
    strainId INTEGER,
    DataSourceId INTEGER,
    CONSTRAINT fkomicsarea FOREIGN KEY (omicsAreaid) REFERENCES OmicsArea (id),
    CONSTRAINT fkDS FOREIGN KEY (DataSourceId) REFERENCES DataSource (id),
    CONSTRAINT fkStrain FOREIGN KEY (strainId) REFERENCES Strain (id)
);

/*------------------------------------------------------------------------------
Analysis
------------------------------------------------------------------------------*/

CREATE TABLE Analysis (
    id TEXT primary key,
    description TEXT,
    completionDate DATE,
    notebook_file TEXT,
    secondary_data_file TEXT
);


/*------------------------------------------------------------------------------
Analysis -- Experiment
------------------------------------------------------------------------------*/

CREATE TABLE Analysis_Experiment (
  id_experiment TEXT,
  id_analysis TEXT,
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
  id_experiment TEXT,
  PRIMARY KEY (id_experiment, id_tag),
  CONSTRAINT fkexperiment FOREIGN KEY (id_experiment) REFERENCES Experiment (id),
  CONSTRAINT fktag FOREIGN KEY (id_tag) REFERENCES Tag (id)
);

/*------------------------------------------------------------------------------
TAG -- Analysis
------------------------------------------------------------------------------*/

CREATE TABLE Tag_Analysis (
  id_tag INTEGER,
  id_analysis TEXT,
  PRIMARY KEY (id_analysis, id_tag),
  CONSTRAINT fkanalysis FOREIGN KEY (id_analysis) REFERENCES Analysis (id),
  CONSTRAINT fktag FOREIGN KEY (id_tag) REFERENCES Tag (id)
);


/*------------------------------------------------------------------------------
Submission
------------------------------------------------------------------------------*/

CREATE TABLE Submission (
  id TEXT primary key,
  submission_date Date,
  status BOOLEAN,
  pixeler_user_id INTEGER,
  CONSTRAINT fkpixeler FOREIGN KEY (pixeler_user_id) REFERENCES pixeler (id)
);

/*------------------------------------------------------------------------------
PixelSet
------------------------------------------------------------------------------*/

CREATE TABLE PixelSet (
  id TEXT primary key,
  name TEXT NOT NULL,
  pixelSet_file TEXT,
  description TEXT,
  id_analysis TEXT,
  id_submission TEXT,
  CONSTRAINT fkanalysis FOREIGN KEY (id_analysis) REFERENCES Analysis (id),
  CONSTRAINT fksubmission FOREIGN KEY (id_submission) REFERENCES Submission (id)
);


/*------------------------------------------------------------------------------
Pixel
------------------------------------------------------------------------------*/

CREATE TABLE Pixel (
  id serial primary key,
  value TEXT,
  quality_score NUMERIc,
  pixelSet_id TEXT,
  cf_feature_name TEXT,
  OmicsUnitType_id INTEGER,
  CONSTRAINT fkpixelerset FOREIGN KEY (pixelSet_id) REFERENCES PixelSet (id),
  CONSTRAINT fkcf FOREIGN KEY (cf_feature_name) REFERENCES ChromosomalFeature (feature_name),
  CONSTRAINT fkout FOREIGN KEY (OmicsUnitType_id) REFERENCES OmicsUnitType (id)
);
