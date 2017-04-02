#line 1 "./src/schema.c"
/*
** Copyright (c) 2007 D. Richard Hipp
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the Simplified BSD License (also
** known as the "2-Clause License" or "FreeBSD License".)

** This program is distributed in the hope that it will be useful,
** but without any warranty; without even the implied warranty of
** merchantability or fitness for a particular purpose.
**
** Author contact information:
**   drh@hwaci.com
**   http://www.hwaci.com/drh/
**
*******************************************************************************
**
** This file contains string constants that implement the database schema.
*/
#include "config.h"
#include "schema.h"

/*
** The database schema for the ~/.fossil configuration database.
*/
const char zConfigSchema[] =



"CREATE TABLE global_config(\n"
"  name TEXT PRIMARY KEY,\n"
"  value TEXT\n"
");\n"
"\n"


"PRAGMA application_id=252006675;\n"
;

#if INTERFACE
/*
** The content tables have a content version number which rarely
** changes.  The aux tables have an arbitrary version number (typically
** a date) which can change frequently.  When the content schema changes,
** we have to execute special procedures to update the schema.  When
** the aux schema changes, all we need to do is rebuild the database.
*/
#define CONTENT_SCHEMA  "2"
#define AUX_SCHEMA_MIN  "2011-04-25 19:50"
#define AUX_SCHEMA_MAX  "2015-01-24"
/* NB:  Some features require the latest schema.  Warning or error messages
** will appear if an older schema is used.  However, the older schemas are
** adequate for many common functions. */

#endif /* INTERFACE */


/*
** The schema for a repository database.
**
** Schema1[] contains parts of the schema that are fixed and unchanging
** across versions.  Schema2[] contains parts of the schema that can
** change from one version to the next.  The information in Schema2[]
** is reconstructed from the information in Schema1[] by the "rebuild"
** operation.
*/
const char zRepositorySchema1[] =












"CREATE TABLE blob(\n"
"  rid INTEGER PRIMARY KEY,\n"
"  rcvid INTEGER,\n"
"  size INTEGER,\n"
"  uuid TEXT UNIQUE NOT NULL,\n"
"  content BLOB,\n"
"  CHECK( length(uuid)>=40 AND rid>0 )\n"
");\n"
"CREATE TABLE delta(\n"
"  rid INTEGER PRIMARY KEY,\n"
"  srcid INTEGER NOT NULL REFERENCES blob\n"
");\n"
"CREATE INDEX delta_i1 ON delta(srcid);\n"
"\n"






"\n"



"CREATE TABLE rcvfrom(\n"
"  rcvid INTEGER PRIMARY KEY,\n"
"  uid INTEGER REFERENCES user,\n"
"  mtime DATETIME,\n"
"  nonce TEXT UNIQUE,\n"
"  ipaddr TEXT\n"
");\n"
"\n"









"CREATE TABLE user(\n"
"  uid INTEGER PRIMARY KEY,\n"
"  login TEXT UNIQUE,\n"
"  pw TEXT,\n"
"  cap TEXT,\n"
"  cookie TEXT,\n"
"  ipaddr TEXT,\n"
"  cexpire DATETIME,\n"
"  info TEXT,\n"
"  mtime DATE,\n"
"  photo BLOB\n"
");\n"
"\n"



"CREATE TABLE config(\n"
"  name TEXT PRIMARY KEY NOT NULL,\n"
"  value CLOB,\n"
"  mtime DATE,\n"
"  CHECK( typeof(name)='text' AND length(name)>=1 )\n"
");\n"
"\n"










"CREATE TABLE shun(\n"
"  uuid UNIQUE,\n"
"  mtime DATE,\n"
"  scom TEXT\n"
");\n"
"\n"




"CREATE TABLE private(rid INTEGER PRIMARY KEY);\n"
"\n"



"CREATE TABLE reportfmt(\n"
"   rn INTEGER PRIMARY KEY,\n"
"   owner TEXT,\n"
"   title TEXT UNIQUE,\n"
"   mtime DATE,\n"
"   cols TEXT,\n"
"   sqlcode TEXT\n"
");\n"
"\n"








"CREATE TABLE concealed(\n"
"  hash TEXT PRIMARY KEY,\n"
"  mtime DATE,\n"
"  content TEXT\n"
");\n"
"\n"


"PRAGMA application_id=252006673;\n"
;

/*
** The default reportfmt entry for the schema. This is in an extra
** script so that (configure reset) can install the default report.
*/
const char zRepositorySchemaDefaultReports[] =
"INSERT INTO reportfmt(title,mtime,cols,sqlcode)\n"
"VALUES('All Tickets',julianday('1970-01-01'),'#ffffff Key:\n"
"#f2dcdc Active\n"
"#e8e8e8 Review\n"
"#cfe8bd Fixed\n"
"#bde5d6 Tested\n"
"#cacae5 Deferred\n"
"#c8c8c8 Closed','SELECT\n"
"  CASE WHEN status IN (''Open'',''Verified'') THEN ''#f2dcdc''\n"
"       WHEN status=''Review'' THEN ''#e8e8e8''\n"
"       WHEN status=''Fixed'' THEN ''#cfe8bd''\n"
"       WHEN status=''Tested'' THEN ''#bde5d6''\n"
"       WHEN status=''Deferred'' THEN ''#cacae5''\n"
"       ELSE ''#c8c8c8'' END AS ''bgcolor'',\n"
"  substr(tkt_uuid,1,10) AS ''#'',\n"
"  datetime(tkt_mtime) AS ''mtime'',\n"
"  type,\n"
"  status,\n"
"  subsystem,\n"
"  title\n"
"FROM ticket');\n"
;

const char zRepositorySchema2[] =


"CREATE TABLE filename(\n"
"  fnid INTEGER PRIMARY KEY,\n"
"  name TEXT UNIQUE\n"
");\n"
"\n"







"\n"


















"CREATE TABLE mlink(\n"
"  mid INTEGER,\n"
"  fid INTEGER,\n"
"  pmid INTEGER,\n"
"  pid INTEGER,\n"
"  fnid INTEGER REFERENCES filename,\n"
"  pfnid INTEGER REFERENCES filename,\n"
"  mperm INTEGER,\n"
"  isaux BOOLEAN DEFAULT 0\n"
");\n"
"CREATE INDEX mlink_i1 ON mlink(mid);\n"
"CREATE INDEX mlink_i2 ON mlink(fnid);\n"
"CREATE INDEX mlink_i3 ON mlink(fid);\n"
"CREATE INDEX mlink_i4 ON mlink(pid);\n"
"\n"


"CREATE TABLE plink(\n"
"  pid INTEGER REFERENCES blob,\n"
"  cid INTEGER REFERENCES blob,\n"
"  isprim BOOLEAN,\n"
"  mtime DATETIME,\n"
"  baseid INTEGER REFERENCES blob,\n"
"  UNIQUE(pid, cid)\n"
");\n"
"CREATE INDEX plink_i2 ON plink(cid,pid);\n"
"\n"







"CREATE TABLE leaf(rid INTEGER PRIMARY KEY);\n"
"\n"


"CREATE TABLE event(\n"
"  type TEXT,\n"
"  mtime DATETIME,\n"
"  objid INTEGER PRIMARY KEY,\n"
"  tagid INTEGER,\n"
"  uid INTEGER REFERENCES user,\n"
"  bgcolor TEXT,\n"
"  euser TEXT,\n"
"  user TEXT,\n"
"  ecomment TEXT,\n"
"  comment TEXT,\n"
"  brief TEXT,\n"
"  omtime DATETIME\n"
");\n"
"CREATE INDEX event_i1 ON event(mtime);\n"
"\n"



"CREATE TABLE phantom(\n"
"  rid INTEGER PRIMARY KEY\n"
");\n"
"\n"




"CREATE TABLE orphan(\n"
"  rid INTEGER PRIMARY KEY,\n"
"  baseline INTEGER\n"
");\n"
"CREATE INDEX orphan_baseline ON orphan(baseline);\n"
"\n"









"CREATE TABLE unclustered(\n"
"  rid INTEGER PRIMARY KEY\n"
");\n"
"\n"




"CREATE TABLE unsent(\n"
"  rid INTEGER PRIMARY KEY\n"
");\n"
"\n"









"CREATE TABLE tag(\n"
"  tagid INTEGER PRIMARY KEY,\n"
"  tagname TEXT UNIQUE\n"
");\n"
"INSERT INTO tag VALUES(1, 'bgcolor');\n"
"INSERT INTO tag VALUES(2, 'comment');\n"
"INSERT INTO tag VALUES(3, 'user');\n"
"INSERT INTO tag VALUES(4, 'date');\n"
"INSERT INTO tag VALUES(5, 'hidden');\n"
"INSERT INTO tag VALUES(6, 'private');\n"
"INSERT INTO tag VALUES(7, 'cluster');\n"
"INSERT INTO tag VALUES(8, 'branch');\n"
"INSERT INTO tag VALUES(9, 'closed');\n"
"INSERT INTO tag VALUES(10,'parent');\n"
"INSERT INTO tag VALUES(11,'note');\n"
"\n"





"CREATE TABLE tagxref(\n"
"  tagid INTEGER REFERENCES tag,\n"
"  tagtype INTEGER,\n"
"  srcid INTEGER REFERENCES blob,\n"
"  origid INTEGER REFERENCES blob,\n"
"  value TEXT,\n"
"  mtime TIMESTAMP,\n"
"  rid INTEGER REFERENCE blob,\n"
"  UNIQUE(rid, tagid)\n"
");\n"
"CREATE INDEX tagxref_i1 ON tagxref(tagid, mtime);\n"
"\n"





"CREATE TABLE backlink(\n"
"  target TEXT,\n"
"  srctype INT,\n"
"  srcid INT,\n"
"  mtime TIMESTAMP,\n"
"  UNIQUE(target, srctype, srcid)\n"
");\n"
"CREATE INDEX backlink_src ON backlink(srcid, srctype);\n"
"\n"



"CREATE TABLE attachment(\n"
"  attachid INTEGER PRIMARY KEY,\n"
"  isLatest BOOLEAN DEFAULT 0,\n"
"  mtime TIMESTAMP,\n"
"  src TEXT,\n"
"  target TEXT,\n"
"  filename TEXT,\n"
"  comment TEXT,\n"
"  user TEXT\n"
");\n"
"CREATE INDEX attachment_idx1 ON attachment(target, filename, mtime);\n"
"CREATE INDEX attachment_idx2 ON attachment(src);\n"
"\n"





"CREATE TABLE ticket(\n"

"  tkt_id INTEGER PRIMARY KEY,\n"
"  tkt_uuid TEXT UNIQUE,\n"
"  tkt_mtime DATE,\n"
"  tkt_ctime DATE,\n"

"  type TEXT,\n"
"  status TEXT,\n"
"  subsystem TEXT,\n"
"  priority TEXT,\n"
"  severity TEXT,\n"
"  foundin TEXT,\n"
"  private_contact TEXT,\n"
"  resolution TEXT,\n"
"  title TEXT,\n"
"  comment TEXT\n"
");\n"
"CREATE TABLE ticketchng(\n"

"  tkt_id INTEGER REFERENCES ticket,\n"
"  tkt_rid INTEGER REFERENCES blob,\n"
"  tkt_mtime DATE,\n"

"  login TEXT,\n"
"  username TEXT,\n"
"  mimetype TEXT,\n"
"  icomment TEXT\n"
");\n"
"CREATE INDEX ticketchng_idx1 ON ticketchng(tkt_id, tkt_mtime);\n"
;

/*
** Predefined tagid values
*/
#if INTERFACE
# define TAG_BGCOLOR    1     /* Set the background color for display */
# define TAG_COMMENT    2     /* The check-in comment */
# define TAG_USER       3     /* User who made a checking */
# define TAG_DATE       4     /* The date of a check-in */
# define TAG_HIDDEN     5     /* Do not display in timeline */
# define TAG_PRIVATE    6     /* Do not sync */
# define TAG_CLUSTER    7     /* A cluster */
# define TAG_BRANCH     8     /* Value is name of the current branch */
# define TAG_CLOSED     9     /* Do not display this check-in as a leaf */
# define TAG_PARENT     10    /* Change to parentage on a check-in */
# define TAG_NOTE       11    /* Extra text appended to a check-in comment */
#endif

/*
** The schema for the local FOSSIL database file found at the root
** of every check-out.  This database contains the complete state of
** the checkout.
*/
const char zLocalSchema[] =










"CREATE TABLE vvar(\n"
"  name TEXT PRIMARY KEY NOT NULL,\n"
"  value CLOB,\n"
"  CHECK( typeof(name)='text' AND length(name)>=1 )\n"
");\n"
"\n"















"CREATE TABLE vfile(\n"
"  id INTEGER PRIMARY KEY,\n"
"  vid INTEGER REFERENCES blob,\n"
"  chnged INT DEFAULT 0,\n"
"  deleted BOOLEAN DEFAULT 0,\n"
"  isexe BOOLEAN,\n"
"  islink BOOLEAN,\n"
"  rid INTEGER,\n"
"  mrid INTEGER,\n"
"  mtime INTEGER,\n"
"  pathname TEXT,\n"
"  origname TEXT,\n"
"  UNIQUE(pathname,vid)\n"
");\n"
"\n"







"\n"
"CREATE TABLE vmerge(\n"
"  id INTEGER REFERENCES vfile,\n"
"  merge INTEGER,\n"
"  UNIQUE(id, merge)\n"
");\n"
"\n"


"PRAGMA application_id=252006674;\n"
;
