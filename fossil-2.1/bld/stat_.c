#line 1 "./src/stat.c"
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
** This file contains code to implement the stat web page
**
*/
#include "VERSION.h"
#include "config.h"
#include <string.h>
#include "stat.h"

/*
** For a sufficiently large integer, provide an alternative
** representation as MB or GB or TB.
*/
void bigSizeName(int nOut, char *zOut, sqlite3_int64 v){
  if( v<100000 ){
    sqlite3_snprintf(nOut, zOut, "%lld bytes", v);
  }else if( v<1000000000 ){
    sqlite3_snprintf(nOut, zOut, "%lld bytes (%.1fMB)",
                    v, (double)v/1000000.0);
  }else{
    sqlite3_snprintf(nOut, zOut, "%lld bytes (%.1fGB)",
                    v, (double)v/1000000000.0);
  }
}

/*
** Return the approximate size as KB, MB, GB, or TB.
*/
void approxSizeName(int nOut, char *zOut, sqlite3_int64 v){
  if( v<1000 ){
    sqlite3_snprintf(nOut, zOut, "%lld bytes", v);
  }else if( v<1000000 ){
    sqlite3_snprintf(nOut, zOut, "%.1fKB", (double)v/1000.0);
  }else if( v<1000000000 ){
    sqlite3_snprintf(nOut, zOut, "%.1fMB", (double)v/1000000.0);
  }else{
    sqlite3_snprintf(nOut, zOut, "%.1fGB", (double)v/1000000000.0);
  }
}

/*
** WEBPAGE: stat
**
** Show statistics and global information about the repository.
*/
void stat_page(void){
  i64 t, fsize;
  int n, m;
  int szMax, szAvg;
  int brief;
  char zBuf[100];
  const char *p;

  login_check_credentials();
  if( !g.perm.Read ){ login_needed(g.anon.Read); return; }
  brief = P("brief")!=0;
  style_header("Repository Statistics");
  style_adunit_config(ADUNIT_RIGHT_OK);
  if( g.perm.Admin ){
    style_submenu_element("URLs", "urllist");
    style_submenu_element("Schema", "repo_schema");
    style_submenu_element("Web-Cache", "cachestat");
  }
  style_submenu_element("Activity Reports", "reports");
  style_submenu_element("Hash Collisions", "hash-collisions");
  if( sqlite3_compileoption_used("ENABLE_DBSTAT_VTAB") ){
    style_submenu_element("Table Sizes", "repo-tabsize");
  }
  if( g.perm.Admin || g.perm.Setup || db_get_boolean("test_env_enable",0) ){
    style_submenu_element("Environment", "test_env");
  }
  cgi_printf("<table class=\"label-value\">\n"
         "<tr><th>Repository&nbsp;Size:</th><td>\n");
  fsize = file_size(g.zRepositoryName);
  bigSizeName(sizeof(zBuf), zBuf, fsize);
  cgi_printf("%s\n"
         "</td></tr>\n",(zBuf));
  if( !brief ){
    cgi_printf("<tr><th>Number&nbsp;Of&nbsp;Artifacts:</th><td>\n");
    n = db_int(0, "SELECT count(*) FROM blob");
    m = db_int(0, "SELECT count(*) FROM delta");
    cgi_printf("%d (%d fulltext and %d deltas)\n"
           "</td></tr>\n",(n),(n-m),(m));
    if( n>0 ){
      int a, b;
      Stmt q;
      cgi_printf("<tr><th>Uncompressed&nbsp;Artifact&nbsp;Size:</th><td>\n");
      db_prepare(&q, "SELECT total(size), avg(size), max(size)"
                     " FROM blob WHERE size>0 /*scan*/");
      db_step(&q);
      t = db_column_int64(&q, 0);
      szAvg = db_column_int(&q, 1);
      szMax = db_column_int(&q, 2);
      db_finalize(&q);
      bigSizeName(sizeof(zBuf), zBuf, t);
      cgi_printf("%d bytes average, %d bytes max, %s total\n"
             "</td></tr>\n"
             "<tr><th>Compression&nbsp;Ratio:</th><td>\n",(szAvg),(szMax),(zBuf));
      if( t/fsize < 5 ){
        b = 10;
        fsize /= 10;
      }else{
        b = 1;
      }
      a = t/fsize;
      cgi_printf("%d:%d\n"
             "</td></tr>\n",(a),(b));
    }
    if( db_table_exists("repository","unversioned") ){
      Stmt q;
      char zStored[100];
      db_prepare(&q,
        "SELECT count(*), sum(sz), sum(length(content))"
        "  FROM unversioned"
        " WHERE length(hash)>1"
      );
      if( db_step(&q)==SQLITE_ROW && (n = db_column_int(&q,0))>0 ){
        sqlite3_int64 iSz, iStored;
        iSz = db_column_int64(&q,1);
        iStored = db_column_int64(&q,2);
        approxSizeName(sizeof(zBuf), zBuf, iSz);
        approxSizeName(sizeof(zStored), zStored, iStored);
        cgi_printf("<tr><th>Unversioned&nbsp;Files:</th><td>\n"
               "%z%d files</a>,\n"
               "total size %s uncompressed, %s compressed\n"
               "</td></tr>\n",(href("%R/uvlist")),(n),(zBuf),(zStored));
      }
      db_finalize(&q);
    }
    cgi_printf("<tr><th>Number&nbsp;Of&nbsp;Check-ins:</th><td>\n");
    n = db_int(0, "SELECT count(*) FROM event WHERE type='ci' /*scan*/");
    cgi_printf("%d\n"
           "</td></tr>\n"
           "<tr><th>Number&nbsp;Of&nbsp;Files:</th><td>\n",(n));
    n = db_int(0, "SELECT count(*) FROM filename /*scan*/");
    cgi_printf("%d\n"
           "</td></tr>\n"
           "<tr><th>Number&nbsp;Of&nbsp;Wiki&nbsp;Pages:</th><td>\n",(n));
    n = db_int(0, "SELECT count(*) FROM tag  /*scan*/"
                  " WHERE +tagname GLOB 'wiki-*'");
    cgi_printf("%d\n"
           "</td></tr>\n"
           "<tr><th>Number&nbsp;Of&nbsp;Tickets:</th><td>\n",(n));
    n = db_int(0, "SELECT count(*) FROM tag  /*scan*/"
                  " WHERE +tagname GLOB 'tkt-*'");
    cgi_printf("%d\n"
           "</td></tr>\n",(n));
  }
  cgi_printf("<tr><th>Duration&nbsp;Of&nbsp;Project:</th><td>\n");
  n = db_int(0, "SELECT julianday('now') - (SELECT min(mtime) FROM event)"
                " + 0.99");
  cgi_printf("%d days or approximately %.2f years.\n"
         "</td></tr>\n",(n),(n/365.2425));
  p = db_get("project-code", 0);
  if( p ){
    cgi_printf("<tr><th>Project&nbsp;ID:</th>\n"
           "    <td>%h %h</td></tr>\n",(p),(db_get("project-name","")));
  }
  p = db_get("parent-project-code", 0);
  if( p ){
    cgi_printf("<tr><th>Parent&nbsp;Project&nbsp;ID:</th>\n"
           "     <td>%h %h</td></tr>\n",(p),(db_get("parent-project-name","")));
  }
  /* @ <tr><th>Server&nbsp;ID:</th><td>%h(db_get("server-code",""))</td></tr> */
  cgi_printf("<tr><th>Fossil&nbsp;Version:</th><td>\n"
         "%h %h\n"
         "(%h) <a href='version?verbose=1'>(details)</a>\n"
         "</td></tr>\n"
         "<tr><th>SQLite&nbsp;Version:</th><td>%.19s\n"
         "[%.10s] (%s)\n"
         "<a href='version?verbose=2'>(details)</a></td></tr>\n",(MANIFEST_DATE),(MANIFEST_VERSION),(RELEASE_VERSION),(sqlite3_sourceid()),(&sqlite3_sourceid()[20]),(sqlite3_libversion()));
  if( g.eHashPolicy!=HPOLICY_AUTO ){
    cgi_printf("<tr><th>Schema&nbsp;Version:</th><td>%h,\n"
           "%s</td></tr>\n",(g.zAuxSchema),(hpolicy_name()));
  }else{
    cgi_printf("<tr><th>Schema&nbsp;Version:</th><td>%h</td></tr>\n",(g.zAuxSchema));
  }
  cgi_printf("<tr><th>Repository Rebuilt:</th><td>\n"
         "%h\n"
         "By Fossil %h</td></tr>\n"
         "<tr><th>Database&nbsp;Stats:</th><td>\n"
         "%d pages,\n"
         "%d bytes/page,\n"
         "%d free pages,\n"
         "%s,\n"
         "%s mode\n"
         "</td></tr>\n",(db_get_mtime("rebuilt","%Y-%m-%d %H:%M:%S","Never")),(db_get("rebuilt","Unknown")),(db_int(0, "PRAGMA repository.page_count")),(db_int(0, "PRAGMA repository.page_size")),(db_int(0, "PRAGMA repository.freelist_count")),(db_text(0, "PRAGMA repository.encoding")),(db_text(0, "PRAGMA repository.journal_mode")));

  cgi_printf("</table>\n");
  style_footer();
}

/*
** COMMAND: dbstat*
**
** Usage: %fossil dbstat OPTIONS
**
** Shows statistics and global information about the repository.
**
** Options:
**
**   --brief|-b           Only show essential elements
**   --db-check           Run a PRAGMA quick_check on the repository database
**   --omit-version-info  Omit the SQLite and Fossil version information
*/
void dbstat_cmd(void){
  i64 t, fsize;
  int n, m;
  int szMax, szAvg;
  int brief;
  int omitVers;            /* Omit Fossil and SQLite version information */
  int dbCheck;             /* True for the --db-check option */
  char zBuf[100];
  const int colWidth = -19 /* printf alignment/width for left column */;
  const char *p, *z;

  brief = find_option("brief", "b",0)!=0;
  omitVers = find_option("omit-version-info", 0, 0)!=0;
  dbCheck = find_option("db-check",0,0)!=0;
  db_find_and_open_repository(0,0);

  /* We should be done with options.. */
  verify_all_options();

  if( (z = db_get("project-name",0))!=0
   || (z = db_get("short-project-name",0))!=0
  ){
    fossil_print("%*s%s\n", colWidth, "project-name:", z);
  }
  fsize = file_size(g.zRepositoryName);
  bigSizeName(sizeof(zBuf), zBuf, fsize);
  fossil_print( "%*s%s\n", colWidth, "repository-size:", zBuf );
  if( !brief ){
    n = db_int(0, "SELECT count(*) FROM blob");
    m = db_int(0, "SELECT count(*) FROM delta");
    fossil_print("%*s%d (stored as %d full text and %d delta blobs)\n",
                 colWidth, "artifact-count:",
                 n, n-m, m);
    if( n>0 ){
      int a, b;
      Stmt q;
      db_prepare(&q, "SELECT total(size), avg(size), max(size)"
                     " FROM blob WHERE size>0");
      db_step(&q);
      t = db_column_int64(&q, 0);
      szAvg = db_column_int(&q, 1);
      szMax = db_column_int(&q, 2);
      db_finalize(&q);
      bigSizeName(sizeof(zBuf), zBuf, t);
      fossil_print( "%*s%d average, "
                    "%d max, %s total\n",
                    colWidth, "artifact-sizes:",
                    szAvg, szMax, zBuf);
      if( t/fsize < 5 ){
        b = 10;
        fsize /= 10;
      }else{
        b = 1;
      }
      a = t/fsize;
      fossil_print("%*s%d:%d\n", colWidth, "compression-ratio:", a, b);
    }
    n = db_int(0, "SELECT COUNT(*) FROM event e WHERE e.type='ci'");
    fossil_print("%*s%d\n", colWidth, "check-ins:", n);
    n = db_int(0, "SELECT count(*) FROM filename /*scan*/");
    fossil_print("%*s%d across all branches\n", colWidth, "files:", n);
    n = db_int(0, "SELECT count(*) FROM tag  /*scan*/"
                  " WHERE tagname GLOB 'wiki-*'");
    m = db_int(0, "SELECT COUNT(*) FROM event WHERE type='w'");
    fossil_print("%*s%d (%d changes)\n", colWidth, "wiki-pages:", n, m);
    n = db_int(0, "SELECT count(*) FROM tag  /*scan*/"
                  " WHERE tagname GLOB 'tkt-*'");
    m = db_int(0, "SELECT COUNT(*) FROM event WHERE type='t'");
    fossil_print("%*s%d (%d changes)\n", colWidth, "tickets:", n, m);
    n = db_int(0, "SELECT COUNT(*) FROM event WHERE type='e'");
    fossil_print("%*s%d\n", colWidth, "events:", n);
    n = db_int(0, "SELECT COUNT(*) FROM event WHERE type='g'");
    fossil_print("%*s%d\n", colWidth, "tag-changes:", n);
    z = db_text(0, "SELECT datetime(mtime) || ' - about ' ||"
                   " CAST(julianday('now') - mtime AS INTEGER)"
                   " || ' days ago' FROM event "
                   " ORDER BY mtime DESC LIMIT 1");
    fossil_print("%*s%s\n", colWidth, "latest-change:", z);
  }
  n = db_int(0, "SELECT julianday('now') - (SELECT min(mtime) FROM event)"
                " + 0.99");
  fossil_print("%*s%d days or approximately %.2f years.\n",
               colWidth, "project-age:", n, n/365.2425);
  p = db_get("project-code", 0);
  if( p ){
    fossil_print("%*s%s\n", colWidth, "project-id:", p);
  }
#if 0
  /* Server-id is not useful information any more */
  fossil_print("%*s%s\n", colWidth, "server-id:", db_get("server-code", 0));
#endif
  fossil_print("%*s%s\n", colWidth, "schema-version:", g.zAuxSchema);
  if( !omitVers ){
    fossil_print("%*s%s %s [%s] (%s)\n",
                 colWidth, "fossil-version:",
                 MANIFEST_DATE, MANIFEST_VERSION, RELEASE_VERSION,
                 COMPILER_NAME);
    fossil_print("%*s%.19s [%.10s] (%s)\n",
                 colWidth, "sqlite-version:",
                 sqlite3_sourceid(), &sqlite3_sourceid()[20],
                 sqlite3_libversion());
  }
  fossil_print("%*s%d pages, %d bytes/pg, %d free pages, "
               "%s, %s mode\n",
               colWidth, "database-stats:",
               db_int(0, "PRAGMA repository.page_count"),
               db_int(0, "PRAGMA repository.page_size"),
               db_int(0, "PRAGMA repository.freelist_count"),
               db_text(0, "PRAGMA repository.encoding"),
               db_text(0, "PRAGMA repository.journal_mode"));
  if( dbCheck ){
    fossil_print("%*s%s\n", colWidth, "database-check:",
                 db_text(0, "PRAGMA quick_check(1)"));
  }
}

/*
** WEBPAGE: urllist
**
** Show ways in which this repository has been accessed
*/
void urllist_page(void){
  Stmt q;
  int cnt;
  login_check_credentials();
  if( !g.perm.Admin ){ login_needed(0); return; }

  style_header("URLs and Checkouts");
  style_adunit_config(ADUNIT_RIGHT_OK);
  style_submenu_element("Stat", "stat");
  style_submenu_element("Schema", "repo_schema");
  cgi_printf("<div class=\"section\">URLs</div>\n"
         "<table border=\"0\" width='100%%'>\n");
  db_prepare(&q, "SELECT substr(name,9), datetime(mtime,'unixepoch')"
                 "  FROM config WHERE name GLOB 'baseurl:*' ORDER BY 2 DESC");
  cnt = 0;
  while( db_step(&q)==SQLITE_ROW ){
    cgi_printf("<tr><td width='100%%'>%h</td>\n"
           "<td><nobr>%h</nobr></td></tr>\n",(db_column_text(&q,0)),(db_column_text(&q,1)));
    cnt++;
  }
  db_finalize(&q);
  if( cnt==0 ){
    cgi_printf("<tr><td>(none)</td>\n");
  }
  cgi_printf("</table>\n"
         "<div class=\"section\">Checkouts</div>\n"
         "<table border=\"0\" width='100%%'>\n");
  db_prepare(&q, "SELECT substr(name,7), datetime(mtime,'unixepoch')"
                 "  FROM config WHERE name GLOB 'ckout:*' ORDER BY 2 DESC");
  cnt = 0;
  while( db_step(&q)==SQLITE_ROW ){
    cgi_printf("<tr><td width='100%%'>%h</td>\n"
           "<td><nobr>%h</nobr></td></tr>\n",(db_column_text(&q,0)),(db_column_text(&q,1)));
    cnt++;
  }
  db_finalize(&q);
  if( cnt==0 ){
    cgi_printf("<tr><td>(none)</td>\n");
  }
  cgi_printf("</table>\n");
  style_footer();
}

/*
** WEBPAGE: repo_schema
**
** Show the repository schema
*/
void repo_schema_page(void){
  Stmt q;
  login_check_credentials();
  if( !g.perm.Admin ){ login_needed(0); return; }

  style_header("Repository Schema");
  style_adunit_config(ADUNIT_RIGHT_OK);
  style_submenu_element("Stat", "stat");
  style_submenu_element("URLs", "urllist");
  if( sqlite3_compileoption_used("ENABLE_DBSTAT_VTAB") ){
    style_submenu_element("Table Sizes", "repo-tabsize");
  }
  db_prepare(&q,
      "SELECT sql FROM repository.sqlite_master WHERE sql IS NOT NULL");
  cgi_printf("<pre>\n");
  while( db_step(&q)==SQLITE_ROW ){
    cgi_printf("%h;\n",(db_column_text(&q, 0)));
  }
  cgi_printf("</pre>\n");
  db_finalize(&q);
  style_footer();
}

/*
** WEBPAGE: repo-tabsize
**
** Show relative sizes of tables in the repository database.
*/
void repo_tabsize_page(void){
  int nPageFree;
  sqlite3_int64 fsize;
  char zBuf[100];

  login_check_credentials();
  if( !g.perm.Read ){ login_needed(g.anon.Read); return; }
  style_header("Repository Table Sizes");
  style_adunit_config(ADUNIT_RIGHT_OK);
  style_submenu_element("Stat", "stat");
  if( g.perm.Admin ){
    style_submenu_element("Schema", "repo_schema");
  }
  db_multi_exec(
    "CREATE TEMP TABLE trans(name TEXT PRIMARY KEY,tabname TEXT)WITHOUT ROWID;"
    "INSERT INTO trans(name,tabname)"
    "   SELECT name, tbl_name FROM repository.sqlite_master;"
    "CREATE TEMP TABLE piechart(amt REAL, label TEXT);"
    "INSERT INTO piechart(amt,label)"
    "  SELECT count(*), "
    "  coalesce((SELECT tabname FROM trans WHERE trans.name=dbstat.name),name)"
    "    FROM dbstat('repository')"
    "   GROUP BY 2 ORDER BY 2;"
  );
  nPageFree = db_int(0, "PRAGMA repository.freelist_count");
  if( nPageFree>0 ){
    db_multi_exec(
      "INSERT INTO piechart(amt,label) VALUES(%d,'freelist')",
      nPageFree
    );
  }
  fsize = file_size(g.zRepositoryName);
  approxSizeName(sizeof(zBuf), zBuf, fsize);
  cgi_printf("<h2>Repository Size: %s</h2>\n"
         "<center><svg width='800' height='500'>\n",(zBuf));
  piechart_render(800,500,PIE_OTHER|PIE_PERCENT);
  cgi_printf("</svg></center>\n");

  if( g.localOpen ){
    db_multi_exec(
      "DELETE FROM trans;"
      "INSERT INTO trans(name,tabname)"
      "   SELECT name, tbl_name FROM localdb.sqlite_master;"
      "DELETE FROM piechart;"
      "INSERT INTO piechart(amt,label)"
      "  SELECT count(*), "
      " coalesce((SELECT tabname FROM trans WHERE trans.name=dbstat.name),name)"
      "    FROM dbstat('localdb')"
      "   GROUP BY 2 ORDER BY 2;"
    );
    nPageFree = db_int(0, "PRAGMA localdb.freelist_count");
    if( nPageFree>0 ){
      db_multi_exec(
        "INSERT INTO piechart(amt,label) VALUES(%d,'freelist')",
        nPageFree
      );
    }
    fsize = file_size(g.zLocalDbName);
    approxSizeName(sizeof(zBuf), zBuf, fsize);
    cgi_printf("<h2>%h Size: %s</h2>\n"
           "<center><svg width='800' height='500'>\n",(file_tail(g.zLocalDbName)),(zBuf));
    piechart_render(800,500,PIE_OTHER|PIE_PERCENT);
    cgi_printf("</svg></center>\n");
  }
  style_footer();
}
