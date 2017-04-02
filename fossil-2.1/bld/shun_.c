#line 1 "./src/shun.c"
/*
** Copyright (c) 2008 D. Richard Hipp
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
** This file contains code used to manage SHUN table of the repository
*/
#include "config.h"
#include "shun.h"
#include <assert.h>

/*
** Return true if the given artifact ID should be shunned.
*/
int uuid_is_shunned(const char *zUuid){
  static Stmt q;
  int rc;
  if( zUuid==0 || zUuid[0]==0 ) return 0;
  if( g.eHashPolicy==HPOLICY_SHUN_SHA1 && zUuid[HNAME_LEN_SHA1]==0 ) return 1;
  db_static_prepare(&q, "SELECT 1 FROM shun WHERE uuid=:uuid");
  db_bind_text(&q, ":uuid", zUuid);
  rc = db_step(&q);
  db_reset(&q);
  return rc==SQLITE_ROW;
}

/*
** WEBPAGE: shun
**
** View the hashes of all shunned artifacts.  Add new hashes
** to the shun set.  Requires Admin privilege.
*/
void shun_page(void){
  Stmt q;
  int cnt = 0;
  const char *zUuid = P("uuid");
  const char *zShun = P("shun");
  const char *zAccept = P("accept");
  const char *zRcvid = P("rcvid");
  int nRcvid = 0;
  int numRows = 3;
  char *zCanonical = 0;

  login_check_credentials();
  if( !g.perm.Admin ){
    login_needed(0);
    return;
  }
  if( P("rebuild") ){
    db_close(1);
    db_open_repository(g.zRepositoryName);
    db_begin_transaction();
    rebuild_db(0, 0, 0);
    admin_log("Rebuilt database.");
    db_end_transaction(0);
  }
  if( zUuid ){
    char *p;
    int i = 0;
    int j = 0;
    zCanonical = fossil_malloc(strlen(zUuid)+2);
    while( zUuid[i] ){
      if( fossil_isspace(zUuid[i]) ){
        if( j && zCanonical[j-1] ){
          zCanonical[j] = 0;
          j++;
        }
      }else{
        zCanonical[j] = zUuid[i];
        j++;
      }
      i++;
    }
    zCanonical[j+1] = zCanonical[j] = 0;
    p = zCanonical;
    while( *p ){
      int nUuid = strlen(p);
      if( !hname_validate(p, nUuid) ){
        cgi_printf("<p class=\"generalError\">Error: Bad artifact IDs.</p>\n");
        fossil_free(zCanonical);
        zCanonical = 0;
        break;
      }else{
        canonical16(p, nUuid);
        p += nUuid+1;
      }
    }
    zUuid = zCanonical;
  }
  style_header("Shunned Artifacts");
  if( zUuid && P("sub") ){
    const char *p = zUuid;
    int allExist = 1;
    login_verify_csrf_secret();
    while( *p ){
      db_multi_exec("DELETE FROM shun WHERE uuid=%Q", p);
      if( !db_exists("SELECT 1 FROM blob WHERE uuid=%Q", p) ){
        allExist = 0;
      }
      admin_log("Unshunned %Q", p);
      p += strlen(p)+1;
    }
    if( allExist ){
      cgi_printf("<p class=\"noMoreShun\">Artifact(s)<br />\n");
      for( p = zUuid ; *p ; p += strlen(p)+1 ){
        cgi_printf("<a href=\"%R/artifact/%s\">%s</a><br />\n",(p),(p));
      }
      cgi_printf("are no longer being shunned.</p>\n");
    }else{
      cgi_printf("<p class=\"noMoreShun\">Artifact(s)<br />\n");
      for( p = zUuid ; *p ; p += strlen(p)+1 ){
        cgi_printf("%s<br />\n",(p));
      }
      cgi_printf("will no longer be shunned.  But they may not exist in the repository.\n"
             "It may be necessary to rebuild the repository using the\n"
             "<b>fossil rebuild</b> command-line before the artifact content\n"
             "can pulled in from other repositories.</p>\n");
    }
  }
  if( zUuid && P("add") ){
    const char *p = zUuid;
    int rid, tagid;
    login_verify_csrf_secret();
    while( *p ){
      db_multi_exec(
        "INSERT OR IGNORE INTO shun(uuid,mtime)"
        " VALUES(%Q, now())", p);
      db_multi_exec("DELETE FROM attachment WHERE src=%Q", p);
      rid = db_int(0, "SELECT rid FROM blob WHERE uuid=%Q", p);
      if( rid ){
        db_multi_exec("DELETE FROM event WHERE objid=%d", rid);
      }
      tagid = db_int(0, "SELECT tagid FROM tag WHERE tagname='tkt-%q'", p);
      if( tagid ){
        db_multi_exec("DELETE FROM ticket WHERE tkt_uuid=%Q", p);
        db_multi_exec("DELETE FROM tag WHERE tagid=%d", tagid);
        db_multi_exec("DELETE FROM tagxref WHERE tagid=%d", tagid);
      }
      admin_log("Shunned %Q", p);
      p += strlen(p)+1;
    }
    cgi_printf("<p class=\"shunned\">Artifact(s)<br />\n");
    for( p = zUuid ; *p ; p += strlen(p)+1 ){
      cgi_printf("<a href=\"%R/artifact/%s\">%s</a><br />\n",(p),(p));
    }
    cgi_printf("have been shunned.  They will no longer be pushed.\n"
           "They will be removed from the repository the next time the repository\n"
           "is rebuilt using the <b>fossil rebuild</b> command-line</p>\n");
  }
  if( zRcvid ){
    nRcvid = atoi(zRcvid);
    numRows = db_int(0, "SELECT min(count(), 10) FROM blob WHERE rcvid=%d",
                     nRcvid);
  }
  cgi_printf("<p>A shunned artifact will not be pushed nor accepted in a pull and the\n"
         "artifact content will be purged from the repository the next time the\n"
         "repository is rebuilt.  A list of shunned artifacts can be seen at the\n"
         "bottom of this page.</p>\n"
         "\n"
         "<a name=\"addshun\"></a>\n"
         "<p>To shun artifacts, enter their artifact hashes (the 40- or\n"
         "64-character lowercase hexadecimal hash of the artifact content) in the\n"
         "following box and press the \"Shun\" button.  This will cause the artifacts\n"
         "to be removed from the repository and will prevent the artifacts from being\n"
         "readded to the repository by subsequent sync operation.</p>\n"
         "\n"
         "<p>Note that you must enter the full 40- or 64-character artifact hashes,\n"
         "not an abbreviation or a symbolic tag.</p>\n"
         "\n"
         "<p>Warning:  Shunning should only be used to remove inappropriate content\n"
         "from the repository.  Inappropriate content includes such things as\n"
         "spam added to Wiki, files that violate copyright or patent agreements,\n"
         "or artifacts that by design or accident interfere with the processing\n"
         "of the repository.  Do not shun artifacts merely to remove them from\n"
         "sight - set the \"hidden\" tag on such artifacts instead.</p>\n"
         "\n"
         "<blockquote>\n"
         "<form method=\"post\" action=\"%s/%s\"><div>\n",(g.zTop),(g.zPath));
  login_insert_csrf_secret();
  cgi_printf("<textarea class=\"fullsize-text\" cols=\"50\" rows=\"%d\" name=\"uuid\">\n",(numRows));
  if( zShun ){
    if( strlen(zShun) ){
      cgi_printf("%h\n",(zShun));
    }else if( nRcvid ){
      db_prepare(&q, "SELECT uuid FROM blob WHERE rcvid=%d", nRcvid);
      while( db_step(&q)==SQLITE_ROW ){
        cgi_printf("%s\n",(db_column_text(&q, 0)));
      }
      db_finalize(&q);
    }
  }
  cgi_printf("</textarea>\n"
         "<input type=\"submit\" name=\"add\" value=\"Shun\" />\n"
         "</div></form>\n"
         "</blockquote>\n"
         "\n"
         "<a name=\"delshun\"></a>\n"
         "<p>Enter the UUIDs of previously shunned artifacts to cause them to be\n"
         "accepted again in the repository.  The artifacts content is not\n"
         "restored because the content is unknown.  The only change is that\n"
         "the formerly shunned artifacts will be accepted on subsequent sync\n"
         "operations.</p>\n"
         "\n"
         "<blockquote>\n"
         "<form method=\"post\" action=\"%s/%s\"><div>\n",(g.zTop),(g.zPath));
  login_insert_csrf_secret();
  cgi_printf("<textarea class=\"fullsize-text\" cols=\"50\" rows=\"%d\" name=\"uuid\">\n",(numRows));
  if( zAccept ){
    if( strlen(zAccept) ){
      cgi_printf("%h\n",(zAccept));
    }else if( nRcvid ){
      db_prepare(&q, "SELECT uuid FROM blob WHERE rcvid=%d", nRcvid);
      while( db_step(&q)==SQLITE_ROW ){
        cgi_printf("%s\n",(db_column_text(&q, 0)));
      }
      db_finalize(&q);
    }
  }
  cgi_printf("</textarea>\n"
         "<input type=\"submit\" name=\"sub\" value=\"Accept\" />\n"
         "</div></form>\n"
         "</blockquote>\n"
         "\n"
         "<p>Press the Rebuild button below to rebuild the repository.  The\n"
         "content of newly shunned artifacts is not purged until the repository\n"
         "is rebuilt.  On larger repositories, the rebuild may take minute or\n"
         "two, so be patient after pressing the button.</p>\n"
         "\n"
         "<blockquote>\n"
         "<form method=\"post\" action=\"%s/%s\"><div>\n",(g.zTop),(g.zPath));
  login_insert_csrf_secret();
  cgi_printf("<input type=\"submit\" name=\"rebuild\" value=\"Rebuild\" />\n"
         "</div></form>\n"
         "</blockquote>\n"
         "\n"
         "<hr /><p>Shunned Artifacts:</p>\n"
         "<blockquote><p>\n");
  db_prepare(&q,
     "SELECT uuid, EXISTS(SELECT 1 FROM blob WHERE blob.uuid=shun.uuid)"
     "  FROM shun ORDER BY uuid");
  while( db_step(&q)==SQLITE_ROW ){
    const char *zUuid = db_column_text(&q, 0);
    int stillExists = db_column_int(&q, 1);
    cnt++;
    if( stillExists ){
      cgi_printf("<b><a href=\"%R/artifact/%s\">%s</a></b><br />\n",(zUuid),(zUuid));
    }else{
      cgi_printf("<b>%s</b><br />\n",(zUuid));
    }
  }
  if( cnt==0 ){
    cgi_printf("<i>no artifacts are shunned on this server</i>\n");
  }
  db_finalize(&q);
  cgi_printf("</p></blockquote>\n");
  style_footer();
  fossil_free(zCanonical);
}

/*
** Remove from the BLOB table all artifacts that are in the SHUN table.
*/
void shun_artifacts(void){
  Stmt q;
  db_multi_exec(
     "CREATE TEMP TABLE toshun(rid INTEGER PRIMARY KEY);"
     "INSERT INTO toshun SELECT rid FROM blob, shun WHERE blob.uuid=shun.uuid;"
  );
  db_prepare(&q,
     "SELECT rid FROM delta WHERE srcid IN toshun"
  );
  while( db_step(&q)==SQLITE_ROW ){
    int srcid = db_column_int(&q, 0);
    content_undelta(srcid);
  }
  db_finalize(&q);
  db_multi_exec(
     "DELETE FROM delta WHERE rid IN toshun;"
     "DELETE FROM blob WHERE rid IN toshun;"
     "DROP TABLE toshun;"
     "DELETE FROM private "
     " WHERE NOT EXISTS (SELECT 1 FROM blob WHERE rid=private.rid);"
  );
}

/*
** WEBPAGE: rcvfromlist
**
** Show a listing of RCVFROM table entries.
**
** The RCVFROM table records where this repository received each
** artifact, including the time of receipt, user, and IP address.
**
** Access requires Admin privilege.
*/
void rcvfromlist_page(void){
  int ofst = atoi(PD("ofst","0"));
  int showAll = P("all")!=0;
  int cnt;
  Stmt q;

  login_check_credentials();
  if( !g.perm.Admin ){
    login_needed(0);
    return;
  }
  style_header("Artifact Receipts");
  if( showAll ){
    ofst = 0;
  }else{
    style_submenu_element("All", "rcvfromlist?all=1");
  }
  if( ofst>0 ){
    style_submenu_element("Newer", "rcvfromlist?ofst=%d",
                           ofst>30 ? ofst-30 : 0);
  }
  db_multi_exec(
    "CREATE TEMP TABLE rcvidUsed(x INTEGER PRIMARY KEY);"
    "INSERT OR IGNORE INTO rcvidUsed(x) SELECT rcvid FROM blob;"
  );
  if( db_table_exists("repository","unversioned") ){
    db_multi_exec(
      "INSERT OR IGNORE INTO rcvidUsed(x) SELECT rcvid FROM unversioned;"
    );
  }
  db_prepare(&q,
    "SELECT rcvid, login, datetime(rcvfrom.mtime), rcvfrom.ipaddr,"
    "       EXISTS(SELECT 1 FROM rcvidUsed WHERE x=rcvfrom.rcvid)"
    "  FROM rcvfrom LEFT JOIN user USING(uid)"
    " ORDER BY rcvid DESC LIMIT %d OFFSET %d",
    showAll ? -1 : 31, ofst
  );
  cgi_printf("<p>Whenever new artifacts are added to the repository, either by\n"
         "push or using the web interface, an entry is made in the RCVFROM table\n"
         "to record the source of that artifact.  This log facilitates\n"
         "finding and fixing attempts to inject illicit content into the\n"
         "repository.</p>\n"
         "\n"
         "<p>Click on the \"rcvid\" to show a list of specific artifacts received\n"
         "by a transaction.  After identifying illicit artifacts, remove them\n"
         "using the \"Shun\" button.  If an \"rcvid\" is not hyperlinked, that means\n"
         "all artifacts associated with that rcvid have already been shunned\n"
         "or purged.</p>\n"
         "\n"
         "<table cellpadding=\"0\" cellspacing=\"0\" border=\"0\">\n"
         "<tr><th style=\"padding-right: 15px;text-align: right;\">rcvid</th>\n"
         "    <th style=\"padding-right: 15px;text-align: left;\">Date</th>\n"
         "    <th style=\"padding-right: 15px;text-align: left;\">User</th>\n"
         "    <th style=\"text-align: left;\">IP&nbsp;Address</th></tr>\n");
  cnt = 0;
  while( db_step(&q)==SQLITE_ROW ){
    int rcvid = db_column_int(&q, 0);
    const char *zUser = db_column_text(&q, 1);
    const char *zDate = db_column_text(&q, 2);
    const char *zIpAddr = db_column_text(&q, 3);
    if( cnt==30 && !showAll ){
      style_submenu_element("Older", "rcvfromlist?ofst=%d", ofst+30);
    }else{
      cnt++;
      cgi_printf("<tr>\n");
      if( db_column_int(&q,4) ){
        cgi_printf("<td style=\"padding-right: 15px;text-align: right;\">\n"
               "<a href=\"rcvfrom?rcvid=%d\">%d</a></td>\n",(rcvid),(rcvid));
      }else{
        cgi_printf("<td style=\"padding-right: 15px;text-align: right;\">%d</td>\n",(rcvid));
      }
      cgi_printf("<td style=\"padding-right: 15px;text-align: left;\">%s</td>\n"
             "<td style=\"padding-right: 15px;text-align: left;\">%h</td>\n"
             "<td style=\"text-align: left;\">%s</td>\n"
             "</tr>\n",(zDate),(zUser),(zIpAddr));
    }
  }
  db_finalize(&q);
  cgi_printf("</table>\n");
  style_footer();
}

/*
** WEBPAGE: rcvfrom
**
** Show a single RCVFROM table entry identified by the rcvid= query
** parameters.  Requires Admin privilege.
*/
void rcvfrom_page(void){
  int rcvid = atoi(PD("rcvid","0"));
  Stmt q;
  int cnt;

  login_check_credentials();
  if( !g.perm.Admin ){
    login_needed(0);
    return;
  }
  style_header("Artifact Receipt %d", rcvid);
  if( db_exists(
    "SELECT 1 FROM blob WHERE rcvid=%d AND"
    " NOT EXISTS (SELECT 1 FROM shun WHERE shun.uuid=blob.uuid)", rcvid)
  ){
    style_submenu_element("Shun All", "shun?shun&rcvid=%d#addshun", rcvid);
  }
  if( db_exists(
    "SELECT 1 FROM blob WHERE rcvid=%d AND"
    " EXISTS (SELECT 1 FROM shun WHERE shun.uuid=blob.uuid)", rcvid)
  ){
    style_submenu_element("Unshun All", "shun?accept&rcvid=%d#delshun", rcvid);
  }
  db_prepare(&q,
    "SELECT login, datetime(rcvfrom.mtime), rcvfrom.ipaddr"
    "  FROM rcvfrom LEFT JOIN user USING(uid)"
    " WHERE rcvid=%d",
    rcvid
  );
  cgi_printf("<table cellspacing=\"15\" cellpadding=\"0\" border=\"0\">\n"
         "<tr><th valign=\"top\" align=\"right\">rcvid:</th>\n"
         "<td valign=\"top\">%d</td></tr>\n",(rcvid));
  if( db_step(&q)==SQLITE_ROW ){
    const char *zUser = db_column_text(&q, 0);
    const char *zDate = db_column_text(&q, 1);
    const char *zIpAddr = db_column_text(&q, 2);
    cgi_printf("<tr><th valign=\"top\" align=\"right\">User:</th>\n"
           "<td valign=\"top\">%s</td></tr>\n"
           "<tr><th valign=\"top\" align=\"right\">Date:</th>\n"
           "<td valign=\"top\">%s</td></tr>\n"
           "<tr><th valign=\"top\" align=\"right\">IP&nbsp;Address:</th>\n"
           "<td valign=\"top\">%s</td></tr>\n",(zUser),(zDate),(zIpAddr));
  }
  db_finalize(&q);
  db_multi_exec(
    "CREATE TEMP TABLE toshow(rid INTEGER PRIMARY KEY);"
    "INSERT INTO toshow SELECT rid FROM blob WHERE rcvid=%d", rcvid
  );
  describe_artifacts("IN toshow");
  db_prepare(&q,
    "SELECT blob.rid, blob.uuid, blob.size, description.summary\n"
    "  FROM blob LEFT JOIN description ON (blob.rid=description.rid)"
    " WHERE blob.rcvid=%d", rcvid
  );
  cnt = 0;
  while( db_step(&q)==SQLITE_ROW ){
    const char *zUuid = db_column_text(&q, 1);
    int size = db_column_int(&q, 2);
    const char *zDesc = db_column_text(&q, 3);
    if( zDesc==0 ) zDesc = "";
    if( cnt==0 ){
      cgi_printf("<tr><th valign=\"top\" align=\"right\">Artifacts:</th>\n"
             "<td valign=\"top\">\n");
    }
    cnt++;
    cgi_printf("<a href=\"%R/info/%s\">%s</a>\n"
           "%h (size: %d)<br />\n",(zUuid),(zUuid),(zDesc),(size));
  }
  if( cnt>0 ){
    cgi_printf("<p>\n");
    if( db_exists(
      "SELECT 1 FROM blob WHERE rcvid=%d AND"
      " NOT EXISTS (SELECT 1 FROM shun WHERE shun.uuid=blob.uuid)", rcvid)
    ){
      cgi_printf("<form action='%R/shun'>\n"
             "<input type=\"hidden\" name=\"shun\">\n"
             "<input type=\"hidden\" name=\"rcvid\" value='%d'>\n"
             "<input type=\"submit\" value=\"Shun All These Artifacts\">\n"
             "</form>\n",(rcvid));
    }
    if( db_exists(
      "SELECT 1 FROM blob WHERE rcvid=%d AND"
      " EXISTS (SELECT 1 FROM shun WHERE shun.uuid=blob.uuid)", rcvid)
    ){
      cgi_printf("<form action='%R/shun'>\n"
             "<input type=\"hidden\" name=\"unshun\">\n"
             "<input type=\"hidden\" name=\"rcvid\" value='%d'>\n"
             "<input type=\"submit\" value=\"Unshun All These Artifacts\">\n"
             "</form>\n",(rcvid));
    }
    cgi_printf("</td></tr>\n");
  }
  if( db_table_exists("repository","unversioned") ){
    cnt = 0;
    if( PB("uvdelete") && PB("confirmdelete") ){
      db_multi_exec(
        "DELETE FROM unversioned WHERE rcvid=%d", rcvid
      );
    }
    db_finalize(&q);
    db_prepare(&q,
      "SELECT name, hash, sz\n"
      "  FROM unversioned "
      " WHERE rcvid=%d", rcvid
    );
    while( db_step(&q)==SQLITE_ROW ){
      const char *zName = db_column_text(&q,0);
      const char *zHash = db_column_text(&q,1);
      int size = db_column_int(&q,2);
      int isDeleted = zHash==0;
      if( cnt==0 ){
        cgi_printf("<tr><th valign=\"top\" align=\"right\">Unversioned&nbsp;Files:</th>\n"
               "<td valign=\"top\">\n");
      }
      cnt++;
      if( isDeleted ){
        cgi_printf("%h (deleted)<br />\n",(zName));
      }else{
        cgi_printf("<a href=\"%R/uv/%h\">%h</a> (size: %d)<br />\n",(zName),(zName),(size));
      }
    }
    if( cnt>0 ){
      cgi_printf("<p><form action='%R/rcvfrom'>\n"
             "<input type=\"hidden\" name=\"rcvid\" value='%d'>\n"
             "<input type=\"hidden\" name=\"uvdelete\" value=\"1\">\n",(rcvid));
      if( PB("uvdelete") ){
        cgi_printf("<input type=\"hidden\" name=\"confirmdelete\" value=\"1\">\n"
               "<input type=\"submit\" value=\"Confirm Deletion of These Files\">\n");
      }else{
        cgi_printf("<input type=\"submit\" value=\"Delete These Unversioned Files\">\n");
      }
      cgi_printf("</form>\n"
             "</td></tr>\n");
    }
  }
  cgi_printf("</table>\n");
  db_finalize(&q);
  style_footer();
}
