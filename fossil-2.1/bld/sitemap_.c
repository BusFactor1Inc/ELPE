#line 1 "./src/sitemap.c"
/*
** Copyright (c) 2014 D. Richard Hipp
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
** This file contains code to implement the sitemap webpage.
*/
#include "config.h"
#include "sitemap.h"
#include <assert.h>

/*
** WEBPAGE: sitemap
**
** List some of the web pages offered by the Fossil web engine.  This
** page is intended as a supplement to the menu bar on the main screen.
** That is, this page is designed to hold links that are omitted from
** the main menu due to lack of space.
*/
void sitemap_page(void){
  int srchFlags;
  login_check_credentials();
  srchFlags = search_restrict(SRCH_ALL);
  style_header("Site Map");
  style_adunit_config(ADUNIT_RIGHT_OK);
#if 0
  cgi_printf("<p>\n"
         "The following links are just a few of the many web-pages available for\n"
         "this Fossil repository:\n"
         "</p>\n"
         "\n");
#endif
  cgi_printf("<ul>\n"
         "<li>%zHome Page</a>\n",(href("%R/home")));
  if( srchFlags & SRCH_DOC ){
    cgi_printf("  <ul>\n"
           "  <li>%zSearch Project Documentation</a></li>\n"
           "  </ul>\n",(href("%R/docsrch")));
  }
  cgi_printf("</li>\n");
  if( g.perm.Read ){
    cgi_printf("<li>%zFile Browser</a></li>\n"
           "  <ul>\n"
           "  <li>%zTree-view,\n"
           "       Trunk Check-in</a></li>\n"
           "  <li>%zFlat-view</a></li>\n"
           "  <li>%zFile ages for Trunk</a></li>\n"
           "</ul>\n",(href("%R/tree")),(href("%R/tree?type=tree&ci=trunk")),(href("%R/tree?type=flat")),(href("%R/fileage?name=trunk")));
  }
  if( g.perm.Read ){
    cgi_printf("<li>%zProject Timeline</a></li>\n"
           "<ul>\n"
           "  <li>%zActivity Reports</a></li>\n"
           "  <li>%zFile name changes</a></li>\n"
           "  <li>%zForks</a></li>\n"
           "  <li>%zFirst 10\n"
           "      check-ins</a></li>\n"
           "</ul>\n",(href("%R/timeline?n=200")),(href("%R/reports")),(href("%R/timeline?n=all&namechng")),(href("%R/timeline?n=all&forks")),(href("%R/timeline?a=1970-01-01&y=ci&n=10")));
  }
  if( g.perm.Read ){
    cgi_printf("<li>%zBranches</a></li>\n"
           "<ul>\n"
           "  <li>%zLeaf Check-ins</a></li>\n"
           "  <li>%zList of Tags</a></li>\n"
           "</ul>\n"
           "</li>\n",(href("%R/brlist")),(href("%R/leaves")),(href("%R/taglist")));
  }
  if( g.perm.RdWiki ){
    cgi_printf("<li>%zWiki</a>\n"
           "  <ul>\n",(href("%R/wikihelp")));
    if( srchFlags & SRCH_WIKI ){
      cgi_printf("    <li>%zWiki Search</a></li>\n",(href("%R/wikisrch")));
    }
    cgi_printf("    <li>%zList of Wiki Pages</a></li>\n"
           "    <li>%zRecent activity</a></li>\n"
           "    <li>%zWiki Formatting Rules</a></li>\n"
           "    <li>%zMarkdown Formatting Rules</a></li>\n"
           "    <li>%zSandbox</a></li>\n"
           "    <li>%zList of Attachments</a></li>\n"
           "  </ul>\n"
           "</li>\n",(href("%R/wcontent")),(href("%R/timeline?y=w")),(href("%R/wiki_rules")),(href("%R/md_rules")),(href("%R/wiki?name=Sandbox")),(href("%R/attachlist")));
  }
  if( g.perm.RdTkt ){
    cgi_printf("<li>%zTickets</a>\n"
           "  <ul>\n",(href("%R/reportlist")));
    if( srchFlags & SRCH_TKT ){
      cgi_printf("  <li>%zTicket Search</a></li>\n",(href("%R/tktsrch")));
    }
    cgi_printf("  <li>%zRecent activity</a></li>\n"
           "  <li>%zList of Attachments</a></li>\n"
           "  </ul>\n"
           "</li>\n",(href("%R/timeline?y=t")),(href("%R/attachlist")));
  }
  if( g.perm.Read ){
    cgi_printf("<li>%zUnversioned Files</a>\n",(href("%R/uvlist")));
  }
  if( srchFlags ){
    cgi_printf("<li>%zFull-Text Search</a></li>\n",(href("%R/search")));
  }
  cgi_printf("<li>%zLogin/Logout/Change Password</a></li>\n",(href("%R/login")));
  if( g.perm.Read ){
    cgi_printf("<li>%zRepository Status</a>\n"
           "  <ul>\n"
           "  <li>%zCollisions on hash prefixes</a></li>\n",(href("%R/stat")),(href("%R/hash-collisions")));
    if( g.perm.Admin ){
      cgi_printf("  <li>%zList of URLs used to access\n"
             "      this repository</a></li>\n",(href("%R/urllist")));
    }
    cgi_printf("  <li>%zList of Artifacts</a></li>\n"
           "  <li>%zList of \"Timewarp\" Check-ins</a></li>\n"
           "  </ul>\n"
           "</li>\n",(href("%R/bloblist")),(href("%R/timewarps")));
  }
  cgi_printf("<li>On-line Documentation\n"
         "  <ul>\n"
         "  <li>%zList of All Commands and Web Pages</a></li>\n"
         "  <li>%zAll \"help\" text on a single page</a></li>\n"
         "  <li>%zFilename suffix to mimetype map</a></li>\n"
         "  </ul></li>\n",(href("%R/help")),(href("%R/test-all-help")),(href("%R/mimetype_list")));
  if( g.perm.Admin ){
    cgi_printf("<li>%zAdministration Pages</a>\n"
           "  <ul>\n"
           "  <li>%zPending Moderation Requests</a></li>\n"
           "  <li>%zAdmin log</a></li>\n"
           "  <li>%zStatus of the web-page cache</a></li>\n"
           "  </ul></li>\n",(href("%R/setup")),(href("%R/modreq")),(href("%R/admin_log")),(href("%R/cachestat")));
  }
  cgi_printf("<li>Test Pages\n"
         "  <ul>\n");
  if( g.perm.Admin || db_get_boolean("test_env_enable",0) ){
    cgi_printf("  <li>%zCGI Environment Test</a></li>\n",(href("%R/test_env")));
  }
  if( g.perm.Read ){
    cgi_printf("  <li>%zList of file renames</a></li>\n",(href("%R/test-rename-list")));
  }
  cgi_printf("  <li>%zPage to experiment with the automatic\n"
         "      colors assigned to branch names</a>\n"
         "  <li>%zRandom ASCII-art Captcha image</a></li>\n"
         "  </ul></li>\n"
         "</ul></li>\n",(href("%R/hash-color-test")),(href("%R/test-captcha")));
  style_footer();
}
