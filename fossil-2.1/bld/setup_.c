#line 1 "./src/setup.c"
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
** Implementation of the Setup page
*/
#include "config.h"
#include <assert.h>
#include "setup.h"

/*
** Output a single entry for a menu generated using an HTML table.
** If zLink is not NULL or an empty string, then it is the page that
** the menu entry will hyperlink to.  If zLink is NULL or "", then
** the menu entry has no hyperlink - it is disabled.
*/
void setup_menu_entry(
  const char *zTitle,
  const char *zLink,
  const char *zDesc
){
  cgi_printf("<tr><td valign=\"top\" align=\"right\">\n");
  if( zLink && zLink[0] ){
    cgi_printf("<a href=\"%s\">%h</a>\n",(zLink),(zTitle));
  }else{
    cgi_printf("%h\n",(zTitle));
  }
  cgi_printf("</td><td width=\"5\"></td><td valign=\"top\">%h</td></tr>\n",(zDesc));
}



/*
** WEBPAGE: setup
**
** Main menu for the administrative pages.  Requires Admin privileges.
*/
void setup_page(void){
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
  }

  style_header("Server Administration");

  /* Make sure the header contains <base href="...">.   Issue a warning
  ** if it does not. */
  if( !cgi_header_contains("<base href=") ){
    cgi_printf("<p class=\"generalError\"><b>Configuration Error:</b> Please add\n"
           "<tt>&lt;base href=\"$secureurl/$current_page\"&gt;</tt> after\n"
           "<tt>&lt;head&gt;</tt> in the <a href=\"setup_skinedit?w=2\">HTML header</a>!</p>\n");
  }

#if !defined(_WIN32)
  /* Check for /dev/null and /dev/urandom.  We want both devices to be present,
  ** but they are sometimes omitted (by mistake) from chroot jails. */
  if( access("/dev/null", R_OK|W_OK) ){
    cgi_printf("<p class=\"generalError\">WARNING: Device \"/dev/null\" is not available\n"
           "for reading and writing.</p>\n");
  }
  if( access("/dev/urandom", R_OK) ){
    cgi_printf("<p class=\"generalError\">WARNING: Device \"/dev/urandom\" is not available\n"
           "for reading. This means that the pseudo-random number generator used\n"
           "by SQLite will be poorly seeded.</p>\n");
  }
#endif

  cgi_printf("<table border=\"0\" cellspacing=\"3\">\n");
  setup_menu_entry("Users", "setup_ulist",
    "Grant privileges to individual users.");
  setup_menu_entry("Access", "setup_access",
    "Control access settings.");
  setup_menu_entry("Configuration", "setup_config",
    "Configure the WWW components of the repository");
  setup_menu_entry("Settings", "setup_settings",
    "Web interface to the \"fossil settings\" command");
  setup_menu_entry("Timeline", "setup_timeline",
    "Timeline display preferences");
  setup_menu_entry("Login-Group", "setup_login_group",
    "Manage single sign-on between this repository and others"
    " on the same server");
  setup_menu_entry("Tickets", "tktsetup",
    "Configure the trouble-ticketing system for this repository");
  setup_menu_entry("Search","srchsetup",
    "Configure the built-in search engine");
  setup_menu_entry("Transfers", "xfersetup",
    "Configure the transfer system for this repository");
  setup_menu_entry("Skins", "setup_skin",
    "Select and/or modify the web interface \"skins\"");
  setup_menu_entry("Moderation", "setup_modreq",
    "Enable/Disable requiring moderator approval of Wiki and/or Ticket"
    " changes and attachments.");
  setup_menu_entry("Ad-Unit", "setup_adunit",
    "Edit HTML text for an ad unit inserted after the menu bar");
  setup_menu_entry("Web-Cache", "cachestat",
    "View the status of the expensive-page cache");
  setup_menu_entry("Logo", "setup_logo",
    "Change the logo and background images for the server");
  setup_menu_entry("Shunned", "shun",
    "Show artifacts that are shunned by this repository");
  setup_menu_entry("Artifact Receipts Log", "rcvfromlist",
    "A record of received artifacts and their sources");
  setup_menu_entry("User Log", "access_log",
    "A record of login attempts");
  setup_menu_entry("Administrative Log", "admin_log",
    "View the admin_log entries");
  setup_menu_entry("Unversioned Files", "uvlist?byage=1",
    "Show all unversioned files held");
  setup_menu_entry("Stats", "stat",
    "Repository Status Reports");
  setup_menu_entry("Sitemap", "sitemap",
    "Links to miscellaneous pages");
  setup_menu_entry("SQL", "admin_sql",
    "Enter raw SQL commands");
  setup_menu_entry("TH1", "admin_th1",
    "Enter raw TH1 commands");
  cgi_printf("</table>\n");

  style_footer();
}

/*
** WEBPAGE: setup_ulist
**
** Show a list of users.  Clicking on any user jumps to the edit
** screen for that user.  Requires Admin privileges.
*/
void setup_ulist(void){
  Stmt s;

  login_check_credentials();
  if( !g.perm.Admin ){
    login_needed(0);
    return;
  }

  style_submenu_element("Add", "setup_uedit");
  style_submenu_element("Log", "access_log");
  style_submenu_element("Help", "setup_ulist_notes");
  style_header("User List");
  cgi_printf("<table border=1 cellpadding=2 cellspacing=0 class='userTable'>\n"
         "<thead><tr>\n"
         "  <th>UID <th>Category\n"
         "  <th>Capabilities (<a href='%R/setup_ucap_list'>key</a>)\n"
         "  <th>Info <th>Last Change</tr></thead>\n"
         "<tbody>\n");
  db_prepare(&s,
     "SELECT uid, login, cap, date(mtime,'unixepoch')"
     "  FROM user"
     " WHERE login IN ('anonymous','nobody','developer','reader')"
     " ORDER BY login"
  );
  while( db_step(&s)==SQLITE_ROW ){
    int uid = db_column_int(&s, 0);
    const char *zLogin = db_column_text(&s, 1);
    const char *zCap = db_column_text(&s, 2);
    const char *zDate = db_column_text(&s, 4);
    cgi_printf("<tr>\n"
           "<td><a href='setup_uedit?id=%d'>%d</a>\n"
           "<td><a href='setup_uedit?id=%d'>%h</a>\n"
           "<td>%h\n",(uid),(uid),(uid),(zLogin),(zCap));

    if( fossil_strcmp(zLogin,"anonymous")==0 ){
      cgi_printf("<td>All logged-in users\n");
    }else if( fossil_strcmp(zLogin,"developer")==0 ){
      cgi_printf("<td>Users with '<b>v</b>' capability\n");
    }else if( fossil_strcmp(zLogin,"nobody")==0 ){
      cgi_printf("<td>All users without login\n");
    }else if( fossil_strcmp(zLogin,"reader")==0 ){
      cgi_printf("<td>Users with '<b>u</b>' capability\n");
    }else{
      cgi_printf("<td>\n");
    }
    if( zDate && zDate[0] ){
      cgi_printf("<td>%h\n",(zDate));
    }else{
      cgi_printf("<td>\n");
    }
    cgi_printf("</tr>\n");
  }
  db_finalize(&s);
  cgi_printf("</tbody></table>\n"
         "<div class='section'>Users</div>\n"
         "<table border=1 cellpadding=2 cellspacing=0 class='userTable' id='userlist'>\n"
         "<thead><tr>\n"
         "<th>ID<th>Login<th>Caps<th>Info<th>Date<th>Expire</tr></thead>\n"
         "<tbody>\n");
  db_prepare(&s,
     "SELECT uid, login, cap, info, date(mtime,'unixepoch'), lower(login) AS sortkey, "
     "       CASE WHEN info LIKE '%%expires 20%%'"
             "    THEN substr(info,instr(lower(info),'expires')+8,10)"
             "    END AS exp"
     "  FROM user"
     " WHERE login NOT IN ('anonymous','nobody','developer','reader')"
     " ORDER BY sortkey"
  );
  while( db_step(&s)==SQLITE_ROW ){
    int uid = db_column_int(&s, 0);
    const char *zLogin = db_column_text(&s, 1);
    const char *zCap = db_column_text(&s, 2);
    const char *zInfo = db_column_text(&s, 3);
    const char *zDate = db_column_text(&s, 4);
    const char *zSortKey = db_column_text(&s,5);
    const char *zExp = db_column_text(&s,6);
    cgi_printf("<tr>\n"
           "<td><a href='setup_uedit?id=%d'>%d</a>\n"
           "<td data-sortkey='%h'><a href='setup_uedit?id=%d'>%h</a>\n"
           "<td>%h\n"
           "<td>%h\n"
           "<td>%h\n"
           "<td>%h\n"
           "</tr>\n",(uid),(uid),(zSortKey),(uid),(zLogin),(zCap),(zInfo),(zDate?zDate:""),(zExp?zExp:""));
  }
  cgi_printf("</tbody></table>\n");
  db_finalize(&s);
  output_table_sorting_javascript("userlist","nktxTT",2);
  style_footer();
}

/*
** Render the user-capability table
*/
static void setup_usercap_table(void){
  cgi_printf("<table>\n"
            "<tr><th valign=\"top\">a</th>\n"
            "  <td><i>Admin:</i> Create and delete users</td></tr>\n"
            "<tr><th valign=\"top\">b</th>\n"
            "  <td><i>Attach:</i> Add attachments to wiki or tickets</td></tr>\n"
            "<tr><th valign=\"top\">c</th>\n"
            "  <td><i>Append-Tkt:</i> Append to tickets</td></tr>\n"
            "<tr><th valign=\"top\">d</th>\n"
            "  <td><i>Delete:</i> Delete wiki and tickets</td></tr>\n"
            "<tr><th valign=\"top\">e</th>\n"
            "  <td><i>Email:</i> View sensitive data such as EMail addresses</td></tr>\n"
            "<tr><th valign=\"top\">f</th>\n"
            "  <td><i>New-Wiki:</i> Create new wiki pages</td></tr>\n"
            "<tr><th valign=\"top\">g</th>\n"
            "  <td><i>Clone:</i> Clone the repository</td></tr>\n"
            "<tr><th valign=\"top\">h</th>\n"
            "  <td><i>Hyperlinks:</i> Show hyperlinks to detailed\n"
            "  repository history</td></tr>\n"
            "<tr><th valign=\"top\">i</th>\n"
            "  <td><i>Check-In:</i> Commit new versions in the repository</td></tr>\n"
            "<tr><th valign=\"top\">j</th>\n"
            "  <td><i>Read-Wiki:</i> View wiki pages</td></tr>\n"
            "<tr><th valign=\"top\">k</th>\n"
            "  <td><i>Write-Wiki:</i> Edit wiki pages</td></tr>\n"
            "<tr><th valign=\"top\">l</th>\n"
            "  <td><i>Mod-Wiki:</i> Moderator for wiki pages</td></tr>\n"
            "<tr><th valign=\"top\">m</th>\n"
            "  <td><i>Append-Wiki:</i> Append to wiki pages</td></tr>\n"
            "<tr><th valign=\"top\">n</th>\n"
            "  <td><i>New-Tkt:</i> Create new tickets</td></tr>\n"
            "<tr><th valign=\"top\">o</th>\n"
            "  <td><i>Check-Out:</i> Check out versions</td></tr>\n"
            "<tr><th valign=\"top\">p</th>\n"
            "  <td><i>Password:</i> Change your own password</td></tr>\n"
            "<tr><th valign=\"top\">q</th>\n"
            "  <td><i>Mod-Tkt:</i> Moderator for tickets</td></tr>\n"
            "<tr><th valign=\"top\">r</th>\n"
            "  <td><i>Read-Tkt:</i> View tickets</td></tr>\n"
            "<tr><th valign=\"top\">s</th>\n"
            "  <td><i>Setup/Super-user:</i> Setup and configure this website</td></tr>\n"
            "<tr><th valign=\"top\">t</th>\n"
            "  <td><i>Tkt-Report:</i> Create new bug summary reports</td></tr>\n"
            "<tr><th valign=\"top\">u</th>\n"
            "  <td><i>Reader:</i> Inherit privileges of\n"
            "  user <tt>reader</tt></td></tr>\n"
            "<tr><th valign=\"top\">v</th>\n"
            "  <td><i>Developer:</i> Inherit privileges of\n"
            "  user <tt>developer</tt></td></tr>\n"
            "<tr><th valign=\"top\">w</th>\n"
            "  <td><i>Write-Tkt:</i> Edit tickets</td></tr>\n"
            "<tr><th valign=\"top\">x</th>\n"
            "  <td><i>Private:</i> Push and/or pull private branches</td></tr>\n"
            "<tr><th valign=\"top\">y</th>\n"
            "  <td><i>Write-Unver:</i> Push unversioned files</td></tr>\n"
            "<tr><th valign=\"top\">z</th>\n"
            "  <td><i>Zip download:</i> Download a ZIP archive or tarball</td></tr>\n"
         "</table>\n");
}

/*
** WEBPAGE: setup_ulist_notes
**
** A documentation page showing notes about user configuration.  This information
** used to be a side-bar on the user list page, but has been factored out for
** improved presentation.
*/
void setup_ulist_notes(void){
  style_header("User Configuration Notes");
  cgi_printf("<h1>User Configuration Notes:</h1>\n"
         "<ol>\n"
         "<li><p>\n"
         "Every user, logged in or not, inherits the privileges of\n"
         "<span class=\"usertype\">nobody</span>.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "Any human can login as <span class=\"usertype\">anonymous</span> since the\n"
         "password is clearly displayed on the login page for them to type. The\n"
         "purpose of requiring anonymous to log in is to prevent access by spiders.\n"
         "Every logged-in user inherits the combined privileges of\n"
         "<span class=\"usertype\">anonymous</span> and\n"
         "<span class=\"usertype\">nobody</span>.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "Users with privilege <span class=\"capability\">u</span> inherit the combined\n"
         "privileges of <span class=\"usertype\">reader</span>,\n"
         "<span class=\"usertype\">anonymous</span>, and\n"
         "<span class=\"usertype\">nobody</span>.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "Users with privilege <span class=\"capability\">v</span> inherit the combined\n"
         "privileges of <span class=\"usertype\">developer</span>,\n"
         "<span class=\"usertype\">anonymous</span>, and\n"
         "<span class=\"usertype\">nobody</span>.\n"
         "</p></li>\n"
         "\n"
         "<li><p>The permission flags are as follows:</p>\n");
  setup_usercap_table();
  cgi_printf("</li>\n"
         "</ol>\n");
  style_footer();
}

/*
** WEBPAGE: setup_ucap_list
**
** A documentation page showing the meaning of the various user capabilities
** code letters.
*/
void setup_ucap_list(void){
  style_header("User Capability Codes");
  setup_usercap_table();
  style_footer();
}

/*
** Return true if zPw is a valid password string.  A valid
** password string is:
**
**  (1)  A zero-length string, or
**  (2)  a string that contains a character other than '*'.
*/
static int isValidPwString(const char *zPw){
  if( zPw==0 ) return 0;
  if( zPw[0]==0 ) return 1;
  while( zPw[0]=='*' ){ zPw++; }
  return zPw[0]!=0;
}

/*
** WEBPAGE: setup_uedit
**
** Edit information about a user or create a new user.
** Requires Admin privileges.
*/
void user_edit(void){
  const char *zId, *zLogin, *zInfo, *zCap, *zPw;
  const char *zGroup;
  const char *zOldLogin;
  int doWrite;
  int uid, i;
  int higherUser = 0;  /* True if user being edited is SETUP and the */
                       /* user doing the editing is ADMIN.  Disallow editing */
  const char *inherit[128];
  int a[128];
  const char *oa[128];

  /* Must have ADMIN privileges to access this page
  */
  login_check_credentials();
  if( !g.perm.Admin ){ login_needed(0); return; }

  /* Check to see if an ADMIN user is trying to edit a SETUP account.
  ** Don't allow that.
  */
  zId = PD("id", "0");
  uid = atoi(zId);
  if( zId && !g.perm.Setup && uid>0 ){
    char *zOldCaps;
    zOldCaps = db_text(0, "SELECT cap FROM user WHERE uid=%d",uid);
    higherUser = zOldCaps && strchr(zOldCaps,'s');
  }

  if( P("can") ){
    cgi_redirect("setup_ulist");  /* User pressed the Cancel button */
    return;
  }

  /* If we have all the necessary information, write the new or
  ** modified user record.  After writing the user record, redirect
  ** to the page that displays a list of users.
  */
  doWrite = cgi_all("login","info","pw") && !higherUser;
  if( doWrite ){
    char c;
    char zCap[50], zNm[4];
    zNm[0] = 'a';
    zNm[2] = 0;
    for(i=0, c='a'; c<='z'; c++){
      zNm[1] = c;
      a[c&0x7f] = (c!='s' || g.perm.Setup) && P(zNm)!=0;
      if( a[c&0x7f] ) zCap[i++] = c;
    }

    zCap[i] = 0;
    zPw = P("pw");
    zLogin = P("login");
    if( strlen(zLogin)==0 ){
      style_header("User Creation Error");
      cgi_printf("<span class=\"loginError\">Empty login not allowed.</span>\n"
             "\n"
             "<p><a href=\"setup_uedit?id=%d\">[Bummer]</a></p>\n",(uid));
      style_footer();
      return;
    }
    if( isValidPwString(zPw) ){
      zPw = sha1_shared_secret(zPw, zLogin, 0);
    }else{
      zPw = db_text(0, "SELECT pw FROM user WHERE uid=%d", uid);
    }
    zOldLogin = db_text(0, "SELECT login FROM user WHERE uid=%d", uid);
    if( db_exists("SELECT 1 FROM user WHERE login=%Q AND uid!=%d", zLogin, uid) ){
      style_header("User Creation Error");
      cgi_printf("<span class=\"loginError\">Login \"%h\" is already used by\n"
             "a different user.</span>\n"
             "\n"
             "<p><a href=\"setup_uedit?id=%d\">[Bummer]</a></p>\n",(zLogin),(uid));
      style_footer();
      return;
    }
    login_verify_csrf_secret();
    db_multi_exec(
       "REPLACE INTO user(uid,login,info,pw,cap,mtime) "
       "VALUES(nullif(%d,0),%Q,%Q,%Q,%Q,now())",
      uid, zLogin, P("info"), zPw, zCap
    );
    admin_log( "Updated user [%q] with capabilities [%q].",
               zLogin, zCap );
    if( atoi(PD("all","0"))>0 ){
      Blob sql;
      char *zErr = 0;
      blob_zero(&sql);
      if( zOldLogin==0 ){
        blob_appendf(&sql,
          "INSERT INTO user(login)"
          "  SELECT %Q WHERE NOT EXISTS(SELECT 1 FROM user WHERE login=%Q);",
          zLogin, zLogin
        );
        zOldLogin = zLogin;
      }
      blob_appendf(&sql,
        "UPDATE user SET login=%Q,"
        "  pw=coalesce(shared_secret(%Q,%Q,"
                "(SELECT value FROM config WHERE name='project-code')),pw),"
        "  info=%Q,"
        "  cap=%Q,"
        "  mtime=now()"
        " WHERE login=%Q;",
        zLogin, P("pw"), zLogin, P("info"), zCap,
        zOldLogin
      );
      login_group_sql(blob_str(&sql), "<li> ", " </li>\n", &zErr);
      blob_reset(&sql);
      admin_log( "Updated user [%q] in all login groups "
                 "with capabilities [%q].",
                 zLogin, zCap );
      if( zErr ){
        style_header("User Change Error");
        admin_log( "Error updating user '%q': %s'.", zLogin, zErr );
        cgi_printf("<span class=\"loginError\">%s</span>\n"
               "\n"
               "<p><a href=\"setup_uedit?id=%d\">[Bummer]</a></p>\n",(zErr),(uid));
        style_footer();
        return;
      }
    }
    cgi_redirect("setup_ulist");
    return;
  }

  /* Load the existing information about the user, if any
  */
  zLogin = "";
  zInfo = "";
  zCap = "";
  zPw = "";
  for(i='a'; i<='z'; i++) oa[i] = "";
  if( uid ){
    zLogin = db_text("", "SELECT login FROM user WHERE uid=%d", uid);
    zInfo = db_text("", "SELECT info FROM user WHERE uid=%d", uid);
    zCap = db_text("", "SELECT cap FROM user WHERE uid=%d", uid);
    zPw = db_text("", "SELECT pw FROM user WHERE uid=%d", uid);
    for(i=0; zCap[i]; i++){
      char c = zCap[i];
      if( c>='a' && c<='z' ) oa[c&0x7f] = " checked=\"checked\"";
    }
  }

  /* figure out inherited permissions */
  memset((char *)inherit, 0, sizeof(inherit));
  if( fossil_strcmp(zLogin, "developer") ){
    char *z1, *z2;
    z1 = z2 = db_text(0,"SELECT cap FROM user WHERE login='developer'");
    while( z1 && *z1 ){
      inherit[0x7f & *(z1++)] =
         "<span class=\"ueditInheritDeveloper\"><sub>[D]</sub></span>";
    }
    free(z2);
  }
  if( fossil_strcmp(zLogin, "reader") ){
    char *z1, *z2;
    z1 = z2 = db_text(0,"SELECT cap FROM user WHERE login='reader'");
    while( z1 && *z1 ){
      inherit[0x7f & *(z1++)] =
          "<span class=\"ueditInheritReader\"><sub>[R]</sub></span>";
    }
    free(z2);
  }
  if( fossil_strcmp(zLogin, "anonymous") ){
    char *z1, *z2;
    z1 = z2 = db_text(0,"SELECT cap FROM user WHERE login='anonymous'");
    while( z1 && *z1 ){
      inherit[0x7f & *(z1++)] =
           "<span class=\"ueditInheritAnonymous\"><sub>[A]</sub></span>";
    }
    free(z2);
  }
  if( fossil_strcmp(zLogin, "nobody") ){
    char *z1, *z2;
    z1 = z2 = db_text(0,"SELECT cap FROM user WHERE login='nobody'");
    while( z1 && *z1 ){
      inherit[0x7f & *(z1++)] =
           "<span class=\"ueditInheritNobody\"><sub>[N]</sub></span>";
    }
    free(z2);
  }

  /* Begin generating the page
  */
  style_submenu_element("Cancel", "setup_ulist");
  if( uid ){
    style_header("Edit User %h", zLogin);
  }else{
    style_header("Add A New User");
  }
  cgi_printf("<div class=\"ueditCapBox\">\n"
         "<form action=\"%s\" method=\"post\"><div>\n",(g.zPath));
  login_insert_csrf_secret();
  if( login_is_special(zLogin) ){
    cgi_printf("<input type=\"hidden\" name=\"login\" value=\"%s\">\n"
           "<input type=\"hidden\" name=\"info\" value=\"\">\n"
           "<input type=\"hidden\" name=\"pw\" value=\"*\">\n",(zLogin));
  }
  cgi_printf("<script>\n"
         "function updateCapabilityString(){\n"
         "  /*\n"
         "  ** This function updates the \"#usetupEditCapability\" span content\n"
         "  ** with the capabilities selected by the interactive user, based\n"
         "  ** upon the state of the capability checkboxes.\n"
         "  */\n"
         "  try {\n"
         "    var inputs = document.getElementsByTagName('input');\n"
         "    if( inputs && inputs.length ){\n"
         "      var output = document.getElementById('usetupEditCapability');\n"
         "      if( output ){\n"
         "        var permsIds = [], x = 0;\n"
         "        for(var i = 0; i < inputs.length; i++){\n"
         "          var e = inputs[i];\n"
         "          if( !e.name || !e.type ) continue;\n"
         "          if( e.type.toLowerCase()!=='checkbox' ) continue;\n"
         "          if( e.name.length===2 && e.name[0]==='a' ){\n"
         "            // looks like a capability checkbox\n"
         "            if( e.checked ){\n"
         "              // grab the second character of the element\n"
         "              // name, which is the textual flag for this\n"
         "              // capability, and then add it to the result\n"
         "              // array.\n"
         "              permsIds[x++] = e.name[1];\n"
         "            }\n"
         "          }\n"
         "        }\n"
         "        permsIds.sort();\n"
         "        output.innerHTML = permsIds.join('');\n"
         "      }\n"
         "    }\n"
         "  } catch (e) {\n"
         "    /* ignore errors */\n"
         "  }\n"
         "}\n"
         "</script>\n"
         "<table>\n"
         "<tr>\n"
         "  <td class=\"usetupEditLabel\">User ID:</td>\n");
  if( uid ){
    cgi_printf("  <td>%d <input type=\"hidden\" name=\"id\" value=\"%d\" /></td>\n",(uid),(uid));
  }else{
    cgi_printf("  <td>(new user)<input type=\"hidden\" name=\"id\" value=\"0\" /></td>\n");
  }
  cgi_printf("</tr>\n"
         "<tr>\n"
         "  <td class=\"usetupEditLabel\">Login:</td>\n");
  if( login_is_special(zLogin) ){
    cgi_printf("   <td><b>%h</b></td>\n",(zLogin));
  }else{
    cgi_printf("  <td><input type=\"text\" name=\"login\" value=\"%h\" /></td>\n"
           "</tr>\n"
           "<tr>\n"
           "  <td class=\"usetupEditLabel\">Contact&nbsp;Info:</td>\n"
           "  <td><textarea name=\"info\" cols=\"40\" rows=\"2\">%h</textarea></td>\n",(zLogin),(zInfo));
  }
  cgi_printf("</tr>\n"
         "<tr>\n"
         "  <td class=\"usetupEditLabel\">Capabilities:</td>\n"
         "  <td>\n");
#define B(x) inherit[x]
  cgi_printf("<table border=0><tr><td valign=\"top\">\n");
  if( g.perm.Setup ){
    cgi_printf(" <label><input type=\"checkbox\" name=\"as\"%s\n"
           "               onchange=\"updateCapabilityString()\"/>\n"
           " Setup%s</label><br />\n",(oa['s']),(B('s')));
  }
  cgi_printf(" <label><input type=\"checkbox\" name=\"aa\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Admin%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ad\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Delete%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ae\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Email%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ap\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Password%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ai\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Check-In%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ao\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Check-Out%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ah\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Hyperlinks%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ab\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Attachments%s</label><br />\n"
         "</td><td><td width=\"40\"></td><td valign=\"top\">\n"
         " <label><input type=\"checkbox\" name=\"au\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Reader%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"av\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Developer%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ag\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Clone%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"aj\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Read Wiki%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"af\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " New Wiki%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"am\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Append Wiki%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ak\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Write Wiki%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"al\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Moderate Wiki%s</label><br />\n"
         "</td><td><td width=\"40\"></td><td valign=\"top\">\n"
         " <label><input type=\"checkbox\" name=\"ar\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Read Ticket%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"an\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " New Tickets%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ac\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Append To Ticket%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"aw\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Write Tickets%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"aq\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Moderate Tickets%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"at\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Ticket Report%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ax\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Private%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"ay\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Write Unversioned%s</label><br />\n"
         " <label><input type=\"checkbox\" name=\"az\"%s\n"
         "               onchange=\"updateCapabilityString()\" />\n"
         " Download Zip%s</label>\n"
         "</td></tr>\n"
         "</table>\n"
         "  </td>\n"
         "</tr>\n"
         "<tr>\n"
         "  <td class=\"usetupEditLabel\">Selected Cap.:</td>\n"
         "  <td>\n"
         "    <span id=\"usetupEditCapability\">(missing JS?)</span>\n"
         "  </td>\n"
         "</tr>\n",(oa['a']),(B('a')),(oa['d']),(B('d')),(oa['e']),(B('e')),(oa['p']),(B('p')),(oa['i']),(B('i')),(oa['o']),(B('o')),(oa['h']),(B('h')),(oa['b']),(B('b')),(oa['u']),(B('u')),(oa['v']),(B('v')),(oa['g']),(B('g')),(oa['j']),(B('j')),(oa['f']),(B('f')),(oa['m']),(B('m')),(oa['k']),(B('k')),(oa['l']),(B('l')),(oa['r']),(B('r')),(oa['n']),(B('n')),(oa['c']),(B('c')),(oa['w']),(B('w')),(oa['q']),(B('q')),(oa['t']),(B('t')),(oa['x']),(B('x')),(oa['y']),(B('y')),(oa['z']),(B('z')));
  if( !login_is_special(zLogin) ){
    cgi_printf("<tr>\n"
           "  <td align=\"right\">Password:</td>\n");
    if( zPw[0] ){
      /* Obscure the password for all users */
      cgi_printf("  <td><input type=\"password\" name=\"pw\" value=\"**********\" /></td>\n");
    }else{
      /* Show an empty password as an empty input field */
      cgi_printf("  <td><input type=\"password\" name=\"pw\" value=\"\" /></td>\n");
    }
    cgi_printf("</tr>\n");
  }
  zGroup = login_group_name();
  if( zGroup ){
    cgi_printf("<tr>\n"
           "<td valign=\"top\" align=\"right\">Scope:</td>\n"
           "<td valign=\"top\">\n"
           "<input type=\"radio\" name=\"all\" checked value=\"0\">\n"
           "Apply changes to this repository only.<br />\n"
           "<input type=\"radio\" name=\"all\" value=\"1\">\n"
           "Apply changes to all repositories in the \"<b>%h</b>\"\n"
           "login group.</td></tr>\n",(zGroup));
  }
  if( !higherUser ){
    cgi_printf("<tr>\n"
           "  <td>&nbsp;</td>\n"
           "  <td><input type=\"submit\" name=\"submit\" value=\"Apply Changes\" /></td>\n"
           "</tr>\n");
  }
  cgi_printf("</table>\n"
         "</div></form>\n"
         "</div>\n"
         "<script>updateCapabilityString();</script>\n"
         "<h2>Privileges And Capabilities:</h2>\n"
         "<ul>\n");
  if( higherUser ){
    cgi_printf("<li><p class=\"missingPriv\">\n"
           "User %h has Setup privileges and you only have Admin privileges\n"
           "so you are not permitted to make changes to %h.\n"
           "</p></li>\n"
           "\n",(zLogin),(zLogin));
  }
  cgi_printf("<li><p>\n"
         "The <span class=\"capability\">Setup</span> user can make arbitrary\n"
         "configuration changes. An <span class=\"usertype\">Admin</span> user\n"
         "can add other users and change user privileges\n"
         "and reset user passwords.  Both automatically get all other privileges\n"
         "listed below.  Use these two settings with discretion.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The \"<span class=\"ueditInheritNobody\"><sub>N</sub></span>\" subscript suffix\n"
         "indicates the privileges of <span class=\"usertype\">nobody</span> that\n"
         "are available to all users regardless of whether or not they are logged in.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The \"<span class=\"ueditInheritAnonymous\"><sub>A</sub></span>\" subscript suffix\n"
         "indicates the privileges of <span class=\"usertype\">anonymous</span> that\n"
         "are inherited by all logged-in users.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The \"<span class=\"ueditInheritDeveloper\"><sub>D</sub></span>\" subscript suffix\n"
         "indicates the privileges of <span class=\"usertype\">developer</span> that\n"
         "are inherited by all users with the\n"
         "<span class=\"capability\">Developer</span> privilege.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The \"<span class=\"ueditInheritReader\"><sub>R</sub></span>\" subscript suffix\n"
         "indicates the privileges of <span class=\"usertype\">reader</span> that\n"
         "are inherited by all users with the <span class=\"capability\">Reader</span>\n"
         "privilege.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The <span class=\"capability\">Delete</span> privilege give the user the\n"
         "ability to erase wiki, tickets, and attachments that have been added\n"
         "by anonymous users.  This capability is intended for deletion of spam.\n"
         "The delete capability is only in effect for 24 hours after the item\n"
         "is first posted.  The <span class=\"usertype\">Setup</span> user can\n"
         "delete anything at any time.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The <span class=\"capability\">Hyperlinks</span> privilege allows a user\n"
         "to see most hyperlinks. This is recommended ON for most logged-in users\n"
         "but OFF for user \"nobody\" to avoid problems with spiders trying to walk\n"
         "every diff and annotation of every historical check-in and file.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The <span class=\"capability\">Zip</span> privilege allows a user to\n"
         "see the \"download as ZIP\"\n"
         "hyperlink and permits access to the <tt>/zip</tt> page.  This allows\n"
         "users to download ZIP archives without granting other rights like\n"
         "<span class=\"capability\">Read</span> or\n"
         "<span class=\"capability\">Hyperlink</span>.  The \"z\" privilege is recommended\n"
         "for user <span class=\"usertype\">nobody</span> so that automatic package\n"
         "downloaders can obtain the sources without going through the login\n"
         "procedure.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The <span class=\"capability\">Check-in</span> privilege allows remote\n"
         "users to \"push\". The <span class=\"capability\">Check-out</span> privilege\n"
         "allows remote users to \"pull\". The <span class=\"capability\">Clone</span>\n"
         "privilege allows remote users to \"clone\".\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The <span class=\"capability\">Read Wiki</span>,\n"
         "<span class=\"capability\">New Wiki</span>,\n"
         "<span class=\"capability\">Append Wiki</span>, and\n"
         "<b>Write Wiki</b> privileges control access to wiki pages.  The\n"
         "<span class=\"capability\">Read Ticket</span>,\n"
         "<span class=\"capability\">New Ticket</span>,\n"
         "<span class=\"capability\">Append Ticket</span>, and\n"
         "<span class=\"capability\">Write Ticket</span> privileges control access\n"
         "to trouble tickets.\n"
         "The <span class=\"capability\">Ticket Report</span> privilege allows\n"
         "the user to create or edit ticket report formats.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "Users with the <span class=\"capability\">Password</span> privilege\n"
         "are allowed to change their own password.  Recommended ON for most\n"
         "users but OFF for special users <span class=\"usertype\">developer</span>,\n"
         "<span class=\"usertype\">anonymous</span>,\n"
         "and <span class=\"usertype\">nobody</span>.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The <span class=\"capability\">EMail</span> privilege allows the display of\n"
         "sensitive information such as the email address of users and contact\n"
         "information on tickets. Recommended OFF for\n"
         "<span class=\"usertype\">anonymous</span> and for\n"
         "<span class=\"usertype\">nobody</span> but ON for\n"
         "<span class=\"usertype\">developer</span>.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The <span class=\"capability\">Attachment</span> privilege is needed in\n"
         "order to add attachments to tickets or wiki.  Write privilege on the\n"
         "ticket or wiki is also required.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "Login is prohibited if the password is an empty string.\n"
         "</p></li>\n"
         "</ul>\n"
         "\n"
         "<h2>Special Logins</h2>\n"
         "\n"
         "<ul>\n"
         "<li><p>\n"
         "No login is required for user <span class=\"usertype\">nobody</span>. The\n"
         "capabilities of the <span class=\"usertype\">nobody</span> user are\n"
         "inherited by all users, regardless of whether or not they are logged in.\n"
         "To disable universal access to the repository, make sure that the\n"
         "<span class=\"usertype\">nobody</span> user has no capabilities\n"
         "enabled. The password for <span class=\"usertype\">nobody</span> is ignored.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "Login is required for user <span class=\"usertype\">anonymous</span> but the\n"
         "password is displayed on the login screen beside the password entry box\n"
         "so anybody who can read should be able to login as anonymous.\n"
         "On the other hand, spiders and web-crawlers will typically not\n"
         "be able to login.  Set the capabilities of the\n"
         "<span class=\"usertype\">anonymous</span>\n"
         "user to things that you want any human to be able to do, but not any\n"
         "spider.  Every other logged-in user inherits the privileges of\n"
         "<span class=\"usertype\">anonymous</span>.\n"
         "</p></li>\n"
         "\n"
         "<li><p>\n"
         "The <span class=\"usertype\">developer</span> user is intended as a template\n"
         "for trusted users with check-in privileges. When adding new trusted users,\n"
         "simply select the <span class=\"capability\">developer</span> privilege to\n"
         "cause the new user to inherit all privileges of the\n"
         "<span class=\"usertype\">developer</span>\n"
         "user.  Similarly, the <span class=\"usertype\">reader</span> user is a\n"
         "template for users who are allowed more access than\n"
         "<span class=\"usertype\">anonymous</span>,\n"
         "but less than a <span class=\"usertype\">developer</span>.\n"
         "</p></li>\n"
         "</ul>\n");
  style_footer();
}


/*
** Generate a checkbox for an attribute.
*/
static void onoff_attribute(
  const char *zLabel,   /* The text label on the checkbox */
  const char *zVar,     /* The corresponding row in the VAR table */
  const char *zQParm,   /* The query parameter */
  int dfltVal,          /* Default value if VAR table entry does not exist */
  int disabled          /* 1 if disabled */
){
  const char *zQ = P(zQParm);
  int iVal = db_get_boolean(zVar, dfltVal);
  if( zQ==0 && !disabled && P("submit") ){
    zQ = "off";
  }
  if( zQ ){
    int iQ = fossil_strcmp(zQ,"on")==0 || atoi(zQ);
    if( iQ!=iVal ){
      login_verify_csrf_secret();
      db_set(zVar, iQ ? "1" : "0", 0);
      admin_log("Set option [%q] to [%q].",
                zVar, iQ ? "on" : "off");
      iVal = iQ;
    }
  }
  cgi_printf("<label><input type=\"checkbox\" name=\"%s\"\n",(zQParm));
  if( iVal ){
    cgi_printf("checked=\"checked\"\n");
  }
  if( disabled ){
    cgi_printf("disabled=\"disabled\"\n");
  }
  cgi_printf("/> <b>%s</b></label>\n",(zLabel));
}

/*
** Generate an entry box for an attribute.
*/
void entry_attribute(
  const char *zLabel,   /* The text label on the entry box */
  int width,            /* Width of the entry box */
  const char *zVar,     /* The corresponding row in the VAR table */
  const char *zQParm,   /* The query parameter */
  const char *zDflt,    /* Default value if VAR table entry does not exist */
  int disabled          /* 1 if disabled */
){
  const char *zVal = db_get(zVar, zDflt);
  const char *zQ = P(zQParm);
  if( zQ && fossil_strcmp(zQ,zVal)!=0 ){
    const int nZQ = (int)strlen(zQ);
    login_verify_csrf_secret();
    db_set(zVar, zQ, 0);
    admin_log("Set entry_attribute %Q to: %.*s%s",
              zVar, 20, zQ, (nZQ>20 ? "..." : ""));
    zVal = zQ;
  }
  cgi_printf("<input type=\"text\" id=\"%s\" name=\"%s\" value=\"%h\" size=\"%d\"\n",(zQParm),(zQParm),(zVal),(width));
  if( disabled ){
    cgi_printf("disabled=\"disabled\"\n");
  }
  cgi_printf("/> <b>%s</b>\n",(zLabel));
}

/*
** Generate a text box for an attribute.
*/
const char *textarea_attribute(
  const char *zLabel,   /* The text label on the textarea */
  int rows,             /* Rows in the textarea */
  int cols,             /* Columns in the textarea */
  const char *zVar,     /* The corresponding row in the VAR table */
  const char *zQP,      /* The query parameter */
  const char *zDflt,    /* Default value if VAR table entry does not exist */
  int disabled          /* 1 if the textarea should  not be editable */
){
  const char *z = db_get(zVar, zDflt);
  const char *zQ = P(zQP);
  if( zQ && !disabled && fossil_strcmp(zQ,z)!=0){
    const int nZQ = (int)strlen(zQ);
    login_verify_csrf_secret();
    db_set(zVar, zQ, 0);
    admin_log("Set textarea_attribute %Q to: %.*s%s",
              zVar, 20, zQ, (nZQ>20 ? "..." : ""));
    z = zQ;
  }
  if( rows>0 && cols>0 ){
    cgi_printf("<textarea id=\"id%s\" name=\"%s\" rows=\"%d\"\n",(zQP),(zQP),(rows));
    if( disabled ){
      cgi_printf("disabled=\"disabled\"\n");
    }
    cgi_printf("cols=\"%d\">%h</textarea>\n",(cols),(z));
    if( zLabel && *zLabel ){
      cgi_printf("<span class=\"textareaLabel\">%s</span>\n",(zLabel));
    }
  }
  return z;
}

/*
** Generate a text box for an attribute.
*/
static void multiple_choice_attribute(
  const char *zLabel,   /* The text label on the menu */
  const char *zVar,     /* The corresponding row in the VAR table */
  const char *zQP,      /* The query parameter */
  const char *zDflt,    /* Default value if VAR table entry does not exist */
  int nChoice,          /* Number of choices */
  const char *const *azChoice /* Choices. 2 per choice: (VAR value, Display) */
){
  const char *z = db_get(zVar, zDflt);
  const char *zQ = P(zQP);
  int i;
  if( zQ && fossil_strcmp(zQ,z)!=0){
    const int nZQ = (int)strlen(zQ);
    login_verify_csrf_secret();
    db_set(zVar, zQ, 0);
    admin_log("Set multiple_choice_attribute %Q to: %.*s%s",
              zVar, 20, zQ, (nZQ>20 ? "..." : ""));
    z = zQ;
  }
  cgi_printf("<select size=\"1\" name=\"%s\" id=\"id%s\">\n",(zQP),(zQP));
  for(i=0; i<nChoice*2; i+=2){
    const char *zSel = fossil_strcmp(azChoice[i],z)==0 ? " selected" : "";
    cgi_printf("<option value=\"%h\"%s>%h</option>\n",(azChoice[i]),(zSel),(azChoice[i+1]));
  }
  cgi_printf("</select> <b>%h</b>\n",(zLabel));
}


/*
** WEBPAGE: setup_access
**
** The access-control settings page.  Requires Admin privileges.
*/
void setup_access(void){
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }

  style_header("Access Control Settings");
  db_begin_transaction();
  cgi_printf("<form action=\"%s/setup_access\" method=\"post\"><div>\n",(g.zTop));
  login_insert_csrf_secret();
  cgi_printf("<hr />\n");
  onoff_attribute("Redirect to HTTPS on the Login page",
     "redirect-to-https", "redirhttps", 0, 0);
  cgi_printf("<p>When selected, force the use of HTTPS for the Login page.\n"
         "<p>Details:  When enabled, this option causes the $secureurl TH1\n"
         "variable is set to an \"https:\" variant of $baseurl.  Otherwise,\n"
         "$secureurl is just an alias for $baseurl.  Also when enabled, the\n"
         "Login page redirects to https if accessed via http.\n"
         "<hr />\n");
  onoff_attribute("Require password for local access",
     "localauth", "localauth", 0, 0);
  cgi_printf("<p>When enabled, the password sign-in is always required for\n"
         "web access.  When disabled, unrestricted web access from 127.0.0.1\n"
         "is allowed for the <a href=\"%R/help/ui\">fossil ui</a> command or\n"
         "from the <a href=\"%R/help/server\">fossil server</a>,\n"
         "<a href=\"%R/help/http\">fossil http</a> commands when the\n"
         "\"--localauth\" command line options is used, or from the\n"
         "<a href=\"%R/help/cgi\">fossil cgi</a> if a line containing\n"
         "the word \"localauth\" appears in the CGI script.\n"
         "\n"
         "<p>A password is always required if any one or more\n"
         "of the following are true:\n"
         "<ol>\n"
         "<li> This button is checked\n"
         "<li> The inbound TCP/IP connection is not from 127.0.0.1\n"
         "<li> The server is started using either of the\n"
         "<a href=\"%R/help/server\">fossil server</a> or\n"
         "<a href=\"%R/help/server\">fossil http</a> commands\n"
         "without the \"--localauth\" option.\n"
         "<li> The server is started from CGI without the \"localauth\" keyword\n"
         "in the CGI script.\n"
         "</ol>\n"
         "\n"
         "<hr />\n");
  onoff_attribute("Enable /test_env",
     "test_env_enable", "test_env_enable", 0, 0);
  cgi_printf("<p>When enabled, the %h/test_env URL is available to all\n"
         "users.  When disabled (the default) only users Admin and Setup can visit\n"
         "the /test_env page.\n"
         "</p>\n"
         "\n"
         "<hr />\n",(g.zBaseURL));
  onoff_attribute("Allow REMOTE_USER authentication",
     "remote_user_ok", "remote_user_ok", 0, 0);
  cgi_printf("<p>When enabled, if the REMOTE_USER environment variable is set to the\n"
         "login name of a valid user and no other login credentials are available,\n"
         "then the REMOTE_USER is accepted as an authenticated user.\n"
         "</p>\n"
         "\n"
         "<hr />\n");
  entry_attribute("IP address terms used in login cookie", 3,
                  "ip-prefix-terms", "ipt", "2", 0);
  cgi_printf("<p>The number of octets of of the IP address used in the login cookie.\n"
         "Set to zero to omit the IP address from the login cookie.  A value of\n"
         "2 is recommended.\n"
         "</p>\n"
         "\n"
         "<hr />\n");
  entry_attribute("Login expiration time", 6, "cookie-expire", "cex",
                  "8766", 0);
  cgi_printf("<p>The number of hours for which a login is valid.  This must be a\n"
         "positive number.  The default is 8766 hours which is approximately equal\n"
         "to a year.</p>\n");

  cgi_printf("<hr />\n");
  entry_attribute("Download packet limit", 10, "max-download", "mxdwn",
                  "5000000", 0);
  cgi_printf("<p>Fossil tries to limit out-bound sync, clone, and pull packets\n"
         "to this many bytes, uncompressed.  If the client requires more data\n"
         "than this, then the client will issue multiple HTTP requests.\n"
         "Values below 1 million are not recommended.  5 million is a\n"
         "reasonable number.</p>\n");

  cgi_printf("<hr />\n");
  entry_attribute("Download time limit", 11, "max-download-time", "mxdwnt",
                  "30", 0);

  cgi_printf("<p>Fossil tries to spend less than this many seconds gathering\n"
         "the out-bound data of sync, clone, and pull packets.\n"
         "If the client request takes longer, a partial reply is given similar\n"
         "to the download packet limit. 30s is a reasonable default.</p>\n");

  cgi_printf("<hr />\n");
  entry_attribute("Server Load Average Limit", 11, "max-loadavg", "mxldavg",
                  "0.0", 0);
  cgi_printf("<p>Some expensive operations (such as computing tarballs, zip archives,\n"
         "or annotation/blame pages) are prohibited if the load average on the host\n"
         "computer is too large.  Set the threshold for disallowing expensive\n"
         "computations here.  Set this to 0.0 to disable the load average limit.\n"
         "This limit is only enforced on Unix servers.  On Linux systems,\n"
         "access to the /proc virtual filesystem is required, which means this limit\n"
         "might not work inside a chroot() jail.</p>\n");

  cgi_printf("<hr />\n");
  onoff_attribute(
      "Enable hyperlinks for \"nobody\" based on User-Agent and Javascript",
      "auto-hyperlink", "autohyperlink", 1, 0);
  cgi_printf("<p>Enable hyperlinks (the equivalent of the \"h\" permission) for all users\n"
         "including user \"nobody\", as long as (1) the User-Agent string in the\n"
         "HTTP header indicates that the request is coming from an actual human\n"
         "being and not a robot or spider and (2) the user agent is able to\n"
         "run Javascript in order to set the href= attribute of hyperlinks.  Bots\n"
         "and spiders can forge a User-Agent string that makes them seem to be a\n"
         "normal browser and they can run javascript just like browsers.  But most\n"
         "bots do not go to that much trouble so this is normally an effective\n"
         "defense.</p>\n"
         "\n"
         "<p>You do not normally want a bot to walk your entire repository because\n"
         "if it does, your server will end up computing diffs and annotations for\n"
         "every historical version of every file and creating ZIPs and tarballs of\n"
         "every historical check-in, which can use a lot of CPU and bandwidth\n"
         "even for relatively small projects.</p>\n"
         "\n"
         "<p>Additional parameters that control this behavior:</p>\n"
         "<blockquote>\n");
  onoff_attribute("Enable hyperlinks for humans (as deduced from the UserAgent "
                  " HTTP header string)",
                  "auto-hyperlink-ishuman", "ahis", 0, 0);
  cgi_printf("<br />\n");
  onoff_attribute("Require mouse movement before enabling hyperlinks",
                  "auto-hyperlink-mouseover", "ahmo", 0, 0);
  cgi_printf("<br />\n");
  entry_attribute("Delay before enabling hyperlinks (milliseconds)", 5,
                  "auto-hyperlink-delay", "ah-delay", "10", 0);
  cgi_printf("</blockquote>\n"
         "<p>Hyperlinks for user \"nobody\" are normally enabled as soon as the page\n"
         "finishes loading.  But the first check-box below can be set to require mouse\n"
         "movement before enabling the links. One can also set a delay prior to enabling\n"
         "links by enter a positive number of milliseconds in the entry box above.</p>\n");

  cgi_printf("<hr />\n");
  onoff_attribute("Require a CAPTCHA if not logged in",
                  "require-captcha", "reqcapt", 1, 0);
  cgi_printf("<p>Require a CAPTCHA for edit operations (appending, creating, or\n"
         "editing wiki or tickets or adding attachments to wiki or tickets)\n"
         "for users who are not logged in.</p>\n");

  cgi_printf("<hr />\n");
  entry_attribute("Public pages", 30, "public-pages",
                  "pubpage", "", 0);
  cgi_printf("<p>A comma-separated list of glob patterns for pages that are accessible\n"
         "without needing a login and using the privileges given by the\n"
         "\"Default privileges\" setting below.  Example use case: Set this field\n"
         "to \"/doc/trunk/www/*\" to give anonymous users read-only permission to the\n"
         "latest version of the embedded documentation in the www/ folder without\n"
         "allowing them to see the rest of the source code.\n"
         "</p>\n");

  cgi_printf("<hr />\n");
  onoff_attribute("Allow users to register themselves",
                  "self-register", "selfregister", 0, 0);
  cgi_printf("<p>Allow users to register themselves through the HTTP UI.\n"
         "The registration form always requires filling in a CAPTCHA\n"
         "(<em>auto-captcha</em> setting is ignored). Still, bear in mind that anyone\n"
         "can register under any user name. This option is useful for public projects\n"
         "where you do not want everyone in any ticket discussion to be named\n"
         "\"Anonymous\".</p>\n");

  cgi_printf("<hr />\n");
  entry_attribute("Default privileges", 10, "default-perms",
                  "defaultperms", "u", 0);
  cgi_printf("<p>Permissions given to users that... <ul><li>register themselves using\n"
         "the self-registration procedure (if enabled), or <li>access \"public\"\n"
         "pages identified by the public-pages glob pattern above, or <li>\n"
         "are users newly created by the administrator.</ul>\n"
         "</p>\n");

  cgi_printf("<hr />\n");
  onoff_attribute("Show javascript button to fill in CAPTCHA",
                  "auto-captcha", "autocaptcha", 0, 0);
  cgi_printf("<p>When enabled, a button appears on the login screen for user\n"
         "\"anonymous\" that will automatically fill in the CAPTCHA password.\n"
         "This is less secure than forcing the user to do it manually, but is\n"
         "probably secure enough and it is certainly more convenient for\n"
         "anonymous users.</p>\n");

  cgi_printf("<hr />\n"
         "<p><input type=\"submit\"  name=\"submit\" value=\"Apply Changes\" /></p>\n"
         "</div></form>\n");
  db_end_transaction(0);
  style_footer();
}

/*
** WEBPAGE: setup_login_group
**
** Change how the current repository participates in a login
** group.
*/
void setup_login_group(void){
  const char *zGroup;
  char *zErrMsg = 0;
  Blob fullName;
  char *zSelfRepo;
  const char *zRepo = PD("repo", "");
  const char *zLogin = PD("login", "");
  const char *zPw = PD("pw", "");
  const char *zNewName = PD("newname", "New Login Group");

  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }
  file_canonical_name(g.zRepositoryName, &fullName, 0);
  zSelfRepo = fossil_strdup(blob_str(&fullName));
  blob_reset(&fullName);
  if( P("join")!=0 ){
    login_group_join(zRepo, zLogin, zPw, zNewName, &zErrMsg);
  }else if( P("leave") ){
    login_group_leave(&zErrMsg);
  }
  style_header("Login Group Configuration");
  if( zErrMsg ){
    cgi_printf("<p class=\"generalError\">%s</p>\n",(zErrMsg));
  }
  zGroup = login_group_name();
  if( zGroup==0 ){
    cgi_printf("<p>This repository (in the file named \"%h\")\n"
           "is not currently part of any login-group.\n"
           "To join a login group, fill out the form below.</p>\n"
           "\n"
           "<form action=\"%s/setup_login_group\" method=\"post\"><div>\n",(zSelfRepo),(g.zTop));
    login_insert_csrf_secret();
    cgi_printf("<blockquote><table border=\"0\">\n"
           "\n"
           "<tr><th align=\"right\">Repository filename in group to join:</th>\n"
           "<td width=\"5\"></td><td>\n"
           "<input type=\"text\" size=\"50\" value=\"%h\" name=\"repo\"></td></tr>\n"
           "\n"
           "<tr><th align=\"right\">Login on the above repo:</th>\n"
           "<td width=\"5\"></td><td>\n"
           "<input type=\"text\" size=\"20\" value=\"%h\" name=\"login\"></td></tr>\n"
           "\n"
           "<tr><th align=\"right\">Password:</th>\n"
           "<td width=\"5\"></td><td>\n"
           "<input type=\"password\" size=\"20\" name=\"pw\"></td></tr>\n"
           "\n"
           "<tr><th align=\"right\">Name of login-group:</th>\n"
           "<td width=\"5\"></td><td>\n"
           "<input type=\"text\" size=\"30\" value=\"%h\" name=\"newname\">\n"
           "(only used if creating a new login-group).</td></tr>\n"
           "\n"
           "<tr><td colspan=\"3\" align=\"center\">\n"
           "<input type=\"submit\" value=\"Join\" name=\"join\"></td></tr>\n"
           "</table></blockquote></div></form>\n",(zRepo),(zLogin),(zNewName));
  }else{
    Stmt q;
    int n = 0;
    cgi_printf("<p>This repository (in the file \"%h\")\n"
           "is currently part of the \"<b>%h</b>\" login group.\n"
           "Other repositories in that group are:</p>\n"
           "<table border=\"0\" cellspacing=\"4\">\n"
           "<tr><td colspan=\"2\"><th align=\"left\">Project Name<td>\n"
           "<th align=\"left\">Repository File</tr>\n",(zSelfRepo),(zGroup));
    db_prepare(&q,
       "SELECT value,"
       "       (SELECT value FROM config"
       "         WHERE name=('peer-name-' || substr(x.name,11)))"
       "  FROM config AS x"
       " WHERE name GLOB 'peer-repo-*'"
       " ORDER BY value"
    );
    while( db_step(&q)==SQLITE_ROW ){
      const char *zRepo = db_column_text(&q, 0);
      const char *zTitle = db_column_text(&q, 1);
      n++;
      cgi_printf("<tr><td align=\"right\">%d.</td><td width=\"4\">\n"
             "<td>%h<td width=\"10\"><td>%h</tr>\n",(n),(zTitle),(zRepo));
    }
    db_finalize(&q);
    cgi_printf("</table>\n"
           "\n"
           "<p><form action=\"%s/setup_login_group\" method=\"post\"><div>\n",(g.zTop));
    login_insert_csrf_secret();
    cgi_printf("To leave this login group press\n"
           "<input type=\"submit\" value=\"Leave Login Group\" name=\"leave\">\n"
           "</form></p>\n"
           "<hr /><h2>Implementation Details</h2>\n"
           "<p>The following are fields from the CONFIG table related to login-groups,\n"
           "provided here for instructional and debugging purposes:</p>\n"
           "<table border='1' id='configTab'>\n"
           "<thead><tr><th>Config.Name<th>Config.Value<th>Config.mtime</tr></thead><tbody>\n");
    db_prepare(&q, "SELECT name, value, datetime(mtime,'unixepoch') FROM config"
                   " WHERE name GLOB 'peer-*'"
                   "    OR name GLOB 'project-*'"
                   " ORDER BY name");
    while( db_step(&q)==SQLITE_ROW ){
      cgi_printf("<tr><td>%h</td>\n"
             "<td>%h</td>\n"
             "<td>%h</td></tr>\n",(db_column_text(&q,0)),(db_column_text(&q,1)),(db_column_text(&q,2)));
    }
    db_finalize(&q);
    cgi_printf("</tbody></table>\n");
    output_table_sorting_javascript("configTab","ttt",1);
  }
  style_footer();
}

/*
** WEBPAGE: setup_timeline
**
** Edit administrative settings controlling the display of
** timelines.
*/
void setup_timeline(void){
  double tmDiff;
  char zTmDiff[20];
  static const char *const azTimeFormats[] = {
      "0", "HH:MM",
      "1", "HH:MM:SS",
      "2", "YYYY-MM-DD HH:MM",
      "3", "YYMMDD HH:MM",
      "4", "(off)"
  };
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }

  style_header("Timeline Display Preferences");
  db_begin_transaction();
  cgi_printf("<form action=\"%s/setup_timeline\" method=\"post\"><div>\n",(g.zTop));
  login_insert_csrf_secret();

  cgi_printf("<hr />\n");
  onoff_attribute("Allow block-markup in timeline",
                  "timeline-block-markup", "tbm", 0, 0);
  cgi_printf("<p>In timeline displays, check-in comments can be displayed with or\n"
         "without block markup (paragraphs, tables, etc.)</p>\n");

  cgi_printf("<hr />\n");
  onoff_attribute("Plaintext comments on timelines",
                  "timeline-plaintext", "tpt", 0, 0);
  cgi_printf("<p>In timeline displays, check-in comments are displayed literally,\n"
         "without any wiki or HTML interpretation.  (Note: Use CSS to change\n"
         "display formatting features such as fonts and line-wrapping behavior.)</p>\n");

  cgi_printf("<hr />\n");
  onoff_attribute("Truncate comment at first blank line",
                  "timeline-truncate-at-blank", "ttb", 0, 0);
  cgi_printf("<p>In timeline displays, check-in comments are displayed only through\n"
         "the first blank line.</p>\n");

  cgi_printf("<hr />\n");
  onoff_attribute("Use Universal Coordinated Time (UTC)",
                  "timeline-utc", "utc", 1, 0);
  cgi_printf("<p>Show times as UTC (also sometimes called Greenwich Mean Time (GMT) or\n"
         "Zulu) instead of in local time.  On this server, local time is currently\n");
  tmDiff = db_double(0.0, "SELECT julianday('now')");
  tmDiff = db_double(0.0,
        "SELECT (julianday(%.17g,'localtime')-julianday(%.17g))*24.0",
        tmDiff, tmDiff);
  sqlite3_snprintf(sizeof(zTmDiff), zTmDiff, "%.1f", tmDiff);
  if( strcmp(zTmDiff, "0.0")==0 ){
    cgi_printf("the same as UTC and so this setting will make no difference in\n"
           "the display.</p>\n");
  }else if( tmDiff<0.0 ){
    sqlite3_snprintf(sizeof(zTmDiff), zTmDiff, "%.1f", -tmDiff);
    cgi_printf("%s hours behind UTC.</p>\n",(zTmDiff));
  }else{
    cgi_printf("%s hours ahead of UTC.</p>\n",(zTmDiff));
  }

  cgi_printf("<hr />\n");
  multiple_choice_attribute("Per-Item Time Format", "timeline-date-format",
            "tdf", "0", count(azTimeFormats)/2, azTimeFormats);
  cgi_printf("<p>If the \"HH:MM\" or \"HH:MM:SS\" format is selected, then the date is shown\n"
         "in a separate box (using CSS class \"timelineDate\") whenever the date changes.\n"
         "With the \"YYYY-MM-DD&nbsp;HH:MM\" and \"YYMMDD ...\" formats, the complete date\n"
         "and time is shown on every timeline entry (using the CSS class \"timelineTime\").</p>\n");

  cgi_printf("<hr />\n");
  onoff_attribute("Show version differences by default",
                  "show-version-diffs", "vdiff", 0, 0);
  cgi_printf("<p>The version-information pages linked from the timeline can either\n"
         "show complete diffs of all file changes, or can just list the names of\n"
         "the files that have changed.  Users can get to either page by\n"
         "clicking.  This setting selects the default.</p>\n");

  cgi_printf("<hr />\n");
  entry_attribute("Max timeline comment length", 6,
                  "timeline-max-comment", "tmc", "0", 0);
  cgi_printf("<p>The maximum length of a comment to be displayed in a timeline.\n"
         "\"0\" there is no length limit.</p>\n");

  cgi_printf("<hr />\n"
         "<p><input type=\"submit\"  name=\"submit\" value=\"Apply Changes\" /></p>\n"
         "</div></form>\n");
  db_end_transaction(0);
  style_footer();
}

/*
** WEBPAGE: setup_settings
**
** Change or view miscellaneous settings.  Part of the
** Admin pages requiring Admin privileges.
*/
void setup_settings(void){
  Setting const *pSet;

  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }

  style_header("Settings");
  if(!g.repositoryOpen){
    /* Provide read-only access to versioned settings,
       but only if no repo file was explicitly provided. */
    db_open_local(0);
  }
  db_begin_transaction();
  cgi_printf("<p>This page provides a simple interface to the \"fossil setting\" command.\n"
         "See the \"fossil help setting\" output below for further information on\n"
         "the meaning of each setting.</p><hr />\n"
         "<form action=\"%s/setup_settings\" method=\"post\"><div>\n"
         "<table border=\"0\"><tr><td valign=\"top\">\n",(g.zTop));
  login_insert_csrf_secret();
  for(pSet=aSetting; pSet->name!=0; pSet++){
    if( pSet->width==0 ){
      int hasVersionableValue = pSet->versionable &&
          (db_get_versioned(pSet->name, NULL)!=0);
      onoff_attribute(pSet->name, pSet->name,
                      pSet->var!=0 ? pSet->var : pSet->name,
                      is_truth(pSet->def), hasVersionableValue);
      if( pSet->versionable ){
        cgi_printf(" (v)<br />\n");
      } else {
        cgi_printf("<br />\n");
      }
    }
  }
  cgi_printf("<br /><input type=\"submit\"  name=\"submit\" value=\"Apply Changes\" />\n"
         "</td><td style=\"width:50px;\"></td><td valign=\"top\">\n");
  for(pSet=aSetting; pSet->name!=0; pSet++){
    if( pSet->width!=0 && !pSet->versionable && !pSet->forceTextArea ){
      entry_attribute(pSet->name, /*pSet->width*/ 25, pSet->name,
                      pSet->var!=0 ? pSet->var : pSet->name,
                      (char*)pSet->def, 0);
      cgi_printf("<br />\n");
    }
  }
  for(pSet=aSetting; pSet->name!=0; pSet++){
    if( pSet->width!=0 && !pSet->versionable && pSet->forceTextArea ){
     cgi_printf("<b>%s</b><br />\n",(pSet->name));
      textarea_attribute("", /*rows*/ 3, /*cols*/ 50, pSet->name,
                      pSet->var!=0 ? pSet->var : pSet->name,
                      (char*)pSet->def, 0);
      cgi_printf("<br />\n");
    }
  }
  cgi_printf("</td><td style=\"width:50px;\"></td><td valign=\"top\">\n");
  for(pSet=aSetting; pSet->name!=0; pSet++){
    if( pSet->width!=0 && pSet->versionable ){
      int hasVersionableValue = db_get_versioned(pSet->name, NULL)!=0;
     cgi_printf("<b>%s</b> (v)<br />\n",(pSet->name));
      textarea_attribute("", /*rows*/ 3, /*cols*/ 20, pSet->name,
                      pSet->var!=0 ? pSet->var : pSet->name,
                      (char*)pSet->def, hasVersionableValue);
     cgi_printf("<br />\n");
    }
  }
  cgi_printf("</td></tr></table>\n"
         "</div></form>\n"
         "<p>Settings marked with (v) are 'versionable' and will be overridden\n"
         "by the contents of files named <tt>.fossil-settings/PROPERTY</tt>\n"
         "in the check-out root.\n"
         "If such a file is present, the corresponding field above is not\n"
         "editable.</p><hr /><p>\n"
         "These settings work the same as the\n"
         "<a href='%R/help?cmd=settings'>fossil set</a> command.\n");
  db_end_transaction(0);
  style_footer();
}

/*
** WEBPAGE: setup_config
**
** The "Admin/Configuration" page.  Requires Admin privilege.
*/
void setup_config(void){
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }

  style_header("WWW Configuration");
  db_begin_transaction();
  cgi_printf("<form action=\"%s/setup_config\" method=\"post\"><div>\n",(g.zTop));
  login_insert_csrf_secret();
  cgi_printf("<hr />\n");
  entry_attribute("Project Name", 60, "project-name", "pn", "", 0);
  cgi_printf("<p>Give your project a name so visitors know what this site is about.\n"
         "The project name will also be used as the RSS feed title.\n"
         "</p>\n"
         "<hr />\n");
  textarea_attribute("Project Description", 3, 80,
                     "project-description", "pd", "", 0);
  cgi_printf("<p>Describe your project. This will be used in page headers for search\n"
         "engines as well as a short RSS description.</p>\n"
         "<hr />\n");
  entry_attribute("Tarball and ZIP-archive Prefix", 20, "short-project-name", "spn", "", 0);
  cgi_printf("<p>This is used as a prefix on the names of generated tarballs and ZIP archive.\n"
         "For best results, keep this prefix brief and avoid special characters such\n"
         "as \"/\" and \"\\\".\n"
         "If no tarball prefix is specified, then the full Project Name above is used.\n"
         "</p>\n"
         "<hr />\n");
  onoff_attribute("Enable WYSIWYG Wiki Editing",
                  "wysiwyg-wiki", "wysiwyg-wiki", 0, 0);
  cgi_printf("<p>Enable what-you-see-is-what-you-get (WYSIWYG) editing of wiki pages.\n"
         "The WYSIWYG editor generates HTML instead of markup, which makes\n"
         "subsequent manual editing more difficult.</p>\n"
         "<hr />\n");
  entry_attribute("Index Page", 60, "index-page", "idxpg", "/home", 0);
  cgi_printf("<p>Enter the pathname of the page to display when the \"Home\" menu\n"
         "option is selected and when no pathname is\n"
         "specified in the URL.  For example, if you visit the url:</p>\n"
         "\n"
         "<blockquote><p>%h</p></blockquote>\n"
         "\n"
         "<p>And you have specified an index page of \"/home\" the above will\n"
         "automatically redirect to:</p>\n"
         "\n"
         "<blockquote><p>%h/home</p></blockquote>\n"
         "\n"
         "<p>The default \"/home\" page displays a Wiki page with the same name\n"
         "as the Project Name specified above.  Some sites prefer to redirect\n"
         "to a documentation page (ex: \"/doc/tip/index.wiki\") or to \"/timeline\".</p>\n"
         "\n"
         "<p>Note:  To avoid a redirect loop or other problems, this entry must\n"
         "begin with \"/\" and it must specify a valid page.  For example,\n"
         "\"<b>/home</b>\" will work but \"<b>home</b>\" will not, since it omits the\n"
         "leading \"/\".</p>\n"
         "<hr />\n",(g.zBaseURL),(g.zBaseURL));
  onoff_attribute("Use HTML as wiki markup language",
    "wiki-use-html", "wiki-use-html", 0, 0);
  cgi_printf("<p>Use HTML as the wiki markup language. Wiki links will still be parsed\n"
         "but all other wiki formatting will be ignored. This option is helpful\n"
         "if you have chosen to use a rich HTML editor for wiki markup such as\n"
         "TinyMCE.</p>\n"
         "<p><strong>CAUTION:</strong> when\n"
         "enabling, <i>all</i> HTML tags and attributes are accepted in the wiki.\n"
         "No sanitization is done. This means that it is very possible for malicious\n"
         "users to inject dangerous HTML, CSS and JavaScript code into your wiki.</p>\n"
         "<p>This should <strong>only</strong> be enabled when wiki editing is limited\n"
         "to trusted users. It should <strong>not</strong> be used on a publically\n"
         "editable wiki.</p>\n"
         "<hr />\n"
         "<p><input type=\"submit\"  name=\"submit\" value=\"Apply Changes\" /></p>\n"
         "</div></form>\n");
  db_end_transaction(0);
  style_footer();
}

/*
** WEBPAGE: setup_modreq
**
** Admin page for setting up moderation of tickets and wiki.
*/
void setup_modreq(void){
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }

  style_header("Moderator For Wiki And Tickets");
  db_begin_transaction();
  cgi_printf("<form action=\"%R/setup_modreq\" method=\"post\"><div>\n");
  login_insert_csrf_secret();
  cgi_printf("<hr />\n");
  onoff_attribute("Moderate ticket changes",
     "modreq-tkt", "modreq-tkt", 0, 0);
  cgi_printf("<p>When enabled, any change to tickets is subject to the approval\n"
         "by a ticket moderator - a user with the \"q\" or Mod-Tkt privilege.\n"
         "Ticket changes enter the system and are shown locally, but are not\n"
         "synced until they are approved.  The moderator has the option to\n"
         "delete the change rather than approve it.  Ticket changes made by\n"
         "a user who has the Mod-Tkt privilege are never subject to\n"
         "moderation.\n"
         "\n"
         "<hr />\n");
  onoff_attribute("Moderate wiki changes",
     "modreq-wiki", "modreq-wiki", 0, 0);
  cgi_printf("<p>When enabled, any change to wiki is subject to the approval\n"
         "by a wiki moderator - a user with the \"l\" or Mod-Wiki privilege.\n"
         "Wiki changes enter the system and are shown locally, but are not\n"
         "synced until they are approved.  The moderator has the option to\n"
         "delete the change rather than approve it.  Wiki changes made by\n"
         "a user who has the Mod-Wiki privilege are never subject to\n"
         "moderation.\n"
         "</p>\n");

  cgi_printf("<hr />\n"
         "<p><input type=\"submit\"  name=\"submit\" value=\"Apply Changes\" /></p>\n"
         "</div></form>\n");
  db_end_transaction(0);
  style_footer();

}

/*
** WEBPAGE: setup_adunit
**
** Administrative page for configuring and controlling ad units
** and how they are displayed.
*/
void setup_adunit(void){
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }
  db_begin_transaction();
  if( P("clear")!=0 ){
    db_multi_exec("DELETE FROM config WHERE name GLOB 'adunit*'");
    cgi_replace_parameter("adunit","");
  }

  style_header("Edit Ad Unit");
  cgi_printf("<form action=\"%s/setup_adunit\" method=\"post\"><div>\n",(g.zTop));
  login_insert_csrf_secret();
  cgi_printf("<b>Banner Ad-Unit:</b><br />\n");
 textarea_attribute("", 6, 80, "adunit", "adunit", "", 0);
  cgi_printf("<br />\n"
         "<b>Right-Column Ad-Unit:</b><br />\n");
  textarea_attribute("", 6, 80, "adunit-right", "adright", "", 0);
  cgi_printf("<br />\n");
  onoff_attribute("Omit ads to administrator",
     "adunit-omit-if-admin", "oia", 0, 0);
  cgi_printf("<br />\n");
  onoff_attribute("Omit ads to logged-in users",
     "adunit-omit-if-user", "oiu", 0, 0);
  cgi_printf("<br />\n"
         "<input type=\"submit\" name=\"submit\" value=\"Apply Changes\" />\n"
         "<input type=\"submit\" name=\"clear\" value=\"Delete Ad-Unit\" />\n"
         "</div></form>\n"
         "<hr />\n"
         "<b>Ad-Unit Notes:</b><ul>\n"
         "<li>Leave both Ad-Units blank to disable all advertising.\n"
         "<li>The \"Banner Ad-Unit\" is used for wide pages.\n"
         "<li>The \"Right-Column Ad-Unit\" is used on pages with tall, narrow content.\n"
         "<li>If the \"Right-Column Ad-Unit\" is blank, the \"Banner Ad-Unit\" is used on all pages.\n"
         "<li>Suggested <a href=\"setup_skinedit?w=0\">CSS</a> changes:\n"
         "<blockquote><pre>\n"
         "div.adunit_banner {\n"
         "  margin: auto;\n"
         "  width: 100%;\n"
         "}\n"
         "div.adunit_right {\n"
         "  float: right;\n"
         "}\n"
         "div.adunit_right_container {\n"
         "  min-height: <i>height-of-right-column-ad-unit</i>;\n"
         "}\n"
         "</pre></blockquote>\n"
         "<li>For a place-holder Ad-Unit for testing, Copy/Paste the following\n"
         "with appropriate adjustments to \"width:\" and \"height:\".\n"
         "<blockquote><pre>\n"
         "&lt;div style='\n"
         "  margin: 0 auto;\n"
         "  width: 600px;\n"
         "  height: 90px;\n"
         "  border: 1px solid #f11;\n"
         "  background-color: #fcc;\n"
         "'&gt;Demo Ad&lt;/div&gt;\n"
         "</pre></blockquote>\n"
         "</li>\n");
  style_footer();
  db_end_transaction(0);
}

/*
** WEBPAGE: setup_logo
**
** Administrative page for changing the logo image.
*/
void setup_logo(void){
  const char *zLogoMtime = db_get_mtime("logo-image", 0, 0);
  const char *zLogoMime = db_get("logo-mimetype","image/gif");
  const char *aLogoImg = P("logoim");
  int szLogoImg = atoi(PD("logoim:bytes","0"));
  const char *zBgMtime = db_get_mtime("background-image", 0, 0);
  const char *zBgMime = db_get("background-mimetype","image/gif");
  const char *aBgImg = P("bgim");
  int szBgImg = atoi(PD("bgim:bytes","0"));
  if( szLogoImg>0 ){
    zLogoMime = PD("logoim:mimetype","image/gif");
  }
  if( szBgImg>0 ){
    zBgMime = PD("bgim:mimetype","image/gif");
  }
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }
  db_begin_transaction();
  if( P("setlogo")!=0 && zLogoMime && zLogoMime[0] && szLogoImg>0 ){
    Blob img;
    Stmt ins;
    blob_init(&img, aLogoImg, szLogoImg);
    db_prepare(&ins,
        "REPLACE INTO config(name,value,mtime)"
        " VALUES('logo-image',:bytes,now())"
    );
    db_bind_blob(&ins, ":bytes", &img);
    db_step(&ins);
    db_finalize(&ins);
    db_multi_exec(
       "REPLACE INTO config(name,value,mtime) VALUES('logo-mimetype',%Q,now())",
       zLogoMime
    );
    db_end_transaction(0);
    cgi_redirect("setup_logo");
  }else if( P("clrlogo")!=0 ){
    db_multi_exec(
       "DELETE FROM config WHERE name IN "
           "('logo-image','logo-mimetype')"
    );
    db_end_transaction(0);
    cgi_redirect("setup_logo");
  }else if( P("setbg")!=0 && zBgMime && zBgMime[0] && szBgImg>0 ){
    Blob img;
    Stmt ins;
    blob_init(&img, aBgImg, szBgImg);
    db_prepare(&ins,
        "REPLACE INTO config(name,value,mtime)"
        " VALUES('background-image',:bytes,now())"
    );
    db_bind_blob(&ins, ":bytes", &img);
    db_step(&ins);
    db_finalize(&ins);
    db_multi_exec(
       "REPLACE INTO config(name,value,mtime)"
       " VALUES('background-mimetype',%Q,now())",
       zBgMime
    );
    db_end_transaction(0);
    cgi_redirect("setup_logo");
  }else if( P("clrbg")!=0 ){
    db_multi_exec(
       "DELETE FROM config WHERE name IN "
           "('background-image','background-mimetype')"
    );
    db_end_transaction(0);
    cgi_redirect("setup_logo");
  }
  style_header("Edit Project Logo And Background");
  cgi_printf("<p>The current project logo has a MIME-Type of <b>%h</b>\n"
         "and looks like this:</p>\n"
         "<blockquote><p><img src=\"%s/logo/%z\" alt=\"logo\" border=\"1\" />\n"
         "</p></blockquote>\n"
         "\n"
         "<form action=\"%s/setup_logo\" method=\"post\"\n"
         " enctype=\"multipart/form-data\"><div>\n"
         "<p>The logo is accessible to all users at this URL:\n"
         "<a href=\"%s/logo\">%s/logo</a>.\n"
         "The logo may or may not appear on each\n"
         "page depending on the <a href=\"setup_skinedit?w=0\">CSS</a> and\n"
         "<a href=\"setup_skinedit?w=2\">header setup</a>.\n"
         "To change the logo image, use the following form:</p>\n",(zLogoMime),(g.zTop),(zLogoMtime),(g.zTop),(g.zBaseURL),(g.zBaseURL));
  login_insert_csrf_secret();
  cgi_printf("Logo Image file:\n"
         "<input type=\"file\" name=\"logoim\" size=\"60\" accept=\"image/*\" />\n"
         "<p align=\"center\">\n"
         "<input type=\"submit\" name=\"setlogo\" value=\"Change Logo\" />\n"
         "<input type=\"submit\" name=\"clrlogo\" value=\"Revert To Default\" /></p>\n"
         "</div></form>\n"
         "<hr />\n"
         "\n"
         "<p>The current background image has a MIME-Type of <b>%h</b>\n"
         "and looks like this:</p>\n"
         "<blockquote><p><img src=\"%s/background/%z\" alt=\"background\" border=1 />\n"
         "</p></blockquote>\n"
         "\n"
         "<form action=\"%s/setup_logo\" method=\"post\"\n"
         " enctype=\"multipart/form-data\"><div>\n"
         "<p>The background image is accessible to all users at this URL:\n"
         "<a href=\"%s/background\">%s/background</a>.\n"
         "The background image may or may not appear on each\n"
         "page depending on the <a href=\"setup_skinedit?w=0\">CSS</a> and\n"
         "<a href=\"setup_skinedit?w=2\">header setup</a>.\n"
         "To change the background image, use the following form:</p>\n",(zBgMime),(g.zTop),(zBgMtime),(g.zTop),(g.zBaseURL),(g.zBaseURL));
  login_insert_csrf_secret();
  cgi_printf("Background image file:\n"
         "<input type=\"file\" name=\"bgim\" size=\"60\" accept=\"image/*\" />\n"
         "<p align=\"center\">\n"
         "<input type=\"submit\" name=\"setbg\" value=\"Change Background\" />\n"
         "<input type=\"submit\" name=\"clrbg\" value=\"Revert To Default\" /></p>\n"
         "</div></form>\n"
         "<hr />\n"
         "\n"
         "<p><span class=\"note\">Note:</span>  Your browser has probably cached these\n"
         "images, so you may need to press the Reload button before changes will\n"
         "take effect. </p>\n");
  style_footer();
  db_end_transaction(0);
}

/*
** Prevent the RAW SQL feature from being used to ATTACH a different
** database and query it.
**
** Actually, the RAW SQL feature only does a single statement per request.
** So it is not possible to ATTACH and then do a separate query.  This
** routine is not strictly necessary, therefore.  But it does not hurt
** to be paranoid.
*/
int raw_sql_query_authorizer(
  void *pError,
  int code,
  const char *zArg1,
  const char *zArg2,
  const char *zArg3,
  const char *zArg4
){
  if( code==SQLITE_ATTACH ){
    return SQLITE_DENY;
  }
  return SQLITE_OK;
}


/*
** WEBPAGE: admin_sql
**
** Run raw SQL commands against the database file using the web interface.
** Requires Admin privileges.
*/
void sql_page(void){
  const char *zQ = P("q");
  int go = P("go")!=0;
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }
  add_content_sql_commands(g.db);
  db_begin_transaction();
  style_header("Raw SQL Commands");
  cgi_printf("<p><b>Caution:</b> There are no restrictions on the SQL that can be\n"
         "run by this page.  You can do serious and irrepairable damage to the\n"
         "repository.  Proceed with extreme caution.</p>\n"
         "\n"
         "<p>Only the first statement in the entry box will be run.\n"
         "Any subsequent statements will be silently ignored.</p>\n"
         "\n"
         "<p>Database names:<ul><li>repository\n");
  if( g.zConfigDbName ){
    cgi_printf("<li>configdb\n");
  }
  if( g.localOpen ){
    cgi_printf("<li>localdb\n");
  }
  cgi_printf("</ul></p>\n"
         "\n"
         "<form method=\"post\" action=\"%s/admin_sql\">\n",(g.zTop));
  login_insert_csrf_secret();
  cgi_printf("SQL:<br />\n"
         "<textarea name=\"q\" rows=\"5\" cols=\"80\">%h</textarea><br />\n"
         "<input type=\"submit\" name=\"go\" value=\"Run SQL\">\n"
         "<input type=\"submit\" name=\"schema\" value=\"Show Schema\">\n"
         "<input type=\"submit\" name=\"tablelist\" value=\"List Tables\">\n"
         "</form>\n",(zQ));
  if( P("schema") ){
    zQ = sqlite3_mprintf(
            "SELECT sql FROM repository.sqlite_master WHERE sql IS NOT NULL");
    go = 1;
  }else if( P("tablelist") ){
    zQ = sqlite3_mprintf(
            "SELECT name FROM repository.sqlite_master WHERE type='table'"
            " ORDER BY name");
    go = 1;
  }
  if( go ){
    sqlite3_stmt *pStmt;
    int rc;
    const char *zTail;
    int nCol;
    int nRow = 0;
    int i;
    cgi_printf("<hr />\n");
    login_verify_csrf_secret();
    sqlite3_set_authorizer(g.db, raw_sql_query_authorizer, 0);
    rc = sqlite3_prepare_v2(g.db, zQ, -1, &pStmt, &zTail);
    if( rc!=SQLITE_OK ){
      cgi_printf("<div class=\"generalError\">%h</div>\n",(sqlite3_errmsg(g.db)));
      sqlite3_finalize(pStmt);
    }else if( pStmt==0 ){
      /* No-op */
    }else if( (nCol = sqlite3_column_count(pStmt))==0 ){
      sqlite3_step(pStmt);
      rc = sqlite3_finalize(pStmt);
      if( rc ){
        cgi_printf("<div class=\"generalError\">%h</div>\n",(sqlite3_errmsg(g.db)));
      }
    }else{
      cgi_printf("<table border=1>\n");
      while( sqlite3_step(pStmt)==SQLITE_ROW ){
        if( nRow==0 ){
          cgi_printf("<tr>\n");
          for(i=0; i<nCol; i++){
            cgi_printf("<th>%h</th>\n",(sqlite3_column_name(pStmt, i)));
          }
          cgi_printf("</tr>\n");
        }
        nRow++;
        cgi_printf("<tr>\n");
        for(i=0; i<nCol; i++){
          switch( sqlite3_column_type(pStmt, i) ){
            case SQLITE_INTEGER:
            case SQLITE_FLOAT: {
               cgi_printf("<td align=\"right\" valign=\"top\">\n"
                      "%s</td>\n",(sqlite3_column_text(pStmt, i)));
               break;
            }
            case SQLITE_NULL: {
               cgi_printf("<td valign=\"top\" align=\"center\"><i>NULL</i></td>\n");
               break;
            }
            case SQLITE_TEXT: {
               const char *zText = (const char*)sqlite3_column_text(pStmt, i);
               cgi_printf("<td align=\"left\" valign=\"top\"\n"
                      "style=\"white-space:pre;\">%h</td>\n",(zText));
               break;
            }
            case SQLITE_BLOB: {
               cgi_printf("<td valign=\"top\" align=\"center\">\n"
                      "<i>%d-byte BLOB</i></td>\n",(sqlite3_column_bytes(pStmt, i)));
               break;
            }
          }
        }
        cgi_printf("</tr>\n");
      }
      sqlite3_finalize(pStmt);
      cgi_printf("</table>\n");
    }
  }
  style_footer();
}


/*
** WEBPAGE: admin_th1
**
** Run raw TH1 commands using the web interface.  If Tcl integration was
** enabled at compile-time and the "tcl" setting is enabled, Tcl commands
** may be run as well.  Requires Admin privilege.
*/
void th1_page(void){
  const char *zQ = P("q");
  int go = P("go")!=0;
  login_check_credentials();
  if( !g.perm.Setup ){
    login_needed(0);
    return;
  }
  db_begin_transaction();
  style_header("Raw TH1 Commands");
  cgi_printf("<p><b>Caution:</b> There are no restrictions on the TH1 that can be\n"
         "run by this page.  If Tcl integration was enabled at compile-time and\n"
         "the \"tcl\" setting is enabled, Tcl commands may be run as well.</p>\n"
         "\n"
         "<form method=\"post\" action=\"%s/admin_th1\">\n",(g.zTop));
  login_insert_csrf_secret();
  cgi_printf("TH1:<br />\n"
         "<textarea name=\"q\" rows=\"5\" cols=\"80\">%h</textarea><br />\n"
         "<input type=\"submit\" name=\"go\" value=\"Run TH1\">\n"
         "</form>\n",(zQ));
  if( go ){
    const char *zR;
    int rc;
    int n;
    cgi_printf("<hr />\n");
    login_verify_csrf_secret();
    rc = Th_Eval(g.interp, 0, zQ, -1);
    zR = Th_GetResult(g.interp, &n);
    if( rc==TH_OK ){
      cgi_printf("<pre class=\"th1result\">%h</pre>\n",(zR));
    }else{
      cgi_printf("<pre class=\"th1error\">%h</pre>\n",(zR));
    }
  }
  style_footer();
}

static void admin_log_render_limits(){
  int const count = db_int(0,"SELECT COUNT(*) FROM admin_log");
  int i;
  int limits[] = {
  10, 20, 50, 100, 250, 500, 0
  };
  for(i = 0; limits[i]; ++i ){
    cgi_printf("%s<a href='?n=%d'>%d</a>",
               i ? " " : "",
               limits[i], limits[i]);
    if(limits[i]>count) break;
  }
}

/*
** WEBPAGE: admin_log
**
** Shows the contents of the admin_log table, which is only created if
** the admin-log setting is enabled. Requires Admin or Setup ('a' or
** 's') permissions.
*/
void page_admin_log(){
  Stmt stLog = empty_Stmt;
  Blob qLog = empty_blob;
  int limit;
  int fLogEnabled;
  int counter = 0;
  login_check_credentials();
  if( !g.perm.Setup && !g.perm.Admin ){
    login_needed(0);
    return;
  }
  style_header("Admin Log");
  create_admin_log_table();
  limit = atoi(PD("n","20"));
  fLogEnabled = db_get_boolean("admin-log", 0);
  cgi_printf("<div>Admin logging is %s.\n"
         "(Change this on the <a href=\"setup_settings\">settings</a> page.)</div>\n",(fLogEnabled?"on":"off"));


  cgi_printf("<div>Limit results to: <span>\n");
  admin_log_render_limits();
  cgi_printf("</span></div>\n");

  blob_append_sql(&qLog,
               "SELECT datetime(time,'unixepoch'), who, page, what "
               "FROM admin_log "
               "ORDER BY time DESC ");
  if(limit>0){
    cgi_printf("%d Most recent entries:\n",(limit));
    blob_append_sql(&qLog, "LIMIT %d", limit);
  }
  db_prepare(&stLog, "%s", blob_sql_text(&qLog));
  blob_reset(&qLog);
  cgi_printf("<table id=\"adminLogTable\" class=\"adminLogTable\" width=\"100%%\">\n"
         "<thead>\n"
         "<th>Time</th>\n"
         "<th>User</th>\n"
         "<th>Page</th>\n"
         "<th width=\"60%%\">Message</th>\n"
         "</thead><tbody>\n");
  while( SQLITE_ROW == db_step(&stLog) ){
    const char *zTime = db_column_text(&stLog, 0);
    const char *zUser = db_column_text(&stLog, 1);
    const char *zPage = db_column_text(&stLog, 2);
    const char *zMessage = db_column_text(&stLog, 3);
    cgi_printf("<tr class=\"row%d\">\n"
           "<td class=\"adminTime\">%s</td>\n"
           "<td>%s</td>\n"
           "<td>%s</td>\n"
           "<td>%h</td>\n"
           "</tr>\n",(counter++%2),(zTime),(zUser),(zPage),(zMessage));
  }
  cgi_printf("</tbody></table>\n");
  if(limit>0 && counter<limit){
    cgi_printf("<div>%d entries shown.</div>\n",(counter));
  }
  style_footer();
}

/*
** WEBPAGE: srchsetup
**
** Configure the search engine.  Requires Admin privilege.
*/
void page_srchsetup(){
  login_check_credentials();
  if( !g.perm.Setup && !g.perm.Admin ){
    login_needed(0);
    return;
  }
  style_header("Search Configuration");
  cgi_printf("<form action=\"%s/srchsetup\" method=\"post\"><div>\n",(g.zTop));
  login_insert_csrf_secret();
  cgi_printf("<div style=\"text-align:center;font-weight:bold;\">\n"
         "Server-specific settings that affect the\n"
         "<a href=\"%R/search\">/search</a> webpage.\n"
         "</div>\n"
         "<hr />\n");
  textarea_attribute("Document Glob List", 3, 35, "doc-glob", "dg", "", 0);
  cgi_printf("<p>The \"Document Glob List\" is a comma- or newline-separated list\n"
         "of GLOB expressions that identify all documents within the source\n"
         "tree that are to be searched when \"Document Search\" is enabled.\n"
         "Some examples:\n"
         "<table border=0 cellpadding=2 align=center>\n"
         "<tr><td>*.wiki,*.html,*.md,*.txt<td style=\"width: 4x;\">\n"
         "<td>Search all wiki, HTML, Markdown, and Text files</tr>\n"
         "<tr><td>doc/*.md,*/README.txt,README.txt<td>\n"
         "<td>Search all Markdown files in the doc/ subfolder and all README.txt\n"
         "files.</tr>\n"
         "<tr><td>*<td><td>Search all checked-in files</tr>\n"
         "<tr><td><i>(blank)</i><td>\n"
         "<td>Search nothing. (Disables document search).</tr>\n"
         "</table>\n"
         "<hr />\n");
  entry_attribute("Document Branch", 20, "doc-branch", "db", "trunk", 0);
  cgi_printf("<p>When searching documents, use the versions of the files found at the\n"
         "type of the \"Document Branch\" branch.  Recommended value: \"trunk\".\n"
         "Document search is disabled if blank.\n"
         "<hr />\n");
  onoff_attribute("Search Check-in Comments", "search-ci", "sc", 0, 0);
  cgi_printf("<br />\n");
  onoff_attribute("Search Documents", "search-doc", "sd", 0, 0);
  cgi_printf("<br />\n");
  onoff_attribute("Search Tickets", "search-tkt", "st", 0, 0);
  cgi_printf("<br />\n");
  onoff_attribute("Search Wiki","search-wiki", "sw", 0, 0);
  cgi_printf("<hr />\n"
         "<p><input type=\"submit\"  name=\"submit\" value=\"Apply Changes\" /></p>\n"
         "<hr />\n");
  if( P("fts0") ){
    search_drop_index();
  }else if( P("fts1") ){
    search_drop_index();
    search_create_index();
    search_fill_index();
    search_update_index(search_restrict(SRCH_ALL));
  }
  if( search_index_exists() ){
    cgi_printf("<p>Currently using an SQLite FTS4 search index. This makes search\n"
           "run faster, especially on large repositories, but takes up space.</p>\n");
    onoff_attribute("Use Porter Stemmer","search-stemmer","ss",0,0);
    cgi_printf("<p><input type=\"submit\" name=\"fts0\" value=\"Delete The Full-Text Index\">\n"
           "<input type=\"submit\" name=\"fts1\" value=\"Rebuild The Full-Text Index\">\n");
  }else{
    cgi_printf("<p>The SQLite FTS4 search index is disabled.  All searching will be\n"
           "a full-text scan.  This usually works fine, but can be slow for\n"
           "larger repositories.</p>\n");
    onoff_attribute("Use Porter Stemmer","search-stemmer","ss",0,0);
    cgi_printf("<p><input type=\"submit\" name=\"fts1\" value=\"Create A Full-Text Index\">\n");
  }
  cgi_printf("</div></form>\n");
  style_footer();
}
