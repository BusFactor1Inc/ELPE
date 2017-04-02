#line 1 "./src/style.c"
/*
** Copyright (c) 2006,2007 D. Richard Hipp
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
** This file contains code to implement the basic web page look and feel.
**
*/
#include "VERSION.h"
#include "config.h"
#include "style.h"


/*
** Elements of the submenu are collected into the following
** structure and displayed below the main menu.
**
** Populate these structure with calls to
**
**      style_submenu_element()
**      style_submenu_entry()
**      style_submenu_checkbox()
**      style_submenu_binary()
**      style_submenu_multichoice()
**      style_submenu_sql()
**
** prior to calling style_footer().  The style_footer() routine
** will generate the appropriate HTML text just below the main
** menu.
*/
static struct Submenu {
  const char *zLabel;        /* Button label */
  const char *zLink;         /* Jump to this link when button is pressed */
} aSubmenu[30];
static int nSubmenu = 0;     /* Number of buttons */
static struct SubmenuCtrl {
  const char *zName;         /* Form query parameter */
  const char *zLabel;        /* Label.  Might be NULL for FF_MULTI */
  unsigned char eType;       /* FF_ENTRY, FF_MULTI, FF_BINARY */
  unsigned char isDisabled;  /* True if this control is grayed out */
  short int iSize;           /* Width for FF_ENTRY.  Count for FF_MULTI */
  const char *const *azChoice;/* value/display pairs for FF_MULTI */
  const char *zFalse;        /* FF_BINARY label when false */
} aSubmenuCtrl[20];
static int nSubmenuCtrl = 0;
#define FF_ENTRY    1
#define FF_MULTI    2
#define FF_BINARY   3
#define FF_CHECKBOX 4

/*
** Remember that the header has been generated.  The footer is omitted
** if an error occurs before the header.
*/
static int headerHasBeenGenerated = 0;

/*
** remember, if a sidebox was used
*/
static int sideboxUsed = 0;

/*
** Ad-unit styles.
*/
static unsigned adUnitFlags = 0;


/*
** List of hyperlinks and forms that need to be resolved by javascript in
** the footer.
*/
char **aHref = 0;
int nHref = 0;
int nHrefAlloc = 0;
char **aFormAction = 0;
int nFormAction = 0;

/*
** Generate and return a anchor tag like this:
**
**        <a href="URL">
**  or    <a id="ID">
**
** The form of the anchor tag is determined by the g.javascriptHyperlink
** variable.  The href="URL" form is used if g.javascriptHyperlink is false.
** If g.javascriptHyperlink is true then the
** id="ID" form is used and javascript is generated in the footer to cause
** href values to be inserted after the page has loaded.  If
** g.perm.History is false, then the <a id="ID"> form is still
** generated but the javascript is not generated so the links never
** activate.
**
** If the user lacks the Hyperlink (h) property and the "auto-hyperlink"
** setting is true, then g.perm.Hyperlink is changed from 0 to 1 and
** g.javascriptHyperlink is set to 1.  The g.javascriptHyperlink defaults
** to 0 and only changes to one if the user lacks the Hyperlink (h) property
** and the "auto-hyperlink" setting is enabled.
**
** Filling in the href="URL" using javascript is a defense against bots.
**
** The name of this routine is deliberately kept short so that can be
** easily used within @-lines.  Example:
**
**      @ %z(href("%R/artifact/%s",zUuid))%h(zFN)</a>
**
** Note %z format.  The string returned by this function is always
** obtained from fossil_malloc() so rendering it with %z will reclaim
** that memory space.
**
** There are two versions of this routine: href() does a plain hyperlink
** and xhref() adds extra attribute text.
**
** g.perm.Hyperlink is true if the user has the Hyperlink (h) property.
** Most logged in users should have this property, since we can assume
** that a logged in user is not a bot.  Only "nobody" lacks g.perm.Hyperlink,
** typically.
*/
char *xhref(const char *zExtra, const char *zFormat, ...){
  char *zUrl;
  va_list ap;
  va_start(ap, zFormat);
  zUrl = vmprintf(zFormat, ap);
  va_end(ap);
  if( g.perm.Hyperlink && !g.javascriptHyperlink ){
    char *zHUrl = mprintf("<a %s href=\"%h\">", zExtra, zUrl);
    fossil_free(zUrl);
    return zHUrl;
  }
  if( nHref>=nHrefAlloc ){
    nHrefAlloc = nHrefAlloc*2 + 10;
    aHref = fossil_realloc(aHref, nHrefAlloc*sizeof(aHref[0]));
  }
  aHref[nHref++] = zUrl;
  return mprintf("<a %s id='a%d' href='%R/honeypot'>", zExtra, nHref);
}
char *href(const char *zFormat, ...){
  char *zUrl;
  va_list ap;
  va_start(ap, zFormat);
  zUrl = vmprintf(zFormat, ap);
  va_end(ap);
  if( g.perm.Hyperlink && !g.javascriptHyperlink ){
    char *zHUrl = mprintf("<a href=\"%h\">", zUrl);
    fossil_free(zUrl);
    return zHUrl;
  }
  if( nHref>=nHrefAlloc ){
    nHrefAlloc = nHrefAlloc*2 + 10;
    aHref = fossil_realloc(aHref, nHrefAlloc*sizeof(aHref[0]));
  }
  aHref[nHref++] = zUrl;
  return mprintf("<a id='a%d' href='%R/honeypot'>", nHref);
}

/*
** Generate <form method="post" action=ARG>.  The ARG value is inserted
** by javascript.
*/
void form_begin(const char *zOtherArgs, const char *zAction, ...){
  char *zLink;
  va_list ap;
  if( zOtherArgs==0 ) zOtherArgs = "";
  va_start(ap, zAction);
  zLink = vmprintf(zAction, ap);
  va_end(ap);
  if( g.perm.Hyperlink && !g.javascriptHyperlink ){
    cgi_printf("<form method=\"POST\" action=\"%z\" %s>\n",(zLink),(zOtherArgs));
  }else{
    int n;
    aFormAction = fossil_realloc(aFormAction, (nFormAction+1)*sizeof(char*));
    aFormAction[nFormAction++] = zLink;
    n = nFormAction;
    cgi_printf("<form id=\"form%d\" method=\"POST\" action='%R/login' %s>\n",(n),(zOtherArgs));
  }
}

/*
** Generate javascript that will set the href= attribute on all anchors.
*/
void style_resolve_href(void){
  int i;
  int nDelay = db_get_int("auto-hyperlink-delay",10);
  if( !g.perm.Hyperlink ) return;
  if( nHref==0 && nFormAction==0 ) return;
  cgi_printf("<script>\n"
         "function setAllHrefs(){\n");
  if( g.javascriptHyperlink ){
    for(i=0; i<nHref; i++){
      cgi_printf("gebi(\"a%d\").href=\"%s\";\n",(i+1),(aHref[i]));
    }
  }
  for(i=0; i<nFormAction; i++){
    cgi_printf("gebi(\"form%d\").action=\"%s\";\n",(i+1),(aFormAction[i]));
  }
  cgi_printf("}\n");
  if( sqlite3_strglob("*Opera Mini/[1-9]*", PD("HTTP_USER_AGENT",""))==0 ){
    /* Special case for Opera Mini, which executes JS server-side */
    cgi_printf("var isOperaMini = Object.prototype.toString.call(window.operamini)\n"
           "                  === \"[object OperaMini]\";\n"
           "if( isOperaMini ){\n"
           "  setTimeout(\"setAllHrefs();\",%d);\n"
           "}\n",(nDelay));
  }else if( db_get_boolean("auto-hyperlink-ishuman",0) && g.isHuman ){
    /* Active hyperlinks after a delay */
    cgi_printf("setTimeout(\"setAllHrefs();\",%d);\n",(nDelay));
  }else if( db_get_boolean("auto-hyperlink-mouseover",0) ){
    /* Require mouse movement before starting the teim that will
    ** activating hyperlinks */
    cgi_printf("document.getElementsByTagName(\"body\")[0].onmousemove=function(){\n"
           "  setTimeout(\"setAllHrefs();\",%d);\n"
           "  this.onmousemove = null;\n"
           "}\n",(nDelay));
  }else{
    /* Active hyperlinks after a delay */
    cgi_printf("setTimeout(\"setAllHrefs();\",%d);\n",(nDelay));
  }
  cgi_printf("</script>\n");
}

/*
** Add a new element to the submenu
*/
void style_submenu_element(
  const char *zLabel,
  const char *zLink,
  ...
){
  va_list ap;
  assert( nSubmenu < count(aSubmenu) );
  aSubmenu[nSubmenu].zLabel = zLabel;
  va_start(ap, zLink);
  aSubmenu[nSubmenu].zLink = vmprintf(zLink, ap);
  va_end(ap);
  nSubmenu++;
}
void style_submenu_entry(
  const char *zName,       /* Query parameter name */
  const char *zLabel,      /* Label before the entry box */
  int iSize,               /* Size of the entry box */
  int isDisabled           /* True if disabled */
){
  assert( nSubmenuCtrl < count(aSubmenuCtrl) );
  aSubmenuCtrl[nSubmenuCtrl].zName = zName;
  aSubmenuCtrl[nSubmenuCtrl].zLabel = zLabel;
  aSubmenuCtrl[nSubmenuCtrl].iSize = iSize;
  aSubmenuCtrl[nSubmenuCtrl].isDisabled = isDisabled;
  aSubmenuCtrl[nSubmenuCtrl].eType = FF_ENTRY;
  nSubmenuCtrl++;
}
void style_submenu_checkbox(
  const char *zName,       /* Query parameter name */
  const char *zLabel,      /* Label to display after the checkbox */
  int isDisabled           /* True if disabled */
){
  assert( nSubmenuCtrl < count(aSubmenuCtrl) );
  aSubmenuCtrl[nSubmenuCtrl].zName = zName;
  aSubmenuCtrl[nSubmenuCtrl].zLabel = zLabel;
  aSubmenuCtrl[nSubmenuCtrl].isDisabled = isDisabled;
  aSubmenuCtrl[nSubmenuCtrl].eType = FF_CHECKBOX;
  nSubmenuCtrl++;
}
void style_submenu_binary(
  const char *zName,       /* Query parameter name */
  const char *zTrue,       /* Label to show when parameter is true */
  const char *zFalse,      /* Label to show when the parameter is false */
  int isDisabled           /* True if this control is disabled */
){
  assert( nSubmenuCtrl < count(aSubmenuCtrl) );
  aSubmenuCtrl[nSubmenuCtrl].zName = zName;
  aSubmenuCtrl[nSubmenuCtrl].zLabel = zTrue;
  aSubmenuCtrl[nSubmenuCtrl].zFalse = zFalse;
  aSubmenuCtrl[nSubmenuCtrl].isDisabled = isDisabled;
  aSubmenuCtrl[nSubmenuCtrl].eType = FF_BINARY;
  nSubmenuCtrl++;
}
void style_submenu_multichoice(
  const char *zName,       /* Query parameter name */
  int nChoice,             /* Number of options */
  const char *const *azChoice,/* value/display pairs.  2*nChoice entries */
  int isDisabled           /* True if this control is disabled */
){
  assert( nSubmenuCtrl < count(aSubmenuCtrl) );
  aSubmenuCtrl[nSubmenuCtrl].zName = zName;
  aSubmenuCtrl[nSubmenuCtrl].iSize = nChoice;
  aSubmenuCtrl[nSubmenuCtrl].azChoice = azChoice;
  aSubmenuCtrl[nSubmenuCtrl].isDisabled = isDisabled;
  aSubmenuCtrl[nSubmenuCtrl].eType = FF_MULTI;
  nSubmenuCtrl++;
}
void style_submenu_sql(
  const char *zName,       /* Query parameter name */
  const char *zLabel,      /* Label on the control */
  const char *zFormat,     /* Format string for SQL command for choices */
  ...                      /* Arguments to the format string */
){
  Stmt q;
  int n = 0;
  int nAlloc = 0;
  char **az = 0;
  va_list ap;

  va_start(ap, zFormat);
  db_vprepare(&q, 0, zFormat, ap);
  va_end(ap);
  while( SQLITE_ROW==db_step(&q) ){
    if( n+2>=nAlloc ){
      nAlloc += nAlloc + 20;
      az = fossil_realloc(az, sizeof(char*)*nAlloc);
    }
    az[n++] = fossil_strdup(db_column_text(&q,0));
    az[n++] = fossil_strdup(db_column_text(&q,1));
  }
  db_finalize(&q);
  if( n>0 ){
    aSubmenuCtrl[nSubmenuCtrl].zName = zName;
    aSubmenuCtrl[nSubmenuCtrl].zLabel = zLabel;
    aSubmenuCtrl[nSubmenuCtrl].iSize = n/2;
    aSubmenuCtrl[nSubmenuCtrl].azChoice = (const char *const *)az;
    aSubmenuCtrl[nSubmenuCtrl].isDisabled = 0;
    aSubmenuCtrl[nSubmenuCtrl].eType = FF_MULTI;
    nSubmenuCtrl++;
  }
}


/*
** Compare two submenu items for sorting purposes
*/
static int submenuCompare(const void *a, const void *b){
  const struct Submenu *A = (const struct Submenu*)a;
  const struct Submenu *B = (const struct Submenu*)b;
  return fossil_strcmp(A->zLabel, B->zLabel);
}

/* Use this for the $current_page variable if it is not NULL.  If it is
** NULL then use g.zPath.
*/
static char *local_zCurrentPage = 0;

/*
** Set the desired $current_page to something other than g.zPath
*/
void style_set_current_page(const char *zFormat, ...){
  fossil_free(local_zCurrentPage);
  if( zFormat==0 ){
    local_zCurrentPage = 0;
  }else{
    va_list ap;
    va_start(ap, zFormat);
    local_zCurrentPage = vmprintf(zFormat, ap);
    va_end(ap);
  }
}

/*
** Create a TH1 variable containing the URL for the specified config resource.
** The resulting variable name will be of the form $[zVarPrefix]_url.
*/
static void url_var(
  const char *zVarPrefix,
  const char *zConfigName,
  const char *zPageName
){
  char *zVarName = mprintf("%s_url", zVarPrefix);
  char *zUrl = mprintf("%R/%s?id=%x", zPageName,
                       skin_id(zConfigName));
  Th_Store(zVarName, zUrl);
  free(zUrl);
  free(zVarName);
}

/*
** Create a TH1 variable containing the URL for the specified config image.
** The resulting variable name will be of the form $[zImageName]_image_url.
*/
static void image_url_var(const char *zImageName){
  char *zVarPrefix = mprintf("%s_image", zImageName);
  char *zConfigName = mprintf("%s-image", zImageName);
  url_var(zVarPrefix, zConfigName, zImageName);
  free(zVarPrefix);
  free(zConfigName);
}

/*
** Draw the header.
*/
void style_header(const char *zTitleFormat, ...){
  va_list ap;
  char *zTitle;
  const char *zHeader = skin_get("header");
  login_check_credentials();

  va_start(ap, zTitleFormat);
  zTitle = vmprintf(zTitleFormat, ap);
  va_end(ap);

  cgi_destination(CGI_HEADER);

  cgi_printf("<!DOCTYPE html>\n");

  if( g.thTrace ) Th_Trace("BEGIN_HEADER<br />\n", -1);

  /* Generate the header up through the main menu */
  Th_Store("project_name", db_get("project-name","Unnamed Fossil Project"));
  Th_Store("project_description", db_get("project-description",""));
  Th_Store("title", zTitle);
  Th_Store("baseurl", g.zBaseURL);
  Th_Store("secureurl", login_wants_https_redirect()? g.zHttpsURL: g.zBaseURL);
  Th_Store("home", g.zTop);
  Th_Store("index_page", db_get("index-page","/home"));
  if( local_zCurrentPage==0 ) style_set_current_page("%T", g.zPath);
  Th_Store("current_page", local_zCurrentPage);
  Th_Store("csrf_token", g.zCsrfToken);
  Th_Store("release_version", RELEASE_VERSION);
  Th_Store("manifest_version", MANIFEST_VERSION);
  Th_Store("manifest_date", MANIFEST_DATE);
  Th_Store("compiler_name", COMPILER_NAME);
  url_var("stylesheet", "css", "style.css");
  image_url_var("logo");
  image_url_var("background");
  if( !login_is_nobody() ){
    Th_Store("login", g.zLogin);
  }
  if( g.thTrace ) Th_Trace("BEGIN_HEADER_SCRIPT<br />\n", -1);
  Th_Render(zHeader);
  if( g.thTrace ) Th_Trace("END_HEADER<br />\n", -1);
  Th_Unstore("title");   /* Avoid collisions with ticket field names */
  cgi_destination(CGI_BODY);
  g.cgiOutput = 1;
  headerHasBeenGenerated = 1;
  sideboxUsed = 0;

  /* Make the gebi(x) function available as an almost-alias for
  ** document.getElementById(x) (except that it throws an error
  ** if the element is not found).
  **
  ** Maintenance note: this function must of course be available
  ** before it is called. It "should" go in the HEAD so that client
  ** HEAD code can make use of it, but because the client can replace
  ** the HEAD, and some fossil pages rely on gebi(), we put it here.
  */
  cgi_printf("<script>\n"
         "function gebi(x){\n"
         "if(x.substr(0,1)=='#') x = x.substr(1);\n"
         "var e = document.getElementById(x);\n"
         "if(!e) throw new Error('Expecting element with ID '+x);\n"
         "else return e;}\n"
         "</script>\n");
}

#if INTERFACE
/* Allowed parameters for style_adunit() */
#define ADUNIT_OFF        0x0001       /* Do not allow ads on this page */
#define ADUNIT_RIGHT_OK   0x0002       /* Right-side vertical ads ok here */
#endif

/*
** Various page implementations can invoke this interface to let the
** style manager know what kinds of ads are appropriate for this page.
*/
void style_adunit_config(unsigned int mFlags){
  adUnitFlags = mFlags;
}

/*
** Return the text of an ad-unit, if one should be rendered.  Return
** NULL if no ad-unit is desired.
**
** The *pAdFlag value might be set to ADUNIT_RIGHT_OK if this is
** a right-hand vertical ad.
*/
static const char *style_adunit_text(unsigned int *pAdFlag){
  const char *zAd = 0;
  *pAdFlag = 0;
  if( adUnitFlags & ADUNIT_OFF ) return 0;  /* Disallow ads on this page */
  if( g.perm.Admin && db_get_boolean("adunit-omit-if-admin",0) ){
    return 0;
  }
  if( !login_is_nobody()
   && fossil_strcmp(g.zLogin,"anonymous")!=0
   && db_get_boolean("adunit-omit-if-user",0)
  ){
    return 0;
  }
  if( (adUnitFlags & ADUNIT_RIGHT_OK)!=0
   && !fossil_all_whitespace(zAd = db_get("adunit-right", 0))
   && !cgi_body_contains("<table")
  ){
    *pAdFlag = ADUNIT_RIGHT_OK;
    return zAd;
  }else if( !fossil_all_whitespace(zAd = db_get("adunit",0)) ){
    return zAd;
  }
  return 0;
}

/*
** Draw the footer at the bottom of the page.
*/
void style_footer(void){
  const char *zFooter;
  const char *zAd = 0;
  unsigned int mAdFlags = 0;

  if( !headerHasBeenGenerated ) return;

  /* Go back and put the submenu at the top of the page.  We delay the
  ** creation of the submenu until the end so that we can add elements
  ** to the submenu while generating page text.
  */
  cgi_destination(CGI_HEADER);
  if( nSubmenu+nSubmenuCtrl>0 ){
    int i;
    if( nSubmenuCtrl ){
      cgi_printf("<form id='f01' method='GET' action='%R/%s'>", g.zPath);
    }
    cgi_printf("<div class=\"submenu\">\n");
    if( nSubmenu>0 ){
      qsort(aSubmenu, nSubmenu, sizeof(aSubmenu[0]), submenuCompare);
      for(i=0; i<nSubmenu; i++){
        struct Submenu *p = &aSubmenu[i];
        if( p->zLink==0 ){
          cgi_printf("<span class=\"label\">%h</span>\n",(p->zLabel));
        }else{
          cgi_printf("<a class=\"label\" href=\"%h\">%h</a>\n",(p->zLink),(p->zLabel));
        }
      }
    }
    for(i=0; i<nSubmenuCtrl; i++){
      const char *zQPN = aSubmenuCtrl[i].zName;
      const char *zDisabled = " disabled";
      if( !aSubmenuCtrl[i].isDisabled ){
        zDisabled = "";
        cgi_tag_query_parameter(zQPN);
      }
      switch( aSubmenuCtrl[i].eType ){
        case FF_ENTRY:
          cgi_printf("<span class='submenuctrl'>"
                 "&nbsp;%h"
                 "<input type='text' name='%s' value='%h' ",(aSubmenuCtrl[i].zLabel),(zQPN),(PD(zQPN, "")));
          if( aSubmenuCtrl[i].iSize<0 ){
            cgi_printf("size='%d' ",(-aSubmenuCtrl[i].iSize));
          }else if( aSubmenuCtrl[i].iSize>0 ){
            cgi_printf("size='%d' "
                   "maxlength='%d' ",(aSubmenuCtrl[i].iSize),(aSubmenuCtrl[i].iSize));
          }
          cgi_printf("onchange='gebi(\"f01\").submit();'%s></span>\n",(zDisabled));
          break;
        case FF_MULTI: {
          int j;
          const char *zVal = P(zQPN);
          if( aSubmenuCtrl[i].zLabel ){
            cgi_printf("&nbsp;%h",(aSubmenuCtrl[i].zLabel));
          }
          cgi_printf("<select class='submenuctrl' size='1' name='%s' "
                 "onchange='gebi(\"f01\").submit();'%s>\n",(zQPN),(zDisabled));
          for(j=0; j<aSubmenuCtrl[i].iSize*2; j+=2){
            const char *zQPV = aSubmenuCtrl[i].azChoice[j];
            cgi_printf("<option value='%h'",(zQPV));
            if( fossil_strcmp(zVal, zQPV)==0 ){
              cgi_printf(" selected");
            }
            cgi_printf(">%h</option>\n",(aSubmenuCtrl[i].azChoice[j+1]));
          }
          cgi_printf("</select>\n");
          break;
        }
        case FF_BINARY: {
          int isTrue = PB(zQPN);
          cgi_printf("<select class='submenuctrl' size='1' name='%s' "
                 "onchange='gebi(\"f01\").submit();'%s>\n"
                 "<option value='1'",(zQPN),(zDisabled));
          if( isTrue ){
            cgi_printf(" selected");
          }
          cgi_printf(">%h</option>\n"
                 "<option value='0'",(aSubmenuCtrl[i].zLabel));
          if( !isTrue ){
            cgi_printf(" selected");
          }
          cgi_printf(">%h</option>\n"
                 "</select>\n",(aSubmenuCtrl[i].zFalse));
          break;
        }
        case FF_CHECKBOX:
          cgi_printf("<label class='submenuctrl submenuckbox'>"
                 "<input type='checkbox' name='%s' ",(zQPN));
          if( PB(zQPN) ){
            cgi_printf("checked ");
          }
          cgi_printf("onchange='gebi(\"f01\").submit();'%s>"
                 "%h</label>\n",(zDisabled),(aSubmenuCtrl[i].zLabel));
          break;
      }
    }
    cgi_printf("</div>\n");
    if( nSubmenuCtrl ){
      cgi_query_parameters_to_hidden();
      cgi_tag_query_parameter(0);
      cgi_printf("</form>\n");
    }
  }

  zAd = style_adunit_text(&mAdFlags);
  if( (mAdFlags & ADUNIT_RIGHT_OK)!=0  ){
    cgi_printf("<div class=\"content adunit_right_container\">\n"
           "<div class=\"adunit_right\">\n");
    cgi_append_content(zAd, -1);
    cgi_printf("</div>\n");
  }else{
    if( zAd ){
      cgi_printf("<div class=\"adunit_banner\">\n");
      cgi_append_content(zAd, -1);
      cgi_printf("</div>\n");
    }
    cgi_printf("<div class=\"content\">\n");
  }
  cgi_destination(CGI_BODY);

  if( sideboxUsed ){
    /* Put the footer at the bottom of the page.
    ** the additional clear/both is needed to extend the content
    ** part to the end of an optional sidebox.
    */
    cgi_printf("<div class=\"endContent\"></div>\n");
  }
  cgi_printf("</div>\n");

  /* Set the href= field on hyperlinks.  Do this before the footer since
  ** the footer will be generating </html> */
  style_resolve_href();

  zFooter = skin_get("footer");
  if( g.thTrace ) Th_Trace("BEGIN_FOOTER<br />\n", -1);
  Th_Render(zFooter);
  if( g.thTrace ) Th_Trace("END_FOOTER<br />\n", -1);

  /* Render trace log if TH1 tracing is enabled. */
  if( g.thTrace ){
    cgi_append_content("<span class=\"thTrace\"><hr />\n", -1);
    cgi_append_content(blob_str(&g.thLog), blob_size(&g.thLog));
    cgi_append_content("</span>\n", -1);
  }
}

/*
** Begin a side-box on the right-hand side of a page.  The title and
** the width of the box are given as arguments.  The width is usually
** a percentage of total screen width.
*/
void style_sidebox_begin(const char *zTitle, const char *zWidth){
  sideboxUsed = 1;
  cgi_printf("<div class=\"sidebox\" style=\"width:%s\">\n"
         "<div class=\"sideboxTitle\">%h</div>\n",(zWidth),(zTitle));
}

/* End the side-box
*/
void style_sidebox_end(void){
  cgi_printf("</div>\n");
}


/* The following table contains bits of default CSS that must
** be included if they are not found in the application-defined
** CSS.
*/
const struct strctCssDefaults {
  const char *elementClass;  /* Name of element needed */
  const char *comment;       /* Comment text */
  const char *value;         /* CSS text */
} cssDefaultList[] = {
  { "div.sidebox",
    "The nomenclature sidebox for branches,..",
    "  float: right;\n"
    "  background-color: white;\n"
    "  border-width: medium;\n"
    "  border-style: double;\n"
    "  margin: 10px;\n"
  },
  { "div.sideboxTitle",
    "The nomenclature title in sideboxes for branches,..",
    "  display: inline;\n"
    "  font-weight: bold;\n"
  },
  { "div.sideboxDescribed",
    "The defined element in sideboxes for branches,..",
    "  display: inline;\n"
    "  font-weight: bold;\n"
  },
  { "span.disabled",
    "The defined element in sideboxes for branches,..",
    "  color: red;\n"
  },
  { "span.timelineDisabled",
    "The suppressed duplicates lines in timeline, ..",
    "  font-style: italic;\n"
    "  font-size: small;\n"
  },
  { "table.timelineTable",
    "the format for the timeline data table",
    "  border: 0;\n"
    "  border-collapse: collapse;\n"
  },
  { "td.timelineTableCell",
    "the format for the timeline data cells",
    "  vertical-align: top;\n"
    "  text-align: left;\n"
  },
  { "tr.timelineCurrent",
    "the format for the timeline data cell of the current checkout",
    "  padding: .1em .2em;\n"
    "  border: 1px dashed #446979;\n"
    "  box-shadow: 1px 1px 4px rgba(0, 0, 0, 0.5);\n"
  },
  { "tr.timelineSelected",
    "The row in the timeline table that contains the entry of interest",
    "  padding: .1em .2em;\n"
    "  border: 2px solid lightgray;\n"
    "  background-color: #ffc;\n"
    "  box-shadow: 4px 4px 2px rgba(0, 0, 0, 0.5);\n"
  },
  { "tr.timelineSpacer",
    "An extra row inserted to give vertical space between two rows",
    "  height: 1ex;\n"
  },
  { "span.timelineLeaf",
    "the format for the timeline leaf marks",
    "  font-weight: bold;\n"
  },
  { "a.timelineHistLink",
    "the format for the timeline version links",
    "\n"
  },
  { "span.timelineHistDsp",
    "the format for the timeline version display(no history permission!)",
    "  font-weight: bold;\n"
  },
  { "td.timelineTime",
    "the format for the timeline time display",
    "  vertical-align: top;\n"
    "  text-align: right;\n"
    "  white-space: nowrap;\n"
  },
  { "td.timelineGraph",
    "the format for the graph placeholder cells in timelines",
    "width: 20px;\n"
    "text-align: left;\n"
    "vertical-align: top;\n"
  },
  { ".tl-canvas",
    "timeline graph canvas",
    "  margin: 0 6px 0 10px;\n"
  },
  { ".tl-rail",
    "maximum rail spacing",
    "  width: 18px;\n"
  },
  { ".tl-mergeoffset",
    "maximum spacing between merge risers and primary child risers",
    "  width: 2px;\n"
  },
  { ".tl-nodemark",
    "adjusts the vertical position of graph nodes",
    "  margin-top: 5px;\n"
  },
  { ".tl-node",
    "commit node",
    "  width: 10px;\n"
    "  height: 10px;\n"
    "  border: 1px solid #000;\n"
    "  background: #fff;\n"
    "  cursor: pointer;\n"
  },
  { ".tl-node.leaf:after",
    "leaf commit marker",
    "  content: '';\n"
    "  position: absolute;\n"
    "  top: 3px;\n"
    "  left: 3px;\n"
    "  width: 4px;\n"
    "  height: 4px;\n"
    "  background: #000;\n"
  },
  { ".tl-node.sel:after",
    "selected commit node marker",
    "  content: '';\n"
    "  position: absolute;\n"
    "  top: 2px;\n"
    "  left: 2px;\n"
    "  width: 6px;\n"
    "  height: 6px;\n"
    "  background: red;\n"
  },
  { ".tl-arrow",
    "arrow",
    "  width: 0;\n"
    "  height: 0;\n"
    "  transform: scale(.999);\n"
    "  border: 0 solid transparent;\n"
  },
  { ".tl-arrow.u",
    "up arrow",
    "  margin-top: -1px;\n"
    "  border-width: 0 3px;\n"
    "  border-bottom: 7px solid #000;\n"
  },
  { ".tl-arrow.u.sm",
    "small up arrow",
    "  border-bottom: 5px solid #000;\n"
  },
  { ".tl-line",
    "line",
    "  background: #000;\n"
    "  width: 2px;\n"
  },
  { ".tl-arrow.merge",
    "merge arrow",
    "  height: 1px;\n"
    "  border-width: 2px 0;\n"
  },
  { ".tl-arrow.merge.l",
    "left merge arrow",
    "  border-right: 3px solid #000;\n"
  },
  { ".tl-arrow.merge.r",
    "right merge arrow",
    "  border-left: 3px solid #000;\n"
  },
  { ".tl-line.merge",
    "merge line",
    "  width: 1px;\n"
  },
  { ".tl-arrow.warp",
    "timewarp arrow",
    "  margin-left: 1px;\n"
    "  border-width: 3px 0;\n"
    "  border-left: 7px solid #600000;\n"
  },
  { ".tl-line.warp",
    "timewarp line",
    "  background: #600000;\n"
  },
  { "a.tagLink",
    "the format for the tag links",
    "\n"
  },
  { "span.tagDsp",
    "the format for the tag display(no history permission!)",
    "  font-weight: bold;\n"
  },
  { "span.wikiError",
    "the format for wiki errors",
    "  font-weight: bold;\n"
    "  color: red;\n"
  },
  { "span.infoTagCancelled",
    "the format for fixed/canceled tags,..",
    "  font-weight: bold;\n"
    "  text-decoration: line-through;\n"
  },
  { "span.infoTag",
    "the format for tags,..",
    "  font-weight: bold;\n"
  },
  { "span.wikiTagCancelled",
    "the format for fixed/cancelled tags,.. on wiki pages",
    "  text-decoration: line-through;\n"
  },
  { "table.browser",
    "format for the file display table",
    "/* the format for wiki errors */\n"
    "  width: 100%;\n"
    "  border: 0;\n"
  },
  { "td.browser",
    "format for cells in the file browser",
    "  width: 24%;\n"
    "  vertical-align: top;\n"
  },
  { ".filetree",
    "tree-view file browser",
    "  margin: 1em 0;\n"
    "  line-height: 1.5;\n"
  },
  {
    ".filetree > ul",
    "tree-view top-level list",
    "  display: inline-block;\n"
  },
  { ".filetree ul",
    "tree-view lists",
    "  margin: 0;\n"
    "  padding: 0;\n"
    "  list-style: none;\n"
  },
  { ".filetree ul.collapsed",
    "tree-view collapsed list",
    "  display: none;\n"
  },
  { ".filetree ul ul",
    "tree-view lists below the root",
    "  position: relative;\n"
    "  margin: 0 0 0 21px;\n"
  },
  { ".filetree li",
    "tree-view lists items",
    "  position: relative;\n"
    "  margin: 0;\n"
    "  padding: 0;\n"
  },
  { ".filetree li li:before",
    "tree-view node lines",
    "  content: '';\n"
    "  position: absolute;\n"
    "  top: -.8em;\n"
    "  left: -14px;\n"
    "  width: 14px;\n"
    "  height: 1.5em;\n"
    "  border-left: 2px solid #aaa;\n"
    "  border-bottom: 2px solid #aaa;\n"
  },
  { ".filetree li > ul:before",
    "tree-view directory lines",
    "  content: '';\n"
    "  position: absolute;\n"
    "  top: -1.5em;\n"
    "  bottom: 0;\n"
    "  left: -35px;\n"
    "  border-left: 2px solid #aaa;\n"
  },
  { ".filetree li.last > ul:before",
    "hide lines for last-child directories",
    "  display: none;\n"
  },
  { ".filetree a",
    "tree-view links",
    "  position: relative;\n"
    "  z-index: 1;\n"
    "  display: table-cell;\n"
    "  min-height: 16px;\n"
    "  padding-left: 21px;\n"
    "  background-image: url(data:image/gif;base64,R0lGODlhEAAQAJEAAP"
    "\\/\\/\\/yEhIf\\/\\/\\/wAAACH5BAEHAAIALAAAAAAQABAAAAIvlIKpxqcfmg"
    "OUvoaqDSCxrEEfF14GqFXImJZsu73wepJzVMNxrtNTj3NATMKhpwAAOw==);\n"
    "  background-position: center left;\n"
    "  background-repeat: no-repeat;\n"
  },
  { "ul.browser",
    "list of files in the 'flat-view' file browser",
    "  list-style-type: none;\n"
    "  padding: 10px;\n"
    "  margin: 0px;\n"
    "  white-space: nowrap;\n"
  },
  { "ul.browser li.file",
    "List element in the 'flat-view' file browser for a file",
    "  background-image: url(data:image/gif;base64,R0lGODlhEAAQAJEAAP"
    "\\/\\/\\/yEhIf\\/\\/\\/wAAACH5BAEHAAIALAAAAAAQABAAAAIvlIKpxqcfm"
    "gOUvoaqDSCxrEEfF14GqFXImJZsu73wepJzVMNxrtNTj3NATMKhpwAAOw==);\n"
    "  background-repeat: no-repeat;\n"
    "  background-position: 0px center;\n"
    "  padding-left: 20px;\n"
    "  padding-top: 2px;\n"
  },
  { "ul.browser li.dir",
    "List element in the 'flat-view file browser for a directory",
    "  background-image: url(data:image/gif;base64,R0lGODlhEAAQAJEAAP/WVCIi"
    "Iv\\/\\/\\/wAAACH5BAEHAAIALAAAAAAQABAAAAInlI9pwa3XYniCgQtkrAFfLXkiFo1jaX"
    "po+jUs6b5Z/K4siDu5RPUFADs=);\n"
    "  background-repeat: no-repeat;\n"
    "  background-position: 0px center;\n"
    "  padding-left: 20px;\n"
    "  padding-top: 2px;\n"
  },
  { "div.filetreeline",
    "line of a file tree",
    "  display: table;\n"
    "  width: 100%;\n"
    "  white-space: nowrap;\n"
  },
  { ".filetree .dir > div.filetreeline > a",
    "tree-view directory links",
    "  background-image: url(data:image/gif;base64,R0lGODlhEAAQAJEAAP/WVCIi"
    "Iv\\/\\/\\/wAAACH5BAEHAAIALAAAAAAQABAAAAInlI9pwa3XYniCgQtkrAFfLXkiFo1jaXp"
    "o+jUs6b5Z/K4siDu5RPUFADs=);\n"
  },
  { "div.filetreeage",
    "Last change floating display on the right",
    " display: table-cell;\n"
    " padding-left: 3em;\n"
    " text-align: right;\n"
  },
  { "div.filetreeline:hover",
    "Highlight the line of a file tree",
    " background-color: #eee;\n"
  },
  { "table.login_out",
    "table format for login/out label/input table",
    "  text-align: left;\n"
    "  margin-right: 10px;\n"
    "  margin-left: 10px;\n"
    "  margin-top: 10px;\n"
  },
  { "div.captcha",
    "captcha display options",
    "  text-align: center;\n"
    "  padding: 1ex;\n"
  },
  { "table.captcha",
    "format for the layout table, used for the captcha display",
    "  margin: auto;\n"
    "  padding: 10px;\n"
    "  border-width: 4px;\n"
    "  border-style: double;\n"
    "  border-color: black;\n"
  },
  { "td.login_out_label",
    "format for the label cells in the login/out table",
    "  text-align: center;\n"
  },
  { "span.loginError",
    "format for login error messages",
    "  color: red;\n"
  },
  { "span.note",
    "format for leading text for notes",
    "  font-weight: bold;\n"
  },
  { "span.textareaLabel",
    "format for textarea labels",
    "  font-weight: bold;\n"
  },
  { "table.usetupLayoutTable",
    "format for the user setup layout table",
    "  outline-style: none;\n"
    "  padding: 0;\n"
    "  margin: 25px;\n"
  },
  { "td.usetupColumnLayout",
    "format of the columns on the user setup list page",
    "  vertical-align: top\n"
  },
  { "table.usetupUserList",
    "format for the user list table on the user setup page",
    "  outline-style: double;\n"
    "  outline-width: 1px;\n"
    "  padding: 10px;\n"
  },
  { "th.usetupListUser",
    "format for table header user in user list on user setup page",
    "  text-align: right;\n"
    "  padding-right: 20px;\n"
  },
  { "th.usetupListCap",
    "format for table header capabilities in user list on user setup page",
    "  text-align: center;\n"
    "  padding-right: 15px;\n"
  },
  { "th.usetupListCon",
    "format for table header contact info in user list on user setup page",
    "  text-align: left;\n"
  },
  { "td.usetupListUser",
    "format for table cell user in user list on user setup page",
    "  text-align: right;\n"
    "  padding-right: 20px;\n"
    "  white-space:nowrap;\n"
  },
  { "td.usetupListCap",
    "format for table cell capabilities in user list on user setup page",
    "  text-align: center;\n"
    "  padding-right: 15px;\n"
  },
  { "td.usetupListCon",
    "format for table cell contact info in user list on user setup page",
    "  text-align: left\n"
  },
  { "div.ueditCapBox",
    "layout definition for the capabilities box on the user edit detail page",
    "  float: left;\n"
    "  margin-right: 20px;\n"
    "  margin-bottom: 20px;\n"
  },
  { "td.usetupEditLabel",
    "format of the label cells in the detailed user edit page",
    "  text-align: right;\n"
    "  vertical-align: top;\n"
    "  white-space: nowrap;\n"
  },
  { "span.ueditInheritNobody",
    "color for capabilities, inherited by nobody",
    "  color: green;\n"
    "  padding: .2em;\n"
  },
  { "span.ueditInheritDeveloper",
    "color for capabilities, inherited by developer",
    "  color: red;\n"
    "  padding: .2em;\n"
  },
  { "span.ueditInheritReader",
    "color for capabilities, inherited by reader",
    "  color: black;\n"
    "  padding: .2em;\n"
  },
  { "span.ueditInheritAnonymous",
    "color for capabilities, inherited by anonymous",
    "  color: blue;\n"
    "  padding: .2em;\n"
  },
  { "span.capability",
    "format for capabilities, mentioned on the user edit page",
    "  font-weight: bold;\n"
  },
  { "span.usertype",
    "format for different user types, mentioned on the user edit page",
    "  font-weight: bold;\n"
  },
  { "span.usertype:before",
    "leading text for user types, mentioned on the user edit page",
    "  content:\"'\";\n"
  },
  { "span.usertype:after",
    "trailing text for user types, mentioned on the user edit page",
    "  content:\"'\";\n"
  },
  { "div.selectedText",
    "selected lines of text within a linenumbered artifact display",
    "  font-weight: bold;\n"
    "  color: blue;\n"
    "  background-color: #d5d5ff;\n"
    "  border: 1px blue solid;\n"
  },
  { "p.missingPriv",
    "format for missing privileges note on user setup page",
    " color: blue;\n"
  },
  { "span.wikiruleHead",
    "format for leading text in wikirules definitions",
    "  font-weight: bold;\n"
  },
  { "td.tktDspLabel",
    "format for labels on ticket display page",
    "  text-align: right;\n"
  },
  { "td.tktDspValue",
    "format for values on ticket display page",
    "  text-align: left;\n"
    "  vertical-align: top;\n"
    "  background-color: #d0d0d0;\n"
  },
  { "span.tktError",
    "format for ticket error messages",
    "  color: red;\n"
    "  font-weight: bold;\n"
  },
  { "table.rpteditex",
    "format for example tables on the report edit page",
    "  float: right;\n"
    "  margin: 0;\n"
    "  padding: 0;\n"
    "  width: 125px;\n"
    "  text-align: center;\n"
    "  border-collapse: collapse;\n"
    "  border-spacing: 0;\n"
  },
  { "table.report",
    "Ticket report table formatting",
    "  border-collapse:collapse;\n"
    "  border: 1px solid #999;\n"
    "  margin: 1em 0 1em 0;\n"
    "  cursor: pointer;\n"
  },
  { "td.rpteditex",
    "format for example table cells on the report edit page",
    "  border-width: thin;\n"
    "  border-color: #000000;\n"
    "  border-style: solid;\n"
  },
  { "input.checkinUserColor",
    "format for user color input on check-in edit page",
    "/* no special definitions, class defined, to enable color pickers, f.e.:\n"
    "**  add the color picker found at http:jscolor.com  as java script include\n"
    "**  to the header and configure the java script file with\n"
    "**   1. use as bindClass :checkinUserColor\n"
    "**   2. change the default hash adding behaviour to ON\n"
    "** or change the class defition of element identified by id=\"clrcust\"\n"
    "** to a standard jscolor definition with java script in the footer. */\n"
  },
  { "div.endContent",
    "format for end of content area, to be used to clear page flow.",
    "  clear: both;\n"
  },
  { "p.generalError",
    "format for general errors",
    "  color: red;\n"
  },
  { "p.tktsetupError",
    "format for tktsetup errors",
    "  color: red;\n"
    "  font-weight: bold;\n"
  },
  { "p.xfersetupError",
    "format for xfersetup errors",
    "  color: red;\n"
    "  font-weight: bold;\n"
  },
  { "p.thmainError",
    "format for th script errors",
    "  color: red;\n"
    "  font-weight: bold;\n"
  },
  { "span.thTrace",
    "format for th script trace messages",
    "  color: red;\n"
  },
  { "p.reportError",
    "format for report configuration errors",
    "  color: red;\n"
    "  font-weight: bold;\n"
  },
  { "blockquote.reportError",
    "format for report configuration errors",
    "  color: red;\n"
    "  font-weight: bold;\n"
  },
  { "p.noMoreShun",
    "format for artifact lines, no longer shunned",
    "  color: blue;\n"
  },
  { "p.shunned",
    "format for artifact lines beeing shunned",
    "  color: blue;\n"
  },
  { "span.brokenlink",
    "a broken hyperlink",
    "  color: red;\n"
  },
  { "ul.filelist",
    "List of files in a timeline",
    "  margin-top: 3px;\n"
    "  line-height: 100%;\n"
  },
  { "ul.filelist li",
    "List of files in a timeline",
    "  padding-top: 1px;\n"
  },
  { "table.sbsdiffcols",
    "side-by-side diff display (column-based)",
    "  width: 90%;\n"
    "  border-spacing: 0;\n"
    "  font-size: xx-small;\n"
  },
  { "table.sbsdiffcols td",
    "sbs diff table cell",
    "  padding: 0;\n"
    "  vertical-align: top;\n"
  },
  { "table.sbsdiffcols pre",
    "sbs diff pre block",
    "  margin: 0;\n"
    "  padding: 0;\n"
    "  border: 0;\n"
    "  font-size: inherit;\n"
    "  background: inherit;\n"
    "  color: inherit;\n"
  },
  { "div.difflncol",
    "diff line number column",
    "  padding-right: 1em;\n"
    "  text-align: right;\n"
    "  color: #a0a0a0;\n"
  },
  { "div.difftxtcol",
    "diff text column",
    "  width: 45em;\n"
    "  overflow-x: auto;\n"
  },
  { "div.diffmkrcol",
    "diff marker column",
    "  padding: 0 1em;\n"
  },
  { "span.diffchng",
    "changes in a diff",
    "  background-color: #c0c0ff;\n"
  },
  { "span.diffadd",
    "added code in a diff",
    "  background-color: #c0ffc0;\n"
  },
  { "span.diffrm",
    "deleted in a diff",
    "  background-color: #ffc8c8;\n"
  },
  { "span.diffhr",
    "suppressed lines in a diff",
    "  display: inline-block;\n"
    "  margin: .5em 0 1em;\n"
    "  color: #0000ff;\n"
  },
  { "span.diffln",
    "line numbers in a diff",
    "  color: #a0a0a0;\n"
  },
  { "span.modpending",
    "Moderation Pending message on timeline",
    "  color: #b03800;\n"
    "  font-style: italic;\n"
  },
  { "pre.th1result",
    "format for th1 script results",
    "  white-space: pre-wrap;\n"
    "  word-wrap: break-word;\n"
  },
  { "pre.th1error",
    "format for th1 script errors",
    "  white-space: pre-wrap;\n"
    "  word-wrap: break-word;\n"
    "  color: red;\n"
  },
  { "table.label-value th",
    "The label/value pairs on (for example) the ci page",
    "  vertical-align: top;\n"
    "  text-align: right;\n"
    "  padding: 0.2ex 2ex;\n"
  },
  { ".statistics-report-graph-line",
    "for the /reports views",
    "  background-color: #446979;\n"
  },
  { ".statistics-report-table-events th",
    "",
    "  padding: 0 1em 0 1em;\n"
  },
  { ".statistics-report-table-events td",
    "",
    "  padding: 0.1em 1em 0.1em 1em;\n"
  },
  { ".statistics-report-row-year",
    "",
    "  text-align: left;\n"
  },
  { ".statistics-report-week-number-label",
    "for the /stats_report views",
    "text-align: right;\n"
    "font-size: 0.8em;\n"
  },
  { ".statistics-report-week-of-year-list",
    "for the /stats_report views",
    "font-size: 0.8em;\n"
  },
  { "tr.row0",
    "even table row color",
    "/* use default */\n"
  },
  { "tr.row1",
    "odd table row color",
    "/* Use default */\n"
  },
  { "#usetupEditCapability",
    "format for capabilities string, mentioned on the user edit page",
    "font-weight: bold;\n"
  },
  { "table.adminLogTable",
    "Class for the /admin_log table",
    "text-align: left;\n"
  },
  { ".adminLogTable .adminTime",
    "Class for the /admin_log table",
    "text-align: left;\n"
    "vertical-align: top;\n"
    "white-space: nowrap;\n"
  },
  { ".fileage table",
    "The fileage table",
    "border-spacing: 0;\n"
  },
  { ".fileage tr:hover",
    "Mouse-over effects for the file-age table",
    "background-color: #eee;\n"
  },
  { ".fileage td",
    "fileage table cells",
    "vertical-align: top;\n"
    "text-align: left;\n"
    "border-top: 1px solid #ddd;\n"
    "padding-top: 3px;\n"
  },
  { ".fileage td:first-child",
    "fileage first column (the age)",
    "white-space: nowrap;\n"
  },
  { ".fileage td:nth-child(2)",
    "fileage second column (the filename)",
    "padding-left: 1em;\n"
    "padding-right: 1em;\n"
  },
  { ".fileage td:nth-child(3)",
    "fileage third column (the check-in comment)",
    "word-wrap: break-word;\n"
    "max-width: 50%;\n"
  },
  { ".brlist table",  "The list of branches",
    "border-spacing: 0;\n"
  },
  { ".brlist table th",  "Branch list table headers",
    "text-align: left;\n"
    "padding: 0px 1em 0.5ex 0px;\n"
  },
  { ".brlist table td",  "Branch list table headers",
    "padding: 0px 2em 0px 0px;\n"
    "white-space: nowrap;\n"
  },
  { "th.sort:after",
    "General styles for sortable column marker",
    "margin-left: .4em;\n"
    "cursor: pointer;\n"
    "text-shadow: 0 0 0 #000; /* Makes arrow darker */\n"
  },
  { "th.sort.none:after",
    "None sort column marker",
    "content: '\\2666';\n"
  },
  { "th.sort.asc:after",
    "Ascending sort column marker",
    "content: '\\2193';\n"
  },
  { "th.sort.desc:after",
    "Descending sort column marker",
    "content: '\\2191';\n"
  },
  { "span.snippet>mark",
    "Search markup",
    "background-color: inherit;\n"
    "font-weight: bold;\n"
  },
  { "div.searchForm",
    "Container for the search terms entry box",
    "text-align: center;\n"
  },
  { "p.searchEmpty",
    "Message explaining that there are no search results",
    "font-style: italic;\n"
  },
  { 0,
    0,
    0
  }
};

/*
** Append all of the default CSS to the CGI output.
*/
void cgi_append_default_css(void) {
  int i;

  cgi_printf("%s", builtin_text("skins/default/css.txt"));
  for( i=0; cssDefaultList[i].elementClass; i++ ){
    if( cssDefaultList[i].elementClass[0] ){
      cgi_printf("/* %s */\n%s {\n%s\n}\n\n",
                 cssDefaultList[i].comment,
                 cssDefaultList[i].elementClass,
                 cssDefaultList[i].value
                );
    }
  }
}

/*
** Search string zCss for zSelector.
**
** Return true if found.  Return false if not found
*/
static int containsSelector(const char *zCss, const char *zSelector){
  const char *z;
  int n;
  int selectorLen = (int)strlen(zSelector);

  for(z=zCss; *z; z+=selectorLen){
    z = strstr(z, zSelector);
    if( z==0 ) return 0;
    if( z!=zCss ){
      for( n=-1; z+n!=zCss && fossil_isspace(z[n]); n--);
      if( z+n!=zCss && z[n]!=',' && z[n]!= '}' && z[n]!='/' ) continue;
    }
    for( n=selectorLen; z[n] && fossil_isspace(z[n]); n++ );
    if( z[n]==',' || z[n]=='{' || z[n]=='/' ) return 1;
  }
  return 0;
}

/*
** COMMAND: test-contains-selector
**
** Usage: %fossil test-contains-selector FILENAME SELECTOR
**
** Determine if the CSS stylesheet FILENAME contains SELECTOR.
*/
void contains_selector_cmd(void){
  int found;
  char *zSelector;
  Blob css;
  if( g.argc!=4 ) usage("FILENAME SELECTOR");
  blob_read_from_file(&css, g.argv[2]);
  zSelector = g.argv[3];
  found = containsSelector(blob_str(&css), zSelector);
  fossil_print("%s %s\n", zSelector, found ? "found" : "not found");
  blob_reset(&css);
}


/*
** WEBPAGE: style.css
**
** Return the style sheet.
*/
void page_style_css(void){
  Blob css;
  int i;

  cgi_set_content_type("text/css");
  blob_init(&css,skin_get("css"),-1);

  /* add special missing definitions */
  for(i=1; cssDefaultList[i].elementClass; i++){
    char *z = blob_str(&css);
    if( !containsSelector(z, cssDefaultList[i].elementClass) ){
      blob_appendf(&css, "/* %s */\n%s {\n%s}\n",
          cssDefaultList[i].comment,
          cssDefaultList[i].elementClass,
          cssDefaultList[i].value);
    }
  }

  /* Process through TH1 in order to give an opportunity to substitute
  ** variables such as $baseurl.
  */
  Th_Store("baseurl", g.zBaseURL);
  Th_Store("secureurl", login_wants_https_redirect()? g.zHttpsURL: g.zBaseURL);
  Th_Store("home", g.zTop);
  image_url_var("logo");
  image_url_var("background");
  Th_Render(blob_str(&css));

  /* Tell CGI that the content returned by this page is considered cacheable */
  g.isConst = 1;
}

/*
** WEBPAGE: test_env
**
** Display CGI-variables and other aspects of the run-time
** environment, for debugging and trouble-shooting purposes.
*/
void page_test_env(void){
  char c;
  int i;
  int showAll;
  char zCap[30];
  static const char *const azCgiVars[] = {
    "COMSPEC", "DOCUMENT_ROOT", "GATEWAY_INTERFACE",
    "HTTP_ACCEPT", "HTTP_ACCEPT_CHARSET", "HTTP_ACCEPT_ENCODING",
    "HTTP_ACCEPT_LANGUAGE", "HTTP_CONNECTION", "HTTP_HOST",
    "HTTP_USER_AGENT", "HTTP_REFERER", "PATH_INFO", "PATH_TRANSLATED",
    "QUERY_STRING", "REMOTE_ADDR", "REMOTE_PORT", "REQUEST_METHOD",
    "REQUEST_URI", "SCRIPT_FILENAME", "SCRIPT_NAME", "SERVER_PROTOCOL",
    "HOME", "FOSSIL_HOME", "USERNAME", "USER", "FOSSIL_USER",
    "SQLITE_TMPDIR", "TMPDIR",
    "TEMP", "TMP", "FOSSIL_VFS",
    "FOSSIL_FORCE_TICKET_MODERATION", "FOSSIL_FORCE_WIKI_MODERATION",
    "FOSSIL_TCL_PATH", "TH1_DELETE_INTERP", "TH1_ENABLE_DOCS",
    "TH1_ENABLE_HOOKS", "TH1_ENABLE_TCL", "REMOTE_HOST"
  };

  login_check_credentials();
  if( !g.perm.Admin && !g.perm.Setup && !db_get_boolean("test_env_enable",0) ){
    login_needed(0);
    return;
  }
  for(i=0; i<count(azCgiVars); i++) (void)P(azCgiVars[i]);
  style_header("Environment Test");
  showAll = PB("showall");
  style_submenu_checkbox("showall", "Cookies", 0);
  style_submenu_element("Stats", "%R/stat");

#if !defined(_WIN32)
  cgi_printf("uid=%d, gid=%d<br />\n",(getuid()),(getgid()));
#endif
  cgi_printf("g.zBaseURL = %h<br />\n"
         "g.zHttpsURL = %h<br />\n"
         "g.zTop = %h<br />\n"
         "g.zPath = %h<br />\n",(g.zBaseURL),(g.zHttpsURL),(g.zTop),(g.zPath));
  for(i=0, c='a'; c<='z'; c++){
    if( login_has_capability(&c, 1, 0) ) zCap[i++] = c;
  }
  zCap[i] = 0;
  cgi_printf("g.userUid = %d<br />\n"
         "g.zLogin = %h<br />\n"
         "g.isHuman = %d<br />\n"
         "capabilities = %s<br />\n",(g.userUid),(g.zLogin),(g.isHuman),(zCap));
  for(i=0, c='a'; c<='z'; c++){
    if( login_has_capability(&c, 1, LOGIN_ANON)
         && !login_has_capability(&c, 1, 0) ) zCap[i++] = c;
  }
  zCap[i] = 0;
  if( i>0 ){
    cgi_printf("anonymous-adds = %s<br />\n",(zCap));
  }
  cgi_printf("g.zRepositoryName = %h<br />\n"
         "load_average() = %f<br />\n"
         "<hr />\n",(g.zRepositoryName),(load_average()));
  P("HTTP_USER_AGENT");
  cgi_print_all(showAll);
  if( showAll && blob_size(&g.httpHeader)>0 ){
    cgi_printf("<hr />\n"
           "<pre>\n"
           "%h\n"
           "</pre>\n",(blob_str(&g.httpHeader)));
  }
  if( g.perm.Setup ){
    const char *zRedir = P("redirect");
    if( zRedir ) cgi_redirect(zRedir);
  }
  style_footer();
  if( g.perm.Admin && P("err") ) fossil_fatal("%s", P("err"));
}

/*
** WEBPAGE: honeypot
** This page is a honeypot for spiders and bots.
*/
void honeypot_page(void){
  cgi_set_status(403, "Forbidden");
  cgi_printf("<p>Please enable javascript or log in to see this content</p>\n");
}
