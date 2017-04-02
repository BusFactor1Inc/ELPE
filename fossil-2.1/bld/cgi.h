/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
const char *cgi_ssh_remote_addr(const char *zDefault);
void cgi_modified_since(time_t objectTime);
time_t mkgmtime(struct tm *p);
time_t cgi_rfc822_parsedate(const char *zDate);
void fossil_warning(const char *zFormat,...);
void *fossil_utf8_to_unicode(const char *zUtf8);
void fossil_print(const char *zFormat,...);
int cgi_http_server(int mnPort,int mxPort,const char *zBrowser,const char *zIpAddr,int flags);
#define HTTP_SERVER_REPOLIST       0x0010     /* Allow repo listing */
#define HTTP_SERVER_HAD_CHECKOUT   0x0008     /* Was a checkout open? */
#define HTTP_SERVER_HAD_REPOSITORY 0x0004     /* Was the repository open? */
#define HTTP_SERVER_SCGI           0x0002     /* SCGI instead of HTTP */
#define HTTP_SERVER_LOCALHOST      0x0001     /* Bind to 127.0.0.1 only */
void fossil_free(void *p);
int fossil_isdigit(char c);
void cgi_handle_scgi_request(void);
int fossil_strncmp(const char *zA,const char *zB,int nByte);
void cgi_handle_ssh_transport(const char *zCmd);
char *cgi_handle_ssh_probes(char *zLine,int zSize,char *z,char *zToken);
NORETURN void fossil_panic(const char *zFormat,...);
void cgi_handle_ssh_http_request(const char *zIpAddr);
void cgi_handle_http_request(const char *zIpAddr);
void cgi_vprintf(const char *zFormat,va_list ap);
typedef struct Blob Blob;
int vxprintf(Blob *pBlob,const char *fmt,va_list ap);
void cgi_tag_query_parameter(const char *zName);
typedef struct HQuery HQuery;
void url_add_parameter(HQuery *p,const char *zName,const char *zValue);
struct Blob {
  unsigned int nUsed;            /* Number of bytes used in aData[] */
  unsigned int nAlloc;           /* Number of bytes allocated for aData[] */
  unsigned int iCursor;          /* Next character of input to parse */
  unsigned int blobFlags;        /* One or more BLOBFLAG_* bits */
  char *aData;                   /* Where the information is stored */
  void (*xRealloc)(Blob*, unsigned int); /* Function to reallocate the buffer */
};
struct HQuery {
  Blob url;                  /* The URL */
  const char *zBase;         /* The base URL */
  int nParam;                /* Number of parameters. */
  int nAlloc;                /* Number of allocated slots */
  const char **azName;       /* Parameter names */
  const char **azValue;      /* Parameter values */
};
void cgi_query_parameters_to_url(HQuery *p);
void cgi_query_parameters_to_hidden(void);
int fossil_strnicmp(const char *zA,const char *zB,int nByte);
int fossil_stricmp(const char *zA,const char *zB);
void cgi_print_all(int showAll);
int cgi_all(const char *z,...);
int cgi_any(const char *z,...);
void cgi_debug(const char *zFormat,...);
const char *cgi_parameter_name(int i);
int is_truth(const char *zVal);
char *fossil_strdup(const char *zOrig);
#include <dirent.h>
char *fossil_getenv(const char *zName);
int fossil_isupper(char c);
int blob_uncompress(Blob *pIn,Blob *pOut);
int blob_read_from_channel(Blob *pBlob,FILE *in,int nToRead);
void *fossil_malloc(size_t n);
#if defined(FOSSIL_ENABLE_JSON)
#include "json_detail.h"
void json_main_bootstrap();
#endif
void cgi_init(void);
FILE *fossil_fopen(const char *zName,const char *zMode);
void cgi_trace(const char *z);
#if defined(FOSSIL_ENABLE_JSON)
void json_err(int code,char const *msg,int alsoOutput);
char const *json_guess_content_type();
void json_gc_add(char const *key,cson_value *v);
void cgi_parse_POST_JSON(FILE *zIn,unsigned int contentLen);
#endif
int fossil_tolower(char c);
#if defined(FOSSIL_ENABLE_JSON)
int json_setenv(char const *zKey,cson_value *v);
#endif
int fossil_islower(char c);
int dehttpize(char *z);
int fossil_isspace(char c);
void cgi_setenv(const char *zName,const char *zValue);
void cgi_delete_query_parameter(const char *zName);
void cgi_delete_parameter(const char *zName);
void cgi_replace_query_parameter(const char *zName,const char *zValue);
void cgi_replace_parameter(const char *zName,const char *zValue);
void cgi_set_query_parameter(const char *zName,const char *zValue);
void cgi_set_parameter(const char *zName,const char *zValue);
void *fossil_realloc(void *p,size_t n);
NORETURN void fossil_fatal(const char *zFormat,...);
void cgi_set_parameter_nocopy(const char *zName,const char *zValue,int isQP);
char *vmprintf(const char *zFormat,va_list ap);
NORETURN void cgi_redirectf(const char *zFormat,...);
NORETURN void cgi_redirect_with_method(const char *zURL);
NORETURN void cgi_redirect(const char *zURL);
NORETURN void fossil_exit(int rc);
void cgi_printf(const char *zFormat,...);
#if defined(FOSSIL_ENABLE_TCL)
#include "tcl.h"
#endif
#if defined(FOSSIL_ENABLE_JSON)
#include "cson_amalgamation.h"
#endif
typedef struct Global Global;
typedef struct Th_Interp Th_Interp;
typedef struct UrlData UrlData;
struct UrlData {
  int isFile;      /* True if a "file:" url */
  int isHttps;     /* True if a "https:" url */
  int isSsh;       /* True if an "ssh:" url */
  char *name;      /* Hostname for http: or filename for file: */
  char *hostname;  /* The HOST: parameter on http headers */
  char *protocol;  /* "http" or "https" */
  int port;        /* TCP port number for http: or https: */
  int dfltPort;    /* The default port for the given protocol */
  char *path;      /* Pathname for http: */
  char *user;      /* User id for http: */
  char *passwd;    /* Password for http: */
  char *canonical; /* Canonical representation of the URL */
  char *proxyAuth; /* Proxy-Authorizer: string */
  char *fossil;    /* The fossil query parameter on ssh: */
  unsigned flags;  /* Boolean flags controlling URL processing */
  int useProxy;    /* Used to remember that a proxy is in use */
  char *proxyUrlPath;
  int proxyOrigPort; /* Tunneled port number for https through proxy */
};
typedef struct FossilUserPerms FossilUserPerms;
struct FossilUserPerms {
  char Setup;            /* s: use Setup screens on web interface */
  char Admin;            /* a: administrative permission */
  char Delete;           /* d: delete wiki or tickets */
  char Password;         /* p: change password */
  char Query;            /* q: create new reports */
  char Write;            /* i: xfer inbound. check-in */
  char Read;             /* o: xfer outbound. check-out */
  char Hyperlink;        /* h: enable the display of hyperlinks */
  char Clone;            /* g: clone */
  char RdWiki;           /* j: view wiki via web */
  char NewWiki;          /* f: create new wiki via web */
  char ApndWiki;         /* m: append to wiki via web */
  char WrWiki;           /* k: edit wiki via web */
  char ModWiki;          /* l: approve and publish wiki content (Moderator) */
  char RdTkt;            /* r: view tickets via web */
  char NewTkt;           /* n: create new tickets */
  char ApndTkt;          /* c: append to tickets via the web */
  char WrTkt;            /* w: make changes to tickets via web */
  char ModTkt;           /* q: approve and publish ticket changes (Moderator) */
  char Attach;           /* b: add attachments */
  char TktFmt;           /* t: create new ticket report formats */
  char RdAddr;           /* e: read email addresses or other private data */
  char Zip;              /* z: download zipped artifact via /zip URL */
  char Private;          /* x: can send and receive private content */
  char WrUnver;          /* y: can push unversioned content */
};
#if defined(FOSSIL_ENABLE_TCL)
typedef struct TclContext TclContext;
struct TclContext {
  int argc;              /* Number of original (expanded) arguments. */
  char **argv;           /* Full copy of the original (expanded) arguments. */
  void *hLibrary;        /* The Tcl library module handle. */
  void *xFindExecutable; /* See tcl_FindExecutableProc in th_tcl.c. */
  void *xCreateInterp;   /* See tcl_CreateInterpProc in th_tcl.c. */
  void *xDeleteInterp;   /* See tcl_DeleteInterpProc in th_tcl.c. */
  void *xFinalize;       /* See tcl_FinalizeProc in th_tcl.c. */
  Tcl_Interp *interp;    /* The on-demand created Tcl interpreter. */
  int useObjProc;        /* Non-zero if an objProc can be called directly. */
  int useTip285;         /* Non-zero if TIP #285 is available. */
  char *setup;           /* The optional Tcl setup script. */
  void *xPreEval;        /* Optional, called before Tcl_Eval*(). */
  void *pPreContext;     /* Optional, provided to xPreEval(). */
  void *xPostEval;       /* Optional, called after Tcl_Eval*(). */
  void *pPostContext;    /* Optional, provided to xPostEval(). */
};
#endif
#define MX_AUX  5
struct Global {
  int argc; char **argv;  /* Command-line arguments to the program */
  char *nameOfExe;        /* Full path of executable. */
  const char *zErrlog;    /* Log errors to this file, if not NULL */
  int isConst;            /* True if the output is unchanging & cacheable */
  const char *zVfsName;   /* The VFS to use for database connections */
  sqlite3 *db;            /* The connection to the databases */
  sqlite3 *dbConfig;      /* Separate connection for global_config table */
  char *zAuxSchema;       /* Main repository aux-schema */
  int dbIgnoreErrors;     /* Ignore database errors if true */
  const char *zConfigDbName;/* Path of the config database. NULL if not open */
  sqlite3_int64 now;      /* Seconds since 1970 */
  int repositoryOpen;     /* True if the main repository database is open */
  char *zRepositoryOption; /* Most recent cached repository option value */
  char *zRepositoryName;  /* Name of the repository database file */
  char *zLocalDbName;     /* Name of the local database file */
  char *zOpenRevision;    /* Check-in version to use during database open */
  int localOpen;          /* True if the local database is open */
  char *zLocalRoot;       /* The directory holding the  local database */
  int minPrefix;          /* Number of digits needed for a distinct UUID */
  int eHashPolicy;        /* Current hash policy.  One of HPOLICY_* */
  int fNoDirSymlinks;     /* True if --no-dir-symlinks flag is present */
  int fSqlTrace;          /* True if --sqltrace flag is present */
  int fSqlStats;          /* True if --sqltrace or --sqlstats are present */
  int fSqlPrint;          /* True if -sqlprint flag is present */
  int fQuiet;             /* True if -quiet flag is present */
  int fJail;              /* True if running with a chroot jail */
  int fHttpTrace;         /* Trace outbound HTTP requests */
  int fAnyTrace;          /* Any kind of tracing */
  char *zHttpAuth;        /* HTTP Authorization user:pass information */
  int fSystemTrace;       /* Trace calls to fossil_system(), --systemtrace */
  int fSshTrace;          /* Trace the SSH setup traffic */
  int fSshClient;         /* HTTP client flags for SSH client */
  char *zSshCmd;          /* SSH command string */
  int fNoSync;            /* Do not do an autosync ever.  --nosync */
  int fIPv4;              /* Use only IPv4, not IPv6. --ipv4 */
  char *zPath;            /* Name of webpage being served */
  char *zExtra;           /* Extra path information past the webpage name */
  char *zBaseURL;         /* Full text of the URL being served */
  char *zHttpsURL;        /* zBaseURL translated to https: */
  char *zTop;             /* Parent directory of zPath */
  const char *zContentType;  /* The content type of the input HTTP request */
  int iErrPriority;       /* Priority of current error message */
  char *zErrMsg;          /* Text of an error message */
  int sslNotAvailable;    /* SSL is not available.  Do not redirect to https: */
  Blob cgiIn;             /* Input to an xfer www method */
  int cgiOutput;          /* Write error and status messages to CGI */
  int xferPanic;          /* Write error messages in XFER protocol */
  int fullHttpReply;      /* True for full HTTP reply.  False for CGI reply */
  Th_Interp *interp;      /* The TH1 interpreter */
  char *th1Setup;         /* The TH1 post-creation setup script, if any */
  int th1Flags;           /* The TH1 integration state flags */
  FILE *httpIn;           /* Accept HTTP input from here */
  FILE *httpOut;          /* Send HTTP output here */
  int xlinkClusterOnly;   /* Set when cloning.  Only process clusters */
  int fTimeFormat;        /* 1 for UTC.  2 for localtime.  0 not yet selected */
  int *aCommitFile;       /* Array of files to be committed */
  int markPrivate;        /* All new artifacts are private if true */
  int clockSkewSeen;      /* True if clocks on client and server out of sync */
  int wikiFlags;          /* Wiki conversion flags applied to %W */
  char isHTTP;            /* True if server/CGI modes, else assume CLI. */
  char javascriptHyperlink; /* If true, set href= using script, not HTML */
  Blob httpHeader;        /* Complete text of the HTTP request header */
  UrlData url;            /* Information about current URL */
  const char *zLogin;     /* Login name.  NULL or "" if not logged in. */
  const char *zSSLIdentity;  /* Value of --ssl-identity option, filename of
                             ** SSL client identity */
  int useLocalauth;       /* No login required if from 127.0.0.1 */
  int noPswd;             /* Logged in without password (on 127.0.0.1) */
  int userUid;            /* Integer user id */
  int isHuman;            /* True if access by a human, not a spider or bot */
  int comFmtFlags;        /* Zero or more "COMMENT_PRINT_*" bit flags */

  /* Information used to populate the RCVFROM table */
  int rcvid;              /* The rcvid.  0 if not yet defined. */
  char *zIpAddr;          /* The remote IP address */
  char *zNonce;           /* The nonce used for login */

  /* permissions available to current user */
  struct FossilUserPerms perm;

  /* permissions available to current user or to "anonymous".
  ** This is the logical union of perm permissions above with
  ** the value that perm would take if g.zLogin were "anonymous". */
  struct FossilUserPerms anon;

#ifdef FOSSIL_ENABLE_TCL
  /* all Tcl related context necessary for integration */
  struct TclContext tcl;
#endif

  /* For defense against Cross-site Request Forgery attacks */
  char zCsrfToken[12];    /* Value of the anti-CSRF token */
  int okCsrf;             /* Anti-CSRF token is present and valid */

  int parseCnt[10];       /* Counts of artifacts parsed */
  FILE *fDebug;           /* Write debug information here, if the file exists */
#ifdef FOSSIL_ENABLE_TH1_HOOKS
  int fNoThHook;          /* Disable all TH1 command/webpage hooks */
#endif
  int thTrace;            /* True to enable TH1 debugging output */
  Blob thLog;             /* Text of the TH1 debugging output */

  int isHome;             /* True if rendering the "home" page */

  /* Storage for the aux() and/or option() SQL function arguments */
  int nAux;                    /* Number of distinct aux() or option() values */
  const char *azAuxName[MX_AUX]; /* Name of each aux() or option() value */
  char *azAuxParam[MX_AUX];      /* Param of each aux() or option() value */
  const char *azAuxVal[MX_AUX];  /* Value of each aux() or option() value */
  const char **azAuxOpt[MX_AUX]; /* Options of each option() value */
  int anAuxCols[MX_AUX];         /* Number of columns for option() values */

  int allowSymlinks;             /* Cached "allow-symlinks" option */

  int mainTimerId;               /* Set to fossil_timer_start() */
#ifdef FOSSIL_ENABLE_JSON
  struct FossilJsonBits {
    int isJsonMode;            /* True if running in JSON mode, else
                                  false. This changes how errors are
                                  reported. In JSON mode we try to
                                  always output JSON-form error
                                  responses and always exit() with
                                  code 0 to avoid an HTTP 500 error.
                               */
    int resultCode;            /* used for passing back specific codes
                               ** from /json callbacks. */
    int errorDetailParanoia;   /* 0=full error codes, 1=%10, 2=%100, 3=%1000 */
    cson_output_opt outOpt;    /* formatting options for JSON mode. */
    cson_value *authToken;     /* authentication token */
    const char *jsonp;         /* Name of JSONP function wrapper. */
    unsigned char dispatchDepth /* Tells JSON command dispatching
                                   which argument we are currently
                                   working on. For this purpose, arg#0
                                   is the "json" path/CLI arg.
                                */;
    struct {                   /* "garbage collector" */
      cson_value *v;
      cson_array *a;
    } gc;
    struct {                   /* JSON POST data. */
      cson_value *v;
      cson_array *a;
      int offset;              /* Tells us which PATH_INFO/CLI args
                                  part holds the "json" command, so
                                  that we can account for sub-repos
                                  and path prefixes.  This is handled
                                  differently for CLI and CGI modes.
                               */
      const char *commandStr   /*"command" request param.*/;
    } cmd;
    struct {                   /* JSON POST data. */
      cson_value *v;
      cson_object *o;
    } post;
    struct {                   /* GET/COOKIE params in JSON mode. */
      cson_value *v;
      cson_object *o;
    } param;
    struct {
      cson_value *v;
      cson_object *o;
    } reqPayload;              /* request payload object (if any) */
    cson_array *warnings;      /* response warnings */
    int timerId;               /* fetched from fossil_timer_start() */
  } json;
#endif /* FOSSIL_ENABLE_JSON */
};
extern Global g;
#define CGIDEBUG(X)  if( g.fDebug ) cgi_debug X
void gzip_finish(Blob *pOut);
void gzip_step(const char *pIn,int nIn);
void gzip_begin(sqlite3_int64 now);
void blob_compress(Blob *pIn,Blob *pOut);
int fossil_strcmp(const char *zA,const char *zB);
void cgi_reply(void);
char *cgi_rfc822_datestamp(time_t now);
void blob_appendf(Blob *pBlob,const char *zFormat,...);
void cgi_set_cookie(const char *zName,const char *zValue,const char *zPath,int lifetime);
void cgi_append_header(const char *zLine);
void cgi_set_status(int iStat,const char *zStat);
void blob_zero(Blob *pBlob);
void cgi_set_content(Blob *pNewContent);
char *mprintf(const char *zFormat,...);
void cgi_set_content_type(const char *zType);
char *cgi_extract_content(void);
#define blob_buffer(X)  ((X)->aData)
#define blob_size(X)  ((X)->nUsed)
Blob *cgi_output_blob(void);
void blob_reset(Blob *pBlob);
void cgi_reset_content(void);
void blob_append(Blob *pBlob,const char *aData,int nData);
void cgi_append_content(const char *zData,int nAmt);
int cgi_body_contains(const char *zNeedle);
char *blob_str(Blob *p);
int cgi_header_contains(const char *zNeedle);
NORETURN void cgi_panic(const char *zFormat,...);
void cgi_destination(int dest);
void blobReallocMalloc(Blob *pBlob,unsigned int newSize);
#define BLOB_INITIALIZER  {0,0,0,0,0,blobReallocMalloc}
#define CGI_SSH_FOSSIL           0x0004     /* Use new Fossil SSH transport */
#define CGI_SSH_COMPAT           0x0002     /* Compat for old SSH transport */
#define CGI_SSH_CLIENT           0x0001     /* Client is SSH */
#define CGI_BODY     1
#define CGI_HEADER   0
int cgi_parameter_boolean(const char *zName);
#define PB(x)       cgi_parameter_boolean(x)
char *cgi_parameter_trimmed(const char *zName,const char *zDefault);
#define PDT(x,y)    cgi_parameter_trimmed((x),(y))
#define PT(x)       cgi_parameter_trimmed((x),0)
const char *cgi_parameter(const char *zName,const char *zDefault);
#define PD(x,y)     cgi_parameter((x),(y))
#define P(x)        cgi_parameter((x),0)
#define INTERFACE 0
