/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
int db_lget_int(const char *zName,int dflt);
#include <dirent.h>
typedef struct Blob Blob;
int file_tree_name(const char *zOrigName,Blob *pOut,int absolute,int errFatal);
void db_must_be_within_tree(void);
void annotate_cmd(void);
void style_footer(void);
char *xhref(const char *zExtra,const char *zFormat,...);
void cgi_printf(const char *zFormat,...);
char *href(const char *zFormat,...);
char *mprintf(const char *zFormat,...);
int skin_detail_boolean(const char *zName);
typedef struct HQuery HQuery;
char *url_render(HQuery *p,const char *zName1,const char *zValue1,const char *zName2,const char *zValue2);
void style_submenu_element(const char *zLabel,const char *zLink,...);
void url_add_parameter(HQuery *p,const char *zName,const char *zValue);
void url_initialize(HQuery *p,const char *zBase);
void style_header(const char *zTitleFormat,...);
void compute_direct_ancestors(int rid);
int db_exists(const char *zSql,...);
#if defined(FOSSIL_ENABLE_TCL)
#include "tcl.h"
#endif
#if defined(FOSSIL_ENABLE_JSON)
#include "cson_amalgamation.h"
#include "json_detail.h"
#endif
NORETURN void fossil_redirect_home(void);
const char *cgi_parameter(const char *zName,const char *zDefault);
#define P(x)        cgi_parameter((x),0)
int name_to_typed_rid(const char *zName,const char *zType);
void load_control(void);
int exclude_spiders(void);
void login_needed(int anonOk);
void login_check_credentials(void);
#define PD(x,y)     cgi_parameter((x),(y))
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
void annotation_page(void);
unsigned gradient_color(unsigned c1,unsigned c2,int n,int i);
void db_end_transaction(int rollbackFlag);
typedef struct Stmt Stmt;
int db_finalize(Stmt *pStmt);
int db_reset(Stmt *pStmt);
const char *db_column_text(Stmt *pStmt,int N);
char *fossil_strdup(const char *zOrig);
int db_column_int(Stmt *pStmt,int N);
int db_step(Stmt *pStmt);
int db_bind_int(Stmt *pStmt,const char *zParamName,int iValue);
int db_prepare(Stmt *pStmt,const char *zFormat,...);
int db_multi_exec(const char *zSql,...);
void db_begin_transaction(void);
int content_get(int rid,Blob *pBlob);
int db_int(int iDflt,const char *zSql,...);
struct Stmt {
  Blob sql;               /* The SQL for this statement */
  sqlite3_stmt *pStmt;    /* The results of sqlite3_prepare_v2() */
  Stmt *pNext, *pPrev;    /* List of all unfinalized statements */
  int nStep;              /* Number of sqlite3_step() calls */
};
typedef struct ReCompiled ReCompiled;
void re_free(ReCompiled *pRe);
int blob_write_to_file(Blob *pBlob,const char *zFilename);
void diff_print_filenames(const char *zLeft,const char *zRight,u64 diffFlags);
void verify_all_options(void);
NORETURN void fossil_fatal(const char *zFormat,...);
const char *re_compile(ReCompiled **ppRe,const char *zIn,int noCase);
void diff_tk(const char *zSubCmd,int firstArg);
void test_diff_cmd(void);
void fossil_print(const char *zFormat,...);
int blob_read_from_file(Blob *pBlob,const char *zFilename);
void usage(const char *zFormat);
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
void test_rawdiff_cmd(void);
const char *find_option(const char *zLong,const char *zShort,int hasArg);
u64 diff_options(void);
void blob_to_utf8_no_bom(Blob *pBlob,int useMbcs);
int *text_diff(Blob *pA_Blob,Blob *pB_Blob,Blob *pOut,ReCompiled *pRe,u64 diffFlags);
void diff_errmsg(Blob *pOut,const char *msg,int diffFlags);
void *fossil_realloc(void *p,size_t n);
void blob_reset(Blob *pBlob);
#define blob_size(X)  ((X)->nUsed)
void blob_zero(Blob *pBlob);
int diff_width(u64 diffFlags);
int fossil_isalnum(char c);
char *blob_str(Blob *p);
int diff_context_lines(u64 diffFlags);
void blob_appendf(Blob *pBlob,const char *zFormat,...);
void htmlize_to_blob(Blob *p,const char *zIn,int n);
void blob_append(Blob *pBlob,const char *aData,int nData);
int re_match(ReCompiled *pRe,const unsigned char *zIn,int nIn);
typedef struct ReInput ReInput;
struct ReInput {
  const unsigned char *z;  /* All text */
  int i;                   /* Next byte to read */
  int mx;                  /* EOF when i>=mx */
};
struct ReCompiled {
  ReInput sIn;                /* Regular expression text */
  const char *zErr;           /* Error message to return */
  char *aOp;                  /* Operators for the virtual machine */
  int *aArg;                  /* Arguments to each operator */
  unsigned (*xNextChar)(ReInput*);  /* Next character function */
  unsigned char zInit[12];    /* Initial text to match */
  int nInit;                  /* Number of characters in zInit */
  unsigned nState;            /* Number of entries in aOp[] and aArg[] */
  unsigned nAlloc;            /* Slots allocated for aOp[] and aArg[] */
};
int fossil_isspace(char c);
void fossil_free(void *p);
void *fossil_malloc(size_t n);
#define LENGTH_MASK_SZ  13
#define LENGTH_MASK     ((1<<LENGTH_MASK_SZ)-1)
#define DIFF_WHITESPACE_ONLY \
    "whitespace changes only\n"
#define DIFF_TOO_MANY_CHANGES \
    "more than 10,000 changes\n"
#define DIFF_CANNOT_COMPUTE_SYMLINK \
    "cannot compute difference between symlink and regular file\n"
#define DIFF_CANNOT_COMPUTE_BINARY \
    "cannot compute difference between binary files\n"
#define DIFF_STRIP_EOLCR  (((u64)0x10)<<32) /* Strip trailing CR */
#define DIFF_NOTTOOBIG    (((u64)0x08)<<32) /* Only display if not too big */
#define DIFF_CONTEXT_EX   (((u64)0x04)<<32) /* Use context even if zero */
#define DIFF_INVERT       (((u64)0x02)<<32) /* Invert the diff (debug) */
#define DIFF_NOOPT        (((u64)0x01)<<32) /* Suppress optimizations (debug) */
#define DIFF_LINENO       ((u64)0x40000000) /* Show line numbers */
#define DIFF_HTML         ((u64)0x20000000) /* Render for HTML */
#define DIFF_BRIEF        ((u64)0x10000000) /* Show filenames only */
#define DIFF_VERBOSE      ((u64)0x08000000) /* Missing shown as empty files */
#define DIFF_SIDEBYSIDE   ((u64)0x04000000) /* Generate a side-by-side diff */
#define DIFF_IGNORE_ALLWS ((u64)0x03000000) /* Ignore all whitespace */
#define DIFF_IGNORE_EOLWS ((u64)0x01000000) /* Ignore end-of-line whitespace */
#define DIFF_WIDTH_MASK   ((u64)0x00ff0000) /* side-by-side column width */
#define DIFF_CONTEXT_MASK ((u64)0x0000ffff) /* Lines of context. Default if 0 */
#define INTERFACE 0
