/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
void output_table_sorting_javascript(const char *zTableId,const char *zColumnTypes,int iInitSort);
void test_timewarp_page(void);
void test_timewarp_cmd(void);
int filenames_are_case_sensitive(void);
#include <dirent.h>
typedef struct Blob Blob;
int file_tree_name(const char *zOrigName,Blob *pOut,int absolute,int errFatal);
int name_to_uuid(Blob *pName,int iErrPriority,const char *zType);
#if defined(FOSSIL_ENABLE_TCL)
#include "tcl.h"
#endif
#if defined(FOSSIL_ENABLE_JSON)
#include "cson_amalgamation.h"
#include "json_detail.h"
#endif
void usage(const char *zFormat);
void verify_all_options(void);
NORETURN void fossil_fatal(const char *zFormat,...);
void db_find_and_open_repository(int bFlags,int nArgUsed);
const char *find_option(const char *zLong,const char *zShort,int hasArg);
void timeline_cmd(void);
int fossil_isdigit(char c);
const char *timeline_query_for_tty(void);
int comment_print(const char *zText,const char *zOrigText,int indent,int width,int flags);
int count_nonbranch_children(int pid);
typedef struct Stmt Stmt;
void print_timeline(Stmt *q,int nLimit,int width,int verboseFlag);
#define SRCH_CKIN   0x0001    /* Search over check-in comments */
unsigned int search_restrict(unsigned int srchFlags);
char *fossil_strdup(const char *zOrig);
void blob_init(Blob *pBlob,const char *zData,int size);
typedef struct HQuery HQuery;
void url_add_parameter(HQuery *p,const char *zName,const char *zValue);
void style_submenu_entry(const char *zName,const char *zLabel,int iSize,int isDisabled);
void compute_ancestors(int rid,int N,int directOnly);
void compute_descendants(int rid,int N);
char *blob_sql_text(Blob *p);
void path_reset(void);
typedef struct PathNode PathNode;
PathNode *path_first(void);
int path_common_ancestor(int iMe,int iYou);
PathNode *path_shortest(int iFrom,int iTo,int directOnly,int oneWayOnly);
struct PathNode {
  int rid;                 /* ID for this node */
  u8 fromIsParent;         /* True if pFrom is the parent of rid */
  u8 isPrim;               /* True if primary side of common ancestor */
  u8 isHidden;             /* Abbreviate output in "fossil bisect ls" */
  PathNode *pFrom;         /* Node we came from */
  union {
    PathNode *pPeer;       /* List of nodes of the same generation */
    PathNode *pTo;         /* Next on path from beginning to end */
  } u;
  PathNode *pAll;        /* List of all nodes */
};
# define TAG_HIDDEN     5     /* Do not display in timeline */
void login_anonymous_available(void);
void bisect_create_bilog_table(int iCurrent);
void compute_uses_file(const char *zTab,int fid,int usesFlags);
char *db_text(const char *zDefault,const char *zSql,...);
void style_submenu_checkbox(const char *zName,const char *zLabel,int isDisabled);
int fossil_stricmp(const char *zA,const char *zB);
void cgi_set_query_parameter(const char *zName,const char *zValue);
void cgi_delete_query_parameter(const char *zName);
void cgi_query_parameters_to_url(HQuery *p);
void url_initialize(HQuery *p,const char *zBase);
void login_needed(int anonOk);
void cgi_replace_query_parameter(const char *zName,const char *zValue);
int name_to_typed_rid(const char *zName,const char *zType);
void page_timeline(void);
typedef struct ReCompiled ReCompiled;
void re_free(ReCompiled *pRe);
const char *re_compile(ReCompiled **ppRe,const char *zIn,int noCase);
char *fossil_strndup(const char *zOrig,int len);
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
void blobReallocMalloc(Blob *pBlob,unsigned int newSize);
#define BLOB_INITIALIZER  {0,0,0,0,0,blobReallocMalloc}
char *glob_expr(const char *zVal,const char *zGlobList);
void blob_append_sql(Blob *pBlob,const char *zFormat,...);
void style_submenu_multichoice(const char *zName,int nChoice,const char *const *azChoice,int isDisabled);
char *names_of_file(const char *zUuid);
int db_int(int iDflt,const char *zSql,...);
int symbolic_name_to_rid(const char *zTag,const char *zType);
double db_double(double rDflt,const char *zSql,...);
int fossil_isdate(const char *z);
double symbolic_name_to_mtime(const char *z);
char *url_render(HQuery *p,const char *zName1,const char *zValue1,const char *zName2,const char *zValue2);
void style_submenu_element(const char *zLabel,const char *zLink,...);
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
const char *timeline_query_for_www(void);
int db_multi_exec(const char *zSql,...);
int db_get_boolean(const char *zName,int dflt);
int cgi_parameter_boolean(const char *zName);
#define PB(x)       cgi_parameter_boolean(x)
typedef struct GraphRow GraphRow;
#define GR_MAX_RAIL   40      /* Max number of "rails" to display */
struct GraphRow {
  int rid;                    /* The rid for the check-in */
  i8 nParent;                 /* Number of parents */
  int *aParent;               /* Array of parents.  0 element is primary .*/
  char *zBranch;              /* Branch name */
  char *zBgClr;               /* Background Color */
  char zUuid[41];             /* Check-in for file ID */

  GraphRow *pNext;            /* Next row down in the list of all rows */
  GraphRow *pPrev;            /* Previous row */

  int idx;                    /* Row index.  First is 1.  0 used for "none" */
  int idxTop;                 /* Direct descendent highest up on the graph */
  GraphRow *pChild;           /* Child immediately above this node */
  u8 isDup;                   /* True if this is duplicate of a prior entry */
  u8 isLeaf;                  /* True if this is a leaf node */
  u8 timeWarp;                /* Child is earlier in time */
  u8 bDescender;              /* True if riser from bottom of graph to here. */
  i8 iRail;                   /* Which rail this check-in appears on. 0-based.*/
  i8 mergeOut;                /* Merge out to this rail.  -1 if no merge-out */
  u8 mergeIn[GR_MAX_RAIL];    /* Merge in from non-zero rails */
  int aiRiser[GR_MAX_RAIL];   /* Risers from this node to a higher row. */
  int mergeUpto;              /* Draw the mergeOut rail up to this level */
  u64 mergeDown;              /* Draw merge lines up from bottom of graph */

  u64 railInUse;              /* Mask of occupied rails at this row */
};
int hex_digit_value(char c);
typedef struct GraphContext GraphContext;
void timeline_output_graph_javascript(GraphContext *pGraph,int omitDescenders,int fileDiff);
int db_finalize(Stmt *pStmt);
void graph_free(GraphContext *p);
void graph_finish(GraphContext *p,int omitDescenders);
void fossil_free(void *p);
void blob_appendf(Blob *pBlob,const char *zFormat,...);
char *mprintf(const char *zFormat,...);
void blob_reset(Blob *pBlob);
#define blob_buffer(X)  ((X)->aData)
void blob_append(Blob *pBlob,const char *aData,int nData);
int fossil_isspace(char c);
char *blob_str(Blob *p);
#define blob_size(X)  ((X)->nUsed)
#define WIKI_INLINE         0x002  /* Do not surround with <p>..</p> */
void wiki_convert(Blob *pIn,Blob *pOut,int flags);
void db_column_blob(Stmt *pStmt,int N,Blob *pBlob);
void hyperlink_to_event_tagid(int tagid);
# define TAG_CLOSED     9     /* Do not display this check-in as a leaf */
int db_exists(const char *zSql,...);
int db_prepare(Stmt *pStmt,const char *zFormat,...);
int graph_add_row(GraphContext *p,int rid,int nParent,int *aParent,const char *zBranch,const char *zBgClr,const char *zUuid,int isLeaf);
int db_bind_int(Stmt *pStmt,const char *zParamName,int iValue);
int db_reset(Stmt *pStmt);
int fossil_strnicmp(const char *zA,const char *zB,int nByte);
int moderation_pending(int rid);
const char *db_column_text(Stmt *pStmt,int N);
int db_column_int(Stmt *pStmt,int N);
int db_step(Stmt *pStmt);
void blob_zero(Blob *pBlob);
# define TAG_BRANCH     8     /* Value is name of the current branch */
int db_static_prepare(Stmt *pStmt,const char *zFormat,...);
GraphContext *graph_init(void);
int db_get_int(const char *zName,int dflt);
int db_lget_int(const char *zName,int dflt);
int db_open_local(const char *zDbName);
int fossil_strcmp(const char *zA,const char *zB);
struct GraphContext {
  int nErr;                  /* Number of errors encountered */
  int mxRail;                /* Number of rails required to render the graph */
  GraphRow *pFirst;          /* First row in the list */
  GraphRow *pLast;           /* Last row in the list */
  int nBranch;               /* Number of distinct branches */
  char **azBranch;           /* Names of the branches */
  int nRow;                  /* Number of rows */
  int nHash;                 /* Number of slots in apHash[] */
  GraphRow **apHash;         /* Hash table of GraphRow objects.  Key: rid */
};
struct Stmt {
  Blob sql;               /* The SQL for this statement */
  sqlite3_stmt *pStmt;    /* The results of sqlite3_prepare_v2() */
  Stmt *pNext, *pPrev;    /* List of all unfinalized statements */
  int nStep;              /* Number of sqlite3_step() calls */
};
void www_print_timeline(Stmt *pQuery,int tmFlags,const char *zThisUser,const char *zThisTag,int selectedRid,void(*xExtra)(int));
void style_footer(void);
const char *cgi_parameter(const char *zName,const char *zDefault);
#define PD(x,y)     cgi_parameter((x),(y))
#define P(x)        cgi_parameter((x),0)
void style_header(const char *zTitleFormat,...);
void login_check_credentials(void);
void test_hash_color_page(void);
void fossil_print(const char *zFormat,...);
void test_hash_color(void);
int skin_detail_boolean(const char *zName);
char *hash_color(const char *z);
#define TIMELINE_BISECT   0x0800  /* Show supplimental bisect information */
#define TIMELINE_SHOWRID  0x0400  /* Show RID values in addition to UUIDs */
#define TIMELINE_UNHIDE   0x0200  /* Unhide check-ins with "hidden" tag */
#define TIMELINE_FRENAMES 0x0100  /* Detail only file name changes */
#define TIMELINE_UCOLOR   0x0080  /* Background color by user */
#define TIMELINE_BRCOLOR  0x0040  /* Background color by branch name */
#define TIMELINE_FCHANGES 0x0020  /* Detail file changes */
#define TIMELINE_DISJOINT 0x0010  /* Elements are not contiguous */
#define TIMELINE_GRAPH    0x0008  /* Compute a graph */
#define TIMELINE_BRIEF    0x0004  /* Combine adjacent elements of same object */
#define TIMELINE_LEAFONLY 0x0002  /* Show "Leaf", but not "Merge", "Fork" etc */
#define TIMELINE_ARTID    0x0001  /* Show artifact IDs on non-check-in lines */
#define INTERFACE 0
void hyperlink_to_user(const char *zU,const char *zD,const char *zSuf);
char *href(const char *zFormat,...);
void hyperlink_to_date(const char *zDate,const char *zSuffix);
char *xhref(const char *zExtra,const char *zFormat,...);
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
void hyperlink_to_uuid(const char *zUuid);
void cgi_printf(const char *zFormat,...);
int content_is_private(int rid);
void tag_private_status(int rid);
