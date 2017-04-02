/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
int fossil_any_has_fork(int rcvid);
typedef struct UrlData UrlData;
void transport_global_shutdown(UrlData *pUrlData);
void transport_close(UrlData *pUrlData);
char *db_timespan_name(double rSpan);
void fossil_warning(const char *zFormat,...);
void url_remember(void);
void url_prompt_for_password(void);
#define URL_PROMPTED         0x010  /* Prompted for PW already */
#define URL_PROMPT_PW        0x001  /* Prompt for password if needed */
int fossil_force_newline(void);
void db_set(const char *zName,const char *zValue,int globalFlag);
double db_double(double rDflt,const char *zSql,...);
typedef struct Blob Blob;
int http_exchange(Blob *pSend,Blob *pReply,int useLogin,int maxRedirect);
#define CONFIGSET_OVERWRITE 0x100000     /* Causes overwrite instead of merge */
#define CONFIGSET_OLDFORMAT 0x200000     /* Use the legacy format */
#define CONFIGSET_TKT       0x000004     /* Ticket configuration */
const char *configure_next_name(int iMask);
const char *configure_first_name(int iMask);
void db_record_repository_filename(const char *zName);
void content_enable_dephantomize(int onoff);
#define RELEASE_VERSION_NUMBER 20100
void socket_global_init(void);
void transport_stats(i64 *pnSent,i64 *pnRcvd,int resetFlag);
NORETURN void fossil_fatal(const char *zFormat,...);
int client_sync(unsigned syncFlags,unsigned configRcvMask,unsigned configSendMask);
#define SYNC_UV_DRYRUN      0x0400    /* Do not actually exchange files */
#define SYNC_UV_TRACE       0x0200    /* Describe UV activities */
#define SYNC_FROMPARENT     0x0100    /* Pull from the parent project */
#define SYNC_UV_REVERT      0x0080    /* Copy server unversioned to client */
#define SYNC_UNVERSIONED    0x0040    /* Sync unversioned content */
#define SYNC_RESYNC         0x0020    /* --verily */
#define SYNC_VERBOSE        0x0010    /* Extra diagnostics */
#define SYNC_PRIVATE        0x0008    /* Also transfer private content */
#define SYNC_CLONE          0x0004    /* clone the repository */
#define SYNC_PULL           0x0002    /* pull content server to client */
#define SYNC_PUSH           0x0001    /* push content client to server */
#define INTERFACE 0
char *cgi_extract_content(void);
void fossil_print(const char *zFormat,...);
int blob_read_from_file(Blob *pBlob,const char *zFilename);
#if defined(FOSSIL_ENABLE_TCL)
#include "tcl.h"
#endif
#if defined(FOSSIL_ENABLE_JSON)
#include "cson_amalgamation.h"
#include "json_detail.h"
#endif
void usage(const char *zFormat);
void db_find_and_open_repository(int bFlags,int nArgUsed);
void cmd_test_xfer(void);
void configure_rebuild(void);
void db_end_transaction(int rollbackFlag);
#define MC_PERMIT_HOOKS   1  /*  permit hooks to execute    */
int manifest_crosslink_end(int flags);
void configure_finalize_receive(void);
typedef struct Th_Interp Th_Interp;
void Th_Free(Th_Interp *,void *);
void blobarray_reset(Blob *aBlob,int n);
#define BLOB_SEEK_CUR 2
int blob_seek(Blob *p,int offset,int whence);
#define CONFIGSET_ALL       0x0000ff     /* Everything */
void configure_receive(const char *zName,Blob *pContent,int groupMask);
void configure_prepare_to_receive(int replaceFlag);
int configure_is_exportable(const char *zName);
int configure_send_group(Blob *pOut,int groupMask,sqlite3_int64 iStart);
#define CONFIGSET_ADDR      0x000040     /* The CONCEALED table */
#define CONFIGSET_USER      0x000020     /* The USER table */
int configure_name_to_mask(const char *z,int notFoundIsFatal);
int blob_eq_str(Blob *pBlob,const char *z,int n);
NORETURN void fossil_panic(const char *zFormat,...);
int blob_tokenize(Blob *pIn,Blob *aToken,int nToken);
int blob_line(Blob *pFrom,Blob *pTo);
#define TH_ERROR    1
void manifest_crosslink_begin(void);
void db_begin_transaction(void);
int db_get_int(const char *zName,int dflt);
Blob *cgi_output_blob(void);
int db_schema_is_outofdate(void);
void cgi_reset_content(void);
void cgi_set_content_type(const char *zType);
void blobarray_zero(Blob *aBlob,int n);
void login_check_credentials(void);
void login_set_anon_nobody_capabilities(void);
NORETURN void fossil_redirect_home(void);
const char *cgi_parameter(const char *zName,const char *zDefault);
#define PD(x,y)     cgi_parameter((x),(y))
void page_xfer(void);
int xfer_run_common_script(void);
const char *Th_GetResult(Th_Interp *,int *);
void fossil_error(int iPriority,const char *zFormat,...);
int Th_Eval(Th_Interp *interp,int iFrame,const char *zProg,int nProg);
void Th_Store(const char *zName,const char *zValue);
typedef unsigned int u32;
#define TH_INIT_NONE        ((u32)0x00000000) /* No flags. */
#define TH_INIT_DEFAULT     (TH_INIT_NONE)      /* Default flags. */
void Th_FossilInit(u32 flags);
#define TH_OK       0
int xfer_run_script(const char *zScript,const char *zUuidOrList,int bIsList);
const char *xfer_ticket_code(void);
const char *xfer_commit_code(void);
const char *xfer_push_code(void);
char *db_get(const char *zName,const char *zDefault);
const char *xfer_common_code(void);
void cgi_printf(const char *zFormat,...);
const char *unversioned_content_hash(int debugFlag);
void unversioned_schema(void);
void configure_render_special_name(const char *zName,Blob *pOut);
char *blob_sql_text(Blob *p);
void blob_append_sql(Blob *pBlob,const char *zFormat,...);
#define MC_NONE           0  /*  default handling           */
int content_put(Blob *pBlob);
int md5sum_blob(const Blob *pIn,Blob *pCksum);
void create_cluster(void);
char *mprintf(const char *zFormat,...);
void login_set_capabilities(const char *zCap,unsigned flags);
char *sha1_shared_secret(const char *zPw,const char *zLogin,const char *zProjCode);
int blob_constant_time_cmp(Blob *pA,Blob *pB);
int sha1sum_blob(const Blob *pIn,Blob *pCksum);
void blob_copy(Blob *pTo,Blob *pFrom);
typedef struct Stmt Stmt;
void db_ephemeral_blob(Stmt *pStmt,int N,Blob *pBlob);
int db_get_boolean(const char *zName,int dflt);
#define P(x)        cgi_parameter((x),0)
int fossil_strcmp(const char *zA,const char *zB);
void defossilize(char *z);
char *blob_terminate(Blob *p);
int check_login(Blob *pLogin,Blob *pNonce,Blob *pSig);
#define HNAME_ERROR  0      /* Not a valid hash */
int blob_tail(Blob *pFrom,Blob *pTo);
void db_column_blob(Stmt *pStmt,int N,Blob *pBlob);
i64 db_column_int64(Stmt *pStmt,int N);
const char *db_column_raw(Stmt *pStmt,int N);
int db_column_bytes(Stmt *pStmt,int N);
const char *db_column_text(Stmt *pStmt,int N);
int blob_compare(Blob *pA,Blob *pB);
#define HNAME_LEN_SHA1   40
int db_exists(const char *zSql,...);
int blob_uncompress(Blob *pIn,Blob *pOut);
void db_blob(Blob *pResult,const char *zSql,...);
void blob_append(Blob *pBlob,const char *aData,int nData);
int blob_delta_create(Blob *pOriginal,Blob *pTarget,Blob *pDelta);
char *db_text(const char *zDefault,const char *zSql,...);
int content_is_private(int rid);
int db_int(int iDflt,const char *zSql,...);
void db_unset(const char *zName,int globalFlag);
int db_finalize(Stmt *pStmt);
int db_bind_int64(Stmt *pStmt,const char *zParamName,i64 iValue);
int db_bind_blob(Stmt *pStmt,const char *zParamName,Blob *pBlob);
void blob_compress(Blob *pIn,Blob *pOut);
int db_bind_text(Stmt *pStmt,const char *zParamName,const char *zValue);
int db_prepare(Stmt *pStmt,const char *zFormat,...);
int unversioned_status(const char *zName,sqlite3_int64 mtime,const char *zHash);
void content_rcvid_init(const char *zSrc);
void blob_init(Blob *pBlob,const char *zData,int size);
# define blob_eq(B,S) \
     ((B)->nUsed==sizeof(S)-1 && memcmp((B)->aData,S,sizeof(S)-1)==0)
int blob_is_int64(Blob *pBlob,sqlite3_int64 *pValue);
int blob_is_filename(Blob *pBlob);
int blob_is_reset(Blob *pBlob);
#define MC_NO_ERRORS      2  /*  do not issue errors for a bad parse */
int manifest_crosslink(int rid,Blob *pContent,int flags);
#define blob_buffer(X)  ((X)->aData)
int hname_verify_hash(Blob *pContent,const char *zHash,int nHash);
int blob_delta_apply(Blob *pOriginal,Blob *pDelta,Blob *pTarget);
void content_make_public(int rid);
int db_multi_exec(const char *zSql,...);
int content_get(int rid,Blob *pBlob);
#define blob_size(X)  ((X)->nUsed)
void Th_AppendToList(char **pzList,int *pnList,const char *zElem,int nElem);
int content_put_ex(Blob *pBlob,const char *zUuid,int srcId,int nBlob,int isPrivate);
typedef struct Global Global;
struct Blob {
  unsigned int nUsed;            /* Number of bytes used in aData[] */
  unsigned int nAlloc;           /* Number of bytes allocated for aData[] */
  unsigned int iCursor;          /* Next character of input to parse */
  unsigned int blobFlags;        /* One or more BLOBFLAG_* bits */
  char *aData;                   /* Where the information is stored */
  void (*xRealloc)(Blob*, unsigned int); /* Function to reallocate the buffer */
};
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
void blob_reset(Blob *pBlob);
int uuid_is_shunned(const char *zUuid);
int blob_extract(Blob *pFrom,int N,Blob *pTo);
void blob_zero(Blob *pBlob);
void blob_appendf(Blob *pBlob,const char *zFormat,...);
int blob_is_int(Blob *pBlob,int *pValue);
int blob_is_hname(Blob *pBlob);
int db_bind_int(Stmt *pStmt,const char *zParamName,int iValue);
char *blob_str(Blob *p);
int content_new(const char *zUuid,int isPrivate);
int db_reset(Stmt *pStmt);
int db_column_int(Stmt *pStmt,int N);
int db_step(Stmt *pStmt);
int db_bind_str(Stmt *pStmt,const char *zParamName,Blob *pBlob);
int db_static_prepare(Stmt *pStmt,const char *zFormat,...);
struct Stmt {
  Blob sql;               /* The SQL for this statement */
  sqlite3_stmt *pStmt;    /* The results of sqlite3_prepare_v2() */
  Stmt *pNext, *pPrev;    /* List of all unfinalized statements */
  int nStep;              /* Number of sqlite3_step() calls */
};
