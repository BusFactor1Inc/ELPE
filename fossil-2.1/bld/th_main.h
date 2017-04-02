/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
NORETURN void fossil_fatal(const char *zFormat,...);
int fossil_stricmp(const char *zA,const char *zB);
#if defined(FOSSIL_ENABLE_TH1_HOOKS)
void test_th_hook(void);
#endif
void test_th_source(void);
void test_th_eval(void);
void cgi_reply(void);
typedef struct Blob Blob;
int blob_read_from_file(Blob *pBlob,const char *zFilename);
#if defined(FOSSIL_ENABLE_TCL)
#include "tcl.h"
#endif
#if defined(FOSSIL_ENABLE_JSON)
#include "cson_amalgamation.h"
#include "json_detail.h"
#endif
void usage(const char *zFormat);
void verify_all_options(void);
void login_set_capabilities(const char *zCap,unsigned flags);
void test_th_render(void);
#if defined(FOSSIL_ENABLE_TH1_DOCS)
int Th_AreDocsEnabled(void);
#endif
#if defined(FOSSIL_ENABLE_TH1_HOOKS)
int Th_WebpageNotify(const char *zName,unsigned int cmdFlags);
int Th_WebpageHook(const char *zName,unsigned int cmdFlags);
int Th_CommandNotify(const char *zName,unsigned int cmdFlags);
int Th_CommandHook(const char *zName,unsigned int cmdFlags);
int Th_AreHooksEnabled(void);
#endif
int fossil_isalnum(char c);
int fossil_isalpha(char c);
typedef struct Th_Interp Th_Interp;
int Th_UnsetVar(Th_Interp *,const char *,int);
void Th_Unstore(const char *zName);
void Th_StoreInt(const char *zName,int iValue);
void Th_StoreList(const char *zName,char **pzList,int nList);
void Th_AppendToList(char **pzList,int *pnList,const char *zElem,int nElem);
void Th_Store(const char *zName,const char *zValue);
typedef int(*Th_CommandProc)(Th_Interp *,void *,int,const char **,int *);
int Th_CreateCommand(Th_Interp *interp,const char *zName,Th_CommandProc xProc,void *pContext,void(*xDel)(Th_Interp *,void *));
#if defined(FOSSIL_ENABLE_TCL)
int th_register_tcl(Th_Interp *,void *);
#endif
int db_get_boolean(const char *zName,int dflt);
#include <dirent.h>
char *fossil_getenv(const char *zName);
int th_register_language(Th_Interp *interp);
typedef struct Th_Vtab Th_Vtab;
struct Th_Vtab {
  void *(*xMalloc)(unsigned int);
  void (*xFree)(void *);
};
Th_Interp *Th_CreateInterp(Th_Vtab *pVtab);
#define LOGIN_ANON       0x02         /* Use g.anon instead of g.perm */
#define WIKI_LINKSONLY      0x020  /* No markup.  Only decorate links */
void db_close(int reportErrors);
void db_close_config();
void Th_CloseConfig(int closeRepository);
int db_open_config(int useAttach,int isOptional);
#define OPEN_ANY_SCHEMA      0x002      /* Do not error if schema is wrong */
void Th_OpenConfig(int openRepository);
typedef struct UrlData UrlData;
void transport_close(UrlData *pUrlData);
void transport_send(UrlData *pUrlData,Blob *toSend);
const char *transport_errmsg(UrlData *pUrlData);
int transport_open(UrlData *pUrlData);
const char *get_user_agent();
void fossil_free(void *p);
void blob_appendf(Blob *pBlob,const char *zFormat,...);
void blob_append(Blob *pBlob,const char *aData,int nData);
void url_parse_local(const char *zUrl,unsigned int urlFlags,UrlData *pUrlData);
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
typedef struct ReCompiled ReCompiled;
void re_free(ReCompiled *pRe);
int re_match(ReCompiled *pRe,const unsigned char *zIn,int nIn);
const char *re_compile(ReCompiled **ppRe,const char *zIn,int noCase);
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
typedef struct Glob Glob;
void glob_free(Glob *pGlob);
int glob_match(Glob *pGlob,const char *zString);
Glob *glob_create(const char *zPatternList);
struct Glob {
  int nPattern;        /* Number of patterns */
  char **azPattern;    /* Array of pointers to patterns */
};
char *db_get(const char *zName,const char *zDefault);
int Th_Eval(Th_Interp *interp,int iFrame,const char *zProg,int nProg);
int Th_SetVar(Th_Interp *,const char *,int,const char *,int);
const char *Th_GetResult(Th_Interp *,int *);
int Th_GetVar(Th_Interp *,const char *,int);
int th_strlen(const char *);
int report_query_authorizer(void *pError,int code,const char *zArg1,const char *zArg2,const char *zArg3,const char *zArg4);
int encode16(const unsigned char *pIn,unsigned char *zOut,int N);
typedef struct Th_SubCommand Th_SubCommand;
struct Th_SubCommand {const char *zName; Th_CommandProc xProc;};
int Th_CallSubCommand(Th_Interp *,void *,int,const char **,int *,const Th_SubCommand *);
int unversioned_content(const char *zName,Blob *pContent);
int content_get(int rid,Blob *pBlob);
void style_footer(void);
void style_header(const char *zTitleFormat,...);
int Th_Render(const char *z);
typedef unsigned int u32;
void Th_FossilInit(u32 flags);
void cgi_replace_parameter(const char *zName,const char *zValue);
const char *cgi_parameter(const char *zName,const char *zDefault);
int Th_ErrorMessage(Th_Interp *,const char *,const char *,int);
int db_open_local(const char *zDbName);
#define OPEN_OK_NOT_FOUND    0x001      /* Do not error out if not found */
void db_find_and_open_repository(int bFlags,int nArgUsed);
char *mprintf(const char *zFormat,...);
#define blob_buffer(X)  ((X)->aData)
char *Th_Fetch(const char *zName,int *pSize);
int Th_SplitList(Th_Interp *,const char *,int,char ***,int **,int *);
int fossil_strnicmp(const char *zA,const char *zB,int nByte);
#define SRCH_WIKI   0x0008    /* Search over wiki */
#define SRCH_TKT    0x0004    /* Search over tickets */
#define SRCH_DOC    0x0002    /* Search over embedded documents */
#define SRCH_CKIN   0x0001    /* Search over check-in comments */
#define SRCH_ALL    0x000f    /* Search over everything */
unsigned int search_restrict(unsigned int srchFlags);
int Th_SetResultInt(Th_Interp *,int);
int login_has_capability(const char *zCap,int nCap,u32 flgs);
char *db_text(const char *zDefault,const char *zSql,...);
char *encode64(const char *zData,int nData);
void wiki_convert(Blob *pIn,Blob *pOut,int flags);
#define WIKI_NOBADLINKS     0x010  /* Ignore broken hyperlinks */
#define WIKI_INLINE         0x002  /* Do not surround with <p>..</p> */
void markdown_to_html(struct Blob *input_markdown,struct Blob *output_title,struct Blob *output_body);
void markdown_to_html(struct Blob *input_markdown,struct Blob *output_title,struct Blob *output_body);
void blob_init(Blob *pBlob,const char *zData,int size);
void login_verify_csrf_secret(void);
void login_insert_csrf_secret(void);
NORETURN void cgi_redirect(const char *zURL);
NORETURN void cgi_redirect_with_method(const char *zURL);
typedef struct Manifest Manifest;
void manifest_destroy(Manifest *p);
int db_int(int iDflt,const char *zSql,...);
int fossil_strcmp(const char *zA,const char *zB);
typedef struct ManifestFile ManifestFile;
ManifestFile *manifest_file_next(Manifest *p,int *pErr);
void manifest_file_rewind(Manifest *p);
void blob_reset(Blob *pBlob);
#define blob_size(X)  ((X)->nUsed)
#define CFTYPE_MANIFEST   1
Manifest *manifest_get(int rid,int cfType,Blob *pErr);
struct ManifestFile {
  char *zName;           /* Name of a file */
  char *zUuid;           /* Artifact hash for the file */
  char *zPerm;           /* File permissions */
  char *zPrior;          /* Prior name if the name was changed */
};
struct Blob {
  unsigned int nUsed;            /* Number of bytes used in aData[] */
  unsigned int nAlloc;           /* Number of bytes allocated for aData[] */
  unsigned int iCursor;          /* Next character of input to parse */
  unsigned int blobFlags;        /* One or more BLOBFLAG_* bits */
  char *aData;                   /* Where the information is stored */
  void (*xRealloc)(Blob*, unsigned int); /* Function to reallocate the buffer */
};
struct Manifest {
  Blob content;         /* The original content blob */
  int type;             /* Type of artifact.  One of CFTYPE_xxxxx */
  int rid;              /* The blob-id for this manifest */
  char *zBaseline;      /* Baseline manifest.  The B card. */
  Manifest *pBaseline;  /* The actual baseline manifest */
  char *zComment;       /* Decoded comment.  The C card. */
  double rDate;         /* Date and time from D card.  0.0 if no D card. */
  char *zUser;          /* Name of the user from the U card. */
  char *zRepoCksum;     /* MD5 checksum of the baseline content.  R card. */
  char *zWiki;          /* Text of the wiki page.  W card. */
  char *zWikiTitle;     /* Name of the wiki page. L card. */
  char *zMimetype;      /* Mime type of wiki or comment text.  N card.  */
  double rEventDate;    /* Date of an event.  E card. */
  char *zEventId;       /* Artifact hash for an event.  E card. */
  char *zTicketUuid;    /* UUID for a ticket. K card. */
  char *zAttachName;    /* Filename of an attachment. A card. */
  char *zAttachSrc;     /* Artifact hash for document being attached. A card. */
  char *zAttachTarget;  /* Ticket or wiki that attachment applies to.  A card */
  int nFile;            /* Number of F cards */
  int nFileAlloc;       /* Slots allocated in aFile[] */
  int iFile;            /* Index of current file in iterator */
  ManifestFile *aFile;  /* One entry for each F-card */
  int nParent;          /* Number of parents. */
  int nParentAlloc;     /* Slots allocated in azParent[] */
  char **azParent;      /* Hashes of parents.  One for each P card argument */
  int nCherrypick;      /* Number of entries in aCherrypick[] */
  struct {
    char *zCPTarget;    /* Hash for cherry-picked version w/ +|- prefix */
    char *zCPBase;      /* Hash for cherry-pick baseline. NULL for singletons */
  } *aCherrypick;
  int nCChild;          /* Number of cluster children */
  int nCChildAlloc;     /* Number of closts allocated in azCChild[] */
  char **azCChild;      /* Hashes of referenced objects in a cluster. M cards */
  int nTag;             /* Number of T Cards */
  int nTagAlloc;        /* Slots allocated in aTag[] */
  struct TagType {
    char *zName;           /* Name of the tag */
    char *zUuid;           /* Hash of artifact that the tag is applied to */
    char *zValue;          /* Value if the tag is really a property */
  } *aTag;              /* One for each T card */
  int nField;           /* Number of J cards */
  int nFieldAlloc;      /* Slots allocated in aField[] */
  struct {
    char *zName;           /* Key or field name */
    char *zValue;          /* Value of the field */
  } *aField;            /* One for each J card */
};
int th1_artifact_from_ci_and_filename(Th_Interp *interp,const char *zCI,const char *zFilename);
int symbolic_name_to_rid(const char *zTag,const char *zType);
void cgi_append_content(const char *zData,int nAmt);
char *htmlize(const char *zIn,int n);
#define TH_CONTINUE 4
#define TH_RETURN   3
#define TH_BREAK    2
const char *Th_ReturnCodeName(int rc,int nullIfOk);
char *httpize(const char *z,int n);
#define TH_OK       0
int Th_SetResult(Th_Interp *,const char *,int);
#define TH_ERROR    1
int Th_ToInt(Th_Interp *,const char *,int,int *);
int Th_WrongNumArgs(Th_Interp *interp,const char *zMsg);
typedef struct Stmt Stmt;
int db_finalize(Stmt *pStmt);
void Th_Free(Th_Interp *,void *);
int Th_ListAppend(Th_Interp *,char **,int *,const char *,int);
int db_column_int(Stmt *pStmt,int N);
const char *db_column_text(Stmt *pStmt,int N);
int db_step(Stmt *pStmt);
int db_prepare(Stmt *pStmt,const char *zFormat,...);
int compute_fileage(int vid,const char *zGlob);
int th1_name_to_typed_rid(Th_Interp *interp,const char *zName,const char *zType);
struct Stmt {
  Blob sql;               /* The SQL for this statement */
  sqlite3_stmt *pStmt;    /* The results of sqlite3_prepare_v2() */
  Stmt *pNext, *pPrev;    /* List of all unfinalized statements */
  int nStep;              /* Number of sqlite3_step() calls */
};
char *blob_str(Blob *p);
void fossil_print(const char *zFormat,...);
void Th_PrintTraceLog();
void blob_zero(Blob *pBlob);
const char *find_option(const char *zLong,const char *zShort,int hasArg);
void Th_InitTraceLog();
void fossil_binary_mode(FILE *p);
void Th_ForceCgi(int fullHttpReply);
void blob_vappendf(Blob *pBlob,const char *zFormat,va_list ap);
void Th_Trace(const char *zFormat,...);
int Th_GetOutstandingMalloc();
void *fossil_malloc(size_t n);
#define TH_INIT_FORCE_TCL   ((u32)0x00000002) /* Force Tcl to be enabled? */
#define TH_INIT_FORBID_MASK (TH_INIT_FORCE_TCL) /* Illegal from a script. */
#define TH_INIT_NEED_CONFIG ((u32)0x00000001) /* Open configuration first? */
#define TH_INIT_FORCE_SETUP ((u32)0x00000008) /* Force eval of setup script? */
#define TH_INIT_HOOK        (TH_INIT_NEED_CONFIG | TH_INIT_FORCE_SETUP)
#define TH_INIT_NONE        ((u32)0x00000000) /* No flags. */
#define TH_INIT_DEFAULT     (TH_INIT_NONE)      /* Default flags. */
#define TH_INIT_MASK        ((u32)0x0000000F) /* All possible init flags. */
#define TH_INIT_FORCE_RESET ((u32)0x00000004) /* Force TH1 commands re-added? */
typedef struct Global Global;
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
#define Th_IsConfigOpen()         (g.zConfigDbName!=0)
#define Th_IsRepositoryOpen()     (g.repositoryOpen)
#define Th_IsLocalOpen()          (g.localOpen)
#define INTERFACE 0
