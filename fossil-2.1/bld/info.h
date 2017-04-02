/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
typedef struct Blob Blob;
int blob_read_from_file(Blob *pBlob,const char *zFilename);
void user_select(void);
#if defined(FOSSIL_ENABLE_TCL)
#include "tcl.h"
#endif
#if defined(FOSSIL_ENABLE_JSON)
#include "cson_amalgamation.h"
#include "json_detail.h"
#endif
const char **find_repeatable_option(const char *zLong,const char *zShort,int *pnUsedArgs);
void usage(const char *zFormat);
void ci_amend_cmd(void);
void prompt_for_user_comment(Blob *pComment,Blob *pPrompt);
const unsigned char *get_utf8_bom(int *pnByte);
# define TAG_HIDDEN     5     /* Do not display in timeline */
# define TAG_CLOSED     9     /* Do not display this check-in as a leaf */
int fossil_strncmp(const char *zA,const char *zB,int nByte);
void login_insert_csrf_secret(void);
void form_begin(const char *zOtherArgs,const char *zAction,...);
char *date_in_standard_format(const char *zInputDate);
void login_verify_csrf_secret(void);
# define TAG_BGCOLOR    1     /* Set the background color for display */
char *cgi_parameter_trimmed(const char *zName,const char *zDefault);
#define PT(x)       cgi_parameter_trimmed((x),0)
#define PDT(x,y)    cgi_parameter_trimmed((x),(y))
void ci_edit_page(void);
int is_datetime(const char *zDate);
void db_end_transaction(int rollbackFlag);
int blob_is_reset(Blob *pBlob);
#define MC_PERMIT_HOOKS   1  /*  permit hooks to execute    */
int manifest_crosslink(int rid,Blob *pContent,int flags);
int content_put(Blob *pBlob);
int content_is_private(int rid);
void db_begin_transaction(void);
int md5sum_blob(const Blob *pIn,Blob *pCksum);
const char *login_name(void);
int fossil_isspace(char c);
void render_color_chooser(int fPropagate,const char *zDefaultColor,const char *zIdPropagate,const char *zId,const char *zIdCustom);
void ainfo_page(void);
void event_page(void);
void tktview_page(void);
int name_to_uuid(Blob *pName,int iErrPriority,const char *zType);
void ambiguous_page(void);
void cgi_set_parameter(const char *zName,const char *zValue);
int name_collisions(const char *zName);
void blob_set(Blob *pBlob,const char *zStr);
void info_page(void);
typedef struct Manifest Manifest;
void ticket_output_change_artifact(Manifest *pTkt,const char *zListType);
int db_table_has_column(const char *zDb,const char *zTable,const char *zColumn);
#define CFTYPE_TICKET     5
void tinfo_page(void);
const char *mimetype_from_content(Blob *pBlob);
void blob_to_utf8_no_bom(Blob *pBlob,int useMbcs);
void style_submenu_checkbox(const char *zName,const char *zLabel,int isDisabled);
typedef struct HQuery HQuery;
char *url_render(HQuery *p,const char *zName1,const char *zValue1,const char *zName2,const char *zValue2);
void url_initialize(HQuery *p,const char *zBase);
void artifact_page(void);
char *htmlize(const char *zIn,int n);
void cgi_append_content(const char *zData,int nAmt);
int fossil_isdigit(char c);
void output_text_with_line_numbers(const char *z,const char *zLn);
void url_add_parameter(HQuery *p,const char *zName,const char *zValue);
int name_to_typed_rid(const char *zName,const char *zType);
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
void hexdump_page(void);
#define blob_buffer(X)  ((X)->aData)
void cgi_set_content(Blob *pNewContent);
const char *mimetype_from_name(const char *zName);
int login_is_nobody(void);
int artifact_from_ci_and_filename(HQuery *pUrl);
void rawartifact_page(void);
void cgi_set_content_type(const char *zType);
Blob *cgi_output_blob(void);
NORETURN void fossil_redirect_home(void);
void diff_page(void);
# define TAG_CLUSTER    7     /* A cluster */
int validate16(const char *zIn,int nIn);
#define UUID_SIZE 40
void hyperlink_to_event_tagid(int tagid);
#define blob_size(X)  ((X)->nUsed)
char *fossil_strdup(const char *zOrig);
# define TAG_BRANCH     8     /* Value is name of the current branch */
typedef unsigned int u32;
int object_description(int rid,u32 objdescFlags,Blob *pDownloadName);
#define OBJDESC_DETAIL      0x0001   /* more detail */
#define OBJTYPE_EXE        0x0100
#define OBJTYPE_SYMLINK    0x0080
#define OBJTYPE_TAG        0x0040
#define OBJTYPE_EVENT      0x0020
#define OBJTYPE_ATTACHMENT 0x0010
#define OBJTYPE_TICKET     0x0008
#define OBJTYPE_WIKI       0x0004
#define OBJTYPE_CONTENT    0x0002
#define OBJTYPE_CHECKIN    0x0001
#define INTERFACE 0
typedef struct ManifestFile ManifestFile;
int manifest_file_mperm(ManifestFile *pFile);
ManifestFile *manifest_file_next(Manifest *p,int *pErr);
void manifest_file_rewind(Manifest *p);
void cgi_replace_parameter(const char *zName,const char *zValue);
struct ManifestFile {
  char *zName;           /* Name of a file */
  char *zUuid;           /* Artifact hash for the file */
  char *zPerm;           /* File permissions */
  char *zPrior;          /* Prior name if the name was changed */
};
void vdiff_page(void);
void tag_private_status(int rid);
void blob_appendf(Blob *pBlob,const char *zFormat,...);
void wiki_convert(Blob *pIn,Blob *pOut,int flags);
typedef struct Stmt Stmt;
void db_column_blob(Stmt *pStmt,int N,Blob *pBlob);
#define WIKI_NOBLOCK        0x004  /* No block markup of any kind */
#define WIKI_NOBADLINKS     0x010  /* Ignore broken hyperlinks */
#define WIKI_INLINE         0x002  /* Do not surround with <p>..</p> */
#define CFTYPE_MANIFEST   1
int is_a_version(int rid);
char *vmprintf(const char *zFormat,va_list ap);
void webpage_error(const char *zFormat,...);
void manifest_destroy(Manifest *p);
void wiki_render_by_mimetype(Blob *pWiki,const char *zMimetype);
void blob_init(Blob *pBlob,const char *zData,int size);
int moderation_pending(int rid);
void style_submenu_element(const char *zLabel,const char *zLink,...);
void moderation_approve(int rid);
NORETURN void cgi_redirectf(const char *zFormat,...);
int db_exists(const char *zSql,...);
void moderation_disapprove(int objid);
#define CFTYPE_WIKI       4
Manifest *manifest_get(int rid,int cfType,Blob *pErr);
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
void winfo_page(void);
char *xhref(const char *zExtra,const char *zFormat,...);
int db_get_boolean(const char *zName,int dflt);
void fossil_free(void *p);
char *mprintf(const char *zFormat,...);
int blob_trim(Blob *p);
void hyperlink_to_user(const char *zU,const char *zD,const char *zSuf);
const char *hname_alg(int nHash);
# define TAG_COMMENT    2     /* The check-in comment */
# define TAG_USER       3     /* User who made a checking */
void login_anonymous_available(void);
int db_column_bytes(Stmt *pStmt,int N);
int is_false(const char *zVal);
int is_a_leaf(int rid);
typedef struct ReCompiled ReCompiled;
const char *re_compile(ReCompiled **ppRe,const char *zIn,int noCase);
void style_footer(void);
void style_header(const char *zTitleFormat,...);
int name_to_rid_www(const char *zParamName);
void login_needed(int anonOk);
void login_check_credentials(void);
void ci_page(void);
#define DIFF_STRIP_EOLCR  (((u64)0x10)<<32) /* Strip trailing CR */
#define DIFF_NOOPT        (((u64)0x01)<<32) /* Suppress optimizations (debug) */
#define DIFF_IGNORE_ALLWS ((u64)0x03000000) /* Ignore all whitespace */
const char *cgi_parameter(const char *zName,const char *zDefault);
#define P(x)        cgi_parameter((x),0)
#define DIFF_WIDTH_MASK   ((u64)0x00ff0000) /* side-by-side column width */
#define DIFF_CONTEXT_MASK ((u64)0x0000ffff) /* Lines of context. Default if 0 */
#define PD(x,y)     cgi_parameter((x),(y))
u64 construct_diff_flags(int verboseFlag,int sideBySide);
void append_diff_javascript(int sideBySide);
char *href(const char *zFormat,...);
#define PERM_LNK          2     /*  symlink       */
#define PERM_EXE          1     /*  executable    */
void blob_reset(Blob *pBlob);
#define DIFF_LINENO       ((u64)0x40000000) /* Show line numbers */
char *blob_str(Blob *p);
#define DIFF_NOTTOOBIG    (((u64)0x08)<<32) /* Only display if not too big */
#define DIFF_HTML         ((u64)0x20000000) /* Render for HTML */
int *text_diff(Blob *pA_Blob,Blob *pB_Blob,Blob *pOut,ReCompiled *pRe,u64 diffFlags);
#define DIFF_SIDEBYSIDE   ((u64)0x04000000) /* Generate a side-by-side diff */
int content_get(int rid,Blob *pBlob);
int uuid_to_rid(const char *zUuid,int phantomize);
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
#define TIMELINE_GRAPH    0x0008  /* Compute a graph */
#define TIMELINE_DISJOINT 0x0010  /* Elements are not contiguous */
void www_print_timeline(Stmt *pQuery,int tmFlags,const char *zThisUser,const char *zThisTag,int selectedRid,void(*xExtra)(int));
char *blob_sql_text(Blob *p);
void blob_append_sql(Blob *pBlob,const char *zFormat,...);
int db_multi_exec(const char *zSql,...);
const char *timeline_query_for_www(void);
void blob_append(Blob *pBlob,const char *aData,int nData);
void blob_zero(Blob *pBlob);
void render_checkin_context(int rid,int parentsOnly);
void hyperlink_to_date(const char *zDate,const char *zSuffix);
void hyperlink_to_uuid(const char *zUuid);
void cgi_printf(const char *zFormat,...);
NORETURN void fossil_fatal(const char *zFormat,...);
int name_to_rid(const char *zName);
int db_int(int iDflt,const char *zSql,...);
int db_lget_int(const char *zName,int dflt);
const char *db_repository_filename(void);
void verify_all_options(void);
void db_find_and_open_repository(int bFlags,int nArgUsed);
void db_record_repository_filename(const char *zName);
void db_open_repository(const char *zDbName);
int db_open_config(int useAttach,int isOptional);
#include <dirent.h>
i64 file_size(const char *zFilename);
const char *find_option(const char *zLong,const char *zShort,int hasArg);
void info_cmd(void);
char *db_get(const char *zName,const char *zDefault);
int fossil_strcmp(const char *zA,const char *zB);
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
int comment_print(const char *zText,const char *zOrigText,int indent,int width,int flags);
int db_finalize(Stmt *pStmt);
int db_column_int(Stmt *pStmt,int N);
const char *db_column_text(Stmt *pStmt,int N);
int db_step(Stmt *pStmt);
int db_prepare(Stmt *pStmt,const char *zFormat,...);
void fossil_print(const char *zFormat,...);
struct Stmt {
  Blob sql;               /* The SQL for this statement */
  sqlite3_stmt *pStmt;    /* The results of sqlite3_prepare_v2() */
  Stmt *pNext, *pPrev;    /* List of all unfinalized statements */
  int nStep;              /* Number of sqlite3_step() calls */
};
void show_common_info(int rid,const char *zUuidName,int showComment,int showFamily);
char *db_text(const char *zDefault,const char *zSql,...);
char *info_tags_of_checkin(int rid,int propagatingOnly);
