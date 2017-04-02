/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
int count_nonbranch_children(int pid);
#define SYNC_PUSH           0x0001    /* push content client to server */
typedef struct Blob Blob;
void get_checkin_taglist(int rid,Blob *pOut);
#define MFESTFLG_TAGS 0x04
void undo_reset(void);
void vfile_aggregate_checksum_manifest(int vid,Blob *pOut,Blob *pManOut);
void vfile_compare_repository_to_disk(int vid);
int blob_compare(Blob *pA,Blob *pB);
void vfile_aggregate_checksum_repository(int vid,Blob *pOut);
typedef struct Manifest Manifest;
void manifest_destroy(Manifest *p);
typedef struct Stmt Stmt;
int db_reset(Stmt *pStmt);
int db_bind_text(Stmt *pStmt,const char *zParamName,const char *zValue);
void db_lset_int(const char *zName,int value);
#define MFESTFLG_UUID 0x02
int blob_is_reset(Blob *pBlob);
#define MC_PERMIT_HOOKS   1  /*  permit hooks to execute    */
#define MC_NONE           0  /*  default handling           */
int manifest_crosslink(int rid,Blob *pContent,int flags);
#define MFESTFLG_RAW  0x01
int clearsign(Blob *pIn,Blob *pOut);
Manifest *manifest_get_by_name(const char *zName,int *pRid);
#define CFTYPE_MANIFEST   1
Manifest *manifest_get(int rid,int cfType,Blob *pErr);
int content_deltify(int rid,int srcid,int force);
int content_put(Blob *pBlob);
int contains_merge_marker(Blob *p);
void vfile_aggregate_checksum_disk(int vid,Blob *pOut);
void db_end_transaction(int rollbackFlag);
# define TAG_BRANCH     8     /* Value is name of the current branch */
void db_begin_transaction(void);
int unsaved_changes(unsigned int cksigFlags);
void user_select(void);
NORETURN void fossil_exit(int rc);
int db_get_int(const char *zName,int dflt);
#define SYNC_PULL           0x0002    /* pull content server to client */
int autosync_loop(int flags,int nTries,int doPrompt);
int branch_is_open(const char *zBrName);
int db_get_manifest_setting(void);
void *fossil_realloc(void *p,size_t n);
void url_proxy_options(void);
void commit_cmd(void);
int blob_read_link(Blob *pBlob,const char *zFilename);
char *glob_expr(const char *zVal,const char *zGlobList);
void test_commit_warning(void);
void blob_cp1252_to_utf8(Blob *p);
#include <dirent.h>
FILE *fossil_fopen(const char *zName,const char *zMode);
char *file_newname(const char *zBase,const char *zSuffix,int relFlag);
#define LOOK_LONG    ((int)0x00000040) /* An over length line was found. */
#define LOOK_CRLF    ((int)0x00000020) /* One or more CR/LF pairs were found. */
#define LOOK_LONE_CR ((int)0x00000004) /* An unpaired CR char was found. */
#define LOOK_LONE_LF ((int)0x00000010) /* An unpaired LF char was found. */
#define LOOK_EOL     (LOOK_LONE_CR | LOOK_LONE_LF | LOOK_CRLF) /* Line seps. */
#define LOOK_CR      ((int)0x00000002) /* One or more CR chars were found. */
int invalid_utf8(const Blob *pContent);
#define LOOK_NUL     ((int)0x00000001) /* One or more NUL chars were found. */
#define LOOK_SHORT   ((int)0x00000100) /* Unable to perform full check. */
#define LOOK_BINARY  (LOOK_NUL | LOOK_LONG | LOOK_SHORT) /* May be binary. */
int looks_like_utf8(const Blob *pContent,int stopFlags);
int looks_like_utf16(const Blob *pContent,int bReverse,int stopFlags);
int could_be_utf16(const Blob *pContent,int *pbReverse);
int md5sum_blob(const Blob *pIn,Blob *pCksum);
# define TAG_CLOSED     9     /* Do not display this check-in as a leaf */
int is_a_leaf(int rid);
int content_is_private(int rid);
void content_make_public(int rid);
#define PERM_LNK          2     /*  symlink       */
#define PERM_EXE          1     /*  executable    */
int file_wd_perm(const char *zFilename);
typedef struct ManifestFile ManifestFile;
ManifestFile *manifest_file_next(Manifest *p,int *pErr);
void manifest_file_rewind(Manifest *p);
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
#define INTERFACE 0
#define OPEN_ANY_SCHEMA      0x002      /* Do not error if schema is wrong */
void test_date_format(void);
const char *cgi_parameter(const char *zName,const char *zDefault);
#define PD(x,y)     cgi_parameter((x),(y))
char *date_in_standard_format(const char *zInputDate);
int db_exists(const char *zSql,...);
typedef struct Bag Bag;
int bag_next(Bag *p,int e);
int bag_first(Bag *p);
int bag_count(Bag *p);
void *fossil_malloc(size_t n);
int bag_insert(Bag *p,int e);
void bag_clear(Bag *p);
void bag_init(Bag *p);
struct Bag {
  int cnt;   /* Number of integers in the bag */
  int sz;    /* Number of slots in a[] */
  int used;  /* Number of used slots in a[] */
  int *a;    /* Hash table of integers that are in the bag */
};
int select_commit_files(void);
char *info_tags_of_checkin(int rid,int propagatingOnly);
const char *login_name(void);
const unsigned char *get_utf8_bom(int *pnByte);
typedef struct CheckinInfo CheckinInfo;
struct CheckinInfo {
  Blob *pComment;             /* Check-in comment text */
  const char *zMimetype;      /* Mimetype of check-in command.  May be NULL */
  int verifyDate;             /* Verify that child is younger */
  int closeFlag;              /* Close the branch being committed */
  int integrateFlag;          /* Close merged-in branches */
  Blob *pCksum;               /* Repository checksum.  May be 0 */
  const char *zDateOvrd;      /* Date override.  If 0 then use 'now' */
  const char *zUserOvrd;      /* User override.  If 0 then use login_name() */
  const char *zBranch;        /* Branch name.  May be 0 */
  const char *zColor;         /* One-time background color.  May be 0 */
  const char *zBrClr;         /* Persistent branch color.  May be 0 */
  const char **azTag;         /* Tags to apply to this check-in */
};
void blob_resize(Blob *pBlob,unsigned int newSize);
int fossil_isspace(char c);
#define blob_buffer(X)  ((X)->aData)
int blob_line(Blob *pFrom,Blob *pTo);
void blob_to_lf_only(Blob *p);
void blob_to_utf8_no_bom(Blob *pBlob,int useMbcs);
int blob_read_from_file(Blob *pBlob,const char *zFilename);
int fossil_system(const char *zOrigCmd);
char *db_text(const char *zDefault,const char *zSql,...);
#if defined(_WIN32) || defined(__CYGWIN__)
void blob_add_cr(Blob *p);
#endif
void *fossil_utf8_to_path(const char *zUtf8,int isDir);
char *fossil_getenv(const char *zName);
void prompt_for_user_comment(Blob *pComment,Blob *pPrompt);
int file_rmdir(const char *zName);
typedef struct Glob Glob;
int vfile_dir_scan(Blob *pPath,int nPrefix,unsigned scanFlags,Glob *pIgnore1,Glob *pIgnore2);
void undo_finish(void);
int file_delete(const char *zFilename);
const char *undo_save_message(int rc);
int undo_maybe_save(const char *zPathname,i64 limit);
#define UNDO_SAVED_OK (1) /* The specified file was saved succesfully. */
int fossil_toupper(char c);
void fossil_free(void *p);
void prompt_user(const char *zPrompt,Blob *pIn);
#define UNDO_NONE     (0) /* Placeholder only used to initialize vars. */
extern const Blob empty_blob;
int glob_match(Glob *pGlob,const char *zString);
void undo_begin(void);
#define SCAN_NESTED 0x004    /* Scan for empty dirs in nested checkouts */
void undo_capture_command_line(void);
void clean_cmd(void);
#define SCAN_TEMP   0x002    /* Only Fossil-generated files like *-baseline */
void extras_cmd(void);
void db_find_and_open_repository(int bFlags,int nArgUsed);
void ls_cmd(void);
int compute_fileage(int vid,const char *zGlob);
int symbolic_name_to_rid(const char *zTag,const char *zType);
int leaf_ambiguity_warning(int rid,int currentCkout);
int blob_write_to_file(Blob *pBlob,const char *zFilename);
void db_record_repository_filename(const char *zName);
void show_common_info(int rid,const char *zUuidName,int showComment,int showFamily);
const char *db_repository_filename(void);
void fossil_print(const char *zFormat,...);
void glob_free(Glob *pGlob);
Glob *glob_create(const char *zPatternList);
#define CKSIG_HASH      0x002   /* Verify file content using hashing */
void vfile_check_signature(int vid,unsigned int cksigFlags);
#if defined(FOSSIL_ENABLE_TCL)
#include "tcl.h"
#endif
#if defined(FOSSIL_ENABLE_JSON)
#include "cson_amalgamation.h"
#include "json_detail.h"
#endif
void verify_all_options(void);
#define SCAN_ALL    0x001    /* Includes files that begin with "." */
char *db_get(const char *zName,const char *zDefault);
void db_must_be_within_tree(void);
void status_cmd(void);
const char *find_option(const char *zLong,const char *zShort,int hasArg);
int db_get_boolean(const char *zName,int dflt);
int db_finalize(Stmt *pStmt);
void file_relative_name(const char *zOrigName,Blob *pOut,int slash);
void blob_appendf(Blob *pBlob,const char *zFormat,...);
void blob_append(Blob *pBlob,const char *aData,int nData);
int file_contains_merge_marker(const char *zFullpath);
int file_wd_islink(const char *zFilename);
int file_wd_isfile_or_link(const char *zFilename);
char *mprintf(const char *zFormat,...);
int db_column_int(Stmt *pStmt,int N);
const char *db_column_text(Stmt *pStmt,int N);
int db_step(Stmt *pStmt);
int db_lget_int(const char *zName,int dflt);
int db_bind_int(Stmt *pStmt,const char *zParamName,int iValue);
int db_prepare(Stmt *pStmt,const char *zFormat,...);
const char *fossil_all_reserved_names(int omitRepo);
char *blob_sql_text(Blob *p);
void blob_append_sql(Blob *pBlob,const char *zFormat,...);
int fossil_strcmp(const char *zA,const char *zB);
int file_tree_name(const char *zOrigName,Blob *pOut,int absolute,int errFatal);
void blob_zero(Blob *pBlob);
void blobReallocMalloc(Blob *pBlob,unsigned int newSize);
#define BLOB_INITIALIZER  {0,0,0,0,0,blobReallocMalloc}
struct Stmt {
  Blob sql;               /* The SQL for this statement */
  sqlite3_stmt *pStmt;    /* The results of sqlite3_prepare_v2() */
  Stmt *pNext, *pPrev;    /* List of all unfinalized statements */
  int nStep;              /* Number of sqlite3_step() calls */
};
NORETURN void fossil_fatal(const char *zFormat,...);
int file_access(const char *zFilename,int flags);
void fossil_warning(const char *zFormat,...);
int file_wd_isdir(const char *zFilename);
char *blob_str(Blob *p);
void file_canonical_name(const char *zOrigName,Blob *pOut,int slash);
void blob_reset(Blob *pBlob);
#define blob_size(X)  ((X)->nUsed)
void vfile_scan(Blob *pPath,int nPrefix,unsigned scanFlags,Glob *pIgnore1,Glob *pIgnore2);
void blob_init(Blob *pBlob,const char *zData,int size);
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
const char *filename_collation(void);
int db_multi_exec(const char *zSql,...);
struct Glob {
  int nPattern;        /* Number of patterns */
  char **azPattern;    /* Array of pointers to patterns */
};
