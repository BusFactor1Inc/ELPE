/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
typedef struct Blob Blob;
void blob_reset(Blob *pBlob);
#define blob_size(X)  ((X)->nUsed)
#define blob_buffer(X)  ((X)->aData)
void blob_to_utf8_no_bom(Blob *pBlob,int useMbcs);
void blob_init(Blob *pBlob,const char *zData,int size);
struct Blob {
  unsigned int nUsed;            /* Number of bytes used in aData[] */
  unsigned int nAlloc;           /* Number of bytes allocated for aData[] */
  unsigned int iCursor;          /* Next character of input to parse */
  unsigned int blobFlags;        /* One or more BLOBFLAG_* bits */
  char *aData;                   /* Where the information is stored */
  void (*xRealloc)(Blob*, unsigned int); /* Function to reallocate the buffer */
};
int fossil_utf8_to_console(const char *zUtf8,int nByte,int toStdErr);
void fossil_path_free(void *pOld);
int fossil_isalpha(char c);
void *fossil_utf8_to_path(const char *zUtf8,int isDir);
char *fossil_path_to_utf8(const void *zPath);
void fossil_free(void *p);
void fossil_unicode_free(void *pOld);
void *fossil_utf8_to_unicode(const char *zUtf8);
typedef struct Stmt Stmt;
int db_reset(Stmt *pStmt);
const char *db_column_text(Stmt *pStmt,int N);
char *fossil_strdup(const char *zOrig);
int db_step(Stmt *pStmt);
int db_bind_text16(Stmt *pStmt,const char *zParamName,const char *zValue);
int db_static_prepare(Stmt *pStmt,const char *zFormat,...);
struct Stmt {
  Blob sql;               /* The SQL for this statement */
  sqlite3_stmt *pStmt;    /* The results of sqlite3_prepare_v2() */
  Stmt *pNext, *pPrev;    /* List of all unfinalized statements */
  int nStep;              /* Number of sqlite3_step() calls */
};
void *fossil_malloc(size_t n);
char *fossil_unicode_to_utf8(const void *zUnicode);
#if defined(_WIN32) || defined(__CYGWIN__)
void fossil_mbcs_free(char *zOld);
char *fossil_mbcs_to_utf8(const char *zMbcs);
#endif
