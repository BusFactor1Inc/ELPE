/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dirent.h>
int file_delete(const char *zFilename);
typedef struct Blob Blob;
void blob_copy(Blob *pTo,Blob *pFrom);
int blob_read_from_file(Blob *pBlob,const char *zFilename);
void blob_zero(Blob *pBlob);
void blob_reset(Blob *pBlob);
int fossil_system(const char *zOrigCmd);
int blob_write_to_file(Blob *pBlob,const char *zFilename);
char *mprintf(const char *zFormat,...);
char *db_text(const char *zDefault,const char *zSql,...);
int is_false(const char *zVal);
char *db_get(const char *zName,const char *zDefault);
struct Blob {
  unsigned int nUsed;            /* Number of bytes used in aData[] */
  unsigned int nAlloc;           /* Number of bytes allocated for aData[] */
  unsigned int iCursor;          /* Next character of input to parse */
  unsigned int blobFlags;        /* One or more BLOBFLAG_* bits */
  char *aData;                   /* Where the information is stored */
  void (*xRealloc)(Blob*, unsigned int); /* Function to reallocate the buffer */
};
int clearsign(Blob *pIn,Blob *pOut);
