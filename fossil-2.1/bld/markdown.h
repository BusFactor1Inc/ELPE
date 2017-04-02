/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
void fossil_free(void *p);
typedef struct Blob Blob;
void blobarray_zero(Blob *aBlob,int n);
void *fossil_malloc(size_t n);
void blob_init(Blob *pBlob,const char *zData,int size);
void blobReallocMalloc(Blob *pBlob,unsigned int newSize);
#define BLOB_INITIALIZER  {0,0,0,0,0,blobReallocMalloc}
int fossil_strnicmp(const char *zA,const char *zB,int nByte);
int blob_compare(Blob *pA,Blob *pB);
#define blob_size(X)  ((X)->nUsed)
#define blob_buffer(X)  ((X)->aData)
void blob_append(Blob *pBlob,const char *aData,int nData);
void blob_reset(Blob *pBlob);
typedef struct mkd_renderer mkd_renderer;
void markdown(struct Blob *ob,struct Blob *ib,const struct mkd_renderer *rndr);
void markdown(struct Blob *ob,struct Blob *ib,const struct mkd_renderer *rndrer);
#define MKD_CELL_HEAD           4
#define MKD_CELL_ALIGN_MASK     3
#define MKD_CELL_ALIGN_CENTER   3  /* LEFT | RIGHT */
#define MKD_CELL_ALIGN_RIGHT    2
#define MKD_CELL_ALIGN_LEFT     1
#define MKD_CELL_ALIGN_DEFAULT  0
#define MKD_LI_BLOCK      2  /* <li> containing block data */
#define MKD_LIST_ORDERED  1
struct Blob {
  unsigned int nUsed;            /* Number of bytes used in aData[] */
  unsigned int nAlloc;           /* Number of bytes allocated for aData[] */
  unsigned int iCursor;          /* Next character of input to parse */
  unsigned int blobFlags;        /* One or more BLOBFLAG_* bits */
  char *aData;                   /* Where the information is stored */
  void (*xRealloc)(Blob*, unsigned int); /* Function to reallocate the buffer */
};
enum mkd_autolink {
  MKDA_NOT_AUTOLINK,    /* used internally when it is not an autolink*/
  MKDA_NORMAL,          /* normal http/http/ftp link */
  MKDA_EXPLICIT_EMAIL,  /* e-mail link with explicit mailto: */
  MKDA_IMPLICIT_EMAIL   /* e-mail link without mailto: */
};
typedef enum mkd_autolink mkd_autolink;
struct mkd_renderer {
  /* document level callbacks */
  void (*prolog)(struct Blob *ob, void *opaque);
  void (*epilog)(struct Blob *ob, void *opaque);

  /* block level callbacks - NULL skips the block */
  void (*blockcode)(struct Blob *ob, struct Blob *text, void *opaque);
  void (*blockquote)(struct Blob *ob, struct Blob *text, void *opaque);
  void (*blockhtml)(struct Blob *ob, struct Blob *text, void *opaque);
  void (*header)(struct Blob *ob, struct Blob *text,
            int level, void *opaque);
  void (*hrule)(struct Blob *ob, void *opaque);
  void (*list)(struct Blob *ob, struct Blob *text, int flags, void *opaque);
  void (*listitem)(struct Blob *ob, struct Blob *text,
            int flags, void *opaque);
  void (*paragraph)(struct Blob *ob, struct Blob *text, void *opaque);
  void (*table)(struct Blob *ob, struct Blob *head_row, struct Blob *rows,
              void *opaque);
  void (*table_cell)(struct Blob *ob, struct Blob *text, int flags,
              void *opaque);
  void (*table_row)(struct Blob *ob, struct Blob *cells, int flags,
              void *opaque);

  /* span level callbacks - NULL or return 0 prints the span verbatim */
  int (*autolink)(struct Blob *ob, struct Blob *link,
          enum mkd_autolink type, void *opaque);
  int (*codespan)(struct Blob *ob, struct Blob *text, void *opaque);
  int (*double_emphasis)(struct Blob *ob, struct Blob *text,
            char c, void *opaque);
  int (*emphasis)(struct Blob *ob, struct Blob *text, char c,void*opaque);
  int (*image)(struct Blob *ob, struct Blob *link, struct Blob *title,
            struct Blob *alt, void *opaque);
  int (*linebreak)(struct Blob *ob, void *opaque);
  int (*link)(struct Blob *ob, struct Blob *link, struct Blob *title,
          struct Blob *content, void *opaque);
  int (*raw_html_tag)(struct Blob *ob, struct Blob *tag, void *opaque);
  int (*triple_emphasis)(struct Blob *ob, struct Blob *text,
            char c, void *opaque);

  /* low level callbacks - NULL copies input directly into the output */
  void (*entity)(struct Blob *ob, struct Blob *entity, void *opaque);
  void (*normal_text)(struct Blob *ob, struct Blob *text, void *opaque);

  /* renderer data */
  int max_work_stack; /* prevent arbitrary deep recursion, cf README */
  const char *emph_chars; /* chars that trigger emphasis rendering */
  void *opaque; /* opaque data send to every rendering callback */
};
#define INTERFACE 0
