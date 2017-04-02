/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
int count_nonbranch_children(int pid);
typedef struct GraphContext GraphContext;
void graph_finish(GraphContext *p,int omitDescenders);
int graph_add_row(GraphContext *p,int rid,int nParent,int *aParent,const char *zBranch,const char *zBgClr,const char *zUuid,int isLeaf);
char *mprintf(const char *zFormat,...);
void *fossil_realloc(void *p,size_t n);
int fossil_strcmp(const char *zA,const char *zB);
void graph_free(GraphContext *p);
GraphContext *graph_init(void);
void *fossil_malloc(size_t n);
void *safeMalloc(int nByte);
typedef struct GraphRow GraphRow;
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
#define INTERFACE 0
