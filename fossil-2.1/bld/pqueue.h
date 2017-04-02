/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
typedef struct PQueue PQueue;
int pqueuex_extract(PQueue *p,void **pp);
void pqueuex_insert(PQueue *p,int e,double v,void *pData);
void *fossil_realloc(void *p,size_t n);
void pqueuex_clear(PQueue *p);
void pqueuex_init(PQueue *p);
struct PQueue {
  int cnt;   /* Number of entries in the queue */
  int sz;    /* Number of slots in a[] */
  struct QueueElement {
    int id;          /* ID of the element */
    void *p;         /* Content pointer */
    double value;    /* Value of element.  Kept in ascending order */
  } *a;
};
#define INTERFACE 0
