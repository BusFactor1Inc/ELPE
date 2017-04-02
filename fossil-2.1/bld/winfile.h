/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
void fossil_path_free(void *pOld);
void fossil_free(void *p);
char *fossil_path_to_utf8(const void *zPath);
NORETURN void fossil_fatal(const char *zFormat,...);
void *fossil_malloc(size_t n);
#if defined(_WIN32)
void win32_getcwd(char *zBuf,int nBuf);
int win32_chdir(const wchar_t *zChDir,int bChroot);
int win32_access(const wchar_t *zFilename,int flags);
#endif
#if defined(_WIN32) && (defined(__MSVCRT__) || defined(_MSC_VER))
typedef struct fossilStat fossilStat;
#endif
#include <dirent.h>
#if defined(_WIN32) && (defined(__MSVCRT__) || defined(_MSC_VER))
struct fossilStat {
    i64 st_size;
    i64 st_mtime;
    int st_mode;
};
#endif
#if defined(_WIN32)
int win32_stat(const wchar_t *zFilename,struct fossilStat *buf,int isWd);
#endif
