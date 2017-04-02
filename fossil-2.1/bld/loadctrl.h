/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
void cgi_reply(void);
void cgi_set_status(int iStat,const char *zStat);
void style_footer(void);
void cgi_printf(const char *zFormat,...);
void style_header(const char *zTitleFormat,...);
char *db_get(const char *zName,const char *zDefault);
void load_control(void);
void fossil_print(const char *zFormat,...);
void loadavg_test_cmd(void);
double load_average(void);
