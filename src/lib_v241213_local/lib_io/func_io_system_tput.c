// https://www.aihara.co.jp/~junt/program/popen.html
// http://www.ncad.co.jp/~komata/c-kouza3.htm
// https://peng225.hatenablog.com/entry/2021/05/10/183314
#include <stdio.h>
#include <string.h>
#include "func_base.h"
 
//#define MAXLINE 1024

void c_system_tput_cols_( char *str, int *clen, int *stat ){
  FILE *fp;
  int MAXLINE = 1024;

  if( (fp=popen("tput cols", "r")) == NULL ){
    *stat = 1;
    strcpy("", str);
    *clen = 0;
    return;
  }

  *stat = 0;
  fgets(str, MAXLINE, fp);
  c_lntrim_(str);
  *clen = strlen(str);
  return;
}
