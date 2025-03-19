#include <stdio.h>
#include <string.h>

/* Remove new line code */
void c_lntrim_(char *str) {
  char *p;
  p = strchr(str, '\n');
  if(p != NULL) {
    *p = '\0';
  }
}
