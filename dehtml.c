#include <stdio.h>

/* dehtml:
 *
 * do the following processing on stdin:
 *
 *  <.*>  => ""
 *  &gt;  => '>'
 *  &lt;  => '<'
 *  &nbsp;=> ' '
 *  etc etc etc
 */

#define MAX_AMPR_LEN (7)

int main(void)
{
int c;
char str[MAX_AMPR_LEN+1];
int i;
str[MAX_AMPR_LEN]=0;

while ((c=getchar())!=EOF) {
  switch(c) {
  case '<':
    while ((c=getchar())!=EOF) {
      if (c=='>') break;}
    break;
  case '&':
    for (i=0;i<MAX_AMPR_LEN;i++) {
      if ((c=getchar())==EOF) c=0;
      str[i]=c;
      if (c==';') {
        str[i+1]=0;
        break;}
      if (c==0) break;}
    if (c!=';') {
      printf("&%s",str);}
    if (!strcmp(str,"gt;")) {
      printf(">");}
    else if (!strcmp(str,"lt;")) {
      printf("<");}
    else if (!strcmp(str,"nbsp;")) {
      printf(" ");}
    else if (!strcmp(str,"amp;")) {
      printf("&");}
    else if (!strcmp(str,"eacute;")) {
      printf("e");}
    else if (!strcmp(str,"egrave;")) {
      printf("e");}
    else if (!strcmp(str,"uacute;")) {
      printf("u");}
    else if (!strcmp(str,"ugrave;")) {
      printf("u");}
    else if (!strcmp(str,"Uacute;")) {
      printf("U");}
    else if (!strcmp(str,"Ugrave;")) {
      printf("U");}
    else if (!strcmp(str,"iacute;")) {
      printf("i");}
    else if (!strcmp(str,"iuml;")) {
      printf("i");}
    else if (!strcmp(str,"igrave;")) {
      printf("i");}
    else if (!strcmp(str,"Iacute;")) {
      printf("I");}
    else if (!strcmp(str,"Igrave;")) {
      printf("I");}
    else if (!strcmp(str,"aacute;")) {
      printf("a");}
    else if (!strcmp(str,"agrave;")) {
      printf("a");}
    else if (!strcmp(str,"oacute;")) {
      printf("o");}
    else if (!strcmp(str,"ograve;")) {
      printf("o");}
    else if (!strcmp(str,"ouml;")) {
      printf("oe");}
    else if (!strcmp(str,"Eacute;")) {
      printf("E");}
    else if (!strcmp(str,"Egrave;")) {
      printf("E");}
    else if (!strcmp(str,"Aacute;")) {
      printf("A");}
    else if (!strcmp(str,"Agrave;")) {
      printf("A");}
    else if (!strcmp(str,"Auml;")) {
      printf("Ae");}
    else if (!strcmp(str,"Oacute;")) {
      printf("O");}
    else if (!strcmp(str,"Ograve;")) {
      printf("O");}
    else if (!strcmp(str,"Ouml;")) {
      printf("Oe");}
    else if (!strcmp(str,"Uuml;")) {
      printf("Ue");}
    else if (!strcmp(str,"quot;")) {
      printf("\"");}
    else {
      printf("&%s",str);}
    break;
  default:
    putchar(c);
    break;}}
if (c!='\n') printf("\n");
return(0);
}
