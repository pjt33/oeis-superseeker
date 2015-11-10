/*
 *  To compile:  cc -o getfrom -DSTANDALONE getfrom.c
 *  To run:      getfrom < {mailmessage}
 * The author of this software is Eric Grosse. Copyright (c)1985,1994 by AT&T.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

#include <stdio.h>
#include <string.h>

int
unsafe(char *s)
{
	char c;
	/* check for characters interpreted by the shell (by which nefarious
           users might otherwise break into the system) */
	for (c = (*s); c != '\0'; c = (*++s)) {
		if ( strchr("\"'`$\n;&|^<>()\\", c) || (c & 0200) ){
			fprintf(stderr,"unsafe(%s) saw %c\n",s,c);
			return(1);
		}
	}
	return(0);
}


/*
 * EXTRACT SENDER'S ADDRESS OUT OF RFC822 "FROM" LINE
 *  The sender is either the next first whitespace delimited token or
 *  the first thing enclosed in "<" ">".
 *  (leading "From: " is already deleted before entry)
 *  adapted from /n/bowell/src/cmd/upas (Dave Presotto)
 *  modified by Eric Grosse to stop after comma and to allow nested parens
 */
int   /* 0 on success, 1 if address is unparseable or unsafe */
getfrom(char *line, char *sender)
{
	char	*lp, *sp;
	int	comment = 0;
	int	anticomment = 0;

	sp = sender;
	for (lp = line; *lp; lp++) {
		if (comment) {
			if (*lp == ')') {
				comment--;
			} else if (*lp == '(') {
				comment++;
			}
			continue;
		}
		if (anticomment) {
			if (*lp == '>')
				break;
		}
		switch (*lp) {
		case '\t':
		case '\n':
			break;
		case ' ':
			if (strncmp(lp, " at ", sizeof(" at ") - 1) == 0 ||
				strncmp(lp, " AT ", sizeof(" AT ") - 1) == 0 ) {
				*sp++ = '@';
				lp += sizeof(" at ") - 2;
			}
			break;
		case '<':
			anticomment = 1;
			sp = sender;
			break;
		case '(':
			comment++;
			break;
		case ',':  /* looks like multiple address; chop */
			*sp++ = '\0';
		default:
			*sp++ = *lp;
			break;
		}
	}
	*sp = '\0';
	if(!*sender || unsafe(sender)) return 1;
	return 0;
}


#ifdef STANDALONE
#define LINESIZE 1026
char	line[LINESIZE]; /* raw input line */
char	from[LINESIZE]; /* unfolded from line */
char	address[LINESIZE]; /* return address */

/* like gets(), but safe */
char *
getsn(s,m)
	char *s;
	int m;
{
	if(fgets(s,m,stdin)==NULL)
		return(NULL);
	m = strlen(s)-1;
	if(s[m]=='\n')
		s[m] = '\0';
	return(s);
}

/* case independent prefix compare */
int
cicmp(char *s1, char *s2)
{
        int c1, c2;

        for(; *s1; s1++, s2++){
                c1 = *s1;
                c2 = isupper(*s2) ? tolower(*s2) : *s2;
                if (c1 != c2)
                        return -1;
        }
	return(0);
}

void
main(int argc, char**argv)
{
	int	inside = 0, replyto = 0;
	char	c, *p;

	while(getsn(line, LINESIZE) != NULL){
		c = line[0];
		if(inside){ /* unfolding */
			if( c==' ' || c=='\t' ){
				if( strlen(line) + strlen(from) >= LINESIZE ){
					fprintf(stderr,"From line too long\n");
					exit(1);
				}
				strcat(from, line);
				continue;
			}else{ /* not a continuation */
				inside = 0;
			}
		}
		if ( c == '\0' ) {
			break;	/* end of mail header */
		}else if(!replyto && strncmp("From ",line,5)==0){
			strcpy(from, line + 5);
			if(p = strchr(from,' ')) *p = '\0';  /* chop date */
		}else if(strlen(line)>9 && cicmp("reply-to:",line)==0){
			strcpy(from, line + 9);
			inside = 1;
			replyto = 1;
		}else if(!replyto && strlen(line)>5 && cicmp("from:",line)==0){
			strcpy(from, line + 5);
			inside = 1;
		}
	}
	if(getfrom(from, address))
		exit(1);
	puts(address);
	exit(0);
}
#endif
