#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char* input( ) {

	char *str = malloc(100);
	fgets(str, 100, stdin);
	return str;

}



char* lower(char *s) {

	char *retstr = strdup(s);
	int i = 0;

	while(retstr[i]) {  
	  retstr[i] = tolower(retstr[i]);
	  i++;
	}
	return (retstr);
}



int scompare(char *s1, char *s2) {
	
	s1[strcspn(s1, "\n")] = '\0';
        int retval = strcmp(lower(s1), lower(s2));
        return retval;

}
