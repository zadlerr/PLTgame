#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* input( ) {

	char *str = malloc(100);
	fgets(str, 100, stdin);
	return str;

}



int intcompare(char *s1) {

        int retval = strcmp(s1, "1");
        return retval;

}



int scompare(char *s1, char *s2) {

        int retval = strcmp(s1, s2);
        return retval;

}
