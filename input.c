#include <stdio.h>
#include <stdlib.h>

char* input( ) {

	char *str = malloc(100);
	
	fgets(str, 100, stdin);

	return str;

}
