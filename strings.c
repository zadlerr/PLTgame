#include <stdio.h>
#include <string.h>

int comparestrings(string *s1, string *s2) {

	int retval = strcmp(*s1, *s2);
	return retval;

}
