
#include <stdio.h>

// c.f. ribbit/src/utils.c

int rm_file_c(char* filename)
{
	//printf("filename = \"%s\"\n", filename);
	return remove(filename);
}

