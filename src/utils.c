
#include <stdio.h>

// c.f. ribbit/src/utils.c

int rm_file_c_(char* filename)
{
	//printf("filename = \"%s\"\n", filename);
	return remove(filename);
}

int make_dir_(char* dir)
{
	return mkdir(dir, 0755);
}

