/* !HAVE_OPEN_MEMSTREAM */

#include <stdio.h>
#include <stdlib.h>
#include <sys/errno.h>

#include "memfile.h"
#include "mustach.h"

FILE *memfile_open(char **buffer, size_t *size)
{
	/*
	 * We can't provide *buffer and *size as open_memstream does but
	 * at least clear them so the caller won't get bad data.
	 */
	*buffer = NULL;
	*size = 0;

	return tmpfile();
}

void memfile_abort(FILE *file, char **buffer, size_t *size)
{
	fclose(file);
	*buffer = NULL;
	*size = 0;
}

int memfile_close(FILE *file, char **buffer, size_t *size)
{
	int rc;
	size_t s;
	char *b;

	s = (size_t)ftell(file);
	b = malloc(s + 1);
	if (b == NULL) {
		rc = MUSTACH_ERROR_SYSTEM;
		errno = ENOMEM;
		s = 0;
	} else {
		rewind(file);
		if (1 == fread(b, s, 1, file)) {
			rc = 0;
			b[s] = 0;
		} else {
			rc = MUSTACH_ERROR_SYSTEM;
			free(b);
			b = NULL;
			s = 0;
		}
	}
	*buffer = b;
	*size = s;
	return rc;
}
