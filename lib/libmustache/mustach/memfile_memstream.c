/* HAVE_OPEN_MEMSTREAM */

#include <stdio.h>
#include <stdlib.h>

#include "mustach.h"
#include "memfile.h"

FILE *memfile_open(char **buffer, size_t *size)
{
	return open_memstream(buffer, size);
}

void memfile_abort(FILE *file, char **buffer, size_t *size)
{
	fclose(file);
	free(*buffer);
	*buffer = NULL;
	*size = 0;
}

int memfile_close(FILE *file, char **buffer, size_t *size)
{
	int rc;

	/* adds terminating null */
	rc = fputc(0, file) ? MUSTACH_ERROR_SYSTEM : 0;
	fclose(file);
	if (rc == 0)
		/* removes terminating null of the length */
		(*size)--;
	else {
		free(*buffer);
		*buffer = NULL;
		*size = 0;
	}
	return rc;
}
