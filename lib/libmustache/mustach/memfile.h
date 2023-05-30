#ifndef _MEMFILE_H_
#define _MEMFILE_H_

FILE *memfile_open(char **buffer, size_t *size);

void memfile_abort(FILE *file, char **buffer, size_t *size);

int memfile_close(FILE *file, char **buffer, size_t *size);

#endif
