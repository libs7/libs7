#ifndef TEST_COMMON_H
#define TEST_COMMON_H

#include "gopt.h"

void print_usage(char *test);

void set_options(char *test, struct option options[]);

void print_debug_env(void);

s7_scheme *initialize(char *test, int argc, char **argv);

#endif // TEST_COMMON_H
