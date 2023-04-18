#include "s7.h"
#include "libs7.h"

void fs_api_init(s7_scheme *sc);

s7_scheme *libs7_init(void)
{
  s7_scheme *s7 = s7_init();
  fs_api_init(s7);
  return s7;
}
