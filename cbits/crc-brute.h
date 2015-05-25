#include <stdint.h>

typedef struct utf8_c {
  char c[4];
} utf8_c;

int crcBruteUtf8( const utf8_c** tables, const int* widths, const int length, const int* ranges
                , const uint32_t target
                , int (*periodic)(unsigned long long), int (*check)()
                , utf8_c* str
                );
