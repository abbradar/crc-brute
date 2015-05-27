#ifndef CRC_BRUTE_H
#define CRC_BRUTE_H

#include <stdint.h>

typedef union utf8_c {
  uint8_t c[4];
  uint8_t i1;
  uint16_t i2;
  uint32_t i4;
} utf8_c;

void makeCrcTable();

int crcBruteUtf8( const utf8_c** tables, const int* widths, const int length, const int* ranges
                , uint32_t target
                , int (*periodic)(unsigned long long), int (*check)()
                , utf8_c* str
                );

#endif
