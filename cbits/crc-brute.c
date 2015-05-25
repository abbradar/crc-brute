#include <zlib.h>
#include "crc-brute.h"

static const unsigned long long checkEach = 1000000000;

int crcBruteUtf8( const utf8_c** tables, const int* widths, const int length, const int* ranges
                , const uint32_t target
                , int (*periodic)(unsigned long long), int (*check)()
                , utf8_c* str
                ) {
  int code[length];
  for (int i = 0; i < length; ++i) {
    code[i] = 0;
    str[i] = tables[i][0];
  }

  uint32_t partialCrc[length + 1];
  partialCrc[0] = crc32(0, Z_NULL, 0);
  for (int i = 1; i <= length; ++i) {
    partialCrc[i] = crc32(partialCrc[i - 1], (Bytef *)(str + i - 1), widths[i - 1]);
  }
  
  unsigned long long num = 0;
  while (1) {
    if (++num == checkEach) {
      if (!periodic(num)) return 0;
      num = 0;
    }

    if (partialCrc[length] == target) {
      if (check()) return 1;
    }

    int all = 1;
    for (int i = length - 1; i >= 0; --i) {
      if (code[i] != ranges[i] - 1) {
        ++code[i];
        str[i] = tables[i][code[i]];
        for (int j = i + 1; j <= length; ++j) {
          partialCrc[j] = crc32(partialCrc[j - 1], (Bytef *)(str + j - 1), widths[j - 1]);
        }
        all = 0;
        break;
      } else {
        code[i] = 0;
        str[i] = tables[i][0];
      }
    }
    if (all) return 0;
  }
}
