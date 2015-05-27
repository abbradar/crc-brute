#include "crc-brute.h"

static const unsigned long long checkEach = 1000000000;

static volatile int crcTableFilled = 0;
static uint32_t crcTable[4][256];

// "borrowed" from zlib's crc32.c
void makeCrcTable() {
  if (crcTableFilled) return;

  static const int p[] = {0,1,2,4,5,7,8,10,11,12,16,22,23,26};

  uint32_t poly = 0;
  for (int i = 0; i < sizeof(p) / sizeof(int); ++i) {
    poly |= 1 << (31 - p[i]);
  }

  for (int i = 0; i < 256; ++i) {
    uint32_t c = i;
    for (int j = 0; j < 8; ++j) {
      c = c & 1 ? poly ^ (c >> 1) : c >> 1;
    }
    crcTable[0][i] = c;
  }

  for (int i = 0; i < 256; ++i) {
    uint32_t c = crcTable[0][i];
    for (int j = 1; j < 4; ++j) {
      c = crcTable[0][c & 0xff] ^ (c >> 8);
      crcTable[j][i] = c;
    }
  }

  crcTableFilled = 1;
}

// little-endian only
inline static uint32_t fcrc32(const uint32_t crc, const utf8_c str, int length) {
  uint32_t c = crc ^ str.i4;
  switch (length) {
  case 1: ;
    return crcTable[0][c & 0xff] ^ (crc >> 8);
  case 2: ;
    return crcTable[1][c & 0xff] ^ crcTable[0][(c >> 8) & 0xff] ^ (crc >> 16);
  case 3: ;
    return crcTable[2][c & 0xff] ^ crcTable[1][(c >> 8) & 0xff] ^
      crcTable[0][(c >> 16) & 0xff] ^ (crc >> 24);
  case 4: ;
    return crcTable[3][c & 0xff] ^ crcTable[2][(c >> 8) & 0xff] ^
      crcTable[1][(c >> 16) & 0xff] ^ crcTable[0][c >> 24];
  }
}

int crcBruteUtf8( const utf8_c** tables, const int* widths, const int length, const int* ranges
                , uint32_t target
                , int (*periodic)(unsigned long long), int (*check)()
                , utf8_c* str
                ) {
  makeCrcTable();

  int code[length];
  for (int i = 0; i < length; ++i) {
    code[i] = 0;
    str[i] = tables[i][0];
  }

  uint32_t partialCrc[length + 1];
  partialCrc[0] = ~0;
  for (int i = 1; i <= length; ++i) {
    partialCrc[i] = fcrc32(partialCrc[i - 1], str[i - 1], widths[i - 1]);
  }
  
  unsigned long long num = 0;
  target = ~target;
  while (1) {
    if (++num % checkEach == 0) {
      if (!periodic(num)) return 0;
    }

    if (partialCrc[length] == target) {
      if (check()) return 1;
    }

    int i = length - 1;
    for (; i >= 0; --i) {
      if (code[i] != ranges[i] - 1) {
        ++code[i];
        str[i] = tables[i][code[i]];
        for (int j = i + 1; j <= length; ++j) {
          partialCrc[j] = fcrc32(partialCrc[j - 1], str[j - 1], widths[j - 1]);
        }
        break;
      } else {
        code[i] = 0;
        str[i] = tables[i][0];
      }
    }
    if (i < 0) return 0;
  }
}
