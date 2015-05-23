#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <smmintrin.h>
#include <time.h>

inline uint32_t crc32Utf8_16(const uint32_t crc, uint16_t word) {
  if (word && 0xff00) {
    return _mm_crc32_u16(crc, word >> 8 | word << 8);
  } else {
    return _mm_crc32_u8(crc, word);
  }
}

static const unsigned long long checkEach = 1000000000;

int crcBruteUtf8_16(const int* assigns, const int length, const uint16_t** tables, const int* ranges, const uint32_t target, int* stop, int (*callback)(), uint16_t* str) {
  int code[length];
  memset(code, 0, length * sizeof(int));
  for (int i = 0; i < length; ++i) {
    str[i] = tables[assigns[i]][0];
  }

  uint32_t partialCrc[length + 1];
  partialCrc[0] = 0;
  for (int i = 1; i <= length; ++i) {
    partialCrc[i] = crc32Utf8_16(partialCrc[i - 1], str[i - 1]);
  }

  unsigned long long num = 0;
  struct timespec startTime;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &startTime);

  while (1) {
    if (++num == checkEach) {
      struct timespec stopTime;
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &stopTime);
      double time = (double)(stopTime.tv_sec - startTime.tv_sec) * 1000
        + (double)(stopTime.tv_nsec - startTime.tv_nsec) / 1000000;
      printf("processed %llu hashes in %f ms\n", checkEach, time);
      if (*stop) return 0;
      num = 0;
      startTime = stopTime;
    }

    if (partialCrc[length] == target) {
      if (callback()) return 1;
    }

    int all = 1;
    for (int i = length - 1; i >= 0; --i) {
      if (code[i] != ranges[assigns[i]] - 1) {
        ++code[i];
        str[i + 1] = tables[assigns[i]][code[i]];
        for (int j = i + 1; j <= length; ++j) {
          partialCrc[j] = crc32Utf8_16(partialCrc[j - 1], str[j - 1]);
        }
        all = 0;
        break;
      } else {
        code[i] = 0;
        str[i + 1] = tables[assigns[i]][0];
      }
    }
    if (all) return 0;
  }
}
