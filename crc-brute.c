#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include <zlib.h>
#include <openssl/sha.h>

/* known things:
crc32 hash, sha1 hash
13 russian letters, 3 ascii symbols
utf8 encoding
*/

// 13*2+3 = 29
const int from = 29;
const int to = 29;
// see UTF-8 and ASCII character tables for explanation
const char range[] = "\x20\x2f" "\x3a\x40" "\x5b\x60" "\x7b\x7e" "\xd0\xd1" "\x90\xbf" "\x80\x0f";
const long long unsigned int reportEach = 1000000000;

const uLong targetCrc = 0xfb610011;
const unsigned char targetShaHex[] = "35bfe051be8b10feb2c92f4daed7d2f2e387bb68";

int main() {
  unsigned char targetSha[SHA_DIGEST_LENGTH];
  assert(sizeof(targetShaHex) == 2 * SHA_DIGEST_LENGTH + 1);
  for (int i = 0; i < SHA_DIGEST_LENGTH; ++i) {
    sscanf(targetShaHex + 2*i, "%02hhx", &targetSha[i]);
  }

  unsigned char table[256];
  int tableLen = 0;
  for (int i = 0; i < (sizeof(range) - 1) / 2; ++i) {
    for (unsigned char c = range[i * 2]; c <= range[i * 2 + 1]; ++c) {
      table[tableLen] = c;
      assert(++tableLen <= 256);
    }
  }

  for (int len = from; len <= to; ++len) {
    printf("length: %i (trying from %i to %i)\n", len, from, to);
    #pragma omp parallel for
    for (int start = 0; start < tableLen; ++start) {
      printf("running %i/%i\n", start + 1, tableLen);

      // code is used from second character onwards
      unsigned int code[len - 1];
      memset(code, 0, sizeof(code));
      char str[len + 1];
      str[0] = table[start];
      for (int i = 1; i < len; ++i) {
        str[i] = table[0];
      }
      str[len] = '\0';

      long long unsigned int num = 0;
      struct timespec startTime;
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &startTime);

      uLong partialCrc[len + 1];
      partialCrc[0] = crc32(0, Z_NULL, 0);
      for (int i = 1; i <= len; ++i) {
        partialCrc[i] = crc32(partialCrc[i - 1], str + i - 1, 1);
      }
      while (1) {
        ++num;
        if (num == reportEach) {
          struct timespec stopTime;
          clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &stopTime);
          double time = (double)(stopTime.tv_sec - startTime.tv_sec) * 1000
            + (double)(stopTime.tv_nsec - startTime.tv_nsec) / 1000000;
          printf("done %u hashes in %f ms\n", reportEach, time);
          num = 0;
          startTime = stopTime;
        }

        if (partialCrc[len] == targetCrc) {
          printf("possible result (hex): ");
          for (int i = 0; i < len; i++) {
            printf("%02hhx ", str[i]);
          }
          printf("\n");

          unsigned char sha[SHA_DIGEST_LENGTH];
          SHA1(str, len, sha);
          if (memcmp(sha, targetSha, SHA_DIGEST_LENGTH) == 0) {
            printf("result (hex): ");
            for (int i = 0; i < len; i++) {
              printf("%02hhx ", str[i]);
            }
            printf("\n");
            printf("result (str): %s\n", str);
            exit(0);
          }
        }
        
        int all = 1;
        for (int i = len - 1; i >= 0; --i) {
          if (code[i] != tableLen - 1) {
            ++code[i];
            str[i + 1] = table[code[i]];
            for (int j = i + 1; j <= len; ++j) {
              partialCrc[j] = crc32(partialCrc[j - 1], str + j - 1, 1);
            }
            all = 0;
            break;
          } else {
            code[i] = 0;
            str[i + 1] = table[0];
          }
        }
        if (all) break;
      }

      printf("finished %i/%i\n", start + 1, tableLen);
    }
  }
  return 1;
}
