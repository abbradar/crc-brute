#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <zlib.h>
#include <openssl/sha.h>

const int from = 29;
const int to = 30;
const char range[] = "\x21\x2f" "\x3a\x40" "\x5b\x60" "\x7b\x7e" "\x80\xff";

const uLong targetCrc = 0xfb610011;
const unsigned char targetShaHex[] = "35bfe051be8b10feb2c92f4daed7d2f2e387bb68";

int main() {
  const uLong initCrc = crc32(0, Z_NULL, 0);
  
  unsigned char targetSha[SHA_DIGEST_LENGTH];
  assert(sizeof(targetShaHex) == 2 * SHA_DIGEST_LENGTH + 1);
  for (int i = 0; i < SHA_DIGEST_LENGTH; ++i) {
    sscanf(targetShaHex + 2*i, "%02hhx", &targetSha[i]);
  }

  unsigned char table[256];
  int tableLen = 0;
  for (int i = 0; i < (sizeof(range) - 1) / 2; ++i) {
    for (unsigned char c = range[i]; c <= range[i+1]; ++c) {
      table[tableLen] = c;
      assert(++tableLen <= 256);
    }
  }
  
  volatile int nothing = 1;
  for (int len = from; nothing && len <= to; ++len) {
    printf("length: %i (trying from %i to %i)\n", len, from, to);
    #pragma omp parallel for
    for (int start = 0; start < tableLen; ++start) {
      if (!nothing) continue;
      printf("running %i/%i\n", start + 1, tableLen);

      // code is used from second character onwards
      unsigned int code[len - 1];
      memset(code, 0, sizeof(code));
      char str[len + 1];
      str[0] = table[start];
      str[len] = '\0';

      while (1) {
        for (int i = 1; i < len; ++i) {
          str[i] = table[code[i - 1]];
        }

        uLong crc = crc32(initCrc, str, len);
        if (crc == targetCrc) {
          unsigned char sha[SHA_DIGEST_LENGTH];
          SHA1(str, len, sha);
          if (memcmp(sha, targetSha, SHA_DIGEST_LENGTH) == 0) {
            printf("result (hex): ");
            for (int i = 0; i < len; i++) {
              printf("%02hhx ", str[i]);
            }
            printf("\n");
            printf("result (str): %s\n", str);
            nothing = 0;
          }
        }
        
        int all = 1;
        for (int i = len - 1; i >= 0; --i) {
          if (code[i] != tableLen - 1) {
            ++code[i];
            all = 0;
            break;
          } else {
            code[i] = 0;
          }
        }
        if (all) break;
      }

      printf("finished %i/%i\n", start + 1, tableLen);
    }
  }
  return nothing;
}
