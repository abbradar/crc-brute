#include <stdint.h>

int crcBruteUtf8_16(const int* assigns, const int length, const uint16_t** tables, const int* ranges, const uint32_t target, int *stop, int (*check)(), uint16_t* str);
