crc-brute: crc-brute.c
	gcc -O3 -std=c99 -lz -lcrypto -fopenmp -o $@ crc-brute.c
