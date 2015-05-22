crc-brute: crc-brute.c
	gcc -O3 -std=gnu99 -lz -lcrypto -fopenmp -o $@ crc-brute.c
