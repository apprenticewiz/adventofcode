#include "md5.h"

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static inline uint32_t rotate_left(uint32_t x, uint32_t c) {
  return (x << c) | (x >> (32 - c));
}

void md5_digest(const char *input, char *digest) {
  const uint32_t r[] = {
      7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
      5, 9,  14, 20, 5, 9,  14, 20, 5, 9,  14, 20, 5, 9,  14, 20,
      4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
      6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
  };
  const uint32_t k[64] = {
      0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a,
      0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
      0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340,
      0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
      0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8,
      0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
      0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa,
      0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
      0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92,
      0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
      0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
  };
  uint32_t h0 = 0x67452301;
  uint32_t h1 = 0xefcdab89;
  uint32_t h2 = 0x98badcfe;
  uint32_t h3 = 0x10325476;
  size_t input_len = strlen(input);
  uint64_t bit_len = input_len * 8;
  size_t pad_len = 1 + ((56 - (input_len + 1) % 64) % 64);
  size_t padded_len = input_len + pad_len + 8;
  char *msg = (unsigned char *)calloc(padded_len, 1);
  int i, offset;
  uint32_t w[16];
  uint32_t a, b, c, d;
  uint32_t f, g;
  uint32_t temp;

  memcpy(msg, input, input_len);
  msg[input_len] = 0x80;
  for ( i = 0; i < 8; i++ ) {
    msg[input_len + pad_len + i] = (bit_len >> (8 * i)) & 0xff;
  }
  for ( offset = 0; offset < padded_len; offset += 64 ) {
    for ( i = 0; i < 16; i++ ) {
      w[i] = (uint32_t)(((uint8_t)msg[offset + i * 4 + 0]) |
                        ((uint8_t)msg[offset + i * 4 + 1] << 8) |
                        ((uint8_t)msg[offset + i * 4 + 2] << 16) |
                        ((uint8_t)msg[offset + i * 4 + 3] << 24));
    }
    a = h0;
    b = h1;
    c = h2;
    d = h3;
    for ( i = 0; i < 64; i++ ) {
      if ( i < 16 ) {
        f = (b & c) | (~b & d);
        g = i;
      } else if ( i < 32 ) {
        f = (d & b) | (~d & c);
	g = (5 * i + 1) % 16;
      } else if ( i < 48 ) {
        f = b ^ c ^ d;
	g = (3 * i + 5) % 16;
      } else {
        f = c ^ (b | ~d);
	g = (7 * i) % 16;
      }
      temp = d;
      d = c;
      c = b;
      b += rotate_left((a + f + k[i] + w[g]), r[i]);
      a = temp;
    }
    h0 += a;
    h1 += b;
    h2 += c;
    h3 += d;
  }
  for ( i = 0; i < 4; i++ ) {
    sprintf(digest + (i * 2) , "%02x", (h0 >> (8 * i) ) & 0xff);
  }
  for ( i = 0; i < 4; i++ ) {
    sprintf(digest + (i * 2) + 8 , "%02x", (h1 >> (8 * i) ) & 0xff);
  }
  for ( i = 0; i < 4; i++ ) {
    sprintf(digest + (i * 2) + 16 , "%02x", (h2 >> (8 * i) ) & 0xff);
  }
  for ( i = 0; i < 4; i++ ) {
    sprintf(digest + (i * 2) + 24 , "%02x", (h3 >> (8 * i) ) & 0xff);
  }
}
