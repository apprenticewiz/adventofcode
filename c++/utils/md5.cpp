#include <cstdint>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "md5.h"

namespace aoc_utils {
    inline uint32_t rotate_left(uint32_t x, uint32_t c) {
        return (x << c) | (x >> (32 - c));
    }

    std::string md5(const std::string& input) {
        const uint32_t r[] = {
            7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
            5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
            4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
            6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
        };
	    const uint32_t k[64] = {
            0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
            0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
	        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
	        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
	        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
            0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
	        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
	        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
	        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
	        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
	        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
	        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
	        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
	        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
	        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
	        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
	    };
        uint32_t h0 = 0x67452301;
        uint32_t h1 = 0xefcdab89;
        uint32_t h2 = 0x98badcfe;
        uint32_t h3 = 0x10325476;
        std::string msg = input;
        uint64_t bit_len = msg.size() * 8;
        msg += static_cast<char>(0x80);
        while ( (msg.size() * 8) % 512 != 448 ) {
            msg += static_cast<char>(0);
        }
        for ( auto i = 0; i < 8; ++i ) {
            msg += static_cast<char>((bit_len >> (8 * i)) & 0xff);
	}
        for ( auto offset = 0; offset < msg.size(); offset += 64 ) {
            uint32_t w[16];
            for ( auto i = 0; i < 16; ++i ) {
                w[i] = static_cast<uint32_t>(
                    (static_cast<uint8_t>(msg[offset + i * 4 + 0])) |
                    (static_cast<uint8_t>(msg[offset + i * 4 + 1]) << 8) |
                    (static_cast<uint8_t>(msg[offset + i * 4 + 2]) << 16) |
                    (static_cast<uint8_t>(msg[offset + i * 4 + 3]) << 24)
                );
            }
            uint32_t a = h0;
            uint32_t b = h1;
            uint32_t c = h2;
            uint32_t d = h3;
            for ( auto i = 0; i < 64; ++i ) {
                uint32_t f, g;
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
                uint32_t temp = d;
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
        std::ostringstream result;
        auto append_hex = [&](uint32_t val) {
            for ( auto i = 0; i < 4; ++i ) {
                result << std::hex << std::setw(2) << std::setfill('0') << ((val >> (8 * i)) & 0xff);
            }
        };
        append_hex(h0);
        append_hex(h1);
        append_hex(h2);
        append_hex(h3);
        return result.str();
    }
}
