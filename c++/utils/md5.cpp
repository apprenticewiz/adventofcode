#include <cmath>
#include <cstdint>
#include <cstdlib>
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
        uint32_t k[64];
        for ( auto i = 0; i < 64; ++i ) {
            k[i] = static_cast<uint32_t>(std::floor(std::abs(std::sin(i + 1)) * (1ULL << 32)));
        }
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