#include <cstdint>
#include <cstdlib>
#include <string>

#ifndef __MD5_H
#define __MD5_H

class MD5 {
public:
    MD5() { reset(); }

    void update(const unsigned char* input, size_t length);
    void update(const char* input, size_t length)
    {
        update((const unsigned char*)input, length);
    }
    void update(const std::string input)
    {
        update(input.c_str(), input.size());
    }

    std::string digest();
    void reset();

private:
    void transform(const uint8_t block[64]);
    static void encode(uint8_t* output, const uint32_t* input, size_t length);
    static void decode(uint32_t* output, const uint8_t* input, size_t length);

    static inline uint32_t F(uint32_t x, uint32_t y, uint32_t z) { return (x & y) | (~x & z); }
    static inline uint32_t G(uint32_t x, uint32_t y, uint32_t z) { return (x & z) | (y & ~z); }
    static inline uint32_t H(uint32_t x, uint32_t y, uint32_t z) { return x ^ y ^ z; }
    static inline uint32_t I(uint32_t x, uint32_t y, uint32_t z) { return y ^ (x | ~z); }

    static inline uint32_t rotate_left(uint32_t x, int n)
    {
        return (x << n) | (x >> (32 - n));
    }

    static void FF(uint32_t& a, uint32_t b, uint32_t c, uint32_t d,
                   uint32_t x, uint32_t s, uint32_t ac)
    {
        a += F(b, c, d) + x + ac;
        a = rotate_left(a, s);
        a += b;
    }

    static void GG(uint32_t& a, uint32_t b, uint32_t c, uint32_t d,
                   uint32_t x, uint32_t s, uint32_t ac)
    {
        a += G(b, c, d) + x + ac;
        a = rotate_left(a, s);
        a += b;
    }

    static void HH(uint32_t& a, uint32_t b, uint32_t c, uint32_t d,
                   uint32_t x, uint32_t s, uint32_t ac)
    {
        a += H(b, c, d) + x + ac;
        a = rotate_left(a, s);
        a += b;
    }

    static void II(uint32_t& a, uint32_t b, uint32_t c, uint32_t d,
                   uint32_t x, uint32_t s, uint32_t ac)
    {
        a += I(b, c, d) + x + ac;
        a = rotate_left(a, s);
        a += b;
    }

    bool finalized;
    uint8_t buffer[64];    // Input buffer
    uint32_t state[4];     // State (A, B, C, D)
    uint32_t count[2];     // Number of bits, modulo 2^64
    uint8_t digest_[16];   // Final digest
};

#endif
