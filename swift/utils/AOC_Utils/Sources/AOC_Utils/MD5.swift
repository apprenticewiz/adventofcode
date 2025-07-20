import Foundation

@inline(__always)
func rotateLeft(_ x: UInt32, _ c: UInt32) -> UInt32 {
    return (x << c) | (x >> (32 - c))
}

public func md5(_ input: String) -> String {
    let r: [UInt32] = [
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
        5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
        4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
        6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
    ]

    let k: [UInt32] = [
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
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
    ]

    var message = Array(input.utf8)
    let bitLength = UInt64(message.count * 8)

    // Append the '1' bit (0x80)
    message.append(0x80)

    // Pad with zeros until message length ≡ 56 mod 64
    while (message.count % 64) != 56 {
        message.append(0)
    }

    // Append original length in bits as 64-bit little-endian integer
    for i in 0..<8 {
        message.append(UInt8((bitLength >> (8 * i)) & 0xFF))
    }

    // Initial hash values
    var h0: UInt32 = 0x67452301
    var h1: UInt32 = 0xefcdab89
    var h2: UInt32 = 0x98badcfe
    var h3: UInt32 = 0x10325476

    for chunkOffset in stride(from: 0, to: message.count, by: 64) {
        let chunk = message[chunkOffset..<chunkOffset+64]
        var w = [UInt32](repeating: 0, count: 16)

        for i in 0..<16 {
            let base = chunk.startIndex + i * 4
            w[i] = UInt32(chunk[base]) |
                   UInt32(chunk[base + 1]) << 8 |
                   UInt32(chunk[base + 2]) << 16 |
                   UInt32(chunk[base + 3]) << 24
        }

        var a = h0
        var b = h1
        var c = h2
        var d = h3

        for i in 0..<64 {
            let f: UInt32
            let g: Int

            switch i {
            case 0..<16:
                f = (b & c) | (~b & d)
                g = i
            case 16..<32:
                f = (d & b) | (~d & c)
                g = (5 * i + 1) % 16
            case 32..<48:
                f = b ^ c ^ d
                g = (3 * i + 5) % 16
            default:
                f = c ^ (b | ~d)
                g = (7 * i) % 16
            }

            let temp = d
            d = c
            c = b
            b = b &+ rotateLeft(a &+ f &+ k[i] &+ w[g], r[i])
            a = temp
        }

        h0 = h0 &+ a
        h1 = h1 &+ b
        h2 = h2 &+ c
        h3 = h3 &+ d
    }

    func toHex(_ val: UInt32) -> String {
        return (0..<4).map {
            String(format: "%02x", (val >> ($0 * 8)) & 0xff)
        }.joined()
    }

    return toHex(h0) + toHex(h1) + toHex(h2) + toHex(h3)
}

