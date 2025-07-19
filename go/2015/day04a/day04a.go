package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"os"
)

func usage(progname string) {
	fmt.Fprintf(os.Stderr, "usage: %s <key>\n", progname)
	os.Exit(1)
}

func process(key string) uint32 {
	for i := 1 ; ; i++ {
		tryKey := fmt.Sprintf("%s%d", key, i)
		hash := md5.Sum([]byte(tryKey))
		hashStr := hex.EncodeToString(hash[:])
		if hashStr[:5] == "00000" {
			return uint32(i)
		}
	}
	return 0
}

func main() {
	if len(os.Args) < 2 {
		usage(os.Args[0])
	}

	filename := os.Args[1]
	result := process(filename)
	fmt.Printf("result = %d\n", result)
}
