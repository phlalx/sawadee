#!/usr/bin/env python3

import sys
import random

assert(len(sys.argv) == 3)
filename = sys.argv[1]
num = int(sys.argv[2])
assert(num < 1000)

bits = [ random.random() > 0.5 for i in range(num) ]
b = bytearray( (num + 7) // 8)

for i in range(len(bits)):
  b[i // 8] = b[i // 8] | ( bits[i] << ((7 - i) % 8) )

f = open(filename, "wb")
f.write(b)
f.close()
