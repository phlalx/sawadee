#!/usr/bin/python3

import sys
import random

assert(len(sys.argv) == 3)
filename = sys.argv[1]
num = int(sys.argv[2])
assert(num < 1000)

def f():
  b = [random.random() > 0.5 for i in range(8)]
  res = 0
  for i in range(8):
    res = 2*res + (1 if random.random() > 0.4 else 0)
  return res

b = bytearray(num)
for i in range(num):
  b[i] = f()

f = open(filename, "wb")
f.write(b)
f.close()
