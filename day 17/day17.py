# -*- coding: utf-8 -*-
steps = 314
maxNum = 5 * 10 ** 7
circle = 0
curPos = 1
for i in range(1, maxNum):
  step = curPos
  step = ((steps + (curPos - 1)) % i) + 1
  insert = step + 1
  if insert == 2:
    valueAfterZero = i
  curPos = insert
print "Number after zero: " + str(valueAfterZero)