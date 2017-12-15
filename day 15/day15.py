# -*- coding: utf-8 -*-
import datetime
generatorA = 873
generatorB = 583
#generatorA = 65
#generatorB = 8921
factorA = 16807
factorB = 48271
remainder = 2147483647

partB = False
on = 1
remA = generatorA
remB = generatorB
matched = 0
newRoundA = True
newRoundB = True
start = datetime.datetime.now()
if partB:
    maxIter = 5000000
else:
    maxIter = 40000000
while on <= maxIter:
    if partB:
      while remA % 4 != 0 or newRoundA:
          multiplyA = remA * factorA
          remA = multiplyA % remainder
          newRoundA = False
      while remB % 8 != 0 or newRoundB: 
          multiplyB = remB * factorB
          remB = multiplyB % remainder
          newRoundB = False
    else:
        multiplyA = remA * factorA
        remA = multiplyA % remainder
        multiplyB = remB * factorB
        remB = multiplyB % remainder
    # print "A: " + str(remA) + " B: " + str(remB)
    bitA = bin(remA)
    bitB = bin(remB)
    matched += bitA[-16:] == bitB[-16:]
    on += 1
    newRoundA = True
    newRoundB = True
    if on % 1000000 == 0:
        print "\n" + str(on / 1000000) + " million loops done.\n"
end = datetime.datetime.now()
print end - start
print "\nBits have been matched " + str(matched) + " times.\n"