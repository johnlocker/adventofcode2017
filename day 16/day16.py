file = open('C:\Users\Besitzer\Tresor\john.locker\Mega\R working directory\\adventofcode2017\day 16\input.txt', 'r')
danceStream = file.readline().split(",")
# dat = "s1,x3/4,pe/b"

programs = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p']

partA = True
maxIter = 10 ** 9
maxIter = maxIter / len(danceStream)
if partA:
    maxIter = 1

on = 1
while on <= maxIter:
  for i in range(len(danceStream)):
    danceSplit = list(danceStream[i])
    move = danceSplit[0]
    if "/" in danceStream[i]:
        moveProgs = danceStream[i][1:].split("/")
    else:
        moveProgs = int(danceStream[i][1:])
    if move == "s":
      programs = programs[(len(programs) - int(moveProgs)):len(programs)] + programs[0:(len(programs) - moveProgs)]
    if move == "x":
      swap1 = programs[int(moveProgs[0])]
      swap2 = programs[int(moveProgs[1])]
      programs[int(moveProgs[0])] = swap2
      programs[int(moveProgs[1])] = swap1
    if move == "p":
      idx1 = programs.index(moveProgs[0])
      idx2 = programs.index(moveProgs[1])
      switch1 = programs[idx1]
      switch2 = programs[idx2]
      programs[idx1] = switch2
      programs[idx2] = switch1
  on += 1
  if on % 1000 == 0:
      print "Percent done: " + str(on / 1000) + "%\n"
print "Final order: " + ''.join(programs)
