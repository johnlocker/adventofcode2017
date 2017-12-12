path <- readr::read_lines(file.path("day 11", "input.txt"))
# nolint start
# path <- "se,sw,se,sw,sw"
# path <- "ne,ne,sw,sw"
# path <- "ne,ne,ne"
# path <- "ne,ne,s,s"
# nolint end
paths <- unlist(strsplit(path, ","))

allMoves <- c()
stepsAwayVec <- c()
n <- w <- s <- e <- 0
for (i in 1:length(paths)) {
  moves <- paths[i]
  if (nchar(moves) == 2) {
    moves <- unlist(strsplit(moves, split = ""))
    stepSize <- 0.5
  } else {
    stepSize <- 1
  }
  for (move in moves) {
    assign(move, get(move) + stepSize)
  }
  stepsAway <- max(c(w, e)) - min(c(w, e))
  stepsAway <- stepsAway + max(c(n, s)) - min(c(n, s))
  stepsAwayVec <- c(stepsAwayVec, stepsAway)
}
cat("How many steps away?", tail(stepsAwayVec, 1))
cat("How many steps furthest away?", max(stepsAwayVec))
