getSurroundNumbers <- function(row, col, fillTable) {
  return(sum(c(fillTable[row, col + 1],
               fillTable[row - 1, col + 1],
               fillTable[row - 1, col],
               fillTable[row - 1, col - 1],
               fillTable[row, col - 1],
               fillTable[row + 1, col - 1],
               fillTable[row + 1, col],
               fillTable[row + 1, col + 1]), na.rm = TRUE))
}
x <- 0
y <- 0
d <- 1
m <- 1
count <- 1
N <- 10 * 10
fillTable <- matrix(NA, ncol = sqrt(N), nrow = sqrt(N))
curLoc <- beginLoc <- c(ceiling(nrow(fillTable) / 2), ceiling(ncol(fillTable) / 2))
startTime <- Sys.time()
b <- TRUE
targetNumber <- 325489

start <- targetNumber
while (start ^ 0.5 %% 1 != 0) {
  start <- start + 1
}
cat("Part 1\n")
cat("Distance:", (start ^ 0.5) - 1 - ( ( (start ^ 0.5) - 1) - (start - targetNumber)))

while (any(is.na(fillTable))) {
  while (2 * x * d < m) {
    fillTable[curLoc[1] + y, curLoc[2] + x] <- count
    x <- x + d
    if (count > targetNumber) {
      break
    }
    if (b) {
      count <- getSurroundNumbers(curLoc[1] + y,
                                  curLoc[2] + x,
                                  fillTable)
    } else {
      count <- count + 1
    }
  }
  while (2 * y * d < m) {
    fillTable[curLoc[1] + y, curLoc[2] + x] <- count
    y <- y + d
    if (count > targetNumber) {
      break
    }
    if (b) {
      count <- getSurroundNumbers(curLoc[1] + y,
                                   curLoc[2] + x,
                                   fillTable)
    } else {
      count <- count + 1
    }
  }
  d <- -1 * d
  m <- m + 1
  cat("\r Count:", count)
  flush.console()
  if (count > targetNumber) {
    break
  }
}
endTime <- Sys.time()
difftime(endTime, startTime, units = "mins")
fillTable <- apply(fillTable, 2, rev)

cat("Part 2\n")
cat("Next number:", count)
