steps <- 314
# steps <- 3 # nolint
# maxNum <- 10 # nolint
partA <- TRUE
maxNum <- 2017
circle <- 0
curPos <- 1
for (i in 1:maxNum) {
  # cat("Cur circle: ", circle, "\n") # nolint
  step <- curPos
  step <- ( (steps + (curPos - 1)) %% i) + 1
  insert <- step + 1
  if (length(circle) < insert) {
    if (length(circle) == insert) {
      circle <- c(i, circle)
    } else {
      circle <- c(circle, i)
    }
  } else {
    circle <- c(circle[c(1:(insert - 1))], i, circle[c(insert:length(circle))])
  }
  curPos <- insert
}
cat("Value after 2017:", circle[(which(circle == 2017) + 1)])

maxNum <- 5 * 10 ^ 7
circle <- 0
curPos <- 1
for (i in 1:maxNum) {
  step <- curPos
  step <- ( (steps + (curPos - 1)) %% i) + 1
  insert <- step + 1
  if (insert == 2) {
    valueAfterZero <- i
  }
  curPos <- insert
  if (i %% (5 * 10 ^ 5) == 0) {
    cat("Done: ", paste0(round(i / maxNum * 100), "%"), "\n")
  }
}
cat("Value after 0:", valueAfterZero)
