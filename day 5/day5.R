dat <- read.table(file.path("day 5", "input.txt"), stringsAsFactors = FALSE)
dat <- as.numeric(dat$V1)

# dat <- c(0, 3,  0,  1,  -3) # nolint
inVec <- TRUE
curPos <- 1
jumpIncrement <- 1
stepCounter <- 0
part2 <- TRUE
startTime <- Sys.time()
while (inVec) {
  nextJumps <- dat[curPos]
  # increment current
  dat[curPos] <- dat[curPos] + ifelse(nextJumps >= 3 & part2, -1, 1)
  # make jump
  curPos <- curPos + nextJumps
  # check if curPos is in dat
  inVec <- ifelse(curPos <= length(dat), TRUE, FALSE)
  stepCounter <- stepCounter + 1
}
endTime <- Sys.time()
difftime(endTime, startTime, unit = "mins")
cat("Needed", stepCounter, "steps")
