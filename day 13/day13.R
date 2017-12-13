dat <- readr::read_lines(file.path("day 13", "input.txt"))
# dat <- readr::read_lines(file.path("day 13", "test.txt")) # nolint

moveScanner <- function(layDF) {
  # move scanner
  for (j in 1:nrow(layDF)) {
    if (layDF$scannerPosition[j] == 0) {
      next
    }
    nextPos <- ifelse(layDF$scannerUp[j], layDF$scannerPosition[j] + 1, layDF$scannerPosition[j] - 1)
    if (nextPos > layDF$range[j]) {
      layDF$scannerUp[j] <- FALSE
      nextPos <- layDF$scannerPosition[j] - 1
    }
    if (nextPos < 1) {
      layDF$scannerUp[j] <- TRUE
      nextPos <- layDF$scannerPosition[j] + 1
    }
    layDF$scannerPosition[j] <- nextPos
  }
  return(layDF)
}

layDF <- data.frame(depth = numeric(length = length(dat)),
                    range = numeric(length = length(dat)),
                    scannerPosition = 1)
depth <- sapply(dat, function(x) as.numeric(unlist(strsplit(x, ": "))[1]))
range <- sapply(dat, function(x) as.numeric(unlist(strsplit(x, ": "))[2]))
layDF <- data.frame(depth = c(0:max(depth)),
                    range = 0,
                    scannerPosition = 0)
layDF$range[layDF$depth %in% depth] <- range
layDF$scannerPosition[layDF$depth %in% depth] <- 1
layDF$scannerUp <- TRUE

damage <- numeric(length = nrow(layDF))
for (curDepthIdx in 1:nrow(layDF)) {
  # check if moved into scanner
  if (layDF$scannerPosition[curDepthIdx] == 1) {
    damage[curDepthIdx] <- layDF$depth[curDepthIdx] * layDF$range[curDepthIdx]
  }
  layDF <- moveScanner(layDF)
}
cat("\nTotal damage: ", sum(damage), "\n")

delay <- 0
notThrough <- TRUE
layDF <- layDF[layDF$scannerPosition != 0, ]
rangeVec <- layDF$range
depthVec <- layDF$depth
startTime <- Sys.time()
while (notThrough) {
  notThrough <- any(sapply(c(1:length(rangeVec)), function(x) {
    (depthVec[x] + delay) %% ( (rangeVec[x] - 1) * 2) == 0
  }))
  if (notThrough) {
    delay <- delay + 2
    delay <- ifelse(delay %% 4 == 0, delay + 2, delay)
  }
}
cat("\nFinal delay: ", delay, "\n")
endTime <- Sys.time()
difftime(endTime, startTime, units = "sec")
