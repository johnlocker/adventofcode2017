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
while (notThrough) {
  notThrough <- FALSE
  for (i in 1:nrow(layDF)) {
    if (layDF$scannerPosition[i] == 0) {
      next
    }
    depthDelay <- layDF$depth[i] + delay
    if (depthDelay %% ( (layDF$range[i] - 1) * 2) == 0) {
      notThrough <- TRUE
      delay <- delay + 1
      break
    }
  }
  cat("\nCurrent delay: ", delay, "\n")
}
cat("\nFinal delay: ", delay, "\n")
