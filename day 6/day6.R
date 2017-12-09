dat <- readr::read_lines(file.path("day 6", "input.txt"))
dat <- as.numeric(strsplit(dat, split = "\t")[[1]])

# dat <- c(0, 2, 7, 0) # nolint
startConfig <- dat
curConfig <- startConfig
cycleCount <- 0
allConfigs <- character()
curConfigCheck <- paste(curConfig, collapse = "_")
while  (!(curConfigCheck %in% allConfigs)) {
  allConfigs <- c(allConfigs, curConfigCheck)
  # choose highest block
  maxIndx <- which.max(curConfig)
  distBlocks <- curConfig[maxIndx]
  # empty bank
  curConfig[maxIndx] <- 0
  allocIndx <- maxIndx + 1
  while (distBlocks > 0) {
    allocIndx <- ifelse(allocIndx > length(curConfig), 1, allocIndx)
    curConfig[allocIndx] <- curConfig[allocIndx] + 1
    allocIndx <- allocIndx + 1
    distBlocks <- distBlocks - 1
  }
  curConfigCheck <- paste(curConfig, collapse = "_")
  cycleCount <- cycleCount + 1
  cat("\r Cycle Count:", cycleCount)
  flush.console()
}
