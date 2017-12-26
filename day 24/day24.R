dat <- readr::read_lines(file.path("day 24", "input.txt"))
# dat <- readr::read_lines(file.path("day 24", "test.txt")) # nolint

pinList <- list()
for (i in 1:length(dat)) {
  pinList[[i]] <-  as.numeric(unlist(strsplit(dat[i], "/")))
}

#' Recursive function to search most valueable bridge
#' @param pinList List of pins
#' @param lastPin Last pin to wich new component has to be connected
#' @return vector with pins of most valuable bridge
search <- function(pinList, lastPin) {
  curMax <- 0
  maxBridge <- numeric()
  for (i in 1:length(pinList)) {
    pin <- pinList[[i]]
    if (pin[1] == lastPin | pin[2] == lastPin) {
      bridge <- c(pin, search(pinList[-i], ifelse(pin[1] == lastPin, pin[2], pin[1])))
      if (sum(bridge) > curMax) {
        curMax <- sum(bridge)
        maxBridge <- bridge
      }
    }
  }
  return(maxBridge)
}
maxBridge <- search(pinList, 0)
cat("Max bridge:", sum(maxBridge))

# part B
#' Recursive function to search longest bridge
#' @param pinList List of pins
#' @param lastPin Last pin to wich new component has to be connected
#' @return vector with pins of longest bridge
searchLongest <- function(pinList, lastPin) {
  curMax <- 0
  curMaxLen <- 0
  maxBridge <- numeric()
  for (i in 1:length(pinList)) {
    pin <- pinList[[i]]
    if (pin[1] == lastPin | pin[2] == lastPin) {
      bridge <- c(pin, searchLongest(pinList[-i], ifelse(pin[1] == lastPin, pin[2], pin[1])))
      if (length(bridge) >= curMaxLen) {
        if (sum(bridge) < curMax & length(bridge) == curMaxLen) {
          next
        }
        curMaxLen <- length(bridge)
        curMax <- sum(bridge)
        maxBridge <- bridge
      }
    }
  }
  return(maxBridge)
}
maxBridgeLongest <- searchLongest(pinList, 0)
cat("Max bridge Longest:", sum(maxBridgeLongest))
