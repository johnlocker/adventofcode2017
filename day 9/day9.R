stream <- readr::read_lines(file.path("day 9", "input.txt"))
library("dplyr")
# nolint start
# stream <- "{{<!>},{<!>},{<!>},{<a>}}"
# stream <- "{{<a!>},{<a!>},{<a!>},{<ab>}}"
# stream <- "{{<!!>},{<!!>},{<!!>},{<!!>}}"
# stream <- "{{<ab>},{<ab>},{<ab>},{<ab>}}"
# stream <- "{<a>,<a>,<a>,<a>}"
# stream <- '<{o"i!a,<{i<a>'
# stream <- "<!!!>>"
# stream <- "<random characters>"
# stream <- "<<<<>"
# nolint end

splitStream <- unlist(strsplit(stream, split = ""))
currentScore <- 0
scoreVec <- numeric(length = length(splitStream))
garbage <- FALSE
exclamationIgnore <- FALSE
garbageCount <- 0
for (i in 1:length(splitStream)) {
  cur <- splitStream[i]
  if (exclamationIgnore) {
    exclamationIgnore <- FALSE
    next
  }
  if (cur == "!") {
    exclamationIgnore <- TRUE
    next
  }
  if (cur == "<" & !garbage) {
    garbage <- TRUE
    next
  }
  if (cur == ">") {
    garbage <- FALSE
  }
  if (garbage) {
    garbageCount <- garbageCount + 1
  }
  if (cur == "{" & !garbage) {
    currentScore <- currentScore + 1
    scoreVec[i] <- currentScore
  }
  if (cur == "}" & !garbage) {
    currentScore <- currentScore - 1
  }
}
scoreVec
cat("total score groups:", sum(scoreVec))
cat("garbage characters:", garbageCount)
