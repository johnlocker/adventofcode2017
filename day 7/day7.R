rm(list = ls())
library("dplyr")
library("stringr")
dat <- readr::read_lines(file.path("day 7", "input.txt"))
# dat <- readr::read_lines(file.path("day 7", "test.txt")) # nolint

charExtract <- function(x) {
  return(unlist(str_extract_all(x, "[a-z]+")))
}
numExtract <- function(x) {
  return(unlist(str_extract_all(x, "[0-9]+")))
}
checkChild <- function(x, diskDF = diskDF) {
  if (x == "") {
    return("")
  }
  foundParent <- oldParent <- diskDF$parent[which(x == diskDF$child)][1]
  while (!is.na(foundParent)) {
    foundParent <- diskDF$parent[which(foundParent == diskDF$child)[1]]
    oldParent <- ifelse(!is.na(foundParent), foundParent, oldParent)
  }
  return(oldParent)
}

diskDF <- data.frame()
for (i in 1:length(dat)) {
  disk <- strsplit(dat[i], " -> ")[[1]]
  parent <- charExtract(disk[1])
  number <- as.numeric(numExtract(disk[1]))
  if (length(disk) > 1) {
    child <- strsplit(disk[2], ", ")[[1]]
  } else {
    child <- ""
  }
  for (j in 1:length(child)) {
    diskDF <- rbind(diskDF,
                    data.frame(parent = as.character(parent),
                               parentNumber = number,
                               child = child[j],
                               stringsAsFactors = FALSE))
  }
}

diskDF$grandParent <- sapply(diskDF$child, function(x) checkChild(x))
print(table(diskDF$grandParent))

# part B
datChain <- dat
searchError <- TRUE
enterOnce <- TRUE
errorProg <- ""
correction <- 0
deleteProgs <- character()
while (length(datChain) > 0) {
  curDat <- datChain[1]
  if (!grepl("->", curDat)) {
    num <- as.numeric(numExtract(curDat))
    prog <- as.character(charExtract(curDat))
    # check if exists
    if (!exists(prog)) {
      assign(prog, num)
      deleteProgs <- c(deleteProgs, prog)
    }
    datChain <- datChain[c(2:length(datChain))]
  } else {
    # check if elements exist
    if (all(sapply(charExtract(curDat)[c(2:length(charExtract(curDat)))], exists))) {
      children <- charExtract(curDat)[c(2:length(charExtract(curDat)))]
      childNums <- as.numeric(sapply(children, function(x) eval(parse(text = x))))
      prog <- charExtract(curDat)[1]
      num <- as.numeric(numExtract(curDat))
      if (length(unique(childNums)) != 1 | prog == errorProg) {
        cat("something wrong stop\n")
        if (searchError) {
          correction <- (childNums[!(duplicated(childNums)
                        | duplicated(childNums, fromLast = TRUE))]
                        - as.numeric(names(table(childNums))[table(childNums) > 1]))
          errorProg <- children[which(childNums == childNums[!(duplicated(childNums)
                                      | duplicated(childNums, fromLast = TRUE))])]
          searchError <- FALSE
        } else {
          cat("Corrected value:", num - correction)
          break
        }
      }
      if (!exists(prog)) {
        assign(prog, sum(c(num, childNums)))
        deleteProgs <- c(deleteProgs, prog)
      }
      datChain <- datChain[c(2:length(datChain))]
    } else {
      datChain <- c(datChain, datChain[1])
      datChain <- datChain[c(2:length(datChain))]
    }
  }
  if (!searchError & enterOnce) {
    enterOnce <- FALSE
    datChain <- dat
    rm(list = deleteProgs)
  }
}
