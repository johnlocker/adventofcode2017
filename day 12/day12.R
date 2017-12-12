dat <- readr::read_lines(file.path("day 12", "input.txt"))
# dat <- readr::read_lines(file.path("day 12", "test.txt")) # nolint

partB <- FALSE
conDF <- data.frame()
for (i in 1:length(dat)) {
  curCon <- unlist(strsplit(dat[i], split = " <-> "))
  from <- curCon[1]
  to <- unlist(strsplit(curCon[2], split = ", "))
  for (j in 1:length(to)) {
    conDF <- rbind(conDF,
                   data.frame(from = as.numeric(from),
                              to = as.numeric(to[j])))
    conDF <- rbind(conDF,
                   data.frame(from = as.numeric(to[j]),
                              to = as.numeric(from)))
  }
}
conDF <- conDF[!duplicated(conDF), ]

allGroups <- 0
while (nrow(conDF) > 0) {
  cat("\nnrow(conDF):", nrow(conDF), "\n")
  conToTarget <- conDF$from[1]
  removeRows <- c()
  for (k in 1:nrow(conDF)) {
    for (i in 1:nrow(conDF)) {
      if (conDF$to[i] %in% conToTarget) {
        conToTarget <- c(conToTarget, conDF$from[i])
        removeRows <- c(removeRows, i)
      }
      if (conDF$from[i] %in% conToTarget) {
        conToTarget <- c(conToTarget, conDF$to[i])
        removeRows <- c(removeRows, i)
      }
    }
    conToTarget <- unique(conToTarget)
    removeRows <- unique(removeRows)
    oefenwebTools::Progressor(k, nrow(conDF))
  }
  # removeRows
  conDF <- conDF[-removeRows, ]
  allGroups <- allGroups + 1
  if (!partB) {
    cat("\nThere are", length(conToTarget), "programs in group 0.\n")
    break
  }
}
cat("\nThere are", allGroups, "groups in total.\n")
