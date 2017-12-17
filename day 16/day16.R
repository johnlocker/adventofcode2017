dat <- readr::read_lines(file.path("day 16", "input.txt"))
# dat <- "s1,x3/4,pe/b" # nolint
# programs <- letters[c(1:5)] # nolint

partA <- FALSE
danceStream <- unlist(strsplit(dat, split = ","))

for (j in 1:(2 - partA)) {
  programs <- letters[c(1:16)]
  on <- 1
  progVec <- c()
  maxIter <- 10 ^ 9
  if (partA) {
    maxIter <- 1
  } else {
    if (exists("onRemainder")) {
      maxIter <- (maxIter / length(danceStream)) %% onRemainder
    } else {
      maxIter <- (maxIter / length(danceStream))
    }
  }
  while (on <= maxIter) {
    progVec <- c(progVec, paste(programs, collapse = "")) # nolint
    for (i in 1:length(danceStream)) {
      danceSplit <- unlist(strsplit(danceStream[i], split = ""))
      move <- danceSplit[1]
      moveProgs <- unlist(strsplit(paste(danceSplit[c(2:length(danceSplit))],
                                         collapse = ""), split = "/"))
      if (move == "s") {
        programs <- programs[c( ( (length(programs) + 1) - as.numeric(moveProgs)):length(programs),
                              1:(length(programs) - as.numeric(moveProgs)))]
      }
      if (move == "x") {
        swap1 <- programs[as.numeric(moveProgs[1]) + 1]
        swap2 <- programs[as.numeric(moveProgs[2]) + 1]
        programs[as.numeric(moveProgs[1]) + 1] <- swap2
        programs[as.numeric(moveProgs[2]) + 1] <- swap1
      }
      if (move == "p") {
        idx1 <- which(programs == moveProgs[1])
        idx2 <- which(programs == moveProgs[2])
        switch1 <- programs[which(programs == moveProgs[1])]
        switch2 <- programs[which(programs == moveProgs[2])]
        programs[idx1] <- switch2
        programs[idx2] <- switch1
      }
    }
    # find out after how many repetitions the programs are in same state
    if (paste(programs, collapse = "") %in% progVec) {
      onRemainder <- on
      cat("\nonRemainder is:", onRemainder, "\n")
      break
    }
    on <- on + 1
  }
}
cat("Final order:", paste(programs, collapse = ""))
