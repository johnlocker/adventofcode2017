reqSteps <- 12261543

stateList <- list()
stateList$A$null$value <- 1
stateList$A$null$direction <- 1
stateList$A$null$nextState <- "B"
stateList$A$one$value <- 0
stateList$A$one$direction <- -1
stateList$A$one$nextState <- "C"

stateList$B$null$value <- 1
stateList$B$null$direction <- -1
stateList$B$null$nextState <- "A"
stateList$B$one$value <- 1
stateList$B$one$direction <- 1
stateList$B$one$nextState <- "C"

stateList$C$null$value <- 1
stateList$C$null$direction <- 1
stateList$C$null$nextState <- "A"
stateList$C$one$value <- 0
stateList$C$one$direction <- -1
stateList$C$one$nextState <- "D"

stateList$D$null$value <- 1
stateList$D$null$direction <- -1
stateList$D$null$nextState <- "E"
stateList$D$one$value <- 1
stateList$D$one$direction <- -1
stateList$D$one$nextState <- "C"

stateList$E$null$value <- 1
stateList$E$null$direction <- 1
stateList$E$null$nextState <- "F"
stateList$E$one$value <- 1
stateList$E$one$direction <- 1
stateList$E$one$nextState <- "A"

stateList$F$null$value <- 1
stateList$F$null$direction <- 1
stateList$F$null$nextState <- "A"
stateList$F$one$value <- 1
stateList$F$one$direction <- 1
stateList$F$one$nextState <- "E"

cursor <- c(0)
i <- 1
curState <- "A"
step <- 0
while (step < reqSteps) {
  instruction <- stateList[[curState]]
  if (cursor[i] == 0) {
    inst <- instruction$null
  } else {
    inst <- instruction$one
  }
  # make value
  cursor[i] <- inst$value
  i <- i + inst$direction
  curState <- inst$nextState
  if (i == 0) {
    cursor <- c(0, cursor)
    i <- 1
  }
  if (i > length(cursor)) {
    cursor <- c(cursor, 0)
  }
  step <- step + 1
  if (step %% (reqSteps / 100) == 0) {
    cat("Done:", round(step / reqSteps * 100), "%\n")
  }
}
cat("Checksum:", sum(cursor), "\n")
