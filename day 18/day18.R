instructions <- readr::read_lines(file.path("day 18", "input.txt"))

#' Function to get either variable or numeric value
#' @param instruction Instruction with either variable or numberic value
#' @param progDFarg Current data.frame with registers (default progDF)
#' @return numeric value from variable
getValue <- function(instruction, progDFarg = progDF) {
  if (suppressWarnings(is.na(as.numeric(instruction)))) {
    return(progDFarg[, which(colnames(progDFarg) == instruction)])
  } else {
    return(as.numeric(instruction))
  }
}

count <- 1
partA <- FALSE

progLists <- list()
progLists$p0 <- data.frame(p = 0, a = 0, i = 0, b = 0, f = 0,
                           p_s = 0, a_s = 0, i_s = 0, b_s = 0, f_s = 0,
                           stringsAsFactors = FALSE)
progLists$p1 <- data.frame(p = 1, a = 0, i = 0, b = 0, f = 0,
                           stringsAsFactors = FALSE)
sendList <- list()
sendList$p0 <- numeric()
sendList$p1 <- numeric()
prog1Counter <- 0
active <- data.frame(p0 = "s",
                     p1 = "s",
                     stringsAsFactors = FALSE)
iLoopDF <- data.frame(p0 = 1,
                      p1 = 1,
                      stringsAsFactors = FALSE)
prog <- 0
progDF <- progLists[[prog + 1]]
iLoop <- iLoopDF[[prog + 1]]

while (TRUE) {
  curIns <- unlist(strsplit(instructions[iLoop], " "))
  if (curIns[1] == "snd") {
    if (partA) {
      progDF[[paste0(curIns[2], "_s")]] <- getValue(curIns[2])
    } else {
      if (prog == 1) {
        prog1Counter <- prog1Counter + 1
      }
      sendList[[prog + 1]] <- c(sendList[[prog + 1]], getValue(curIns[2]))
    }
  }
  if (curIns[1] == "set") {
    progDF[[curIns[2]]] <- getValue(curIns[3])
  }
  if (curIns[1] == "add") {
    progDF[[curIns[2]]] <- progDF[[curIns[2]]] + getValue(curIns[3])
  }
  if (curIns[1] == "mul") {
    progDF[[curIns[2]]] <- progDF[[curIns[2]]] * getValue(curIns[3])
  }
  if (curIns[1] == "mod") {
    progDF[[curIns[2]]] <- progDF[[curIns[2]]] %% getValue(curIns[3])
  }
  if (curIns[1] == "rcv") {
    if (partA & getValue(curIns[2]) != 0) {
      if (progDF[[paste0(curIns[2], "_s")]] != 0) {
        cat("Non zero recovered frquency:", progDF[[paste0(curIns[2], "_s")]], "\n")
        break
      }
    } else {
      # check if something is in other programs queue
      if (length(sendList[[ifelse(prog == 0, 1, 0) + 1]]) > 0) {
        active[, (prog + 1)] <- "s"
        progDF[[curIns[2]]] <- sendList[[ifelse(prog == 0, 1, 0) + 1]][1]
        sendList[[ifelse(prog == 0, 1, 0) + 1]] <- sendList[[ifelse(prog == 0, 1, 0) + 1]][-1]
      } else {
        if (active[, (ifelse(prog == 0, 1, 0) + 1)] == "d") {
          cat("Both programs terminated\n")
          break
        }
        if (length(sendList[[prog + 1]]) == 0 & active[, (ifelse(prog == 0, 1, 0) + 1)] == "r") {
          break
        }
        progLists[[prog + 1]] <- progDF
        iLoopDF[, (prog + 1)] <- iLoop
        active[, (prog + 1)] <- "r"
        # change programs
        prog <- ifelse(prog == 1, 0, 1)
        progDF <- progLists[[prog + 1]]
        iLoop <- iLoopDF[, (prog + 1)] - 1
      }
    }
  }
  # nolint start
  # cat("This is inst:", instructions[iLoop], ", value is ", curIns[2], ":",
  #     getValue(curIns[2]), "\n")
  # nolint end
  if (curIns[1] == "jgz" & getValue(curIns[2]) > 0) {
    iLoop <- iLoop + (getValue(curIns[3]) - 1)
  }
  count <- count + 1
  iLoop <- iLoop + 1
  if (iLoop < 1 | iLoop > length(instructions)) {
    if (active[, ifelse(prog == 0, 1, 0) + 1] == "d") {
      cat("Program jumped outside. Terminating.\n")
      break
    }
    progLists[[prog + 1]] <- progDF
    iLoopDF[, (prog + 1)] <- iLoop
    active[, (prog + 1)] <- "d"
    # change programs
    prog <- ifelse(prog == 1, 0, 1)
    progDF <- progLists[[prog + 1]]
    iLoop <- iLoopDF[, (prog + 1)]
  }
}
if (!partA) {
  cat("Program 1 sended:", prog1Counter, "times.\n")
}
