instructions <- readr::read_lines(file.path("day 23", "input.txt"))

regDF <- data.frame(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0)
#' Function to get either variable or numeric value
#' @param instruction Instruction with either variable or numberic value
#' @param progDFarg Current data.frame with registers (default progDF)
#' @return numeric value from variable
getValue <- function(instruction, progDFarg = regDF) {
  if (suppressWarnings(is.na(as.numeric(instruction)))) {
    return(progDFarg[, which(colnames(progDFarg) == instruction)])
  } else {
    return(as.numeric(instruction))
  }
}

iLoop <- 1
mulInv <- 0
while (TRUE) {
  curIns <- unlist(strsplit(instructions[iLoop], " "))
  if (curIns[1] == "set") {
    regDF[[curIns[2]]] <- getValue(curIns[3])
  }
  if (curIns[1] == "sub") {
    regDF[[curIns[2]]] <- regDF[[curIns[2]]] - getValue(curIns[3])
  }
  if (curIns[1] == "mul") {
    mulInv <- mulInv + 1
    regDF[[curIns[2]]] <- regDF[[curIns[2]]] * getValue(curIns[3])
  }
  if (curIns[1] == "jnz" & getValue(curIns[2]) != 0) {
    iLoop <- iLoop + (getValue(curIns[3]) - 1)
  }
  iLoop <- iLoop + 1
  if (iLoop < 1 | iLoop > length(instructions)) {
    cat("outside loop, terminated.\n")
    break
  }
}
cat(mulInv, "times evoked.\n")

# part B
b <- 93
c <- 0
d <- 0
e <- 0
f <- 0
g <- 0
h <- 0
b <- b * 100
b <- b + 100000
c <- b + 17000
done <- FALSE
while (TRUE) {
  f <- 1
  d <- 2
  e <- 2
  cat("\n", letters[c(2:8)], "\n")
  cat(eval(parse(text = paste("c(", paste(letters[c(2:8)], collapse = ", "), ")\n"))))
  while (TRUE) {
    if (b %% d == 0) {
      f <- 0
    }
    d <- d + 1
    if (d != b) {
      next
    }
    if (f == 0) {
      h <- h + 1
    }
    if (b == c) {
      done <- TRUE
      break
    }
    b <- b + 17
    break
  }
  if (done) {
    cat("\nh is:", h, "\n")
    break
  }
}
