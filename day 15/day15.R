generatorA <- 873
generatorB <- 583
# generatorA <- 65 # nolint
# generatorB <- 8921 # nolint
factorA <- 16807
factorB <- 48271
remainder <- 2147483647

partB <- TRUE
on <- 1
remA <- generatorA
remB <- generatorB
matched <- 0
if (partB) {
  maxIter <- 5000000
} else {
  maxIter <- 40000000
}
newRoundA <- TRUE
newRoundB <- TRUE
startTime <- Sys.time()
while (on <= maxIter) {
  if (partB) {
    while (newRoundA | remA %% 4 != 0) {
      multiplyA <- remA * factorA
      remA <- multiplyA %% remainder
      newRoundA <- FALSE
    }
    while (newRoundB | remB %% 8 != 0) {
      multiplyB <- remB * factorB
      remB <- multiplyB %% remainder
      newRoundB <- FALSE
    }
  } else {
    multiplyA <- remA * factorA
    multiplyB <- remB * factorB
    remA <- multiplyA %% remainer
    remB <- multiplyB %% remainer
  }
  bitA <- as.integer(intToBits(remA))
  bitB <- as.integer(intToBits(remB))
  newRoundA <- TRUE
  newRoundB <- TRUE
  matched <- matched + as.numeric(all(bitA[c(1:16)] == bitB[c(1:16)]))
  on <- on + 1
  if (on %% 1000000 == 0) {
    cat("\n", on / 1000000, "million loops done.\n")
  }
}
endTime <- Sys.time()
cat("Bits have been matched", matched, "times.\n")
cat("It took", difftime(endTime, startTime, units = "mins"))
