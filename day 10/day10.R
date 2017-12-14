readIn_lengths <- readr::read_lines(file.path("day 10", "input.txt"))

#lengths <- c(3, 4, 1, 5) # nolint
ring <- c(0:255)
#ring <- c(0, 1, 2, 3, 4) # nolint

partB <- TRUE
# readIn_lengths <- "AoC 2017" # nolint
# readIn_lengths <- "" # nolint
# readIn_lengths <- "1,2,3" # nolint
makeByte <- function(x) {
  x <- unlist(strsplit(x, split = ""))
  patNums <- as.character(c(",", "-", " ", 0:9, letters, toupper(letters)))
  repNums <- as.character(c("44", "45", 32, 48:57, 97:122, 65:90))
  for (j in 1:length(patNums)) {
    x[x == patNums[j]] <- gsub(patNums[j], repNums[j], x[x == patNums[j]])
  }
  return(x)
}

if (partB) {
  lengthsExtra <- c(makeByte(readIn_lengths), 17, 31, 73, 47, 23)
} else {
  lengthsExtra <- as.numeric(unlist(strsplit(readIn_lengths, split = ",")))
}

skipSize <- 0
curPosition <- 1
startLength <- lengthsExtra
lengths <- as.numeric(startLength)
if (partB) {
  rounds <- 64
} else {
  rounds <- 1
}

for (r in 1:rounds) {
  for (i in 1:length(lengths)) {
    startIndx <- curPosition
    endIndx <- curPosition + (lengths[i] - 1)
    if (endIndx > length(ring)) {
      indices <- c(curPosition:length(ring), c(1:(endIndx - length(ring))))
    } else {
      indices <- c(startIndx:endIndx)
    }
    subRing <- ring[indices]
    subRing <- rev(subRing)
    ring[indices] <- subRing
    # movement
    movement <- (lengths[i] + skipSize) %% length(ring)
    skipSize <- skipSize + 1
    curPosition <- curPosition + movement
    curPosition <- ifelse(curPosition > length(ring), curPosition %% length(ring), curPosition)
  }
}
if (!partB) {
  cat("The multiplier is:", ring[1] * ring[2])
}

makeDenseRing <- function(x) {
  curBit <- intToBits(x[1])
  for (i in 1:(length(x) - 1)) {
    nextBit <- intToBits(x[i + 1])
    curBit <- xor(curBit, nextBit)
  }
  return(packBits(curBit, type = "integer"))
}

denseRing <- unlist(lapply(split(ring, ceiling(seq_along(ring) / 16)), makeDenseRing))

makeHexRing <- function(x) {
  hex <- as.character(as.hexmode(x))
  returnHex <- ifelse(nchar(hex) == 1, paste0("0", hex), hex)
  return(paste(returnHex, collapse = ""))
}

hexRing <- paste(sapply(denseRing, makeHexRing), collapse = "")
hexRing
