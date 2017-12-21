dat <- readr::read_lines(file.path("day 21", "input.txt"))
# dat <- readr::read_lines(file.path("day 21", "test.txt")) # nolint

#' Function to make a matrix based on string
#' @param string String with matrix information
#' @return matrix
makeMatrix <- function(string) {
  stringSplit <- unlist(strsplit(string, ""))
  maCol <- which(stringSplit == "/") - 1
  return(matrix(stringSplit[-which(stringSplit == "/")], ncol = maCol, byrow = TRUE))
}

#' Function to make all rotated and flipped version of a matrix
#' @param pattern input matrix
#' @return list with all variations of input matrix
makeVariations <- function(pattern) {
  rotate <- function(x) t(apply(x, 2, rev))
  flipH <- function (x) apply(t(x), 1, rev)
  flipV <- function (x) t(apply(x, 1, rev))
  variationList <- list()
  variationList$input <- pattern
  variationList$transpose <- t(pattern)
  variationList$rotate1 <- rotate(pattern)
  variationList$rotate2 <- rotate(rotate(pattern))
  variationList$rotate3 <- rotate(rotate(rotate(pattern)))
  variationList$flip1 <- flipH(pattern)
  variationList$flip2 <- flipV(pattern)
  variationList$rotate1_flipH <- rotate(variationList$flip1)
  variationList$rotate2_flipH <- rotate(rotate(variationList$flip1))
  variationList$rotate3_flipH <- rotate(rotate(rotate(variationList$flip1)))
  variationList$rotate1_flipV <- rotate(variationList$flip2)
  variationList$rotate2_flipV <- rotate(rotate(variationList$flip2))
  variationList$rotate3_flipV <- rotate(rotate(rotate(variationList$flip2)))
  return(variationList)
}

#' Function to find correct enhancement rule and associated output pattern
#' @param inputPattern Input pattern
#' @param bookList list with all enhancement rules
#' @return output pattern based on enhancement rule
recPattern <- function(inputPattern, bookList) {
  outputList <- list()
  for (p in 1:length(inputPattern)) {
    beginOn <- sum(inputPattern[[p]] == "#")
    beginCol <- ncol(inputPattern[[p]])
    curBookListIdx <- sapply(bookList, function(x) x[["on"]] == beginOn & x[["ncols"]] == beginCol)
    curBookList <- bookList[curBookListIdx]
    beginVariations <- makeVariations(inputPattern[[p]])
    matched <- 0
    for (i in 1:length(curBookList)) {
      patternVar <- curBookList[[i]]$input
      if (any(sapply(beginVariations, function(x) identical(x, patternVar)))) {
        outputList[[p]] <- curBookList[[i]]$output
        matched <- matched + 1
        break
      }
    }
  }
  return(outputList)
}

#' Function to split matrix in 2-width and 3-width squares
#' @param output Matrix that should be splited
#' @return List of splited matrices
maSplit <- function(output) {
  if (ncol(output) %% 2 == 0) {
    splitLength <- 2
  } else {
    splitLength <- 3
  }
  matrixFill <- c(1:( (ncol(output) * ncol(output)) / (splitLength * splitLength)))
  k <- kronecker(matrix(matrixFill, ncol = ncol(output) / splitLength,
                        byrow = TRUE), matrix(1, splitLength, splitLength))
  return(lapply(split(output, k), matrix, nr = splitLength))
}

#' Function to join list of matrices to one matrix
#' @param output List of matrices that should be joined
#' @return Joined matrix based on input matrices list
joinMatrix <- function(output) {
  lengthRowSquares <- sqrt(length(output))
  if (lengthRowSquares == 1) {
    return(output[[1]])
  }
  rowList <- data.frame()
  for (i in 1:lengthRowSquares) {
    rowList <- rbind(rowList, do.call(cbind, output[cut(c(1:length(output)),
                                breaks = lengthRowSquares,
                                labels = c(1:lengthRowSquares)) == i]))
  }
  return(as.matrix(rowList))
}

# make enhancement rule list
bookList <- list()
for (i in 1:length(dat)) {
  book <- list()
  book$input <- makeMatrix(unlist(strsplit(dat[i], " => "))[1])
  book$output <- makeMatrix(unlist(strsplit(dat[i], " => "))[2])
  book$ncols <- ncol(makeMatrix(unlist(strsplit(dat[i], " => "))[1]))
  book$on <- sum(makeMatrix(unlist(strsplit(dat[i], " => "))[1]) == "#")
  bookList[[i]] <- book
}

inputPattern <- matrix(c(".", "#", ".",
                         ".", ".", "#",
                         "#", "#", "#"), ncol = 3, byrow = TRUE) # nolint
count <- 1
partA <- TRUE
maxIterations <- ifelse(partA, 5, 18)
while (count <= maxIterations) {
  if (!is.list(inputPattern)) {
    output <- recPattern(list(inputPattern), bookList)
  } else {
    output <- recPattern(inputPattern, bookList)
  }
  outputJoined <- joinMatrix(output)
  inputPattern <- maSplit(outputJoined)
  count <- count + 1
  cat("Iterations", count, "done.\n")
}
cat("Pixels on:", sum(sapply(output, function(x) sum(x == "#"))), "\n")
