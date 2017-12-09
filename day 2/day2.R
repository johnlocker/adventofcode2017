dat <- read.table(file.path("day 2", "input.txt"))

checkSum <- function(x) {
  return(max(x) - min(x))
}
sum(apply(dat, 1, checkSum))

checkSum2 <- function(x) {
  x <- sort(x, decreasing = TRUE)
  result <- numeric()
  for (i in 1:length(x)) {
    for (j in i:(length(x))) {
      result <- c(result, x[i] / x[j])
      result <- result[result > 1]
    }
  }
  return(result[result %% 1 == 0])
}
sum(apply(dat, 1, checkSum2))
