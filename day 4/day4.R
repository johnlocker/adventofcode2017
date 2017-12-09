dat <- readr::read_lines(file.path("day 4", 'input.txt'))

validPassphrase <-  function(dat, part1 = TRUE) {
  dupVec <- vector(length = length(dat))
  for (i in 1:length(dat)) {
    row <- unlist(strsplit(dat[i], " "))
    if (!part1) {
      row <- sapply(row, function(x) paste(sort(strsplit(x, split = "")[[1]]), collapse = ""))
    }
    dupVec[i]  <- any(duplicated(row))
  }
  return(!dupVec)
}

table(validPassphrase(dat))
table(validPassphrase(dat, part1 = FALSE))