rm(list = ls())
dat <- readr::read_lines(file.path("day 8", "input.txt"))
#dat <- readr::read_lines(file.path("day 8", "test.txt")) # nolint
library("dplyr")
library("stringr")
modFunction <- function(x) {
  return(str_replace_all(x, c("inc" = "+", "dec" = "-")))
}
charExtract <- function(x) {
  return(unlist(str_extract_all(x, "[a-z]+")))
}
c <- q <- NA

registerVec <- character()
maxVal <- 0

for (i in 1:length(dat)) {
  instruction <- dat[i]
  inSplit <- unlist(strsplit(instruction, split = " if "))
  mod <- modFunction(inSplit[1])
  cond <- inSplit[2]
  if (!exists(charExtract(mod))) {
    assign(charExtract(mod), 0)
  }
  if (is.na(eval(parse(text = charExtract(mod))))) {
    assign(charExtract(mod), 0)
  }
  if (!exists(charExtract(cond))) {
    assign(charExtract(cond), 0)
  }
  if (is.na(eval(parse(text = charExtract(cond))))) {
    assign(charExtract(cond), 0)
  }
  if (!charExtract(mod) %in% registerVec) {
    registerVec <- c(registerVec, charExtract(mod))
  }
  if (!charExtract(cond) %in% registerVec) {
    registerVec <- c(registerVec, charExtract(cond))
  }
  # process instructions
  condEval <- eval(parse(text = cond))
  if (condEval) {
    eval(parse(text = paste0(charExtract(mod), " <- ", mod)))
  }
  curVal <- max(eval(parse(text = paste0("c(",
                                         paste(registerVec, collapse = ", "), ")"))))
  maxVal <- ifelse(curVal > maxVal, curVal, maxVal)
}
registerVec[which.max(eval(parse(text = paste0("c(", paste(registerVec, collapse = ", "), ")"))))]
maxVal
