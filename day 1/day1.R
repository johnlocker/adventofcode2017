captcha <- read.table(file.path("day 1", "captcha.txt"), colClasses = "character")$V1

calculateCaptcha <- function(captcha,
                             halfWay = FALSE) {
  getHalf <- function(captchaSplit) {
    half <- length(captchaSplit) / 2
    outputNum <- rep(0, length(captchaSplit))
    for (i in 1:length(captchaSplit)) {
      if (captchaSplit[i] == captchaSplit[ifelse( (i + half) > length(captchaSplit),
                                                 (i + half) - length(captchaSplit),
                                                 (i + half))]) {
        outputNum[i] <- captchaSplit[i]
      }
    }
    return(sum(outputNum))
  }
  captchaSplit <- as.numeric(strsplit(captcha, split = "")[[1]])
  if (!halfWay) {
    diffCap <- c(diff(captchaSplit), diff(c(tail(captchaSplit, 1),
                                            head(captchaSplit, 1))))
    output <- sum(captchaSplit[!as.logical(diffCap)])
  } else {
    output <- getHalf(captchaSplit)
  }
  return(output)
}
# first part
calculateCaptcha(captcha)
# second part
calculateCaptcha(captcha, halfWay = TRUE)
