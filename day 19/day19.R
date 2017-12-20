grid <- readr::read_lines(file.path("day 19", "input.txt"))
# grid <- readr::read_lines(file.path("day 19", "test.txt")) # nolint

maGrid <- as.matrix(grid)
grid <- matrix(NA, nrow = nrow(maGrid), ncol = length(unlist(strsplit(maGrid[1, ], ""))))
for (i in 1:nrow(maGrid)) {
  grid[i, ] <- unlist(strsplit(maGrid[i, ], ""))
}

#' Function to determine new location
#' @param grid Grid where to move in
#' @param location Current location of plus sign
#' @param lastDirection Last direction we moved to
#' @return new direction to move to
getNewDirection <- function(grid, location, lastDirection) {
  locDF <- data.frame(r = c(location[1] - 1,
                            location[1],
                            location[1] + 1,
                            location[1]),
                      c = c(location[2],
                            location[2] + 1,
                            location[2],
                            location[2] - 1),
                      dir = c("u", "r", "d", "l"),
                      stringsAsFactors = FALSE)
  if (lastDirection == "d") {
    locDF <- locDF[-which.min(locDF$r), ]
  }
  if (lastDirection == "u") {
    locDF <- locDF[-which.max(locDF$r), ]
  }
  if (lastDirection == "l") {
    locDF <- locDF[-which.max(locDF$c), ]
  }
  if (lastDirection == "r") {
    locDF <- locDF[-which.min(locDF$c), ]
  }
  if (lastDirection == "d" | lastDirection == "u") {
    for (j in 1:nrow(locDF)) {
      if (grid[locDF[j, 1], locDF[j, 2]] != " ") {
        return(locDF$dir[j])
      }
    }
  }
  if (lastDirection == "r" | lastDirection == "l") {
    for (j in 1:nrow(locDF)) {
      if (grid[locDF[j, 1], locDF[j, 2]] != " ") {
        return(locDF$dir[j])
      }
    }
  }
}

direction <- "d"
lastLoc <- c(1, which(grid[1, ] == "|"))
pathContinues <- TRUE
collectLetter <- c()
count <- 0
while (pathContinues) {
  if (direction == "d") {
    curLoc <- lastLoc
    curLoc[1] <- curLoc[1] + 1
  }
  if (direction == "u") {
    curLoc <- lastLoc
    curLoc[1] <- curLoc[1] - 1
  }
  if (direction == "l") {
    curLoc <- lastLoc
    curLoc[2] <- curLoc[2] - 1
  }
  if (direction == "r") {
    curLoc <- lastLoc
    curLoc[2] <- curLoc[2] + 1
  }
  count <- count + 1
  if (curLoc[1] == 0 | curLoc[2] == 0) {
    cat("Done.")
    pathContinues <- FALSE
    break
  }
  if (grid[curLoc[1], curLoc[2]] == " ") {
    cat("Done.")
    pathContinues <- FALSE
  }
  if (grid[curLoc[1], curLoc[2]] == "+") {
    direction <- getNewDirection(grid, curLoc, lastDirection = direction)
  }
  if (grid[curLoc[1], curLoc[2]] %in% toupper(letters)) {
    collectLetter <- c(collectLetter, grid[curLoc[1], curLoc[2]])
  }
  lastLoc <- curLoc
}
cat("Collected letters are: ", paste(collectLetter, collapse = ""), "\n")
cat("Steps taken:", count)
