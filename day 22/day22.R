dat <- readr::read_lines(file.path("day 22", "input.txt"))
#dat <- readr::read_lines(file.path("day 22", "test.txt")) # nolint

#' Function to get new direction
#' @param virCarDir Current direction
#' @param currentNode State of current node
#' @param pA Boolian, if part A or part B of task
#' @return updated direction
getNewDirection <- function(virCarDir, currentNode, pA = partA) {
  if (!pA) {
    if (currentNode == "W") {
      return(virCarDir)
    }
    if (currentNode == "F") {
      if (virCarDir == "u") return("d")
      if (virCarDir == "d") return("u")
      if (virCarDir == "l") return("r")
      if (virCarDir == "r") return("l")
    }
  }
  changeDir <- ifelse(currentNode == ".", "left", "right")
  if (virCarDir == "u") {
    return(ifelse(changeDir == "left", "l", "r"))
  }
  if (virCarDir == "d") {
    return(ifelse(changeDir == "left", "r", "l"))
  }
  if (virCarDir == "l") {
    return(ifelse(changeDir == "left", "d", "u"))
  }
  if (virCarDir == "r") {
    return(ifelse(changeDir == "left", "u", "d"))
  }
}

#' Function to get new location
#' @param newDir Current direction
#' @param virCarLoc Current location
#' @return new carrier location
carrierMove <- function(newDir, virCarLoc) {
  if (newDir == "l") {
    virCarLoc[2] <- virCarLoc[2] - 1
  }
  if (newDir == "r") {
    virCarLoc[2] <- virCarLoc[2] + 1
  }
  if (newDir == "u") {
    virCarLoc[1] <- virCarLoc[1] - 1
  }
  if (newDir == "d") {
    virCarLoc[1] <- virCarLoc[1] + 1
  }
  return(virCarLoc)
}

#' Function to extend grid
#' @param grid Current grid
#' @param virCarLoc Current location of virus carrier
#' @return extended grid with correct location
correctGrid <- function(grid, virCarLoc) {
  if (virCarLoc[1] < 1) {
    grid <- rbind(rep(".", ncol(grid)), grid)
    virCarLoc[1] <- 1
  }
  if (virCarLoc[1] > nrow(grid)) {
    grid <- rbind(grid, rep(".", ncol(grid)))
  }
  if (virCarLoc[2] < 1) {
    grid <- cbind(rep(".", nrow(grid)), grid)
    virCarLoc[2] <- 1
  }
  if (virCarLoc[2] > ncol(grid)) {
    grid <- cbind(grid, rep(".", nrow(grid)))
  }
  return(list(newGrid = grid,
              newVirCarLoc = virCarLoc))
}

#' Function to mark location in grid
#' @param grid Current grid
#' @param virCarLoc Current location of virus carrier
#' @return returns new state of location
markGrid <- function(grid, virCarLoc) {
  return(ifelse(grid[virCarLoc[1], virCarLoc[2]] == ".", "#", "."))
}

#' Function to show grid for debugging
#' @param grid Current grid
#' @param virCarLoc Current location of virus carrier
#' @return prints the grid with marked current location
showGrid <- function(grid, virCarLoc) {
  showGrid <- grid
  maxGrid <- virCarLoc + 5
  minGrid <- virCarLoc - 5
  showGrid[virCarLoc[1], virCarLoc[2]] <- paste0("[", showGrid[virCarLoc[1], virCarLoc[2]], "]")
  print(showGrid[c(minGrid[1]:maxGrid[1]), c(minGrid[2]:maxGrid[2])])
}

#' Function to get new state of location
#' @param stateMa Locations with state of location
#' @param virCarLoc Current location of virus carrier
#' @return new state of location
checkMa <- function(stateMa, virCarLoc) {
  idx <- which(virCarLoc[1] == stateMa[, 1] & virCarLoc[2] == stateMa[, 2])
  return(ifelse(length(idx) == 0, ".", c("#", "W", "F")[stateMa[idx, 3]]))
}

grid <- matrix(NA, ncol = length(unlist(strsplit(dat[1], ""))),
               nrow = length(dat))
for (i in 1:length(dat)) {
  grid[i, ] <- unlist(strsplit(dat[i], ""))
}

# make infection ma
stateMa <- matrix(NA, ncol = 3, nrow = sum(grid == "#"))
stateGroupSize <- 5
stateList <- list()
i <- 1
for (co in 1:ncol(grid)) {
  for (ro in 1:nrow(grid)) {
    if (grid[ro, co] == "#") {
      stateMa[i, ] <- c(ro, co, 1)
      listKey <- paste0(floor(ro / stateGroupSize) * stateGroupSize, "-",
                        (floor(ro / stateGroupSize) * stateGroupSize) + stateGroupSize)
      if (is.null(stateList[[listKey]])) {
        stateList[[listKey]] <- matrix(c(ro, co, 1), ncol = 3)
      } else {
        stateList[[listKey]] <- rbind(stateList[[listKey]], matrix(c(ro, co, 1), ncol = 3))
      }
      i <- i + 1
    }
  }
}

virCarStartLoc <- virCarLoc <- c(ceiling(nrow(grid) / 2), ceiling(ncol(grid) / 2))
virCarDir <- "u"
bursts <- 0
infection <- 0
maxBursts <- 10 ^ 4
while (bursts < maxBursts)  {
  # new direction
  virCarDir <- getNewDirection(virCarDir, grid[virCarLoc[1], virCarLoc[2]], pA = TRUE)
  # change node
  if (grid[virCarLoc[1], virCarLoc[2]] == ".") {
      infection <- infection + 1
  }

  grid[virCarLoc[1], virCarLoc[2]] <- markGrid(grid, virCarLoc)
  # move node
  virCarLoc <- carrierMove(virCarDir, virCarLoc)
  # extend grid if necessary
  correctedVars <- correctGrid(grid, virCarLoc)
  grid <- correctedVars$newGrid
  virCarLoc <- correctedVars$newVirCarLoc
  bursts <- bursts + 1
  # showGrid(grid, virCarLoc) # nolint
  if (bursts %% (maxBursts / 100) == 0) {
    cat("Done", round( (bursts / maxBursts) * 100), "%\n")
  }
}
cat("There were", bursts, "bursts.\n")
cat(infection, "of them caused an infection.")

# part B
virCarLoc <- virCarStartLoc
virCarDir <- "u"
currentNode <- checkMa(stateMa, virCarLoc)
infection <- 0
bursts <- 0
maxBursts <- 10 ^ 7
# 1 is infection
# 2 is weakeing
# 3 is flagged
startTime <- Sys.time()
oldCurCat <- floor(virCarLoc[1] / stateGroupSize) * stateGroupSize
listKey <- paste0(floor(virCarLoc[1] / stateGroupSize) * stateGroupSize, "-",
                  (floor(virCarLoc[1] / stateGroupSize) * stateGroupSize) + stateGroupSize)
while (bursts < maxBursts)  {
  # new direction
  virCarDir <- getNewDirection(virCarDir, currentNode, pA = FALSE)
  # count infections
  if (currentNode == "W") {
    infection <- infection + 1
    stateMa[virCarLoc[1] == stateMa[, 1] & virCarLoc[2] == stateMa[, 2], 3] <- 1
  }
  if (currentNode == "F") {
    # remove flag
    stateMa <- stateMa[-which(virCarLoc[1] == stateMa[, 1] & virCarLoc[2] == stateMa[, 2]), ]
  }
  if (currentNode == ".") {
    # add to weakening
    stateMa <- rbind(c(virCarLoc, 2), stateMa)
  }
  if (currentNode == "#") {
    stateMa[virCarLoc[1] == stateMa[, 1] & virCarLoc[2] == stateMa[, 2], 3] <- 3
  }
  # move node
  virCarLoc <- carrierMove(virCarDir, virCarLoc)
  # get curStateMa
  if (oldCurCat != floor(virCarLoc[1] / stateGroupSize) * stateGroupSize) {
    oldCurCat <- floor(virCarLoc[1] / stateGroupSize) * stateGroupSize
    stateList[[listKey]] <- stateMa
    listKey <- paste0(floor(virCarLoc[1] / stateGroupSize) * stateGroupSize, "-",
                      (floor(virCarLoc[1] / stateGroupSize) * stateGroupSize) + stateGroupSize)
    stateMa <- stateList[[listKey]]
  }
  # get new current node
  currentNode <- checkMa(stateMa, virCarLoc)
  bursts <- bursts + 1
  if (bursts %% (maxBursts / 100) == 0) {
    endTime <- Sys.time()
    cat("Done", round( (bursts / maxBursts) * 100), "%\n")
    cat("Percentage duration", difftime(endTime, startTime, units = "secs"), "\n")
    startTime <- Sys.time()
  }
}
cat("There were", bursts, "bursts.\n")
cat(infection, "of them caused an infection.")
