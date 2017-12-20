dat <- readr::read_lines(file.path("day 20", "input.txt"))
#' Function to extract all numbers
#' @param string String with numbers
#' @return vector with all numbers from input string
numExtract <- function(string) {
  return(as.numeric(unlist(stringi::str_extract_all(string, "\\-*\\d+\\.*\\d*"))))
}
#' Function to update a particle
#' @param particle Particle data.frame with v, a and p column
#' @return updated particle data.frame
particleUpdate <- function(particle) {
  particle$v[1] <- particle$v[1] + particle$a[1]
  particle$v[2] <- particle$v[2] + particle$a[2]
  particle$v[3] <- particle$v[3] + particle$a[3]
  particle$p[1] <- particle$p[1] + particle$v[1]
  particle$p[2] <- particle$p[2] + particle$v[2]
  particle$p[3] <- particle$p[3] + particle$v[3]
  return(particle)
}
#' Function to calculate manhatten distance from (0, 0, 0) to particle position
#' @param string String with numbers
#' @return Manhatten distance between particle location and origin
particleDistance <- function(particle) {
  return(sum(abs(particle$p)))
}
#' Function to remove colliding particles from particleList
#' @param collisionDF data.frame with all particle locations
#' @return particleList without colliding particles
removeCollisions <- function(collisionDF, particleList) {
  while (sum(duplicated(collisionDF)) > 0) {
    duplicateRow <- collisionDF[which(duplicated(collisionDF))[1], ]
    validRows <- apply(collisionDF, 1,
                       function(x) !all(x == duplicateRow))
    particleList <- particleList[validRows]
    collisionDF <- collisionDF[validRows, ]
  }
  return(particleList)
}

particleList <- list()
for (i in 1:length(dat)) {
  particleSplit <- unlist(strsplit(dat[i], ", "))
  particleList[[i]] <- data.frame(p = numExtract(particleSplit[1]),
                                  v = numExtract(particleSplit[2]),
                                  a = numExtract(particleSplit[3]))
}

distVec <- numeric(length = length(particleList))
count <- 0
partA <- FALSE

maxCount <- 10 ^ 3
while (count < maxCount) {
  collisionDF <- matrix(NA, nrow = length(particleList), ncol = 3)
  for (j in 1:length(particleList)) {
    # update particle
    particleList[[j]] <- particleUpdate(particleList[[j]])
    if (partA) {
      distVec[j] <- particleDistance(particleList[[j]])
    } else {
      collisionDF[j, ] <- t(as.matrix(particleList[[j]]$p))
    }
  }
  if (!partA) {
    particleList <- removeCollisions(collisionDF, particleList)
  }
  count <- count + 1
  if (count %% (maxCount / 100) == 0) {
    if (partA) {
      cat("Done:", round(count / maxCount * 100), "%\n")
      cat("Smallest particles:", order(distVec)[c(1:5)] - 1, "\n")
      cat("Smallest distances:", sort(distVec)[c(1:5)], "\n")
    } else {
      cat("Left particles:", length(particleList), "\n")
    }
  }
}
if (partA) {
  cat("Smallest particles:", order(distVec)[c(1:5)] - 1, "\n")
  cat("Smallest distances:", sort(distVec)[c(1:5)], "\n")
} else {
  cat("Left particles:", length(particleList), "\n")
}
