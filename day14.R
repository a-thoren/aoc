input <- readLines("inputs/day14.txt") |> 
  stringr::str_extract_all("[-]{0,1}\\d+")

positions <- sapply(input, \(x) as.numeric(x[1]) + as.numeric(x[2]) * 1i)
velocities <- sapply(input, \(x) as.numeric(x[3]) + as.numeric(x[4]) * 1i)
dim <- c(101, 103)

plot_positions <- function() {
  m <- matrix(".", dim[1], dim[2])
  
  for (p in positions) {
    if (m[Re(p) + 1, Im(p) + 1] == ".") {
      m[Re(p) + 1, Im(p) + 1] <- "1"
    } else {
      m[Re(p) + 1, Im(p) + 1] <- as.character(as.numeric(m[Re(p) + 1, Im(p) + 1]) + 1)
    }
  }
  
  print(t(m))
}
i <- 1

directions <- expand.grid(r = -2:2, c = -2:2) |> 
  dplyr::mutate(dir = r + c * 1i) |> 
  dplyr::filter(!(r == 0 & c == 0))
found <- FALSE
while (!found) {
  for (j in seq(positions)) {
    new_pos <- positions[j] + velocities[j]
    positions[j] <- (Re(new_pos) %% dim[1]) + (Im(new_pos) %% dim[2]) * 1i
  }
  if (i == 100) {
    quadrants <- rep(0, 4)
    
    for (robot in positions) {
      if (Re(robot) < floor(dim[1] / 2)) {
        if (Im(robot) < floor(dim[2] / 2)) {
          quadrants[1] <- quadrants[1] + 1
        } else if (Im(robot) > floor(dim[2] / 2)) {
          quadrants[2] <- quadrants[2] + 1
        }
      } else if (Re(robot) > floor(dim[1] / 2)) {
        if (Im(robot) < floor(dim[2] / 2)) {
          quadrants[3] <- quadrants[3] + 1
        } else if (Im(robot) > floor(dim[2] / 2)) {
          quadrants[4] <- quadrants[4] + 1
        }
      }
    }
    
    prod(quadrants)
  }
  
  for (pos in positions) {
    if (all((pos + directions$dir) %in% positions)) {
      print(i)
      found <- TRUE
    }
  }
  
  i <- i + 1
}




