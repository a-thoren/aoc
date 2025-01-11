lines <- readLines("inputs/day6.txt") |> 
  strsplit("") |> 
  do.call(what = rbind)

modify_dir <- function(dir) {
  if (all(dir == c(0, -1))) { # west -> north
    return(c(-1, 0))
  }
  if (all(dir == c(-1, 0))) { # north -> east
    return(c(0, 1))
  } 
  if (all(dir == c(0, 1))) { # east -> south
    return(c(1, 0))
  } 
  if (all(dir == c(1, 0))) { #south -> west
    return(c(0, -1))
  } 
}

start_dir <- c(-1, 0)
start <- which(lines == "^", arr.ind = TRUE)
pos <- start
dir <- start_dir
modified_lines <- lines

while (TRUE) {
  modified_lines[pos] <- "X"
  new_pos <- pos + dir
  if (any(new_pos > dim(modified_lines) | new_pos < 1)) {
    break
  }
  if (modified_lines[new_pos] == "#") {
    dir <- modify_dir(dir)
    new_pos <- pos + dir
  }
  pos <- new_pos
}

sum(modified_lines == "X")

move <- function(map, pos, dir) {
  new_pos <- pos + dir
  while (valid_pos(map, new_pos) && map[new_pos] == "#") {
    dir <- modify_dir(dir)
    new_pos <- pos + dir
  }
  return(list(pos = new_pos, dir = dir))
}

valid_pos <- function(map, pos) {
  return(!any(pos > dim(map) | pos < 1))
}

is_loop <- function(map) {
  modified_map <- map
  tortoise_move <- move(map, start, start_dir)
  tortoise_pos <- tortoise_move[["pos"]]
  tortoise_dir <- tortoise_move[["dir"]]
  hare_move <- move(map, start, start_dir)
  hare_pos <- hare_move[["pos"]]
  hare_dir <- hare_move[["dir"]]
  hare_move <- move(map, hare_pos, hare_dir)
  hare_pos <- hare_move[["pos"]]
  hare_dir <- hare_move[["dir"]]
  
  while (!all(hare_pos == tortoise_pos & hare_dir == tortoise_dir)) {
    modified_map[hare_pos[1], hare_pos[2]] <- "h"
    tortoise_move <- move(map, tortoise_pos, tortoise_dir)
    tortoise_pos <- tortoise_move[["pos"]]
    tortoise_dir <- tortoise_move[["dir"]]
    if (!valid_pos(map, tortoise_pos)) {
      return(FALSE)
    }
    for (i in 1:2) {
      hare_move <- move(map, hare_pos, hare_dir)
      hare_pos <- hare_move[["pos"]]
      hare_dir <- hare_move[["dir"]]
      if (!valid_pos(map, hare_pos)) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

n_loops <- 0
pos <- which(modified_lines == "X", arr.ind = TRUE)
for (i in seq(nrow(pos))) {
  if (i %% 25 == 0) message(i)
  p <- pos[i, ]
  modified_lines <- lines
  modified_lines[p[1], p[2]] <- "#"
  
  if (is_loop(modified_lines)) {
    n_loops <- n_loops + 1
  }
  
}
n_loops

