input <- readLines("inputs/day16.txt") |> 
  strsplit("") |> 
  do.call(what = rbind)

directions <- c(
  0 + 1i,
  1 + 0i,
  0 - 1i,
  -1 + 0i
)

pos <- which(input == "S", arr.ind = TRUE)
goal <- which(input == "E", arr.ind = TRUE)
dir <- directions[1]
pos <- pos[1] + pos[2] * 1i

q <- collections::priority_queue()
q$push(c(pos, dir))

score <- array(Inf, c(4, dim(input)))
score[1, Re(pos), Im(pos)] <- 0
valid <- which(input != "#", arr.ind = TRUE)
valid <- valid[, 1] + valid[, 2] * 1i
previous <- array(dim = c(2, 4, dim(input)))
previous <- collections::dict()

find_neighbors <- function(cur_pos, cur_dir) {
  neighbors <- matrix(c(cur_pos + cur_dir, cur_dir), ncol = 2)
  neighbors <- rbind(neighbors, matrix(c(rep(cur_pos, 3), directions[directions != cur_dir]), ncol = 2))
  return(neighbors)
}

while (TRUE) {
  current <- q$pop()
  cur_pos <- current[1]
  cur_dir <- current[2]
  if (cur_pos %in% valid) {
    cur_score <- score[which(directions == cur_dir), Re(cur_pos), Im(cur_pos)]
    if (cur_score >= min(score[, goal[1], goal[2]])) break
    neighbors <- find_neighbors(cur_pos, cur_dir)
    new_score <- cur_score + 1 * ifelse(cur_dir == neighbors[, 2], 1, 1000)
    for (i in seq(nrow(neighbors))) {
      neighbor <- neighbors[i, ]
      neighbor_score <- score[which(directions == neighbor[2]), Re(neighbor[1]), Im(neighbor[1])]
      if (new_score[i] <= neighbor_score) {
        if (!previous$has(c(neighbor[1], neighbor[2]))) {
          previous$set(c(neighbor[1], neighbor[2]), c(cur_pos, cur_dir))
        } else {
          previous$set(c(neighbor[1], neighbor[2]), list(previous$get(c(neighbor[1], neighbor[2])), c(cur_pos, cur_dir)))
        }
        if (new_score[i] < neighbor_score) {
          score[which(directions == neighbor[2]), Re(neighbor[1]), Im(neighbor[1])] <- new_score[i]
          q$push(neighbor, priority = -new_score[i])
        }
      }
    }
  }
}

min(score[, goal[1], goal[2]])

min_dir <- which.min(score[, goal[1], goal[2]])

path <- c()

stack <- collections::stack()
stack$push(c(goal[1] + goal[2] * 1i, directions[min_dir]))

while (stack$size() > 0) {
  p <- stack$pop()
  to_add <- c()
  if (inherits(p, "complex")) {
    p <- list(p)
  }
  for (p_item in p) {
    to_add <- c(to_add, p_item[1])
    if (previous$has(p_item)) {
      stack$push(previous$get(p_item))
    }
  }
  path <- c(path, to_add)
}

length(unique(path))