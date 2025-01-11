input <- readLines("inputs/day20.txt") |> 
  strsplit("") |> 
  do.call(what = rbind)


pos <- which(input == "S", arr.ind = TRUE)
pos <- pos[1] + pos[2] * 1i
goal <- which(input == "E", arr.ind = TRUE)
goal <- goal[1] + goal[2] * 1i
n <- nrow(input)

directions <- c(
  0 + 1i,
  1 + 0i,
  0 - 1i,
  -1 + 0i
)

valid <- which(input != "#", arr.ind = TRUE)
valid <- valid[, 1] + valid[, 2] * 1i

path <- pos
while (path[1] != goal) {
  neighbors <- path[1] + directions
  path <- c(neighbors[neighbors %in% valid & !neighbors %in% path], path)
}

cache <- collections::dict()
for (i in seq_along(path)) {
  cache$set(path[i], i)
}

path <- rev(path)

find_cheat_paths <- function(n_cheats) {
  paths <- collections::dict()
  for (i in 1:(length(path) - 1)) {
    dist <- abs(Re(valid) - Re(path[i])) + abs(Im(valid) - Im(path[i]))
    idx <- dist <= n_cheats
    cheats <- valid[idx]
    for (j in seq_along(cheats)) {
      paths$set(c(path[i], cheats[j]), cache$get(path[i]) - cache$get(cheats[j]) - dist[idx[j]])
    }
  }
  return(paths)
}

paths <- find_cheat_paths(n_cheats = 2)
saved <- unlist(paths$values())
sum(length(saved[saved >= 100]))

paths <- find_cheat_paths(n_cheats = 20)
saved <- unlist(paths$values())
sum(length(saved[saved >= 100]))




path_df <- data.frame(pos = path) |> 
  dplyr::mutate(dist_from_goal = dplyr::n():1)
path_df <- path_df |> 
  dplyr::cross_join(path_df) |> 
  dplyr::mutate(
    dist = abs(Re(pos.x) - Re(pos.y)) + abs(Im(pos.x) - Im(pos.y)),
    saved = dist_from_goal.x - dist_from_goal.y - dist
  )

path_df |> 
  dplyr::filter(
    dist <= 2,
    saved >= 100
  ) |> 
  nrow()

path_df |> 
  dplyr::filter(
    dist <= 20,
    saved >= 100
  ) |> 
  nrow()
