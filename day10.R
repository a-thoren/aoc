input <- readLines("inputs/day10.txt") |> 
  strsplit("") |> 
  sapply(as.numeric)

input_test <- readLines("inputs/day10_test.txt") |> 
  strsplit("") |> 
  sapply(as.numeric)

directions <- list(
  c(0, 1),
  c(1, 0),
  c(0, -1),
  c(-1, 0)
)

valid_pos <- function(matrix, pos) {
  return(!any(pos > dim(matrix) | pos < 1))
}

find_trailhead <- function(pos, input, visits) {
  if (input[pos[1], pos[2]] == 0) {
    visits <- mutate(
      visits,
      n = if_else(
        row == pos[1] & col == pos[2],
        n + 1,
        n
      )
    )
    return(visits)
  }
  for (dir in directions) {
    new_pos <- pos + dir
    if (valid_pos(input, new_pos) && input[new_pos[1], new_pos[2]] == input[pos[1], pos[2]] - 1) {
      visits <- find_trailhead(new_pos, input, visits)
    }
  }
  return(visits)
}

start <- which(input == 9, arr.ind = TRUE)
visits <- as.data.frame(which(input == 0, arr.ind = TRUE)) |> 
  mutate(
    n = 0,
    rating = 0
  )
for (i in seq(nrow(start))) {
  visits_tmp <- as.data.frame(which(input == 0, arr.ind = TRUE)) |> 
    mutate(n = 0)
  visits_tmp <- find_trailhead(start[i, ], input, visits_tmp)
  visits$n <- visits$n + (visits_tmp$n > 0)
  visits$rating <- visits$rating + visits_tmp$n
}

sum(visits$n)
sum(visits$rating)