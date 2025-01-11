input <- readLines("inputs/day18.txt") |> 
  stringr::str_extract_all("\\d+", simplify = TRUE) |> 
  apply(2, as.numeric)

n <- 71

pos <- 1 + 1i
goal <- n + n * 1i
directions <- c(
  0 + 1i,
  1 + 0i,
  0 - 1i,
  -1 + 0i
)
positions <- expand.grid(1:n, 1:n) |> 
  as.matrix()
positions <- positions[, 1] + positions[, 2] * 1i

find_path <- function(n_bytes = 1024) {
  
  bytes <- input[1:n_bytes, ]
  bytes <- (bytes[, 1] + 1) + (bytes[, 2] + 1) * 1i
  valid <- setdiff(positions, bytes)
  
  q <- collections::priority_queue()
  q$push(pos)
  score <- matrix(1e10, n, n)
  score[1, 1] <- 0
  previous <- matrix(nrow = n, ncol = n)
  
  while (TRUE) {
    if (q$size() == 0) return(NA)
    current <- q$pop()
    if (!current %in% valid) next
    s <- score[Re(current), Im(current)]
    neighbors <- current + directions
    new_s <- s + 1
    if (s >= min(score[Re(goal), Im(goal)])) break
    for (neighbor in neighbors) {
      if (!neighbor %in% valid) next
      nei_s <- score[Re(neighbor), Im(neighbor)]
      if (new_s <= nei_s) {
        previous[Re(neighbor), Im(neighbor)] <- current
        if (new_s < nei_s) {
          score[Re(neighbor), Im(neighbor)] <- new_s
          q$push(neighbor, priority = -new_s)
        }
      }
    }
  }
  
  path <- goal
  while (path[1] != pos) {
    path <- c(previous[Re(path[1]), Im(path[1])], path)
  }
  return(path)
}

length(find_path()) - 1

i <- 1024
bytes <- (input[, 1] + 1) + (input[, 2] + 1) * 1i
while (TRUE) {
  path <- find_path(i)
  if (!is.complex(path)) {
    paste0(input[i + 1, ], collapse = ",")
    break
  }
  idx <- which(bytes %in% path)
  i <- idx[idx > i][1]
}