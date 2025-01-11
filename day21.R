input <- readLines("inputs/day21.txt") |> 
  strsplit("")

nums <- sapply(
  input,
  \(x) paste0(x, collapse = "") |> 
    stringr::str_extract_all(pattern = "\\d+") |> 
    as.numeric()
)

numeric_keypad <- matrix(c(1:9, "#", 0, "A"), 4, 3, byrow = TRUE)
numeric_keypad[1, ] <- numeric_keypad[3, ]
numeric_keypad[3, ] <- 1:3

directional_keypad <- matrix(c("#", "^", "A", "<", "v", ">"), 2, 3, byrow = TRUE)

arr_ind_to_cplx <- function(arr_ind) {
  return(arr_ind[, 1] + arr_ind[, 2] * 1i)
}

get_move <- function(start, goal, map) {
  invalid <- which(map == "#", arr.ind = TRUE, useNames = FALSE)
  invalid <- arr_ind_to_cplx(invalid)
  start <- arr_ind_to_cplx(which(map == start, arr.ind = TRUE, useNames = FALSE))
  goal <- arr_ind_to_cplx(which(map == goal, arr.ind = TRUE, useNames = FALSE))
  diff <- goal - start
  hor <- rep(ifelse(Im(diff) < 0, "<", ">"), abs(Im(diff)))
  ver <- rep(ifelse(Re(diff) < 0, "^", "v"), abs(Re(diff)))
  
  if (Re(start) + Im(goal) * 1i == invalid) {
    return(c(ver, hor, "A"))
  } else if (Im(start) * 1i + Re(goal) == invalid) {
    return(c(hor, ver, "A"))
  } else if (Im(diff) < 0) {
    return(c(hor, ver, "A"))
  } else {
    return(c(ver, hor, "A"))
  }
}

generate_path <- function(input, map, add_start = FALSE) {
  if (add_start) {
    input <- c("A", input)
  }
  res <- c()
  for (i in 2:length(input)) {
    res <- c(res, get_move(input[i - 1], input[i], map))
  }
  return(res)
}

path_cache <- collections::dict()

recurse_path <- function(path, i) {
  if (path_cache$has(list(path, i))) {
    return(path_cache$get(list(path, i)))
  }
  
  if (i == 1) {
    path <- generate_path(path, directional_keypad, TRUE)
    res <- length(path)
  } else {
    res <- 0
    tmp_path <- c("A", path)
    for (j in 2:length(tmp_path)) {
      p <- generate_path(tmp_path[c(j - 1, j)], directional_keypad)
      res <- res + recurse_path(p, i - 1)
    }
  }
  
  path_cache$set(list(path, i), res)
  return(res)
}


solve_path <- function(input, n_keypads) {
  p <- generate_path(input, numeric_keypad, TRUE)
  res <- recurse_path(p, n_keypads)
  return(res)
}

solve_path(input[[1]], 2)

p1 <- sum(sapply(input, solve_path, n_keypads = 2) * nums)
p2 <- sum(sapply(input, solve_path, n_keypads = 25) * nums)
