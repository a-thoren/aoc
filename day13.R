input <- readLines("inputs/day13.txt")
input <- input[input != ""]

s <- 0

for (i in seq(1, length(input), 3)) {
  m <- stringr::str_extract_all(input[i + 0:1], "\\d+", simplify = TRUE) |> 
    apply(2, as.numeric) |> 
    t()
  
  ans <- stringr::str_extract_all(input[i + 2], "\\d+", simplify = TRUE) |> 
    as.numeric()
  
  solution <- solve(m, ans)

  if (isTRUE(all.equal(round(solution), solution))) {
    s <- s + (c(3, 1) * solution)
  }
}

sum(s)

s <- 0

t <- 1e13

for (i in seq(1, length(input), 3)) {
  m <- stringr::str_extract_all(input[i + 0:1], "\\d+", simplify = TRUE) |> 
    apply(2, as.numeric) |> 
    t()
  
  ans <- stringr::str_extract_all(input[i + 2], "\\d+", simplify = TRUE) |> 
    as.numeric()
  
  m_inv <- MASS::ginv(m)
  
  solution <- Rmpfr::mpfr(m_inv, 100) %*% (ans + t)
  if (all(gmp::is.whole(round(solution, 2)))) {
    s <- s + sum(c(3, 1) * round(solution, 2))
  }
}

sum(s)
