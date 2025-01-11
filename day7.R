lines <- readLines("inputs/day7.txt")


found <- c()
s <- 0
for (i in seq(lines)) {
  line <- lines[[i]]
  numbers <- stringr::str_extract_all(line, "\\d+", simplify = TRUE) |> as.numeric()
  ans <- numbers[1]
  ops <- numbers[-1]
  reduced <- Reduce(\(x, y) c(x + y, x * y), ops)
  if (any(reduced == ans)) {
    found <- c(found, i)
    s <- s + ans
  }
}

s_2 <- 0
for (i in setdiff(seq(lines), found)) {
  line <- lines[[i]]
  numbers <- stringr::str_extract_all(line, "\\d+", simplify = TRUE) |> as.numeric()
  ans <- numbers[1]
  ops <- numbers[-1]
  reduced <- Reduce(\(x, y) c(x + y, x * y, x * 10^(floor(log10(y)) + 1) + y), ops)
  if (any(reduced == ans)) {
    s_2 <- s_2 + ans
  }
}

message(s + s_2)
