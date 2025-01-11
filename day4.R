library(tidyverse)

lines <- readLines("inputs/day4.txt") |> lapply(\(x) strsplit(x, "")[[1]])

lines <- do.call(rbind, lines)

s <- 0
xmas <- c("X", "M", "A", "S")
d <- dim(lines)[1]

antidiag <- function(matrix) {
  out <- c()
  for (i in 1:dim(matrix)[1]) {
    out <- c(out, matrix[i, dim(matrix)[1] - i + 1])
  }
  return(out)
}


for (i in 1:d) {
  for (j in 1:(d - 3)) {
    line <- lines[i, j:(j + 3)]
    col <- lines[j:(j + 3), i]
    s <- s + all(line == xmas) + all(rev(line) == xmas)
    s <- s + all(col == xmas) + all(rev(col) == xmas)
    }
}

for (i in 1:(d - 3)) {
  for (j in 1:(d - 3)) {
    diag <- lines[i:(i + 3), j:(j + 3)] |> diag()
    antid <- lines[i:(i + 3), j:(j + 3)] |> antidiag()
    s <- s + all(diag == xmas) + all(rev(diag) == xmas)
    s <- s + all(antid == xmas) + all(rev(antid) == xmas)
  }
}

s

# part 2
s <- 0
mas <- c("M", "A", "S")
for (i in 2:(d - 1)) {
  for (j in 2:(d - 1)) {
    sub <- lines[(i - 1):(i + 1), (j - 1):(j + 1)]
    diag <- diag(sub)
    antid <- antidiag(sub)
    diag_found <- (all(diag == mas) | all(rev(diag) == mas))
    antidiag_found <- (all(antid == mas) | all(rev(antid) == mas))
    s <- s + as.numeric(diag_found && antidiag_found)
  }
}

s
