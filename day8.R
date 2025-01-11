input <- readLines("inputs/day8.txt") |>
  strsplit("") |>
  do.call(what = rbind)

antenna <- input |>
  as.vector() |>
  unique()
antenna <- antenna[-which(antenna == ".")]
antinodes <- list()
antenna_map <- input

valid_pos <- function(matrix, pos) {
  return(!any(pos > dim(matrix) | pos < 1))
}

for (ant in antenna) {
  idx <- which(input == ant, arr.ind = TRUE)
  for (i in seq(nrow(idx))) {
    for (j in seq(nrow(idx))) {
      if (i == j) next
      dist <- idx[i, ] - idx[j, ]
      pos <- idx[i, ] + dist
      if (valid_pos(input, pos)) {
        antinodes <- c(antinodes, list(pos))
        antenna_map[pos[1], pos[2]] <- "#"
      }
    }
  }
}

antinodes |>
  unique() |>
  length()

antinodes <- list()
for (ant in antenna) {
  idx <- which(input == ant, arr.ind = TRUE)
  for (i in seq(nrow(idx))) {
    antinodes <- c(antinodes, list(idx[i, ]))
    for (j in seq(nrow(idx))) {
      if (i == j) next
      dist <- idx[i, ] - idx[j, ]
      pos <- idx[i, ] + dist
      while (valid_pos(input, pos)) {
        antinodes <- c(antinodes, list(pos))
        antenna_map[pos[1], pos[2]] <- "#"
        pos <- pos + dist
      }
    }
  }
}

antinodes |>
  unique() |>
  length()
