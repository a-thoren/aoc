input <- readLines("inputs/day12_test.txt") |> 
  strsplit("") |> 
  do.call(what = rbind)

valid_pos <- function(matrix, pos) {
  p <- c(Re(pos), Im(pos))
  return(all(p > 0 & p <= dim(matrix)))
}


directions <- c(
  0 + 1i,
  1 + 0i,
  0 - 1i,
  -1 + 0i
)

indices <- expand.grid(1:dim(input)[1], 1:dim(input)[2])
indices <- indices$Var1 + indices$Var2 * 1i

groups <- list()

recurse <- function(pos, target) {
  if (valid_pos(input, pos) && (pos %in% indices) && input[Re(pos), Im(pos)] == target) {
    indices <<- indices[-which(indices == pos)]
    r <- unlist(sapply(pos + directions, recurse, target = target))
    return(c(pos, r))
  } else {
    return(NULL)
  }
}

while (length(indices) > 0) {
  i <- indices[1]
  val <- input[Re(i), Im(i)]
  group <- recurse(i, target = val) |> list()
  group <- setNames(group, val)
  groups <- c(groups, group)
}

calculate_perimeter <- function(group) {
  perimeter <- c()
  target <- input[Re(group[1]), Im(group[1])]
  for (pos in group) {
    for (x in pos + directions) {
      if (!valid_pos(input, x) || input[Re(x), Im(x)] != target) {
        perimeter <- c(perimeter, x)
      }
    }
  }
  return(perimeter)
}

perimeters <- sapply(groups, calculate_perimeter)

sum(sapply(groups, length) * sapply(perimeters, length))

calculate_sides <- function(group) {
  p <- expand.grid(pos = (group), dir = directions) |> 
    dplyr::mutate(perimeter = pos + dir) |> 
    dplyr::distinct(perimeter, dir, .keep_all = TRUE) |> 
    dplyr::filter(!perimeter %in% pos) |> 
    dplyr::mutate(rotated_perimeter = pos + dir * 1i)
  
  p <- p |> 
    dplyr::anti_join(
      p,
      by = dplyr::join_by(pos == rotated_perimeter, dir)
    )
  return(nrow(p))
}

sum(sapply(groups, length) * sapply(groups, calculate_sides))
