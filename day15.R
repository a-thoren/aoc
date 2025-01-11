input <- readLines("inputs/day15.txt")
input <- split(input, cumsum(input == ""))

map <- input[[1]] |> 
  strsplit("") |> 
  do.call(what = rbind)

original_map <- map

instructions <- strsplit(input[[2]], "") |> unlist()

pos <- which(map == "@", arr.ind = TRUE)

get_direction <- function(x) {
  if (x == "<") {
    return(c(0, -1))
  } else if (x == ">") {
    return(c(0, 1))
  } else if (x == "^") {
    return(c(-1, 0))
  } else if(x == "v") {
    return(c(
      1, 0))
  }
}

execute_instruction <- function(instruction) {
  dir <- get_direction(instruction)
  new_pos <- pos + dir
  if (map[new_pos[1], new_pos[2]] == ".") {
    map[new_pos[1], new_pos[2]] <<- "@"
    map[pos[1], pos[2]] <<- "."
    pos <<- new_pos
  } else if (map[new_pos[1], new_pos[2]] == "O") {
    tmp_pos <- new_pos
    while (map[tmp_pos[1], tmp_pos[2]] == "O") {
      tmp_pos <- tmp_pos + dir
    }
    if (map[tmp_pos[1], tmp_pos[2]] == ".") {
      map[tmp_pos[1], tmp_pos[2]] <<- "O"
      map[new_pos[1], new_pos[2]] <<- "@"
      map[pos[1], pos[2]] <<- "."
      pos <<- new_pos
    }
  }
}

for (instruction in instructions) {
  execute_instruction(instruction)
}

idx <- which(map == "O", arr.ind = TRUE)
s <- 0
for (i in seq(nrow(idx))) {
  t <- (idx[i, 1] - 1) * 100 + idx[i, 2] - 1
  s <- s + t
}

s

new_map <- matrix(".", dim(original_map)[1], dim(original_map)[2] * 2)

for (i in seq(dim(original_map)[1])) {
  for (j in seq(dim(original_map)[2])) {
    if (original_map[i, j] == "#") {
      new_map[i, j * 2 - 1 + 0:1] <- "#"
    } else if (original_map[i, j] == "O") {
      new_map[i, j * 2 - 1 + 0:1] <- c("[", "]")
    } else if(original_map[i, j] == "@") {
      new_map[i, j * 2 - 1 + 0:1] <- c("@", ".")
    }
  }
}

map <- new_map

execute_instruction_2 <- function(instruction) {
  dir <- get_direction(instruction)
  new_pos <- pos + dir
  if (map[new_pos[1], new_pos[2]] == ".") {
    map[new_pos[1], new_pos[2]] <<- "@"
    map[pos[1], pos[2]] <<- "."
    pos <<- new_pos
  } else if (map[new_pos[1], new_pos[2]] %in% c("[", "]")) {
    if (dir[1] == 0) {
      move_box_horizontally(instruction)
    } else {
      move_box_vertically(instruction)
    }
  }
}

move_box_horizontally <- function(instruction) {
  dir <- get_direction(instruction)
  new_pos <- pos + dir
  if (map[new_pos[1], new_pos[2]] %in% c("[", "]")) {
    tmp_pos <- new_pos
    while (map[tmp_pos[1], tmp_pos[2]] %in% c("[", "]")) {
      tmp_pos <- tmp_pos + dir
    }
    if (map[tmp_pos[1], tmp_pos[2]] == ".") {
      map[new_pos[1], (new_pos + dir)[2]:tmp_pos[2]] <<- map[new_pos[1], new_pos[2]:(tmp_pos - dir)[2]]
      map[pos[1], pos[2]] <<- "."
      map[new_pos[1], new_pos[2]] <<- "@"
      pos <<- new_pos
    }
  }
}

move_box_vertically <- function(instruction) {
  add_queue <<- data.frame()
  remove_queue <<- data.frame()
  dir <- get_direction(instruction)
  new_pos <- pos + dir
  if (map[new_pos[1], new_pos[2]] %in% c("[", "]")) {
    if (map[new_pos[1], new_pos[2]] == "[") {
      r <- recurse_box_vertically(new_pos, new_pos + c(0, 1), dir)
    } else {
      r <- recurse_box_vertically(new_pos - c(0, 1), new_pos, dir)
    }
  }
  if (r) {
    
    for (i in seq(nrow(remove_queue))) {
      old_left <- remove_queue$left[i]
      old_right <- remove_queue$right[i]
      map[Re(old_left), Im(old_left)] <<- "."
      map[Re(old_right), Im(old_right)] <<- "."
    }
    
    for (i in seq(nrow(add_queue))) {
      new_left <- add_queue$left[i]
      new_right <- add_queue$right[i]
      map[Re(new_left), Im(new_left)] <<- "["
      map[Re(new_right), Im(new_right)] <<- "]"
    }
    
    map[new_pos[1], new_pos[2]] <<- "@"
    map[pos[1], pos[2]] <<- "."
    pos <<- new_pos
  }
}

remove_queue <- data.frame()
add_queue <- list()

recurse_box_vertically <- function(left_pos, right_pos, dir) {
  new_left <- left_pos + dir
  new_right <- right_pos + dir
  if (map[new_left[1], new_left[2]] == "[" && map[new_right[1], new_right[2]] == "]") {
    
    r <- recurse_box_vertically(new_left, new_right, dir)
    
  } else if (map[new_left[1], new_left[2]] == "]" && map[new_right[1], new_right[2]] == "[") {
    
    r <- recurse_box_vertically(new_left - c(0, 1), new_right - c(0, 1), dir)
    r <- r && recurse_box_vertically(new_left + c(0, 1), new_right + c(0, 1), dir)
    
  } else if (map[new_left[1], new_left[2]] == "]" && map[new_right[1], new_right[2]] != "#") {
    
    r <- recurse_box_vertically(new_left - c(0, 1), new_right - c(0, 1), dir)
    
  } else if (map[new_right[1], new_right[2]] == "[" && map[new_left[1], new_left[2]] != "#") {
    
    r <- recurse_box_vertically(new_left + c(0, 1), new_right + c(0, 1), dir)
    
  } else if (map[new_left[1], new_left[2]] == "." && map[new_right[1], new_right[2]] == ".") {
    
    r <- TRUE
    
  } else {
    
    r <- FALSE
    
  }
  
  if (r) {
    remove_queue <<- dplyr::bind_rows(remove_queue, data.frame(left = left_pos[1] + left_pos[2] * 1i, right = right_pos[1] + right_pos[2] * 1i))
    add_queue <<- dplyr::bind_rows(add_queue, data.frame(left = new_left[1] + new_left[2] * 1i, right = new_right[1] + new_right[2] * 1i))
  }
  
  return(r)
}

pos <- which(map == "@", arr.ind = TRUE)

for (instruction in instructions) {
  execute_instruction_2(instruction)
}

idx <- which(map == "[", arr.ind = TRUE)
s <- 0
for (i in seq(nrow(idx))) {
  t <- (idx[i, 1] - 1) * 100 + idx[i, 2] - 1
  s <- s + t
}

s