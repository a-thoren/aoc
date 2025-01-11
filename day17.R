input <- readLines("inputs/day17.txt")

a <- stringr::str_extract(input[1], "\\d+") |> 
  as.numeric()
b <- stringr::str_extract(input[2], "\\d+") |> 
  as.numeric()
c <- stringr::str_extract(input[3], "\\d+") |> 
  as.numeric()

program <- stringr::str_extract_all(input[5], "\\d", simplify = TRUE) |> 
  as.numeric()

run_program <- function(a) {
  i <- 0
  output <- c()
  while (i < length(program)) {
    
    opcode <- program[i + 1]
    literal_operand <- program[i + 2]
    if (literal_operand %in% 0:3) {
      combo_operand <- literal_operand
    } else if (literal_operand == 4) {
      combo_operand <- a
    } else if (literal_operand == 5) {
      combo_operand <- b
    } else if (literal_operand == 6) {
      combo_operand <- c
    }
    if (opcode == 0) {
      a <- trunc(a / 2^combo_operand)
    } else if (opcode == 1) {
      b <- bitwXor(b, literal_operand)
    } else if (opcode == 2) {
      b <- combo_operand %% 8
    } else if (opcode == 3) {
      if (a != 0) {
        i <- literal_operand
      }
    } else if (opcode == 4) {
      b <- bitwXor(b, c)
    } else if (opcode == 5) {
      output <- c(output, combo_operand %% 8)
    } else if (opcode == 6) {
      b <- trunc(a / 2^combo_operand)
    } else if (opcode == 7) {
      c <- trunc(a / 2^combo_operand)
    }
    
    if (!(opcode == 3 && a != 0)) {
      i <- i + 2
    }
  }
  return(output)
}

output <- run_program(a)

run <- function(a) {
  b <- bitwXor(a %% 8, 3)
  c <- a %/% 2^b
  return(bitwXor(bitwXor(b, c %% 8), 5))
}

a <- 0
for (i in rev(seq_along(program))) {
  tmp <- c()
  for (a_2 in a) {
    x <- a_2 * 8 + 0:7
    out <- sapply(x, run)
    tmp <- c(tmp, x[which(out == program[i])])
  }
  a <- tmp
}
min(a)

