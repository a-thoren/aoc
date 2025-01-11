input <- readLines("inputs/day24.txt")
input <- split(input, cumsum(input == ""))

operations <- input[[2]][-1]
operations <- gsub(" ->", "", operations) |> 
  strsplit(" ")

input <- input[[1]]

process_operations <- function(operations) {
  op_queue <- collections::queue()
  values <- collections::dict()
  
  for (inp in input) {
    key <- stringr::str_extract(inp, "[a-z]\\d{2}")
    value <- stringr::str_extract(inp, "(\\d+)$")
    values$set(key, as.numeric(value))
  }
  
  for (op in operations) {
    op_queue$push(op)
  }
  
  while (op_queue$size() > 0) {
    op <- op_queue$pop()
    if (values$has(op[1]) && values$has(op[3])) {
      x <- values$get(op[1])
      y <- values$get(op[3])
      if (op[2] == "AND") {
        r <- x && y
      } else if (op[2] == "OR") {
        r <- x || y
      } else if (op[2] == "XOR") {
        r <- x != y
      }
      values$set(op[4], r)
    } else {
      op_queue$push(op)
    }
  }
  return(values)
}

values <- process_operations(operations)

keys <- sort(unlist(values$ks))
keys <- keys[startsWith(keys, "z")]
v <- unlist(values$vs[match(keys, values$ks)])
message(sum(v * 2^(1:length(v) - 1)))

# for each bit-adder, c_in is not available for first bit-adder
# xor(x, y) = a
# x & y = b
# xor(a, c_in) = z
# c_in & a = d
# d | b = c_out (this c_out becomes z for the last operation)

wrong <- c()

for (i in seq_along(operations)) {
  op <- operations[[i]]
  if (startsWith(op[4], "z") && !op[4] == "z45" && op[2] != "XOR") {
    wrong <- c(wrong, list(op))
  } else if (op[2] == "XOR" && !startsWith(op[4], "z") && !grepl("[xy]", op[1])) {
    wrong <- c(wrong, list(op))
  } else if (op[2] == "AND" && !grepl("[xy]00", op[1])) {
    for (j in seq_along(operations)) {
      op_2 <- operations[[j]]
      if ((op_2[1] == op[4] || op_2[3] == op[4]) && op_2[2] != "OR") {
        wrong <- c(wrong, list(op))
        break
      }
    }
  } else if (op[2] == "XOR") {
    for (j in seq_along(operations)) {
      op_2 <- operations[[j]]
      if ((op_2[1] == op[4] || op_2[3] == op[4]) && op_2[2] == "OR") {
        wrong <- c(wrong, list(op))
        break
      }
    }
  }
}

paste0(sort(sapply(wrong, \(x) x[4])), collapse = ",")
