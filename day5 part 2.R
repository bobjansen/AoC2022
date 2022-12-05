library(data.table)

# Puzzle 1
file <- "input5.txt"
txt <- readChar(file, file.info(file)$size)
parts <- strsplit(txt, "\n\n")[[1L]]

filter_empty <- function(stack) {
  stack[!stack %in% c("[_]", NA_character_)]
}

stacks <- gsub("    ", " [_]", parts[[1L]])
stacks <- strsplit(stacks, "\n")[[1L]] |>
  strsplit(split = "\n")

stacks <- stacks[1:(length(stacks) - 1L)] |>
  sapply(strsplit, split = " ") |>
  data.table::transpose() |>
  lapply(rev) |>
  sapply(filter_empty)

split_moves <- function(line) {
  instructions <- strsplit(line, " ")[[1L]]
  list(
    count = as.integer(instructions[[2L]]),
    from = as.integer(instructions[[4L]]),
    to = as.integer(instructions[[6L]])
  )
}

moves <- parts[[2L]] |> strsplit(split = "\n")
moves <- lapply(moves[[1L]], split_moves)

do_move <- function(count, from, to) {
  split_at <- length(stacks[from][[1L]]) - count
  if (split_at == 0L) {
    stacks[to][[1L]] <- c(stacks[to][[1L]], rev(stacks[from][[1L]]))
    stacks[from][[1L]] <- character(0L)
  } else {
    to_move <- stacks[from][[1L]][(split_at + 1L):length(stacks[from][[1L]])]
    stacks[from][[1L]] <- stacks[from][[1L]][1:split_at]
    stacks[to][[1L]] <- c(stacks[to][[1L]], rev(to_move))
  }
  stacks <<- stacks
}

do_move2 <- function(count, from, to) {
  split_at <- length(stacks[from][[1L]]) - count
  if (split_at == 0L) {
    stacks[to][[1L]] <- c(stacks[to][[1L]], stacks[from][[1L]])
    stacks[from][[1L]] <- character(0L)
  } else {
    to_move <- stacks[from][[1L]][(split_at + 1L):length(stacks[from][[1L]])]
    stacks[from][[1L]] <- stacks[from][[1L]][1:split_at]
    stacks[to][[1L]] <- c(stacks[to][[1L]], to_move)
  }
  stacks <<- stacks
}

move_count <- length(moves)
# sapply(moves[1:(move_count / 2L)], do.call, what = do_move)
# sapply(moves[((move_count / 2L) + 1L):move_count], do.call, what = do_move2)

sapply(moves, do.call, what = do_move2)

cat(gsub("\\[|\\]", "", paste0(sapply(stacks, tail, 1L), collapse = "")))
