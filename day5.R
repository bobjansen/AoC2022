source("helpers.R")
file <- "input5.txt"

filter_empty <- function(stack) {
  stack[!stack %in% c("[_]", NA_character_)]
}

split_moves <- function(line) {
  instructions <- strsplit(line, " ")[[1L]]
  list(
    count = as.integer(instructions[[2L]]),
    from = as.integer(instructions[[4L]]),
    to = as.integer(instructions[[6L]])
  )
}

do_move <- function(stacks, count, from, to, move_fun = identity) {
  stacks[[to]] <- c(stacks[[to]], move_fun(tail(stacks[[from]], count)))
  stacks[[from]] <- stacks[[from]][seq_len(length(stacks[[from]]) - count)]
  stacks
}

prepare_data <- function(file) {
  txt <- readFile(file)
  parts <- strsplit(txt, "\n\n")[[1L]]

  stacks <- gsub("    ", " [_]", parts[[1L]]) |>
    ustrsplit(split = "\n") |>
    (\(.) head(., length(.) - 1L))() |>
    sapply(strsplit, split = " ") |>
    data.table::transpose() |>
    lapply(rev) |>
    sapply(filter_empty)

  moves <- parts[[2L]] |>
    ustrsplit(split = "\n") |>
    lapply(split_moves)

  list(stacks = stacks, moves = moves)
}

inputs <- prepare_data(file)
stacks <- inputs[["stacks"]]
for (move in inputs[["moves"]]) {
  stacks <- do_move(
    stacks, move[["count"]], move[["from"]], move[["to"]], move_fun = rev
  )
}
cat_solution(
  9L, paste(gsub("\\[|\\]", "", sapply(stacks, tail, 1L)), collapse = "")
)

inputs <- prepare_data(file)
stacks <- inputs[["stacks"]]
for (move in inputs[["moves"]]) {
  stacks <- do_move(stacks, move[["count"]], move[["from"]], move[["to"]])
}
cat_solution(
  10L, paste(gsub("\\[|\\]", "", sapply(stacks, tail, 1L)), collapse = "")
)
