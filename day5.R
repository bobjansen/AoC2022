source("helpers.R")
file <- "input5.txt"

filter_empty <- function(stack) {
  sapply(stack, \(.) .[!. %in% c("[_]", NA_character_)])
}

split_moves <- function(lines) {
  instructions <- strsplit(lines, " ")
  lapply(instructions, \(instr) {
    list(
      count = as.integer(instr[[2L]]),
      from = as.integer(instr[[4L]]),
      to = as.integer(instr[[6L]])
    )
  })
}

do_move <- function(stacks, count, from, to, move_fun = identity) {
  stacks[[to]] <- c(stacks[[to]], move_fun(tail(stacks[[from]], count)))
  stacks[[from]] <- stacks[[from]][seq_len(length(stacks[[from]]) - count)]
  stacks
}

prepare_data <- function(file) {
  parts <- ustrsplit(readFile(file), "\n\n")

  stacks <- gsub("    ", " [_]", parts[[1L]]) |>
    ustrsplit(split = "\n") |>
    stacks => head(stacks, length(stacks) - 1L) |>
    strsplit(split = " ") |>
    data.table::transpose() |>
    lapply(rev) |>
    filter_empty()

  moves <- parts[[2L]] |>
    ustrsplit(split = "\n") |>
    split_moves()

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
