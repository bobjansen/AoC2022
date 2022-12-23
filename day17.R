source("helpers.R")

block_texts <- "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##"

gusts <- ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
gusts <- readFile("input17.txt")
gusts <- ustrsplit(gsub("\n", "", gusts), "")

steps <- 2022L
left_offset = 3L
board_witdh = 7L

block_texts <- ustrsplit(block_texts, "\n\n")

create_coord <- function(x, y) {
  structure(list(x = x, y = y), class = 'coord')
}

print.coord <- function(x, ...) {
  print(paste(x$x, x$y, sep = ", "))
}

create_bb <- function(rows) {
  create_coord(nchar(rows[[1L]]), length(rows))
}

create_block <- function(lines) {
  rows <- ustrsplit(lines, "\n")
  bb <- create_bb(rows)
  block <- matrix(0L, bb$y, bb$x)
  for (y in 1:bb$y) {
    parts <- ustrsplit(rows[[y]], "")
    for (x in 1:bb$x) {
      if (parts[[x]] == "#") {
        block[bb$y - y + 1L, x] <- 1L
      }
    }
  }
  block
}

bbs <- sapply(
  block_texts, \(.) create_bb(ustrsplit(., "\n")),
  simplify = FALSE, USE.NAMES = FALSE
)

blocks <- sapply(
  block_texts, create_block, simplify = FALSE, USE.NAMES = FALSE
)

print_board <- function(state, start = 1L, rows = 10L) {
  stopifnot(rows > 0L)
  for (i in seq(rows, 1L, -1L) + start - 1L) {
    row_output <- paste(state$board[i, ], collapse = "")
    row_output <- gsub("0", ".", row_output)
    row_output <- gsub("1", "@", row_output)
    row_output <- gsub("2", "#", row_output)
    catn(sprintf("%04d", i), ": ", row_output, sep = "")
  }
  catn("     +-------+\n")
}
# 4L * 2022L +
state <- list(
  board = matrix(0L, 50000L, board_witdh),
  top_left = create_coord(left_offset, 3L),
  top_row = 0L,
  moving = FALSE
)

add_block <- function(state, block_index, value = 1L) {
  bb <- bbs[[block_index]]
  block <- blocks[[block_index]]
  position <- state$top_left
  board <- state$board

  ys <- 1:bb$y + state$top_left$y - nrow(block)
  xs <- 1:bb$x + state$top_left$x - 1L
  state$board[ys, xs] <- state$board[ys, xs] + blocks[[block_index]] * value
  state
}

remove_block <- function(state, block_index) {
  bb <- bbs[[block_index]]
  block <- blocks[[block_index]]

  ys <- 1:bb$y + state$top_left$y - nrow(block)
  xs <- 1:bb$x + state$top_left$x - 1L

  state$board[ys, xs] <- state$board[ys, xs] - block
  state
}

push_left <- function(state, block_index) {
  state <- remove_block(state, block_index)
  new_location <- create_coord(state$top_left$x - 1L, state$top_left$y)
  if (new_location$x >= 1L && can_move(state, block_index, new_location)) {
    state$top_left <- new_location
    state <- add_block(state, block_index)
  } else {
    state <- add_block(state, block_index)
  }
  state
}

push_right <- function(state, block_index) {
  new_location <- create_coord(state$top_left$x + 1L, state$top_left$y)

  if (new_location$x + ncol(blocks[[block_index]]) - 1L <= board_witdh &&
      can_move(state, block_index, new_location)) {
    state <- remove_block(state, block_index)
    state$top_left <- new_location
    state <- add_block(state, block_index)
  }
  state
}

push_down <- function(state, block_index) {
  state <- remove_block(state, block_index)
  new_location <- create_coord(state$top_left$x, state$top_left$y - 1L)

  if (
    new_location$y - nrow(blocks[[block_index]]) >= 0L &&
    can_move(state, block_index, new_location)
    ) {

    state$top_left <- new_location
    state <- add_block(state, block_index)
  } else {
    state <- add_block(state, block_index, value = 2L)
    state$top_row <- max(state$top_row, state$top_left$y)
    state$moving <- FALSE
  }
  state
}

can_move <- function(state, block_index, new_location) {
  bb <- bbs[[block_index]]
  block <- blocks[[block_index]]
  ys <- 1:bb$y + new_location$y - nrow(block)
  xs <- 1:bb$x + new_location$x - 1L

  all(state$board[ys, xs] + blocks[[block_index]] < 3L)
}

block_count <- 0L
i <- 0L
# tops <- c()
# block_counts <- c()
# while (block_count <= 5L * 2022L) {
while (block_count <= 1724L + 1725L + 1601L) {
# while (i < 4 * length(gusts)) {
  i <- i + 1L
  if (!state$moving) {
    block_count <- block_count + 1L
    delta <- sum(state$board == 2L)
    last_sum <- last_sum + delta
    deltas <- c(deltas, delta)
    block_index <- (block_count - 1L) %% length(blocks) + 1L
    state$top_left <- create_coord(
      left_offset, state$top_row + nrow(blocks[[block_index]]) + 3L
    )
    state <- add_block(state, block_index)

    state$moving <- TRUE
    # catn("Begin falling")
    # print_board(state)
    # print(state$top_left)
  }

  gust_index <- (i - 1L) %% length(gusts) + 1L
  if (gust_index == length(gusts)) {
    catn("Block count:", block_count)
    catn("Top row:", state$top_row)
    # print_board(state, state$top_row - 10L, 20L)
    # block_counts <- c(block_counts, block_count)
    # tops <- c(tops, state$top_row)
  }
  gust <- gusts[[gust_index]]
  if (gust == "<") {
    # catn("Moving left")
    state <- push_left(state, block_index)
  } else {
    # catn("Moving right")
    state <- push_right(state, block_index)
  }
  # print_board(state)
  # print(state$top_left)
  # catn("Moving down")
  state <- push_down(state, block_index)
  # print_board(state)
  # print(state$top_left)
}

cat_solution(35, state$top_row)

options(digits = 18L)
num_blocks <- 1000000000000
block_per_cycle <- block_counts[[3L]] - block_counts[[2L]]
full_cycles <- floor((num_blocks - block_counts[[1L]]) / block_per_cycle)

num_blocks_at_end <- 1000000000000 -
  (full_cycles * block_per_cycle + block_counts[[1L]])
end_rows <- 2526
full_cycles * (tops[[3L]] - tops[[2L]]) + tops[[1L]] + end_rows
