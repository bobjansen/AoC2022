source("helpers.R")

# Enriched with the ideas from:
# https://www.reddit.com/r/adventofcode/comments/zgnice/comment/izhzxb6/

txt <- readFile("input9.txt")
moves <- strsplit(ustrsplit(txt, "\n"), " ")

directions <- list(
  R = c(1L, 0L),
  D = c(0L, -1L),
  L = c(-1L, 0L),
  U = c(0L, 1L)
)

do_moves <- function(lines, snake_size = 2L) {
  snake <- matrix(0L, 10L, 2L)
  tail_path_1 <- dictionary()
  tail_path_9 <- dictionary()

  for (line in lines) {
    for (i in 1:line[[2L]]) {
      snake[1L,] <- snake[1L,] + directions[[line[[1L]]]]

      for (j in 2:snake_size) {
        delta <- snake[j - 1L,] - snake[j,]
        if (abs(delta[[1L]]) >= 2L || abs(delta[[2L]]) >= 2L) {
          snake[j, ] <- snake[j, ] + c(sign(delta[[1L]]), sign(delta[[2L]]))
        }
      }
      tail_path_1 <- setValue(tail_path_1, toString(snake[2L,]), TRUE)
      tail_path_9 <- setValue(tail_path_9, toString(snake[10L,]), TRUE)
    }
  }
  cat_solution(17, size(tail_path_1))
  cat_solution(18, size(tail_path_9))
}
do_moves(moves, 10L)
