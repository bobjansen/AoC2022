source("helpers.R")

txt <- "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"

txt   <- readFile("input9.txt")
moves <- strsplit(ustrsplit(txt, "\n"), " ")
moves

directions <- list(
  R = complex(1, 1, 0),
  D = complex(1, 0, -1),
  L = complex(1, -1, 0),
  U = complex(1, 0, 1)
)

calculate_change <- function(p1, p2, last_move) {
  delta <- p1 - p2
  move <- complex(1, 0, 0)
  manhattan_distance <- abs(Re(delta)) + abs(Im(delta))
  if (manhattan_distance == 4) {
    move <- last_move
  } else if (manhattan_distance == 3) {
    if (Re(delta) == 2) {
      move <- move + directions[["R"]]
    } else if (Re(delta) == -2) {
      move <- move + directions[["L"]]
    } else if (Im(delta) == 2) {
      move <- move + directions[["U"]]
    } else if (Im(delta) == -2) {
      move <- move + directions[["D"]]
    }
    if (Re(delta) == 1) {
      move <- move + directions[["R"]]
    } else if (Re(delta) == -1) {
      move <- move + directions[["L"]]
    } else if (Im(delta) == 1) {
      move <- move + directions[["U"]]
    } else if (Im(delta) == -1) {
      move <- move + directions[["D"]]
    }
  } else if (manhattan_distance == 2) {
    if (Re(delta) == 2) {
      move <- move + directions[["R"]]
    } else if (Re(delta) == -2) {
      move <- move + directions[["L"]]
    } else if (Im(delta) == 2) {
      move <- move + directions[["U"]]
    } else if (Im(delta) == -2) {
      move <- move + directions[["D"]]
    }
  }
  move
}

do_moves <- function(lines, snake_size = 2L) {
  snake <- replicate(snake_size, complex(1, 0, 0))
  tail_path <- c()

  for (line in lines) {
    direction <- directions[[line[[1]]]]
    move_length <- line[[2L]]

    for (i in seq_len(move_length)) {
      snake[[1L]] <- snake[[1L]] + direction
      last_move <- direction

      for (j in 2:snake_size) {
        last_move <- calculate_change(snake[[j - 1L]], snake[[j]], last_move)
        snake[[j]] <- snake[[j]] + last_move
      }
      tail_path <- c(tail_path, tail(snake, 1L))
    }
  }
  tail_path
}

tail_path <- do_moves(moves)
cat_solution(17, length(unique(tail_path)))
tail_path <- do_moves(moves, 10L)
cat_solution(18, length(unique(tail_path)))
