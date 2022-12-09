source("helpers.R")

txt <- "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"

# txt   <- readFile("input9.txt")
moves <- strsplit(ustrsplit(txt, "\n"), " ")
moves

pos_h <- complex(1, 0, 0)
pos_t <- complex(1, 0, 0)

directions <- list(
  R = complex(1, 1, 0),
  D = complex(1, 0, -1),
  L = complex(1, -1, 0),
  U = complex(1, 0, 1)
)

tail_path <- c()

do_move <- function(direction_letter, move_length) {
  direction <- directions[[direction_letter]]

  for (i in seq_len(move_length)) {
    pos_h <<- pos_h + direction
    delta <- pos_h - pos_t
    manhattan_distance <- abs(Re(delta)) + abs(Im(delta))
    if (manhattan_distance == 3) {
      if (Re(delta) == 2) {
        pos_t <<- pos_t + directions[["R"]]
      } else if (Re(delta) == -2) {
        pos_t <<- pos_t + directions[["L"]]
      } else if (Im(delta) == 2) {
        pos_t <<- pos_t + directions[["U"]]
      } else if (Im(delta) == -2) {
        pos_t <<- pos_t + directions[["D"]]
      }
      if (Re(delta) == 1) {
        pos_t <<- pos_t + directions[["R"]]
      } else if (Re(delta) == -1) {
        pos_t <<- pos_t + directions[["L"]]
      } else if (Im(delta) == 1) {
        pos_t <<- pos_t + directions[["U"]]
      } else if (Im(delta) == -1) {
        pos_t <<- pos_t + directions[["D"]]
      }
    } else if (manhattan_distance == 2) {
      if (Re(delta) == 2) {
        pos_t <<- pos_t + directions[["R"]]
      } else if (Re(delta) == -2) {
        pos_t <<- pos_t + directions[["L"]]
      } else if (Im(delta) == 2) {
        pos_t <<- pos_t + directions[["U"]]
      } else if (Im(delta) == -2) {
        pos_t <<- pos_t + directions[["D"]]
      }
    }
    tail_path <<- c(tail_path, pos_t)
  }
}

sapply(moves, \(line) do_move(line[[1]], line[[2L]]))
cat_solution(17, length(unique(tail_path)))
