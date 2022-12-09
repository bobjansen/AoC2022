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

calculate_change <- function(p1, p2) {
  delta <- p1 - p2
  manhattan_distance <- abs(Re(delta)) + abs(Im(delta))
  if (manhattan_distance == 3) {
    if (Re(delta) == 2) {
      p2 <- p2 + directions[["R"]]
    } else if (Re(delta) == -2) {
      p2 <- p2 + directions[["L"]]
    } else if (Im(delta) == 2) {
      p2 <- p2 + directions[["U"]]
    } else if (Im(delta) == -2) {
      p2 <- p2 + directions[["D"]]
    }
    if (Re(delta) == 1) {
      p2 <- p2 + directions[["R"]]
    } else if (Re(delta) == -1) {
      p2 <- p2 + directions[["L"]]
    } else if (Im(delta) == 1) {
      p2 <- p2 + directions[["U"]]
    } else if (Im(delta) == -1) {
      p2 <- p2 + directions[["D"]]
    }
  } else if (manhattan_distance == 2) {
    if (Re(delta) == 2) {
      p2 <- p2 + directions[["R"]]
    } else if (Re(delta) == -2) {
      p2 <- p2 + directions[["L"]]
    } else if (Im(delta) == 2) {
      p2 <- p2 + directions[["U"]]
    } else if (Im(delta) == -2) {
      p2 <- p2 + directions[["D"]]
    }
  }
  p2
}

do_moves <- function(lines) {
  pos_h <- complex(1, 0, 0)
  pos_t <- complex(1, 0, 0)
  tail_path <- c()

  for (line in lines) {
    direction_letter <- line[[1]]
    move_length <- line[[2L]]
    direction <- directions[[direction_letter]]

    for (i in seq_len(move_length)) {
      pos_h <- pos_h + direction
      pos_t <- calculate_change(pos_h, pos_t)
      tail_path <- c(tail_path, pos_t)
    }
  }
  tail_path
}

tail_path <- do_moves(moves)
cat_solution(17, length(unique(tail_path)))
