source("helpers.R")

txt <- "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."
txt <- readFile("input23.txt")

lines <- strsplit(ustrsplit(txt, "\n"), "")
row <- 0L
positions <- rbindlist(lapply(lines, \(line) {
  row <<- row + 1L
  cols <- which(line == "#")
  if (length(cols) > 0L) data.table::data.table(row = row, col = cols)
}))
positions[, index := .I]
setcolorder(positions, c("index", "row", "col"))

directions <- list(
  "north" = \(north, south, west, east, r, c) {
    if (north == 0L) return(list(r - 1L, c))
  },
  "south" = \(north, south, west, east, r, c) {
    if (south == 0L) return(list(r + 1L, c))
  },
  "west" = \(north, south, west, east, r, c) {
    if (west == 0L) return(list(r, c - 1L))
  },
  "east" = \(north, south, west, east, r, c) {
    if (east == 0L) return(list(r, c + 1L))
  }
)


next_position <- function(r, c, dt, round) {
  north <- nrow(dt[row == r - 1L & col >= c - 1L & col <= c + 1L])
  south <- nrow(dt[row == r + 1L & col >= c - 1L & col <= c + 1L])
  west <- nrow(dt[col == c - 1L & row >= r - 1L & row <= r + 1L])
  east <- nrow(dt[col == c + 1L & row >= r - 1L & row <= r + 1L])
  if (north + south + west + east == 0L) {
    return(list(r, c))
  }

  for (i in (1:length(directions)) + round - 1L) {
    index <- (i - 1L) %% length(directions) + 1L
    candidate = directions[[index]](north, south, west, east, r, c)
    if (!is.null(candidate)) return(candidate)
  }
  list(r, c)
}


run <- function() {
changes <- Inf
i <- 0L
while (changes > 0L) {
  i <- i + 1L
  positions[,
    c("next_row", "next_col") := next_position(
      .SD$row, .SD$col, dt = positions, round = i
    ), by = index
  ]
  valid_set = positions[, .N, .(next_row, next_col)][N == 1]
  positions[
    valid_set,
    ':='(new_row = next_row, new_col = next_col),
    on = .(next_row, next_col)
  ]
  sel <- positions[, !is.na(new_row) & (row != new_row | col != new_col)]
  changes <- sum(sel)
  catn(i, ": changes: ", changes, sep = "")
  if (i > 15L) print(head(positions[sel]))
  positions[sel, ':='(row = new_row, col = new_col)]
  if (i == 10) {
    result <-
      positions[,
      .(rows = max(row) - min(row) + 1L, cols = max(col) - min(col) + 1L)]
    cat_solution(46L, result$rows * result$cols - nrow(positions))
    break
  }
}
}
