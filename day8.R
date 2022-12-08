source("helpers.R")

txt <- readFile("input8.txt")
rows <- ustrsplit(txt, "\n")
num_rows <- length(rows)
matrix <- strsplit(rows, "")
num_columns <- length(matrix[[1L]])

row_index <- 0L
forest <- rbindlist(lapply(matrix, \(row) {
  row_index <<- row_index + 1L
  dt <- data.table::data.table(tree = as.integer(row))
  dt[, ':='(column = .I, row = row_index)]
  dt
}))
forest <- data.table::setcolorder(forest, rev(names(forest)))
data.table::setorderv(forest, c("row", "column"))
forest[, ':='(ID = .I, on_edge = FALSE)]
forest[
  row == 1L | column == 1L | row == num_rows | column == num_columns,
  ':='(on_edge = TRUE)
]

find_blocker <- function(vec) {
  if (any(vec)) {
    which.max(vec)
  } else {
    length(vec)
  }
}

run <- function() {
  rows <- forest[, row]
  columns <- forest[, column]
  visible <- 2L * num_rows + 2L * (num_columns - 2L)
  max_scenic_score <- 0L
  for (tree_index in forest[on_edge == FALSE, ID]) {
    the_row <- rows[[tree_index]]
    the_column <- columns[[tree_index]]
    the_tree <- as.integer(matrix[[the_row]][[the_column]])

    left_trees <- forest[row == the_row & column < the_column, tree]
    right_trees <- forest[row == the_row & column > the_column, tree]
    top_trees <- forest[row < the_row & column == the_column, tree]
    bottom_trees <- forest[row > the_row & column == the_column, tree]

    if (all(left_trees < the_tree) || all(right_trees < the_tree) ||
        all(top_trees < the_tree) || all(bottom_trees < the_tree)) {
      visible <- visible + 1L
    }

    left <- find_blocker(rev(left_trees) >= the_tree)
    right <- find_blocker(right_trees >= the_tree)
    top <- find_blocker(rev(top_trees) >= the_tree)
    bottom <- find_blocker(bottom_trees >= the_tree)
    max_scenic_score = max(left * right * top * bottom, max_scenic_score)
  }
  cat_solution(15, visible)
  cat_solution(16, max_scenic_score)
}

