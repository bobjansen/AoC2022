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

forest[, ':='(ID = .I, on_edge = FALSE)]
forest[
  row == 1L | column == 1L | row == num_rows | column == num_columns,
  ':='(on_edge = TRUE)
]

forest[, visible := TRUE]
for (tree_index in forest[on_edge == FALSE, ID]) {
  the_row <- forest[tree_index, row]
  the_column <- forest[tree_index, column]
  the_tree <- forest[tree_index, tree]

}

find_blocker <- function(vec) {
  if (any(vec)) {
    which.max(vec)
  } else {
    length(vec)
  }
}

data.table::setkeyv(forest, c("row", "column"))

max_scenic_score <- 0L
for (tree_index in forest[on_edge == FALSE, ID]) {
  the_row <- forest[tree_index, row]
  the_column <- forest[tree_index, column]
  the_tree <- forest[tree_index, tree]

  left_trees <- forest[row == the_row & column < the_column, tree]
  right_trees <- forest[row == the_row & column > the_column, tree]
  top_trees <- forest[row < the_row & column == the_column, tree]
  bottom_trees <- forest[row > the_row & column == the_column, tree]

  if (any(left_trees >= the_tree) && any(right_trees >= the_tree) &&
      any(top_trees >= the_tree) && any(bottom_trees >= the_tree)) {
    forest[tree_index, visible := FALSE]
  }

  left <- find_blocker(rev(left_trees) >= the_tree)
  right <- find_blocker(right_trees >= the_tree)
  top <- find_blocker(rev(top_trees) >= the_tree)
  bottom <- find_blocker(bottom_trees >= the_tree)
  scenic_score <- left * right * top * bottom
  max_scenic_score = max(scenic_score, max_scenic_score)
}
cat_solution(15, forest[, sum(visible)])
cat_solution(16, max_scenic_score)
