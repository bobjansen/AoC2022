source("helpers.R")

txt <- "30373
25512
65332
33549
35390"

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

  if (forest[row == the_row & column < the_column, any(tree >= the_tree)] &&
      forest[row == the_row & column > the_column, any(tree >= the_tree)] &&
      forest[row < the_row & column == the_column, any(tree >= the_tree)] &&
      forest[row > the_row & column == the_column, any(tree >= the_tree)]) {
    forest[tree_index, visible := FALSE]
  }
}
cat_solution(15, forest[, sum(visible)])

find_blocker <- function(vec) {
  if (any(vec)) {
    which.max(vec)
  } else {
    length(vec)
  }
}

max_scenic_score <- 0L
for (tree_index in forest[on_edge == FALSE, ID]) {
  the_row <- forest[tree_index, row]
  the_column <- forest[tree_index, column]
  the_tree <- forest[tree_index, tree]


  left <- find_blocker(
    forest[row == the_row & column < the_column, rev(tree)] >= the_tree)
  right <- find_blocker(
    forest[row == the_row & column > the_column, tree] >= the_tree)
  top <- find_blocker(
    forest[row < the_row & column == the_column, rev(tree)] >= the_tree)
  bottom <- find_blocker(
    forest[row > the_row & column == the_column, tree] >= the_tree)
  scenic_score <- left * right * top * bottom
  max_scenic_score = max(scenic_score, max_scenic_score)
}
cat_solution(16, max_scenic_score)
