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

row_index <- 0
forest <- rbindlist(lapply(matrix, \(row) {
  row_index <<- row_index + 1L
  dt <- data.table::data.table(tree = row)
  dt[, ':='(column = .I, row = row_index)]
  dt
}))
forest <- data.table::setcolorder(forest, rev(names(forest)))
forest[, visible := FALSE]

for (tree_index in 1:nrow(forest)) {
  the_row <- forest[tree_index, row]
  the_column <- forest[tree_index, column]
  the_tree <- forest[tree_index, tree]

  if (forest[row == the_row & column < the_column, any(tree, -1)] < the_tree ||
      forest[row == the_row & column > the_column, max(tree, -1)] < the_tree ||
      forest[row < the_row & column == the_column, max(tree, -1)] < the_tree ||
      forest[row > the_row & column == the_column, max(tree, -1)] < the_tree) {
    forest[tree_index, visible := TRUE]
  }
}
cat_solution(15, forest[, sum(visible)])
