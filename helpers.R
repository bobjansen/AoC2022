library(data.table)
library(recollections)
library(zeallot)

readFile <- function(file) {
  readChar(file, file.info(file)$size)
}

ustrsplit <- function(...) {
  unlist(tstrsplit(...))
}

catn <- function(...) {
  cat(...)
  cat("\n")
}

cat_solution <- function(puzzle_id, solution) {
  catn(sprintf("Puzzle %2d: %s", puzzle_id, solution))
}

dev_null <- \(...) invisible()
