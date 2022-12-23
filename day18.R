source("helpers.R")
txt <- "
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"
txt <- readFile("input18.csv")

cubes <- data.table::fread(text=txt)
cols <-c("x", "y", "z")
setnames(cubes, cols)
setkeyv(cubes, cols)

cubes[, ':='(x = x + 1L, y = y + 1L, z = z + 1L)]

max_x <- cubes[, max(x)]
max_y <- cubes[, max(y)]
max_z <- cubes[, max(z)]

neighbors <- c(
  (max_z + 1L) * (max_y + 1L),
  (max_z + 1L),
  1L
)
neighbors <- c(neighbors, -neighbors)

cubes[, index := x * neighbors[[1L]] + y * neighbors[[2L]] + z + 1L]

space <- rep(FALSE, max_x * neighbors[[1L]] + max_y * neighbors[[2L]] + max_z)
for (index in cubes$index) {
  space[index] <- TRUE
}

neighbor_count <- 0L
for (index in cubes$index) {
  for (neighbor in neighbors) {
    candidate <- index + neighbor
    if (candidate >= 0L && candidate <= length(space) && space[candidate]) {
      neighbor_count <- neighbor_count + 1L
    }
  }
}

cat_solution(35L, nrow(cubes) * 6L - neighbor_count)
