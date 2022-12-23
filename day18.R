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

to_index <- function(x, y, z) {
  x * neighbors[[1L]] + y * neighbors[[2L]] + z + 1L
}

cubes[, index := to_index(x, y, z)]

space_size <- max_x * neighbors[[1L]] + max_y * neighbors[[2L]] + max_z

space <- rep(FALSE, space_size)
for (index in cubes$index) {
  space[index] <- TRUE
}

neighbor_count <- 0L
for (index in cubes$index) {
  for (neighbor in neighbors) {
    candidate <- index + neighbor
    if (candidate >= 1L && candidate <= length(space) && space[candidate]) {
      neighbor_count <- neighbor_count + 1L
    }
  }
}

answer1 <- nrow(cubes) * 6L - neighbor_count
cat_solution(35L, answer1)

todo <- priorityQueue(to_index(0L, 0L, 0L))
known <- rep(FALSE, space_size)
visited <- rep(FALSE, space_size)
hit <- rep(FALSE, space_size)

while (!empty(todo)) {
  index <- topAndPop(todo)
  visited[[index]] <- TRUE
  for (neighbor in neighbors) {
    candidate <- index + neighbor
    if (candidate >= 1L && candidate <= length(space) && !hit[[candidate]]) {
      if (space[candidate]) {
        hit[[index]] <- TRUE
      } else if (!known[[candidate]] && !visited[[candidate]]) {
        known[[candidate]] <- TRUE
        push(todo, candidate)
      }
    }
  }
}

neighbor_count <- 0L
for (index in which(!space & !visited)) {
  for (neighbor in neighbors) {
    candidate <- index + neighbor
    if (candidate >= 1L && candidate <= length(space) && space[candidate]) {
      neighbor_count <- neighbor_count + 1L
    }
  }
}

cat_solution(36L, answer1 - neighbor_count)
