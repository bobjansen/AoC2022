source("helpers.R")
txt <- readFile("input18.csv")

cubes <- data.table::fread(text=txt)
cols <-
setnames(cubes, c("x", "y", "z"))
setkey(cubes, "x", "y", "z")

neighbors <- list(
  c(1L, 0L, 0L),
  c(0L, 1L, 0L),
  c(0L, 0L, 1L),
  c(-1L, 0L, 0L),
  c(0L, -1L, 0L),
  c(0L, 0L, -1L)
)

locations <- list()
for (i in 1:nrow(cubes)) {
  locations[toString(cubes[i, ])] <- TRUE
}
locations <- names(locations)

neighbor_count <- 0L
for (i in 1:nrow(cubes)) {
  for (neighbor in neighbors) {
    if (toString(cubes[i, ] + neighbor) %in% locations) {
      neighbor_count <- neighbor_count + 1L
    }
  }
}

cat_solution(35L, nrow(cubes) * 6L - neighbor_count)
