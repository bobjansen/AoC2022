source("helpers.R")

groups <- strsplit(ustrsplit(readFile("input13.txt"), "\n\n"), "\n")

LOWER <- -1L
EQUAL <- 0L
HIGHER <- 1L

compare_element <- function(left, right) {
  if (is.numeric(left) && is.numeric(right)) {
    sign(left - right)
  } else {
    if (is.numeric(left)) left <- list(left)
    if (is.numeric(right)) right <- list(right)

    for (i in seq_along(left)) {
      if (i > length(right)) {
        return(HIGHER)
      } else {
        res <- compare_element(left[[i]], right[[i]])
        if (res != EQUAL) return(res)
      }
    }

    if (length(left) < length(right)) {
      LOWER
    } else {
      EQUAL
    }
  }
}

compare_group <- function(left, right) {
  left = jsonlite::fromJSON(left, simplifyVector = FALSE)
  right = jsonlite::fromJSON(right, simplifyVector = FALSE)
  compare_element(left, right) < 0L
}

orders <- sapply(groups, \(group) compare_group(group[[1L]], group[[2L]]))
cat_solution(25L, sum(which(orders)))

groups <- unlist(groups)
divider_start <- "[[2]]"
divider_end <- "[[6]]"
groups <- c(groups, divider_start, divider_end)

# Neat: https://stackoverflow.com/a/70138588/862288
groups <- structure(groups, class = "packet")
`>.packet` <- compare_group

groups <- unclass(sort(groups, decreasing = TRUE))

cat_solution(26L, which(groups == divider_start) * which(groups == divider_end))
