source("helpers.R")

txt <- "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"

txt <- readFile("input13.txt")
txt <- gsub("]", ")", gsub("[", "list(", txt, fixed = TRUE), fixed = TRUE)
groups <- strsplit(ustrsplit(txt, "\n\n"), "\n")

LOWER <- -1L
EQUAL <- 0L
HIGHER <- 1L

compare_number <- function(left, right) {
  if (left == right) {
    EQUAL
  } else if (left < right) {
    LOWER
  } else {
    HIGHER
  }
}

compare_element <- function(left, right) {
  if (is.numeric(left) && is.numeric(right)) {
    compare_number(left, right)
  } else if (is.numeric(left) && !is.numeric(right)) {
    compare_element(list(left), right)
  } else if (!is.numeric(left) && is.numeric(right)) {
    compare_element(left, list(right))
  } else {
    # if (length(left) == 3L) browser()
    for (i in seq_along(left)) {
      if (i > length(right)) {
        return(HIGHER)
      } else {
        res <- compare_element(left[[i]], right[[i]])
        if (res %in% c(LOWER, HIGHER)) return(res)
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
  left = eval(parse(text = left))
  right = eval(parse(text = right))

  for (i in seq_along(left)) {
    if (i > length(right)) {
      return(FALSE)
    } else {
      res <- compare_element(left[[i]], right[[i]])
      if (res == LOWER) {
        return(TRUE)
      } else if (res == HIGHER) {
        return(FALSE)
      }
    }
  }
  TRUE
}

orders <- sapply(groups, \(group) compare_group(group[[1L]], group[[2L]]))
cat_solution(25L, sum(which(orders)))

groups <- unlist(groups)
divider_start <- "list(list(2))"
divider_end <- "list(list(6))"
groups <- c(groups, divider_start, divider_end)

groups <- structure(groups, class = "packet")
`[.packet` <- function(x, i, ...) structure(unclass(x)[i], class = "packet")
`==.packet` <- function(a, b) {identical(a, b)}

`>.packet` <- compare_group

groups <- unclass(sort(groups, decreasing = TRUE))

cat_solution(25L, which(groups == divider_start) * which(groups == divider_end))
