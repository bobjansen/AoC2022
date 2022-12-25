source("helpers.R")
txt <- "1
2
-3
3
-2
0
4
"
txt <- readFile("input20.csv")
numbers <- as.integer(ustrsplit(txt, "\n"))
numbers <- data.table(number = numbers)
numbers <- numbers[, start_index := .I]

insert_at <- function(dt, old, new) {

  if (old == new) {
    dt
  } else {
    old_row <- dt[old]
    dt <- dt[-old,]
    rbind(
      dt[seq_len(new - 1L)],
      old_row,
      dt[seq_len(number_count - new) + new - 1L]
    )
  }
}

number_count <- nrow(numbers)
for (i in 1:nrow(numbers)) {
  sel <- numbers[, i == start_index]
  from <- which(sel)
  to <- (from + numbers[sel, number]) %% (number_count - 1L)
  if (to == 1L) to <- number_count
  numbers <- insert_at(numbers, from, to)
}

zero_at <- numbers[, which(number == 0L)]
gps <- numbers[(zero_at + c(1000L, 2000L, 3000L)) %% nrow(numbers)]
cat_solution(39L, gps[, sum(number)])
