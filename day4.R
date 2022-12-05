library(data.table)

assignments <- data.table::fread("input4.csv", sep="", header = FALSE)
assignments <- assignments[, tstrsplit(
  V1, "-|,", type.convert = TRUE, names = c("lb1", "ub1", "lb2", "ub2")
)]

contained <- assignments[(lb1 >= lb2 & ub1 <= ub2) | (lb2 >= lb1 & ub2 <= ub1)]
cat("Puzzle 7:", nrow(contained), "\n")

left <- assignments[, .(ID = .I, lb1, ub1)]
right <- assignments[, .(ID = .I, lb2, ub2)]

data.table::setkeyv(left, c("ID", "lb1", "ub1"))
data.table::setkeyv(right, c("ID", "lb2", "ub2"))

overlapped <- data.table::foverlaps(left, right)[!is.na(lb2)]
cat("Puzzle 8:", nrow(overlapped), "\n")
