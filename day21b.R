source("helpers.R")

options(digits = 18L) # For printing large numerics without scientific notation

txt <- "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"
txt <- readFile("input21.txt")

part2 <- FALSE

monkies <- data.table::fread(text = txt, sep = ":", header = FALSE)
setnames(monkies, c("monkey", "value"))
monkies <- monkies[,
  c("input1", "op", "input2") := tstrsplit(value, split = " ", )]
monkies[, ':='(
  index = .I,
  resolved = grepl("[[:digit:]]", value)
)]
monkies <- setorder(monkies, "monkey")

last_rows <- Inf
while (nrow(monkies) < last_rows && nrow(monkies) > 1L) {
  last_rows <- nrow(monkies)
  monkies[
    monkies[resolved == TRUE],
    value1 := as.numeric(i.value),
    on = .(input1 = monkey)]
  monkies[
    monkies[resolved == TRUE],
    value2 := as.numeric(i.value),
    on = .(input2 = monkey)]
  monkies <- monkies[resolved == FALSE]

  todo <- monkies[, !is.na(value1) & !is.na(value2)]
  monkies[todo == TRUE, resolved := TRUE]
  monkies[todo & op == "-", value := value1 - value2]
  monkies[todo & op == "+", value := value1 + value2]
  monkies[todo & op == "*", value := value1 * value2]
  monkies[todo & op == "/", value := value1 / value2]
}

cat_solution(41L, monkies[, value])

options(digits = 8L)
