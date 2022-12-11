source("helpers.R")

input_txt <- ustrsplit(readFile("input11.txt"), "\n\n")

setClass("Monkey", representation(
  ID = "integer",
  items = "numeric",
  operation = "function",
  test_num = "integer",
  goal_true = "integer",
  goal_false = "integer",
  inspection_count = "integer"
))

create_monkey <- function(txt) {
  lines <- ustrsplit(txt, "\n")
  id <- as.integer(gsub("\\D+", "", lines[[1L]]))
  items <- as.numeric(ustrsplit(ustrsplit(lines[[2L]], ":")[[2L]], ","))
  operation_parts <- unlist(tstrsplit(lines[[3L]], " ", keep = 7:8))
  operation <- if (operation_parts[[1L]] == "*") {
    if (operation_parts[[2L]] == "old") {
      function(x) x * x
    } else {
      y <- force(as.numeric(operation_parts[[2L]]))
      function(x) x * y
    }
  } else {
    y <- force(as.numeric(operation_parts[[2L]]))
    function(x) x + y
  }
  test_num <- as.integer(gsub("\\D+", "", lines[[4L]]))
  divisor <<- divisor * test_num
  goal_true <- as.integer(gsub("\\D+", "", lines[[5L]]))
  goal_false <- as.integer(gsub("\\D+", "", lines[[6L]]))

  new(
    "Monkey",
    ID = id,
    items = items,
    operation = operation,
    test_num = test_num,
    goal_true = goal_true + 1L,
    goal_false = goal_false + 1L,
    inspection_count = 0L
  )
}

do_round <- function(monkeys, catter, divisor, reduce_worry = TRUE) {
  for (i in 1:length(monkeys)) {
    catter("Monkey", monkey@ID)
    monkeys[[i]]@inspection_count <-
      monkeys[[i]]@inspection_count + length(monkeys[[i]]@items)

    to_true <- c()
    to_false <- c()

    for (item in monkeys[[i]]@items) {
      catter("\tMonkey inspects an item with a worry level of", item)
      worry_level <- monkeys[[i]]@operation(item)
      catter("\t\tNew worry level of", worry_level)
      worry_level <- worry_level %% divisor
      if (reduce_worry) worry_level <- floor(worry_level / 3L)
      catter("\t\tMonkey got bored. New worry level of", worry_level)
      if (worry_level %% monkeys[[i]]@test_num == 0L) {
        to_true <- c(to_true, worry_level)
      } else {
        to_false <- c(to_false, worry_level)
      }
    }
    monkeys[[monkeys[[i]]@goal_true]]@items <-
      c(monkeys[[monkeys[[i]]@goal_true]]@items, to_true)
    monkeys[[monkeys[[i]]@goal_false]]@items <-
      c(monkeys[[monkeys[[i]]@goal_false]]@items, to_false)
    monkeys[[i]]@items <- integer(0L)
  }
  monkeys
}


divisor <- 1
monkeys <- sapply(input_txt, create_monkey)
names(monkeys) <- paste("Monkey", 1:length(monkeys))

for (i in 1:20) {
  monkeys <- do_round(monkeys, catter = dev_null, divisor=divisor)
}

counts <- c()
for (monkey in monkeys) {
  counts <- c(counts, monkey@inspection_count)
  catn("Monkey ", monkey@ID, ": ", monkey@inspection_count, sep="")
}
cat_solution(21L, prod(sort(counts, decreasing = TRUE)[1:2]))


divisor <- 1
monkeys <- sapply(input_txt, create_monkey)
names(monkeys) <- paste("Monkey", 1:length(monkeys))

for (i in 1:10000) {
  monkeys <- do_round(
    monkeys, catter = dev_null, divisor = divisor, reduce_worry = FALSE
  )
}

counts <- c()
for (monkey in monkeys) {
  counts <- c(counts, monkey@inspection_count)
  catn("Monkey ", monkey@ID, ": ", monkey@inspection_count, sep="")
}
cat_solution(22L, prod(sort(counts, decreasing = TRUE)[1:2]))
