txt <- "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"

# input_txt <- ustrsplit(txt, "\n\n")

input_txt <- ustrsplit(readFile("input11.txt"), "\n\n")

setClass("Monkey", representation(
  ID = "integer",
  items = "numeric",
  operation = "function",
  test = "function",
  goal_true = "integer",
  goal_false = "integer",
  inspection_count = "integer"
))

divisor <- 1L

create_monkey <- function(txt) {
  lines <- ustrsplit(txt, "\n")
  id <- as.integer(gsub("\\D+", "", lines[[1L]]))
  items <- as.numeric(ustrsplit(ustrsplit(lines[[2L]], ":")[[2L]], ","))
  operation_parts <- unlist(tstrsplit(lines[[3L]], " ", keep = 7:8))
  operation <- if (operation_parts[[1L]] == "*") {
    if (operation_parts[[2L]] == "old") {
      function(x) x * x
    } else {
      function(x) x * as.integer(operation_parts[[2L]])
    }
  } else {
    function(x) x + as.integer(operation_parts[[2L]])
  }
  test_num <- as.integer(gsub("\\D+", "", lines[[4L]]))
  divisor <<- divisor * test_num
  test <- function(x) x %% test_num == 0L
  goal_true <- as.integer(gsub("\\D+", "", lines[[5L]]))
  goal_false <- as.integer(gsub("\\D+", "", lines[[6L]]))

  new(
    "Monkey",
    ID = id,
    items = items,
    operation = operation,
    test = test,
    goal_true = goal_true,
    goal_false = goal_false,
    inspection_count = 0L
  )
}

monkeys <- sapply(input_txt, create_monkey)
names(monkeys) <- paste("Monkey", 1:length(monkeys))

do_round <- function(monkeys, divisor) {
  for (i in 1:length(monkeys)) {
    monkey <- monkeys[[i]]
    # catn("Monkey", monkey@ID)
    for (item in monkey@items) {
      monkey@inspection_count <- monkey@inspection_count + 1L
      # catn("\tMonkey inspects an item with a worry level of", item)
      worry_level <- monkey@operation(item)
      # catn("\t\tNew worry level of", worry_level)
      worry_level <- worry_level %% divisor
      # worry_level <- as.integer(floor(worry_level / divisor))
      # catn("\t\tMinkey got bored. New worry level of", worry_level)
      test_result <- monkey@test(worry_level)
      if (test_result) {
        monkeys[[monkey@goal_true + 1L]]@items <- c(
          monkeys[[monkey@goal_true + 1L]]@items, worry_level
        )
      } else {
        monkeys[[monkey@goal_false + 1L]]@items <- c(
          monkeys[[monkey@goal_false + 1L]]@items, worry_level
        )
      }
    }
    monkey@items <- integer(0)
    monkeys[[i]] <- monkey
  }
  monkeys
}

for (i in 1:10000) {
  monkeys <- do_round(monkeys, divisor=divisor)
  if (round(i %% 10000 == 0L)) {
    for (monkey in monkeys) {
      catn("Monkey ", monkey@ID, ": ", toString(monkey@items), sep="")
    }
  }
}

counts <- c()
for (monkey in monkeys) {
  counts <- c(counts, monkey@inspection_count)
  catn("Monkey ", monkey@ID, ": ", monkey@inspection_count, sep="")
}
prod(sort(counts, decreasing = TRUE)[1:2])
