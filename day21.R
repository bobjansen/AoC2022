txt <- readFile("input21.txt")
part2 <- FALSE

monkies <- data.table::fread(text=txt, sep=":", header = FALSE)
setnames(monkies, c("monkey", "value"))
monkies <- monkies[,
  c("val1", "op", "val2") := tstrsplit(value, split = " ", )]
monkies[is.na(val2), ':='(value = val1)]
monkies[!is.na(val2), ':='(value = NA)]
monkies[is.na(val2), ':='(val1 = NA)]
monkies[, index := .I]

if (part2) monkies <- monkies[monkey != "humn"]

while (monkies[monkey == "root", is.na(value)]) {
  for (i in monkies[!is.na(value), index]) {
    monkey_name <- monkies[index == i, monkey]
    monkey_value <- monkies[index == i, value]
    monkies[val1 == monkey_name, val1 := monkey_value]
    monkies[val2 == monkey_name, val2 := monkey_value]
  }
  print(monkies[monkey=="root"])
  monkies <- monkies[is.na(value)]
  print(monkies[monkey=="root"])
  for (i in monkies[is.na(value) & !is.na(op), index]) {
    if (monkies[index == i, op] == "+") {
      monkies[index == i, value := as.numeric(val1) + as.numeric(val2)]
    } else if (monkies[index == i, op] == "*") {
      monkies[index == i, value := as.numeric(val1) * as.numeric(val2)]
    } else if (monkies[index == i, op] == "-") {
      monkies[index == i, value := as.numeric(val1) - as.numeric(val2)]
    } else if (monkies[index == i, op] == "/") {
      monkies[index == i, value := as.numeric(val1) / as.numeric(val2)]
    }
  }
  print(monkies[monkey=="root"])
  print(nrow(monkies))

}

