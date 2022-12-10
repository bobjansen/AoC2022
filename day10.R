source("helpers.R")

txt <- readFile("input10.txt")
instructions <- ustrsplit(txt, "\n")

cycles <- data.table::data.table(Instr = c("noop", "addx"), Cost = c(1L, 2L))

instructions <- data.table::data.table(instructions)
instructions <- instructions[,
  tstrsplit(instructions, split = " ", names = c("Instr", "Arg"))
]
instructions[, InstrIndex := .I]

instructions <- merge(instructions, cycles, on = "Instr")
data.table::setorderv(instructions, "InstrIndex")
instructions[, CycleCount := cumsum(Cost)]
instructions[, RegisterX := NA_integer_]
instructions[Instr == "addx", RegisterX := cumsum(Arg) + 1L]
instructions[, RegisterX := nafill(RegisterX, type = "locf")]

sum(sapply(seq(from = 20L, to = 220, by = 40L), \(t) {
  instructions[CycleCount < t][max(InstrIndex) == InstrIndex, RegisterX * t]
}))
