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
instructions[, ':='(CycleCount = cumsum(Cost), RegisterX = NA_integer_)]
instructions[Instr == "addx", RegisterX := cumsum(Arg) + 1L]
instructions[, RegisterX := nafill(RegisterX, type = "locf")]

cat_solution(19L,
  sum(sapply(seq(from = 20L, to = 220, by = 40L), \(t) {
    instructions[CycleCount < t][max(InstrIndex) == InstrIndex, RegisterX * t]
  }))
)

program_length <- instructions[, max(CycleCount)]
instructions <- merge(
  data.table::data.table(CycleCount = 1:program_length),
  instructions,
  all.x = TRUE,
  on = "CycleCount"
)
instructions[, RegisterX := nafill(
  c(1L, shift(RegisterX, 1L)),
  type = "locf")[2:(program_length + 1L)]]

instructions[, ':='(
  x_pos = (CycleCount - 1) %% 40,
  y_pos = (CycleCount - 1) %/% 40
)]
instructions[, visible := abs(x_pos - RegisterX) <= 1L]

cat("Puzzle 20:")
for (i in 1:nrow(instructions)) {
  if (instructions[i, x_pos == 0L]) catn()
  if (instructions[i, visible]) {
    cat("#")
  } else {
    cat(".")
  }
}
catn()
