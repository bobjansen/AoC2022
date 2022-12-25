source("helpers.R")

txt <- "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"
txt <- readFile("input25.txt")
lines <- ustrsplit(txt, "\n")

base <- 5L

to_decimal <- function(snafu) {
  digits <- rev(ustrsplit(snafu, ""))
  digits <- gsub("-", "-1", digits)
  digits <- gsub("=", "-2", digits)
  digits <- as.integer(digits)
  decimal <- 0L
  for (i in seq_along(digits)) {
    multiplier <- base^(i - 1L)
    decimal <- decimal + multiplier * digits[[i]]
  }
  decimal
}

decimal_answer <- sum(sapply(lines, to_decimal))

snafu_digit_1 <- c("1", "-")
snafu_digit_2 <- c("2", "=")

to_snafu <- function(decimal, max_exponent = NULL, inverse = FALSE) {
  # print(decimal)
  # print(inverse)
  if (missing(max_exponent)) max_exponent <- ceiling(log(decimal / 2, 5L))
  if (max_exponent < 0L) return("")
  number <- base^max_exponent

  if (2 * number / 5 > decimal) {
    return(paste0("0", to_snafu(decimal, max_exponent - 1L, inverse)))
  }
  snafu <- ""
  if (decimal >= 2 * number ||
      decimal > number + 2L * number / 5L) {
    snafu <- paste0(snafu_digit_2[[1L + inverse]], snafu)
    decimal <- decimal - 2 * number
  } else if (decimal >= 2 * number ||
             decimal > 2 * number / 5L) {
    snafu <- paste0(snafu_digit_1[[1L + inverse]], snafu)
    decimal <- decimal - number
  # } else {
  #   rest <- -(positives[[1L]] - decimal)
  #   if (rest < 0L) {
  #     snafu <- paste0("1", to_snafu(-rest, TRUE))
  #     decimal <- decimal - positives[[1L]]
  #   } else {
  #     snafu <- paste0("0", to_snafu(decimal))
  #   }
  }

  if (decimal == 0L) {
    snafu <- paste0(snafu, rep("0", max_exponent))
  } else if (decimal > 0L) {
    snafu <- paste0(snafu, to_snafu(decimal, max_exponent - 1L, inverse))
  } else if (decimal < 0L) {
    snafu <- paste0(snafu, to_snafu(-decimal, max_exponent - 1L, !inverse))
  }
  snafu
}

# debug(to_snafu)

stopifnot(
  to_snafu(1) == "1",
  to_snafu(2) == "2",
  to_snafu(3) == "1=",
  to_snafu(4) == "1-",
  to_snafu(5) == "10",
  to_snafu(6) == "11",
  to_snafu(7) == "12",
  to_snafu(8) == "2=",
  to_snafu(9) == "2-",
  to_snafu(10) == "20",
  to_snafu(15) == "1=0",
  to_snafu(20) == "1-0",
  to_snafu(2022) == "1=11-2",
  to_snafu(12345) == "1-0---0"
)
cat_solution(49L, to_snafu(decimal_answer))
catn("Good enough")
stopifnot(to_snafu(314159265) == "1121-1110-1=0")

