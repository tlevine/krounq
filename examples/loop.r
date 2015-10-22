P.n <- absolute.frequency <- function(n, P.a = 440, a = 49)
  P.a * (2^(1/12))^(n - a)


W <- 2
H <- 1

BPS <- 240 / 60

MELODY <- 49 + c(W, W, H, W, W, W, H)
CHORD <- MELODY[c(1, 3, 5, 7)]

song <- function(seconds) {
  absolute.beat <- floor(seconds * BPS)

  measure <- (absolute.beat %% 4) + 1
  beat <- floor(absolute.beat / 4) + 1

# sin(sin(seconds))
  sin(P.n(CHORD[measure])) * (absolute.beat %% 4) / 4
}

SECONDS <- 10
FRAME.RATE <- 44100
play(wave(song((1:(SECONDS*FRAME.RATE))/:FRAME.RATE)))
