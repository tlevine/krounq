P.n <- absolute.frequency <- function(n, P.a = 440, a = 49)
  P.a * (2^(1/12))^(n - a)


W <- 2
H <- 1

BPS <- 240 / 60

scale <- 49 + c(0 W, W, H, W, W, W, H)

song <- function(seconds) {
  absolute.beat <- floor(seconds * BPS)

  measure <- (absolute.beat %% 4) + 1
  beat <- floor(absolute.beat / 4) + 1

  note <- ceiling(ChickWeight[measure,'weight'] %% 8)
  octave <- floor(ChickWeight[measure,'weight'] / 8)
  scale[n %% 8] + 12 * octave

# sin(sin(seconds))
  sin(P.n(pitch)) * (absolute.beat %% 4) / 4
}

FRAME.RATE <- 44100
w <- wave(song((1:(nrow(ChickWeight)*FRAME.RATE))/FRAME.RATE))
