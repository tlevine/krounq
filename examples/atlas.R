library(devtools)
unloadNamespace('devtools')
devtools::load_all()

source('generate-data.R')
TEMPO <- 216 # multiple of 24, for easy division
f <- function(x) P.n(x, a = 12)

drumlike <- function(freq, duration) {
  base <- 0.8 * sawtooth(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- min(duration, round(SECOND/8))
  base.left <- base[1:duration.left]
  base.right <- silence(max(0, duration - duration.left))
  c(base.left, base.right)
}

attach(intervals)
downbeat <- sequence(f(P5), c(1, 5), 2,
                     instrument = drumlike, tempo = TEMPO)
triplets <- sequence(f(P1), seq(1, 5, length.out = 10)[-10], 1/3, beats = 4,
                     instrument = drumlike, tempo = TEMPO)

#' onbeat.offbeat.23 <- track(110,
#'                            c(2, 3, 5, 6.5, 8),
#'                            durations = c(1, 1, 1.5, 0.5, 1) / 4)

#play(rep(triplets, 2))
play(rep(downbeat + triplets, 2))

detach(intervals)
