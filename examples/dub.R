library(devtools)
unloadNamespace('devtools')
devtools::load_all()

source('generate-data.R')

attach(intervals)
f <- function(x) P.n(x, a = 12)

downbeat <- sequence(f(P5), c(1, 5), 2)
triplets <- sequence(f(P1), seq(1, 5, length.out = 10)[-10], 1/3, beats = 4)

#' onbeat.offbeat.23 <- track(110,
#'                            c(2, 3, 5, 6.5, 8),
#'                            durations = c(1, 1, 1.5, 0.5, 1) / 4)

play(rep(triplets, 2))
#play(rep(downbeat + triplets, 2))
