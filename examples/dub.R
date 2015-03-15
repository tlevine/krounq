library(devtools)
unloadNamespace('devtools')
devtools::load_all()

source('generate-data.R')

key <- P.n(0, a = 0)
attach(intervals)

downbeat <- track(key - P8 + c(P1, P5), c(1, 5), 1)
#' onbeat.offbeat.23 <- track(110,
#'                            c(2, 3, 5, 6.5, 8),
#'                            durations = c(1, 1, 1.5, 0.5, 1) / 4)

play(downbeat)
