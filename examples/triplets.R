library(devtools)
unloadNamespace('devtools')
devtools::load_all()

source('generate-data.R')

# play(curry(overtones, c(1.2,0.8,2.1,2.7,1,0.5,3), sawtooth, 329.6276))

#   play(2^3 * Reduce(tuneR::bind, lapply(P.n(scales$major, a = -12), function(x) wave(SECOND, curry(overtones, 1, square, x)))), 'play')
