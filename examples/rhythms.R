library(devtools)
unloadNamespace('devtools')
devtools::load_all()

freq <- P.n(7, a = 12)
drums <- sequence(freq, c(seq(1, 2, length.out = 4)[-4], 2.5, 3),
                  c(2/3, 2/3, 2/3 + 1/2, 1/2, 1/2),
                  beats = 4, tempo = 60)
play(drums)
