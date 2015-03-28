library(devtools)
unloadNamespace('devtools')
devtools::load_all()

x <- sine(440, SECOND/2)
x <- x + runif(SECOND/2, -0.25, 0.25)
x[x > 1] <- 1
x[x < 1] <- 1
play(x)

#freq <- P.n(7, a = 0)
#drums <- sequence(freq, c(1, 2 - 1/8, 3, 4, 4.5),
#                  c(0.75, 1, 0.75, 0.5, 0.5),
#                  beats = 4, tempo = 90)
#play(rep(drums, 4))
