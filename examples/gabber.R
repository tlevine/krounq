library(devtools)
unloadNamespace('devtools')
devtools::load_all()

# x <- clip(square(220, SECOND/2) + 0.2 * white.noise(SECOND/2))
scratch <- function(freq, duration) {
  base <- 0.8 * sine(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- round(SECOND/8)
  base.left <- base[1:duration.left]
  base.right <- silence(duration - duration.left)
  c(base.left, base.right)
}

kick.instrument <- function(., duration) {
  data(kick)
  if (duration < length(kick))
    stop(paste('Duration must be at least', length(kick)))
  c(kick, silence(duration - length(kick)))
}

# quarter, quarter, triplets | quarter, two eighths, four eighths
b <- sequence(300, c(1, 2, 3, 3 + 2/3, 4 + 1/3,
                     1, 2, 2.5, 3, 3.5, 4, 4.5),
              durations = 1, instrument = kick.instrument,
              tempo = 205)

play(b[-length(b)])

#freq <- P.n(7, a = 0)
#drums <- sequence(freq, c(1, 2 - 1/8, 3, 4, 4.5),
#                  c(0.75, 1, 0.75, 0.5, 0.5),
#                  beats = 4, tempo = 90)
#play(rep(drums, 4))
