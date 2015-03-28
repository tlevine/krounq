library(devtools)
unloadNamespace('devtools')
library(ddr)
data(roland)
unloadNamespace('ddr')
devtools::load_all()
source('generate-data.R')

# Aesthetics
# * Tilt
# * Color
# * X
# * Y
# * 

norm <- function(x) {
  x / max(abs(x))
}

# x <- clip(square(220, SECOND/2) + 0.2 * white.noise(SECOND/2))
bass.drum <- function(freq, duration) {
  base <- 0.8 * sine(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- round(SECOND/8)
  base.left <- base[1:duration.left]
  base.right <- silence(duration - duration.left)
  c(base.left, base.right)
}

scratch <- function(freq, duration)
  (0.8 * sine(freq, duration) ^ 3 + runif(duration, -.2, .2)) * fade.linear(duration)

sample.instrument <- function(the.sample) {
  function(., duration) {
    if (duration < length(the.sample))
      # stop(paste('Duration must be at least', length(the.sample)))
      the.sample[1:duration]
    else
      c(the.sample, silence(duration - length(kick)))
  }
}

kick <- sample.instrument(norm(roland$BD1@left[(1:length(roland$BD1@left)) %% 2 == 0]))
snare <- sample.instrument(norm(roland$SD0@left))
hihat <- sample.instrument(norm(roland$HHO@left))

# quarter, quarter, triplets | quarter, two eighths, four eighths
a <- sequence(0, c(1, 2, 3, 3 + 2/3, 4 + 1/3,
                   5, 6, 6.5, 7, 7.5, 8, 8.5),
              durations = 1, instrument = kick,
              tempo = 205, beats = 8)
b <- sequence(durations = .25, instrument = snare,
              tempo = 205, beats = 8)
d <- sequence(frequencies = P.n(40 + scales$major[c(1,3,2,1)]),
              starts = c(1, 5, 9, 13), durations = 4,
              instrument = scratch, tempo = 205, beats = 16)
#play(d)

play(a + b + 3 * d)
