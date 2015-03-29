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

TEMPO <- 216 # multiple of 24, for easy division

norm <- function(x) {
  x / max(abs(x))
}

# x <- clip(square(220, SECOND/2) + 0.2 * white.noise(SECOND/2))
drumlike <- function(freq, duration) {
  base <- 0.8 * sawtooth(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- min(duration, round(SECOND/8))
  base.left <- base[1:duration.left]
  base.right <- silence(max(0, duration - duration.left))
  c(base.left, base.right)
}

scratch <- function(freq, duration)
  (0.8 * sine(freq, duration) ^ 3 + runif(duration, -.2, .2)) * fade.linear(duration)

sample.instrument <- function(the.sample) {
  function(., duration) {
    if (duration < length(the.sample)) {
      stop(paste('Duration must be at least', length(the.sample)))
      the.sample[1:duration]
    } else {
      c(the.sample, silence(duration - length(kick)))
    }
  }
}

kick <- sample.instrument(norm(roland$BD1@left[(1:length(roland$BD1@left)) %% 2 == 0]))
snare <- sample.instrument(norm(roland$SD0@left))
hihat <- sample.instrument(norm(roland$HHO@left))

# quarter, quarter, triplets | quarter, two eighths, four eighths
#a <- function(base.note)
# sequence(frequencies = P.n(base.note),
#          c(1, 2, 3, 3 + 2/3, 4 + 1/3,
#            5, 6, 6.5, 7, 7.5, 8, 8.5),
#          durations = 1, instrument = drumlike,
#          tempo = TEMPO, beats = 8)

b <- function(duration)
  sequence(durations = duration, instrument = snare,
           tempo = TEMPO, beats = 8)

e <- function(f, duration, speed) {
  starts <- c(1, 2, 3, 4.5, 5, 6, 7, 8, 8.5)
  start.priorities <- c(1, 5, 7, 3, 8, 9, 4, 7, 2)
  sequence(frequencies = f,
           starts = starts[start.priorities[1:(2 + speed)]],
           durations = duration,
           instrument = drumlike,
           tempo = TEMPO,
           beats = 8)
}

phrase <- function(key = 30, speed = 0) {
  base.duration <- 2 ^ (4 - floor(speed))

  c(e(P.n(key + intervals$P1), base.duration, speed),
    e(P.n(key + intervals$P4), base.duration, speed)) +
  b(base.duration) * 3
}

#play(e(220))
#play(phrase(speed = 1) + phrase(speed = 2))
play(c(phrase(speed = 2), phrase(speed = 4),
       phrase(speed = 1)))
# scales$major

# play(c(a(20), a(24), a(22), a(20)))

# play(a + b + 3 * d)
