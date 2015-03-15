library(devtools)
unloadNamespace('devtools')
devtools::load_all()
source('generate-data.R')

# https://en.wikipedia.org/wiki/Funk
# https://en.wikipedia.org/wiki/Dorian_mode
# https://en.wikipedia.org/wiki/Mixolydian_mode
# https://en.wikipedia.org/wiki/Ostinato#Vamp
# https://en.wikipedia.org/wiki/Clave_%28rhythm%29
# https://en.wikipedia.org/wiki/Guajeo

track <- function(frequencies, starts, durations = 0.5,
                  instrument = sinesynth,
                  bpm = 240, beats = 8, sampling.rate = SECOND) {
  notes <- data.frame(frequency = frequencies, start = starts, duration = durations)

  n.samples <- 8 * sampling.rate * 60 / bpm
  beat <- seq(1, 9, length.out = n.samples)
  waveform <- rep(0, n.samples)
  for (i in 1:nrow(notes)) {
    selector <- beat >= notes[i,'start'] & beat < (notes[i,'start'] + notes[i,'duration'])
    waveform[selector] <- instrument(notes[i,'frequency'], sum(selector))
  }
  waveform
}

onbeat.offbeat.23 <- track(220,
                           c(2, 3, 5, 6.5, 8),
                           durations = c(1, 1, 1.5, 0.5, 1),
                           instrument = clave)

sax <- track(P.n(scales$mixolydian[c(1, 3, 5)][c(3, 1, 3, 3, 2, 2, 1)], a = 12),
             c(1, 3, 5.5, 6.5, 7, 7.5, 8),
             durations = c(1, 1, 0.5, 0.5, 0.5, 0.5, 0.5),
             instrument = sinesynth)

arpeggio.starts <- c(1, 2, 3, 3.5, 4.5, 5.5, 6.5, 7, 8)
arpeggio.durations <- c(1, 1, 0.5, 1, 1, 0.5, 1, 1)
