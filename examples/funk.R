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
                  bpm = 120, beats = 8, sampling.rate = SECOND) {
  notes <- data.frame(frequency = frequencies, start = starts, duration = durations)

  n.samples <- sampling.rate * 8 * bpm / 60
  beat <- seq(1, 9, length.out = n.samples)
  waveform <- rep(0, n.samples)
  for (i in 1:nrow(notes)) {
    selector <- beat >= notes[i,'start'] & beat < (notes[i,'start'] + notes[i,'duration'])
    waveform[selector] <- instrument(notes[i,'frequency'], sum(selector))
  }
  waveform
}

play(track(P.n(49 + scales$major), c(seq(1, 3, length.out = 7))))


# Eight beats
clave.starts <- c(2, 3, 5, 6.5, 8)
clave.durations <- c(1, 1, 1.5, 0.5, 1)
clave.pitches <- scales$mixolydian[c(1, 3, 5)]

sax.starts <- c(1, 3, 5.5, 6.5, 7, 7.5, 8)
sax.durations <- c(1, 1, 0.5, 0.5, 0.5, 0.5, 0.5)
sax.pitches <- scales$mixolydian[c(1, 3, 5)][c(3, 1, 3, 3, 2, 2, 1)]

arpeggio.starts <- c(1, 2, 3, 3.5, 4.5, 5.5, 6.5, 7, 8)
arpeggio.durations <- c(1, 1, 0.5, 1, 1, 0.5, 1, 1)
