# https://en.wikipedia.org/wiki/Funk
# https://en.wikipedia.org/wiki/Dorian_mode
# https://en.wikipedia.org/wiki/Mixolydian_mode
# https://en.wikipedia.org/wiki/Ostinato#Vamp
# https://en.wikipedia.org/wiki/Clave_%28rhythm%29
# https://en.wikipedia.org/wiki/Guajeo

eight.beats <- function(bpm, starts, durations, pitches, instrument, fade) {
  n.samples <- SECONDS * 8 * bpm / 60
  eight.beat.add.note(n.samples)
}  
eight.beat.add.note <- function(n.samples, start, duration, pitch, instrument, fade) {
  beat <- seq(1, 9, length.out = n.samples)
  selector <- beat >= start & beat < (start + duration)
  waveform <- rep(0, n.samples)
  waveform[selector] <- instrument(sum(selector)) * fade(sum(selector))
  waveform
}

# Eight beats
clave.starts <- c(2, 3, 5, 6.5, 8)
clave.durations <- c(1, 1, 1.5, 0.5, 1)
clave.pitches <- scales$mixolydian[c(1, 3, 5)]
clave.instrument <- curry(square, 220)
clave.fade <- function(duration) {
  elbow <- round(duration / 8)
  elbow.amplitude <- 0.2
  c(seq(1, elbow.amplitude, length.out = elbow),
    seq(elbow.amplitude, 0, length.out = duration - elbow))
}

sax.starts <- c(1, 3, 5.5, 6.5, 7, 7.5, 8)
sax.durations <- c(1, 1, 0.5, 0.5, 0.5, 0.5, 0.5)
sax.pitches <- scales$mixolydian[c(1, 3, 5)][c(3, 1, 3, 3, 2, 2, 1)]
sax.instrument <- curry(overtones, seq(1, 2^4, 2), square)

arpeggio.starts <- c(1, 2, 3, 3.5, 4.5, 5.5, 6.5, 7, 8)
arpeggio.durations <- c(1, 1, 0.5, 1, 1, 0.5, 1, 1)
