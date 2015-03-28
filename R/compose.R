#' Generate frequencies in 12-tone equal temperment (12-TET)
#' @param n This note number
#' @param P.a Base frequency
#' @param a Base note number
#' @examples
#'   P.n(40)
#'   P.n(c(40, 42, 43))
P.n <- absolute.frequency <- function(n, P.a = 440, a = 49)
  P.a * (2^(1/12))^(n - a)


#' Invert a chord
#' @param inversion 0 for base, 1 for first inversion, 2 for second inversion
invert.chord <- function(chord, full.inversion) {
  inversion <- full.inversion %% 3
  octave.shift <- (full.inversion - inversion) / 3

  bottom <- chord[(1+inversion):length(chord)]
  top <- chord[0:inversion] + intervals$P8
  octave <- intervals$P8 * octave.shift

  # Notes stay in the same order but are shifted.
  # This makes it easy to select the root note.
  c(top, bottom) + octave
}

#' Generate a sequence of a particular instrument
#' @param frequencies Frequencies of the notes
#' @param starts Start beats of the notes
#' @param durations Lengths of the notes, in beats
#' @param instrument Instrument to play the notes with
#' @param tempo Tempo in bpm
#' @param beats Number of beats in the resulting sequence
#' @param sampling.rate Sampling rate for the resulting wave
#' @examples
#' onbeat.offbeat.23 <- track(110,
#'                            c(2, 3, 5, 6.5, 8),
#'                            durations = c(1, 1, 1.5, 0.5, 1) / 4)
sequencer <- function(frequencies, starts, durations = 0.5,
                      instrument = sinesynth,
                      tempo = 240, beats = 8, sampling.rate = SECOND) {
  notes <- data.frame(frequency = frequencies, start = starts, duration = durations)

  n.samples <- beats * sampling.rate * 60 / tempo
  beat <- seq(1, 9, length.out = n.samples)
  waveform <- rep(0, n.samples)
  for (i in 1:nrow(notes)) {
    selector <- beat >= notes[i,'start'] & beat < (notes[i,'start'] + notes[i,'duration'])
    waveform[selector] <- instrument(notes[i,'frequency'], sum(selector))
  }
  waveform
}

