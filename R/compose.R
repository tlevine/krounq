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
sequence <- function(frequencies = 440, starts = NULL, durations = 0.5,
                     instrument = sinesynth,
                     tempo = 240, beats = 8, sampling.rate = SECOND) {
  if (is.null(starts)) {
    if (length(durations) != 1)
      stop('If starts is NULL, durations must have length 1.')
    starts <- seq(1, beats + 1, durations)
    starts <- starts[-length(starts)]
    if (length(starts) == 0)
      starts <- 1
  }
  notes <- data.frame(frequency = frequencies, start = starts, duration = durations)

  n.samples <- beats * sampling.rate * 60 / tempo
  if (!n.samples %% 1 == 0) {
    proposal <- beats * sampling.rate * 60 / round(n.samples)
    stop(paste('Choose the tempo such that the number of samples will be a natural number;', proposal, 'will work.'))
  }
  beat <- seq(1, beats + 1, length.out = n.samples + 1)
  beat <- beat[-(n.samples + 1)]
  waveform <- rep(0, n.samples)
  for (i in 1:nrow(notes)) {
    selector <- beat >= notes[i,'start'] & beat < (notes[i,'start'] + notes[i,'duration'])
    waveform[selector] <- instrument(notes[i,'frequency'], sum(selector))
  }
  waveform[1:n.samples]
}
