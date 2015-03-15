intervals <- list(P1=0, P4=5, P5=7, P8=12,
                  M4=5, m4=4, M5=7, m5=6,
                  M2=2, M3=4, M6=9, M7=11,
                  m2=1, m3=3, m6=8, m7=10)
attach(intervals)

chords <- list(M = c(P1, M3, P5),
               m = c(P1, m3, P5),
               dim = c(P1, m3, m5),
               aug = c(P1, M3, M5))
chords$M7 <- c(chords$M, M7)
chords$m7 <- c(chords$m, m7)
chords$'M7+' = c(chords$aug, M7)
chords$'m7+' = c(chords$aug, m7)

W <- 2
H <- 1
s <- function(xs) cumsum(c(0, xs)[1:7]) # Ignore last note.
scales <- list(major = s(W, W, H, W, W, W, H),
               harmonic.minor = s(W, H, W, W, H, W + H, H),
               natural.minor = s(W, H, W, W, W, H, W),
               dorian = s(W, H, W, W, W, H, W),
               mixolydian = s(W, W, H, W, W, H, W))

detach(intervals)

for (x in c('intervals', 'chords', 'scales')) {
  y <- get(x)
  save(y, file = paste0('data/', x, '.RData'))
}
