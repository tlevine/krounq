intervals <- list(P1=0, P4=5, P5=7, P8=12,
                  M2=2, M3=4, M6=9, M7=11,
                  m2=1, m3=3, m6=8, m7=10)
attach(intervals)

chords <- list(M = cumsum(c(P1, M3, P5)),
               m = cumsum(c(P1, m3, P5)),
               M7 = cumsum(c(P1, M3, P5, M7)),
               m7 = cumsum(c(P1, M3, P5, m7)))

scales <- list(natural.minor = c(P1, m2, m3, P4, P5, m6, m7),
               major = c(P1, M2, M3, P4, P5, M6, M7))

detach(intervals)

for (x in ls())
  save(intervals, paste0('data/', x, '.RData'))
