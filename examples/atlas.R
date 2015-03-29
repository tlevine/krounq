#!/usr/bin/env Rscript
library(MASS)
library(plyr)

library(devtools)
unloadNamespace('devtools')
devtools::load_all()

library(ddr)
data(roland)
unloadNamespace('ddr')

source('generate-data.R')

TEMPO <- 216 # multiple of 24, for easy division
LIGHT.THROUGH.FIBER <- 1.444 * 2 * (1000/299792)

norm <- function(x) {
  x / max(abs(x))
}

drumlike <- function(freq, duration) {
  base <- 0.8 * sawtooth(freq, duration) ^ 3 + runif(duration, -.2, .2)
  duration.left <- min(duration, round(SECOND/8))
  base.left <- base[1:duration.left]
  base.right <- silence(max(0, duration - duration.left))
  c(base.left, base.right)
}

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
rim <- sample.instrument(norm(roland$RIM@left))

phrase <- function(key = 30, speed = 2, pickup = NULL, drums = TRUE,
                   rhythm = c(1, 4)) {
  base.duration <- 2 ^ (4 - floor(speed))

  pounding <- sequence(durations = base.duration,
                       instrument = if (drums) snare else rim,
                       tempo = TEMPO, beats = 8)

  f <- rep(key, length(rhythm))
  if (!is.null(pickup))
    f[floor(rhythm) %% 4 == 0] <- key + pickup
  melody <- sequence(frequencies = P.n(f),
                     starts = rhythm,
                     durations = 0.5,
                     instrument = drumlike,
                     tempo = TEMPO,
                     beats = 8)

  rep(2.5 * pounding + melody, 2)
}


RHYTHMS <- list(c(1, 2, 3, 4.5, 5, 6, 7, 8, 8.5),
                c(1, 2, 3, 3 + 2/3, 4 + 1/3, 5, 6, 6.5, 7, 7.5, 8, 8.5),
                c(1, 3, 5, 7), 1:8)

plot.phrase <- function(df) {
  # Response time divided by speed of light through fiber
  if (nrow(df) > 0) {
     .selector <- order(df$rt)[round(nrow(df)/2)]
    .rt.normalized <- df[.selector,'rt'] / (LIGHT.THROUGH.FIBER * df[.selector,'dist'])
    pickup <- scales$major[round(.rt.normalized)]
  } else {
    pickup <- NULL
  }

  # Number of different cities
  rhythm <- length(unique(df$dst_city)) + 1
  if (rhythm > 4)
    rhythm <- 4

  phrase(key = 30, speed = max(1, nrow(df)),
         pickup = pickup,
         drums = nrow(df) > 0,
         rhythm = RHYTHMS[[rhythm]])
}




library(RColorBrewer)

frame <- function(df.full, df) {
  colors <- paste0(brewer.pal(12, 'Set3'), '99')
  names(colors)[1:length(levels(df.full$dst_city))] <- levels(df.full$dst_city)

  fg <- 'grey60'
  bg <- 'black'
  par(fg = fg, col = fg, col.axis = fg,
      col.lab = fg, col.main = fg, col.sub = fg,
 #    cex.axis = 2,
      family = 'Helvetica',
      las = 1, bg = bg)

  plot(dist ~ dist_theoretical_improvement, data = df.full, type = 'n',
       main = 'Targetting 173.245.58.117 (anycast)',
       axes = F,
       xlab = 'Distance farther than the closest instance (km)',
       ylab = 'Distance to chosen instance (km)',
       sub = 'Each dot is a measurement. Bigger dots have higher latency.',
       xlim = c(0.8, 1.1) * range(df.full$dist_theoretical_improvement),
       ylim = c(0.8, 1.1) * range(df.full$dist))
  axis(1, at = seq(0, max(df.full$dist_theoretical_improvement), 2e2))
  axis(2, at = seq(0, max(df.full$dist), 1e3))
  points(df$dist ~ df$dist_theoretical_improvement,
         pch = 21, lwd = 0, cex = sqrt(df$rt), bg = colors[df$dst_city])
  text(x = c(0.8, 0.1) * max(df.full$dist_theoretical_improvement),
       y = 0.2 * max(df.full$dist),
       label = c('Indirect routes', 'Direct routes'))
}



anycast <- read.csv('../ripe-atlas-anycast/anycast.csv', stringsAsFactors = FALSE)
anycast$as <- anycast$asn_v4
anycast[anycast$asf == 6,'as'] <- anycast[anycast$asf == 6,'asn_v6']
anycast$asn_v4 <- anycast$asn_v6 <- NULL
anycast$af <- factor(anycast$af, levels = c(4, 6))
anycast$dst_city <- factor(anycast$dst_city)
anycast$datetime <- as.POSIXct(anycast$timestamp, origin = '1970-01-01')

anycast.probe <- ddply(anycast, 'prb_id', function(df) {
  df[order(df$rt)[1],]
})

music.step <- 3600 * 3
video.step <- 3600
music.starts <- seq(min(anycast$timestamp), max(anycast$timestamp) + music.step, music.step)
video.starts <- seq(min(anycast$timestamp), max(anycast$timestamp) + video.step, video.step)

video <- function(anycast) {
  for (start in video.starts) {
    png(sprintf('frames/%s.png', start), width = 1600, height = 900)
    df <- subset(anycast, timestamp >= start & timestamp < start + video.step)
    frame(anycast, df)
    dev.off()
  }
}

music <- function(anycast) {
  for (start in music.starts) {
    anycast[anycast$timestamp >= start,'start'] <- start
  }
  df <- anycast[anycast$start == start,]
  do.call(c,lapply(unique(anycast$start),
                   function(start) plot.phrase(df)))
}
# video(anycast)
# music(anycast)
# krounq::play(phrase(anycast.probe, subset(anycast.probe, dst_city == 'LHR')))
