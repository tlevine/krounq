library(devtools)
unloadNamespace('devtools')
load('examples/roland.rda')
devtools::load_all()
source('generate-data.R')

TEMPO <- 240 # multiple of 24, for easy division

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
      the.sample[1:duration]
    } else {
      c(the.sample, silence(duration - length(the.sample)))
    }
  }
}

kick <- sample.instrument(norm(roland$BD1@left[(1:length(roland$BD1@left)) %% 2 == 0]))
snare <- sample.instrument(norm(roland$SD0@left))
hihat <- sample.instrument(norm(roland$HHO@left))
rim <- sample.instrument(norm(roland$RIM@left))

phrase <- function(key = 30, speed = 0, pickup = NULL, drums = TRUE,
                   rhythm = c(1, 4)) {
  base.duration <- 2 ^ (4 - floor(speed))

  pounding <- sequence(durations = base.duration,
                       instrument = if (drums) snare else rim,
                       tempo = TEMPO, beats = 8)

  if (drums) {
    rhythm <- c(1, 2, 2.5, 3, 4, 4.5, 5, 5.5, 6, 6.5, 7, 8)
    freq <- P.n(49 + c((12 + scales$major[c(1, 2, 3, 4, 3, 2, 1, 2, 1)]), scales$major[c(6, 5, 4)]))
  } else {
    f <- rep(key, length(rhythm))
    if (!is.null(pickup) && length(pickup) > 0)
      f[floor(rhythm) %% 4 == 0] <- key + pickup
    freq <- P.n(f)
  }
  melody <- sequence(frequencies = freq,
                     starts = rhythm,
                     durations = 0.5,
                     instrument = drumlike,
                     tempo = TEMPO,
                     beats = 8)

  1 * pounding + melody
}


gallup <- read.csv('examples/gallup_afghanpoll.csv', stringsAsFactors = FALSE)[1:3]
gallup$date <- as.Date(gallup$Date) 
gallup <- subset(gallup, !is.na(date))
gallup$mistake <- gallup[,2] / colSums(gallup[2:3])
gallup$longitude <- as.numeric(gallup$date - min(gallup$date)) / as.numeric(max(gallup$date) - min(gallup$date))
gallup$mistake <- gallup$mistake * 10 + 29
gallup$longitude <- gallup$longitude * 13 + 61

frame <- function(df, j) {
  LONGITUDE <- c(61, 74)
  LATITUDE <- c(29, 39)

  MAPPINGS <- 'Speed ~ Deaths
Pickup note pitch ~ Region
Rhythm ~ Day of week

Drums play for IEDs that killed people.
Incidents are played in the order they occurred.'

  par(bg = 'black', fg = 'white', col = 'white', col.axis = 'white',
      col.main = 'white', col.sub = 'white', col.lab = 'white',
      font = 2, family = 'sans')
  df$density <- 5
  df[nrow(df),'density'] <- 20
  last.row <- df[nrow(df),]
  plot(mistake ~ longitude, xlim = LONGITUDE, ylim = LATITUDE,
       data = subset(gallup, date <= last.row$date),
       type = 'l', axes = FALSE, asp = 1, col = 'white',
       main = strftime(last.row$date, '%B %d, %Y'),
       sub = 'Each dot is an IED ambush.',
       xlab = '', ylab = '"Gallup poll:\nWas it a mistake to send military forces to Afghanistan?"')
  axis(2, at = LATITUDE, labels = c('No', 'Yes'))

  df$cex = 1 + 2 * log(pmax(1, df$kia + df$wia), 2)
  points(x = df$Longitude, y = df$Latitude,
	 cex = df$cex,
	 col = 0,
         bg = COLORS[df$weekday],
	 pch = 21
  )
  if (j == 1) {
    points(x = last.row$Longitude, y = last.row$Latitude,
  	 cex = last.row$cex,
         bg = 'white',
  	 pch = 21
    )
  }
}

COLORS <- c(Weekday = '#FF000030', Friday = '#00FF0030', Saturday = '#0000FF30')

RHYTHMS <- list(
  Weekday = c(1, 2, 3, 4.5, 5, 6, 7, 8, 8.5),
  Friday = c(1, 2, 3, 3 + 2/3, 4 + 1/3, 5, 6, 6.5, 7, 7.5, 8, 8.5),
  Saturday = c(1, 3, 5, 7)
)

p <- function(row)
  phrase(key = 31, speed = 5,
         pickup = scales$major[as.numeric(row$Region)],
         drums = row$kia > row$wia,
         rhythm = RHYTHMS[[row$weekday]])

# Subset
ied <- read.csv('examples/IED_Data.csv', stringsAsFactors = FALSE)
ied$wia <- rowSums(ied[c("FriendlyWIA", "HostNationWIA", "EnemyWIA", "CivilianWIA")])
ied$kia <- rowSums(ied[c("FriendlyKIA", "HostNationKIA", "EnemyKIA", "CivilianKIA")])
ied$date <- strptime(ied$DateOccurred, '%m/%d/%y 0:00')
ied$weekday <- factor(weekdays(ied$date), levels = c('Weekday', 'Friday', 'Saturday'))
ied$weekday[is.na(ied$weekday)] <- 'Weekday'
ied$kia[is.na(ied$kia)] <- 0
ied$wia[is.na(ied$wia)] <- 0
ied$Region <- factor(ied$Region)
ied <- subset(ied, Category == 'IED Ambush')
ied <- head(ied, 20)

# Music
song <- do.call(c,lapply(1:nrow(ied), function(i) p(ied[i,])))
write.wave(wave(song), '/tmp/krounq.wav', do.normalize = TRUE)

# Video
png('/tmp/krounq-%03d.png', width = 800, height = 450)
for (i in 1:nrow(ied)) {
  for (j in 1:2) {
    frame(ied[1:i,], j)
  }
}
dev.off()

system(paste(
  'avconv',
  '-r 1 -i /tmp/krounq-%03d.png -i /tmp/krounq.wav',
  '-y -pix_fmt yuv420p -r 1',
  '-strict -2',
  '/tmp/krounq.webm'))



# play(song)
