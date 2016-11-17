library(ggmap)
theme_set(theme_bw(base_size = 22))
myGthm <- theme(text = element_text(size = 25))
#                 axis.text.y=element_blank(),
#                 axis.text.x=element_blank())


infile <- file.path(getwd(), 'data/201501-citibike-tripdata.csv')
citibike201501 <- read.csv(infile, stringsAsFactors=FALSE)
data <- citibike201501
data$startTimestamp <- strptime(data$starttime, '%m/%d/%Y %H:%M', tz = 'EST5EDT')
data$stopTimestamp  <- strptime(data$stoptime, '%m/%d/%Y %H:%M', tz = 'EST5EDT')
data$startweekday <- factor(weekdays(data$startTimestamp),
                            levels= c("Sunday", "Monday","Tuesday",
                                      "Wednesday", "Thursday", "Friday",
                                      "Saturday"))
data$stopweekday <- factor(weekdays(data$stopTimestamp),
                            levels= c("Sunday", "Monday","Tuesday",
                                      "Wednesday", "Thursday", "Friday",
                                      "Saturday"))
data$startHr <- format(data$startTimestamp, '%H')

start_lat_min <- min(data$start.station.latitude)
start_lat_max <- max(data$start.station.latitude)
start_lon_min <- min(data$start.station.longitude)
start_lon_max <- max(data$start.station.longitude)

plot_lat_bt  <- start_lat_min - 2
plot_lat_up  <- start_lat_max + 2
plot_lon_lft <- start_lon_min - 2
plot_lon_rit <- start_lat_max + 2

## Static Scatter Plot
if (0) {
png(filename = file.path(getwd(), 'fig/scatter_plot.png'),
    width = 850, height = 1100)
q <- qmplot(start.station.longitude, start.station.latitude,
       data = data, maptype = "toner-lite", color = I("red"))
print(q)
dev.off()
}

## Thematic Plot
q <-
  qmplot(start.station.longitude, start.station.latitude,
         data = data, maptype = "toner-lite",
         geom = "blank", zoom = 14,
         legend = "right") +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Pick-Up Activities",
                       low = "ghostwhite", mid = "yellow", high = "red") +
  myGthm
png(filename = file.path(getwd(), 'fig/thematic_plot.png'),
width = 1000, height = 1600)
print(q)
dev.off()
pdf(file.path(getwd(), 'fig/thematic_plot.pdf'),
    width = 7, height = 10)
print(q)
dev.off()
## Only Usertype (2 types)
q1 <- q + facet_wrap( ~ usertype)
png(filename = file.path(getwd(), 'fig/thematic_plot_usertype.png'),
    width = 2000, height = 1600)
print(q1)
dev.off()
pdf(file.path(getwd(), 'fig/thematic_plot_usertype.pdf'), 10, 10)
print(q1)
dev.off()

## Week ~ usertype
q2 <- q + facet_wrap(startweekday ~ usertype)
png(filename = file.path(getwd(), 'fig/thematic_plot_wkd_usertype.png'),
    width = 2000, height = 1600)
print(q2)
dev.off()
pdf(file.path(getwd(), 'fig/thematic_plot_wkd_usertype.pdf'), 20, 10)
print(q2)
dev.off()

## Only Subscribers matter a lot
subscriber <- data[data$usertype == 'Subscriber', ]

## One-day at NYC (weekdays and weekends)
weekdays_idx <- subscriber$startweekday %in% c("Monday","Tuesday",
                                               "Wednesday", "Thursday", "Friday")
weekends_idx <- !weekdays_idx
subscriberWday <- subscriber[weekdays_idx, ]
subscriberWend <- subscriber[weekends_idx, ]
qSubWday <-
  qmplot(start.station.longitude, start.station.latitude,
         data = subscriberWday, maptype = "toner-lite",
         geom = "blank", zoom = 14,
         legend = "right") +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Pick-Up Activities",
                       low = "ghostwhite", mid = "yellow", high = "red") +
  myGthm

qSubWday2 <- qSubWday + facet_wrap(~startHr)
png(filename = file.path(getwd(), 'fig/thematic_plot_wkd_NYC.png'),
    width = 2000, height = 2000)
print(qSubWday2)
dev.off()
pdf(file.path(getwd(), 'fig/thematic_plot_wkd_NYC.pdf'), 20, 20)
print(qSubWday2)
dev.off()

qSubWend <-
  qmplot(start.station.longitude, start.station.latitude,
         data = subscriberWend, maptype = "toner-lite",
         geom = "blank", zoom = 14,
         legend = "right") +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Pick-Up Activities",
                       low = "ghostwhite", mid = "yellow", high = "red") +
  myGthm

qSubWend2 <- qSubWend + facet_wrap(~startHr)
png(filename = file.path(getwd(), 'fig/thematic_plot_wend_NYC.png'),
    width = 2000, height = 2000)
print(qSubWend2)
dev.off()
pdf(file.path(getwd(), 'fig/thematic_plot_wend_NYC.pdf'), 20, 20)
print(qSubWend2)
dev.off()





## Beautiful NYC
# rawdata <- data.frame(as.numeric(citibike201501$start.station.longitude),
#                        as.numeric(citibike201501$start.station.latitude))
# names(rawdata) <- c('lon', 'lat')
# data <- as.matrix(rawdata)
# theta <- -pi / 15
# m <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
# data <- as.matrix(data) %*% m
# par(bg='black')
# plot(data, cex=0.1, col="white", pch=16)
