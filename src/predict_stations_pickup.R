#
# Predict pick-up activity of specific stations in next day
#

# With type-A stations for example
s <- stations_type1_names[1]
sid <- dplyr::filter(stations, STATION_NAME == s) %>%
  select(STATION_ID) %>% as.numeric()
print(s)
# Month of interest
m <- 'Jan'

# station-date-hour-trips data.frame
# Note: in some hour point, there is no activity at all thus need add them
pick_s_days_avail <- dplyr::filter(data,
                                   start.station.name %in% s,
                                   month(startTimestamp, label=T) == m) %>%
  mutate(DATE=date(startTimestamp)) %>%
  group_by(start.station.name, DATE, startHr) %>%
  summarise(TRIPS=n()) %>% as.data.frame()
colnames(pick_s_days_avail) <- c('STATION_NAME', 'DATE', 'HOUR', 'TRIPS')

pick_s_days_grid <- expand.grid(STATION_NAME=s,
                                DATE=unique(pick_s_days_avail$DATE),
                                HOUR=sprintf("%02d", 0:23),
                                TRIPS=0)
pick_s_days <- bind_rows(pick_s_days_avail, pick_s_days_grid) %>%
  group_by(STATION_NAME, DATE, HOUR) %>%
  summarise(NTRIPS=sum(TRIPS)) %>%
  mutate(TIMESTAMP=ymd_h(paste(DATE, HOUR), tz='EST5EDT'),
         YEAR=year(DATE))

print(head(pick_s_days))

p_pick_s <- ggplot(pick_s_days, aes(x=DATE, y=HOUR, fill=NTRIPS)) +
  geom_tile(color = "white", size = 0.4) +
  scale_fill_gradient(low="ghostwhite", high="red") +
  scale_x_date(date_breaks="1 week",  date_labels="%d-%b-%y") +
  facet_wrap("YEAR", ncol = 1) +
  xlab('') + ylab('Hour') + labs(fill='Trips') +
  ggtitle(paste0('Picking activity at ', s))
ggsave(file.path(FIGDIR, paste0('pick_sid', sid, '_', m, '.pdf')),
       p_pick_s, width=15, height = 15)

obs_trips <- as.numeric(pick_s_days$NTRIPS)
ar4 <- ar(obs_trips, F, 4)
fit_trips <- fitted(ar4)
fit_trips[seq_len(4)] <- obs_trips[seq_len(4)]

pick_s_days_ar <- ungroup(pick_s_days) %>% dplyr::select(DATE, HOUR, YEAR) %>%
  mutate(OBS=obs_trips, FIT=fit_trips) %>%
  melt(id.vars=c('DATE', 'HOUR', 'YEAR'), value.name='NTRIPS',
       variable.name='Type')
p_pick_s_ar <- ggplot(pick_s_days_ar, aes(x=DATE, y=HOUR, fill=NTRIPS)) +
  geom_tile(color = "white", size = 0.4) +
  scale_fill_gradient(low="ghostwhite", high="red") +
  scale_x_date(date_breaks="1 week",  date_labels="%d-%b-%y") +
  facet_grid(YEAR~Type) +
  xlab('') + ylab('Hour') + labs(fill='Trips') +
  ggtitle(paste0('Observed and AR(4) fitted picking activity at ', s))
ggsave(file.path(FIGDIR, paste0('pick_sid', sid, '_', m, 'obsNfit.pdf')),
       p_pick_s_ar, width=15, height = 15)

