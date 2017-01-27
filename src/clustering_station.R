##
## Perform clustering on stations pick/dock 24-hr activities
## Goal is to find groups/clusters of stations with different usage pattern
##
set.seed(1234)
## Long-format to wide-format of citybike
citibike_long2wide <- function(data, activity_type = c('pick', 'dock')){
  if (activity_type == 'pick') {
    station_24hr_long <- dplyr::select(data, start.station.name, startHr)
  } else if (activity_type == 'dock') {
    station_24hr_long <- dplyr::select(data, end.station.name, stopHr)
  } else {
    stop('Error: activity type is either pick or dock.')
  }
  colnames(station_24hr_long) <- c('NAME', 'HOUR')
  station_24hr_long <- group_by(station_24hr_long, NAME, HOUR) %>%
    summarise(TRIPS=n())
  station_24hr_wide <- dcast(station_24hr_long, NAME~HOUR,
                             value.var = 'TRIPS', sum, fill=0)
  rownames(station_24hr_wide) <- station_24hr_wide$NAME
  station_24hr_wide <- station_24hr_wide[, -1]
  # print(head(station_24hr_wide))
  return(station_24hr_wide)
}
pick_station_24hr <- citibike_long2wide(data=data, activity_type='pick')
dock_station_24hr <- citibike_long2wide(data=data, activity_type='dock')

## Hclust on 24hour activities of citybike picking and docking
dist_metric <- 'euclidean'
hclust_linkage <- 'complete'
K = 10
get_cluster_labels <- function(data, dist_metric, hclust_linkage, k){
  hclust_station_24hr <- dist(data, method = dist_metric) %>%
    hclust(method = hclust_linkage)

  hclust_station_24hr_lab <- cutree(hclust_station_24hr, k = K)
  ph_station_anno <- data.frame(CLUSTER=as.factor(hclust_station_24hr_lab))
  rownames(ph_station_anno) <- names(hclust_station_24hr_lab)
  return(ph_station_anno)
}
pick_station_24hr_anno <- get_cluster_labels(data=pick_station_24hr,
                                             dist_metric=dist_metric,
                                             hclust_linkage=hclust_linkage,
                                             k=K)
dock_station_24hr_anno <- get_cluster_labels(data=dock_station_24hr,
                                             dist_metric=dist_metric,
                                             hclust_linkage=hclust_linkage,
                                             k=K)

## Viz on 24hour activities of citybike picking and docking
pdf(file.path(FIGDIR, 'pick_station_24hr_kmeans.pdf'), 10, 10)
p_pick_kmeans <- pheatmap(pick_station_24hr,
                          kmeans_k = K,
                          cluster_cols = F,
                          # show_rownames = F,
                          main='24-hr Picking Activities')
dev.off()

pdf(file.path(FIGDIR, 'dock_station_24hr_kmeans.pdf'), 10, 10)
p_dock_kmeans <- pheatmap(dock_station_24hr,
                          kmeans_k = K,
                          cluster_cols = F,
                          # show_rownames = F,
                          main='24-hr Docking Activities')
dev.off()

pdf(file.path(FIGDIR, 'pick_station_24hr_hclust.pdf'), 10, 10)
p_pick_hclust <- pheatmap(pick_station_24hr,
                          clustering_distance_rows = dist_metric,
                          clustering_method = hclust_linkage,
                          cutree_rows = K,
                          annotation_row = pick_station_24hr_anno,
                          cluster_cols = F,
                          main='24-hr Picking Activities'
)
dev.off()

pdf(file.path(FIGDIR, 'dock_station_24hr_hclust.pdf'), 10, 10)
p_dock_hclust <- pheatmap(dock_station_24hr,
                          clustering_distance_rows = dist_metric,
                          clustering_method = hclust_linkage,
                          cutree_rows = K,
                          annotation_row = dock_station_24hr_anno,
                          cluster_cols = F,
                          main='24-hr Docking Activities'
)
dev.off()

##---------
## Seems it has two confounder:
## 1) baseline activities
## 2) hidden distribution
##---------

## Instead of clustering, try ordering stations by its absolute
## pick/docking activities
orderbyRowSum <- function(data){
  o <- mutate(data, SUM=rowSums(data), row_names=rownames(data)) %>%
    dplyr::arrange(desc(SUM), row_names) %>%
    dplyr::select(-SUM)
  rownames(o) <- o$row_names
  o <- dplyr::select(o, -row_names)
  return(o)
}
top_N <- 20
pick_station_24hr_desc <- orderbyRowSum(pick_station_24hr)
dock_station_24hr_desc <- orderbyRowSum(dock_station_24hr)
top_pick_station_names <- rownames(pick_station_24hr_desc)[seq_len(top_N)]
top_dock_station_names <- rownames(dock_station_24hr_desc)[seq_len(top_N)]
desc_pick_station_names <- rownames(pick_station_24hr_desc)
desc_dock_station_names <- rownames(dock_station_24hr_desc)
pdf(file.path(FIGDIR, 'top_activity_station_24hr.pdf'), 10, 10)
pheatmap(pick_station_24hr_desc[top_pick_station_names, ],
         cluster_rows = F, cluster_cols = F,
         main='Picking activities of Stations with Top 24-hr Picking Activities')
pheatmap(dock_station_24hr_desc[top_pick_station_names, ],
         cluster_rows = F, cluster_cols = F,
         main='Docking activities of Stations with Top 24-hr Picking Activities')
pheatmap(pick_station_24hr_desc[top_dock_station_names, ],
         cluster_rows = F, cluster_cols = F,
         main='Picking activities of Stations with Top 24-hr Docking Activities')
pheatmap(dock_station_24hr_desc[top_dock_station_names, ],
         cluster_rows = F, cluster_cols = F,
         main='Docking activities of Stations with Top 24-hr Docking Activities')
dev.off()
## Thus 8Ave&W31St seems to be one good shot for commute

## PD index = (Pick+1)/(Dock+1)
row_norm_byMax <- function(x){
  t(apply(x, 1, function(r) {
    r/max(r)
  }))
}
pd_station_24hr <- (row_norm_byMax(pick_station_24hr)+1) / (row_norm_byMax(dock_station_24hr)+1)
pd_station_24hr <- as.data.frame(pd_station_24hr)

pdf(file.path(FIGDIR, 'pickVSdock_station_24hr.pdf'), 10, 10)
pheatmap(pd_station_24hr[top_pick_station_names, ],
         cluster_rows = F,
         cluster_cols = F,
         main='P/D Index of Stations with Top 24-hr Picking Activities')
pd_station_24hr_anno <- get_cluster_labels(data=pd_station_24hr,
                                             dist_metric=dist_metric,
                                             hclust_linkage=hclust_linkage,
                                             k=K)
p_pd_kmeans <- pheatmap(pd_station_24hr,
                        kmeans_k = K,
                        cluster_cols = F,
                        main='P/D Index of Stations')
tempt <- rownames(pick_station_24hr)
tempt[!(tempt %in% top_pick_station_names[1:K])] <- ' '
p_pd_hclust <- pheatmap(pd_station_24hr,
                        clustering_distance_rows = dist_metric,
                        clustering_method = hclust_linkage,
                        cutree_rows = K,
                        annotation_row = pd_station_24hr_anno,
                        cluster_cols = F,
                        labels_row = tempt,
                        main='P/D Index of Stations')
dev.off()

# P/D index has mean as 1.0313 which suggests balance
# Fluatation (not exactly equals to 1.00) might be due to:
# 1) company is manually balancing stations with back-up bikes
# 2) customers are not always returning bike successfully
print('P/D Index has average:')
print(sum(pd_station_24hr)/prod(dim(pd_station_24hr)))


##
## There are at least 3 types of stations by P/D index in 24hr:
## 1. High in morning & Low in evening: source of working people
## 2. Low in morning & High in evening: destination of working people
## 3. Normal along day-light: no special usage
##
rush_hr_morning_lab <- c('06', '07', '08')
rush_hr_evening_lab <- c('16', '17', '18')
# Manually extract clusters of interest
# double-check before continue
pheatmap(p_pd_kmeans$kmeans$centers, cluster_cols = F)
pd_kmeans_center <- p_pd_kmeans$kmeans$centers
pd_kmeans_cluster <- p_pd_kmeans$kmeans$cluster
## Type-1
stations_type1_names <- names(pd_kmeans_cluster[pd_kmeans_cluster %in% c(5, 9)])
p_station_type1 <- dplyr::filter(stations, STATION_NAME %in% stations_type1_names) %>%
  map_stations_loc()
## Type-2
stations_type2_names <- names(pd_kmeans_cluster[pd_kmeans_cluster %in% c(4, 8, 10)])
p_station_type2 <- dplyr::filter(stations, STATION_NAME %in% stations_type2_names) %>%
  map_stations_loc()
## Type-3
stations_type3_names <- names(pd_kmeans_cluster[pd_kmeans_cluster %in% c(2, 6)])
p_station_type3 <- dplyr::filter(stations, STATION_NAME %in% stations_type3_names) %>%
  map_stations_loc()
## Type-4 unknown: the rest stations

stations_types_summary <- rep('X', NROW(stations))
stations_types_summary[stations$STATION_NAME %in% stations_type1_names] <- LETTERS[1]
stations_types_summary[stations$STATION_NAME %in% stations_type2_names] <- LETTERS[2]
stations_types_summary[stations$STATION_NAME %in% stations_type3_names] <- LETTERS[3]
stations_byTypes <- dplyr::mutate(stations, TYPE=stations_types_summary)
p_stations_byTypes <- qmplot(x=STATION_LON, y=STATION_LAT,
                             data = stations_byTypes,
                             maptype = 'toner-lite',
                             extent = 'device',
                             zoom = 14,
                             color=TYPE,
                             # shape=TYPE,
                             size=I(2.5))
ggsave(file.path(FIGDIR, 'PD_index_stations_all_types.pdf'),
       p_stations_byTypes,
       width = 10, height = 10)
p_stations_type1n2 <- dplyr::filter(stations_byTypes, TYPE %in% LETTERS[1:2]) %>%
  qmplot(data = ., x=STATION_LON, y=STATION_LAT,
         maptype = 'toner-lite',
         extent = 'device',
         zoom = 14,
         color=TYPE,
         # shape=TYPE,
         size=I(2.5))
ggsave(file.path(FIGDIR, 'PD_index_stations_type_AandB.pdf'),
       p_stations_type1n2,
       width = 10, height = 10)

