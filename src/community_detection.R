##
## Community detection based on picking & docking activities
##

# methods summary by igraph
# https://www.r-bloggers.com/summary-of-community-detection-algorithms-in-igraph-0-6/
# hands-on example of community detection of igraph
# http://kateto.net/networks-r-igraph
# hands-on example of visNet package to have interactive network graph
# http://kateto.net/network-visualization

# Minimal Example
pdf(file.path(FIGDIR, 'community_minimum_demo%03d.pdf'), onefile = F, 10, 10)
g <- make_graph("Zachary")
plot(g)
imc <- cluster_infomap(g)
plot(imc, g)
rm(g);rm(imc)
dev.off()


net0_edges_wt <- group_by(data, start.station.name, end.station.name) %>%
  summarise(TRIPS=n()) %>%
  ungroup() %>% as.data.frame()
colnames(net0_edges_wt) <- c('from', 'to', 'weight')

stations <- dplyr::select(stations, STATION_NAME, STATION_ID,
                          STATION_LAT, STATION_LON)
net0 <- graph_from_data_frame(d=net0_edges_wt,
                              directed=T,
                              vertices=stations)
E(net0)$width <- E(net0)$weight / 10
E(net0)$arrow.size <- .2
E(net0)$edge.color <- "gray80"

pdf(file.path(FIGDIR, 'network_stations_raw.pdf'), 10, 10)
plot(net0)
dev.off()

net_simplify <- simplify(net0, remove.multiple = T, remove.loops = F,
                         edge.attr.comb=list(weight="sum","ignore"))
pdf(file.path(FIGDIR, 'network_stations_noMultiple_noLoop.pdf'), 10, 10)
plot(net_simplify)
dev.off()

net_undirected <- as.undirected(net0, mode= "collapse",
                                edge.attr.comb=list(weight="sum", "ignore"))
E(net_undirected)$width <- E(net_undirected)$weight / 10
pdf(file.path(FIGDIR, 'network_stations_forcedUndirected.pdf'), 10, 10)
plot(net_undirected, edge.arrow.size=0.2, edge.color='gray80')
dev.off()

##
## Community detection
## either InfoMap or betweenness
##
community_detect_stations <- function(net, nodes_geo,
                                      method=c('infomap', 'betweenness')){
  if (method == 'infomap'){
    imc <- cluster_infomap(net)
    memb <- membership(imc)
  } else if (method == 'betweenness') {
    ebc <- cluster_edge_betweenness(net)
    memb <- membership(ebc)
  } else {
    stop('Unknown method for community detection')
  }
  stations_community <- data.frame(STATION_NAME=names(memb),
                                   COMMUNITY=factor(memb))
  p_community <- left_join(x=stations_community, y=nodes_geo,
                           by=c('STATION_NAME')) %>%
    qmplot(data = ., x=STATION_LON, y=STATION_LAT,
           maptype = 'toner-lite',
           extent = 'device',
           zoom = 14,
           color=COMMUNITY, shape=COMMUNITY, size=I(2.5))
}
# community detection on entire NYC
# manhatten-brooklyn: make sense but not big value
p_community_infomap_nyc <- community_detect_stations(net=net0,
                                                     nodes_geo=stations,
                                                     method='infomap')
ggsave(file.path(FIGDIR, 'community_infomap_nyc.pdf'),
       p_community_infomap_nyc, width=10, height=10)
# too-slow
# p_community_betweenness_nyc <- community_detect_stations(net=net0,
#                                                          nodes_geo=stations,
#                                                          method='betweenness')
# ggsave(file.path(FIGDIR, 'community_stations_betweenness_nyc.pdf'),
#        p_community_betweenness_nyc, width=10, height=10)

# community detection on type-1 and type-2 together
net_stations_typeAnB <- induced.subgraph(net0, vids=c(stations_type1_names,
                                                    stations_type2_names))
p_community_infomap_typeAnB <- community_detect_stations(net=net_stations_typeAnB,
                                                         nodes_geo=stations,
                                                         method='infomap')
ggsave(file.path(FIGDIR, 'community_infomap_typeAnB.pdf'),
       p_community_infomap_typeAnB, width=10, height=10)

# community detection on type-3 only
net_stations_typeC <- induced.subgraph(net0, vids=c(stations_type3_names))
p_community_infomap_typeC <- community_detect_stations(net=net_stations_typeC,
                                                         nodes_geo=stations,
                                                         method='infomap')
ggsave(file.path(FIGDIR, 'community_infomap_typeC.pdf'),
       p_community_infomap_typeC, width=10, height=10)
