KDViz <-
function(groupDTM, group, graph = FALSE, export = TRUE) {
  if(missing(group)) {
    stop("Group argument must be specified")
  }
  kdNetwork <- networkD3::igraph_to_networkD3(igraph::graph_from_incidence_matrix(groupDTM[[group]]), group = c(rep(0, dim(groupDTM[[group]])[1]), rep(group, dim(groupDTM[[group]])[2])))
  
  KD3 <- networkD3::forceNetwork(Links = kdNetwork$links, Nodes = kdNetwork$nodes,
                                 Source = "source", Target = "target",
                                 NodeID = "name",
                                 Group = "group",
                                 charge = -75,
                                 linkDistance = 15, fontSize = 22,
                                 colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                                 zoom = TRUE, opacity = 0.95, legend = FALSE,
                                 opacityNoHover = 0.25)
  
  if(export) {
    htmlwidgets::saveWidget(KD3, paste0("KDVizGroup",group,".html"))
  }
  
  if(graph) {
    return(KD3)
  }
}