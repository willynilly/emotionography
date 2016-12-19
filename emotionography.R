# clear all environment variables
rm(list=ls())

library("rgexf")
library(igraph) # Load the igraph package
library(data.table) # Load the data.table package

# load edges
setwd("~/Work/emotionography/example-data")
behaviorsDF = read.csv("opinions.csv", header=T, as.is=T)

# behaviorsDF = data.frame(fromPersonId=c("p1", "p1", "p2"), toPersonId =c("p3", "p3", "p3"), emotionalValence = c(1, 1, 1), round=c(1, 2, 1))

behaviorsDT = data.table(behaviorsDF);


#opinions = read.csv("opinions.csv", header=T, as.is=T)

# compress behavior edges

computeTotalEmotionalValence = function(round, minRound, maxRound, emotionalValence) {
  tot = sum(emotionalValence)
  return(tot);
}

computeMeanEmotionalValence = function(round, minRound, maxRound, emotionalValence) {
  tot = mean(emotionalValence)
  return(tot);
}

computeMedianEmotionalValence = function(round, minRound, maxRound, emotionalValence) {
  tot = as.double(median(emotionalValence))
  return(tot);
}

computeTimeAdjustedEmotionalValence = function(round, minRound, maxRound, emotionalValence) {
  relativeTime = (round - minRound + 1) / (maxRound - minRound + 1)
  tot = sum(relativeTime * emotionalValence)
  return(tot);
}

computeBehaviorCount = function(round, minRound, maxRound, emotionalValence) {
  tot = sum(round)
  return(tot);
}

minRound = behaviorsDT[, min(round)]
maxRound = behaviorsDT[, max(round)]
behaviorsDT[, 
            behaviorCount := computeBehaviorCount(round, minRound, maxRound, emotionalValence),
            by=list(fromPersonId, toPersonId)]
behaviorsDT[, 
            totalEmotionalValence := computeTotalEmotionalValence(round, minRound, maxRound, emotionalValence), 
            by=list(fromPersonId, toPersonId)]
behaviorsDT[, 
            meanEmotionalValence := computeMeanEmotionalValence(round, minRound, maxRound, emotionalValence), 
            by=list(fromPersonId, toPersonId)]
behaviorsDT[, 
            medianEmotionalValence := computeMedianEmotionalValence(round, minRound, maxRound, emotionalValence),
            by=list(fromPersonId, toPersonId)]
behaviorsDT[, 
            timeAdjustedEmotionalValence := computeTimeAdjustedEmotionalValence(round, minRound, maxRound, emotionalValence), 
            by=list(fromPersonId, toPersonId)]
setkeyv(behaviorsDT, c('fromPersonId', 'toPersonId'))
aggBehaviorDT = subset(unique(behaviorsDT), select=c(fromPersonId, toPersonId, totalEmotionalValence, meanEmotionalValence, medianEmotionalValence, timeAdjustedEmotionalValence))
aggBehaviorDF = as.data.frame.matrix(aggBehaviorDT) 

aggBehaviorDF$zTotalEmotionalValence = scale(aggBehaviorDF$totalEmotionalValence, center = TRUE, scale = TRUE)
aggBehaviorDF$zTimeAdjustedEmotionalValence = scale(aggBehaviorDF$timeAdjustedEmotionalValence, center = TRUE, scale = TRUE)

#head(aggBehaviorDF)

edges = aggBehaviorDF
edges$weight = aggBehaviorDF$zTotalEmotionalValence

nodes = unique(c(aggBehaviorDF$fromPersonId, aggBehaviorDF$toPersonId)) 
net = graph.data.frame(d=edges, vertices=nodes, directed=T)

nodes$pagerank = page.rank(net)$vector

inDegree = degree(net, mode="in")
outDegree = degree(net, mode="out")
V(net)$scaledpagerank = scale(nodes$pagerank, center = TRUE, scale = TRUE)

V(net)$size = abs(V(net)$scaledpagerank * 20)

minWeightToView = 2.2;
net.sp = delete.edges(net, E(net)[abs(weight) < minWeightToView])
E(net.sp)$edge.color = c("red", "green")[(E(net.sp)$weight > 0) + 1]

getVertexColors = function(net, threshold) {
  colors = c("white", "red", "green");
  colorsIndex = rep(1, vcount(net))
  colorsIndex = colorsIndex + (V(net)$scaledpagerank < -1 * threshold)
  colorsIndex = colorsIndex + 2*(V(net)$scaledpagerank > threshold)
  return(colors[colorsIndex])
}

V(net.sp)$color = getVertexColors(net.sp, 1)

E(net.sp)$edge.width = abs(E(net.sp)$weight) 

par(mar=c(0,0,0,0))
circlelayout = layout.circle(net.sp)
curWeight = 0;
totWeight = sum(V(net)$size)
upperRads=0;
lowerRads=0;
for(i in 1:nrow(circlelayout)) {
  curWeight = curWeight + V(net)$size[i]
  upperRads = 2*pi * (curWeight / totWeight)
  rads = mean(c(lowerRads, upperRads))
  circlelayout[i,] = c(cos(rads), sin(rads))
  lowerRads = upperRads
}
plot(net.sp,
          edge.arrow.size=.2, 
          edge.curved = .5,
          edge.color = E(net.sp)$edge.color,
          edge.width = E(net.sp)$edge.width,
          vertex.size = V(net.sp)$size * 1.2,
          vertex.color= adjustcolor(V(net.sp)$color, alpha.f = .5), 
          vertex.frame.color="#555555",
          vertex.label=V(net.sp)$id, 
          vertex.label.color="black",
          vertex.label.cex=.7, 
          layout=circlelayout) 


# Converts the given igraph object to GEXF format and saves it at the given filepath location
#     g: input igraph object to be converted to gexf format
#     filepath: file location where the output gexf file should be saved
#
saveAsGEXF = function(g, filepath="converted_graph.gexf")
{
  require(igraph)
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(V(g)$label))
    V(g)$label <- as.character(V(g))
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
  
  # combine all graph attributes into a meta-data
  graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
  
  # generate the gexf object
  output <- write.gexf(nodes, edges, 
                       edgesWeight=E(g)$weight,
                       edgesAtt = edgesAtt,
                       nodesAtt = nodesAtt,
                       defaultedgetype = "directed",
                       meta=c(list(creator="Will Riley", description="igraph -> gexf converted file", keywords="igraph, gexf, R, rgexf"), graphAtt))
  
  print(output, filepath, replace=T)
}

#saveAsGEXF(net.sp, "output.gexf")
