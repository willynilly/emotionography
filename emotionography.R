# clear all environment variables
rm(list=ls())

# install and load all required packages
list.of.packages <- c("data.table", "igraph", "rgexf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# library(rgexf)
# library(igraph) # Load the igraph package
# library(data.table) # Load the data.table package

# load behavior data
setwd("~/Work/emotionography/example-data")
behaviorsDF = read.csv("opinions.csv", header=T, as.is=T)
behaviorsDT = data.table(behaviorsDF);

# aggregate behavior edges

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


# plot behavior data
# each node is a person
# each edge represents aggregate behavior from one person to another

edges = aggBehaviorDF

# set the edge weights to equal the z-scores of 
# the total emotional valence from one person to another person
edges$weight = aggBehaviorDF$zTotalEmotionalValence

nodes = unique(c(aggBehaviorDF$fromPersonId, aggBehaviorDF$toPersonId)) 
net = graph.data.frame(d=edges, vertices=nodes, directed=T)
V(net)$label = V(net)$name

attachVertexStats = function(net) {
  # compute a pagerank of the edge weights
  V(net)$pagerank = page.rank(net)$vector
  V(net)$inDegree = degree(net, mode="in")
  V(net)$outDegree = degree(net, mode="out")
  V(net)$scaledpagerank = scale(V(net)$pagerank, center = TRUE, scale = TRUE)
  
  positiveBehaviorsReceivedCount = rep(0, vcount(net))
  negativeBehaviorsReceivedCount = rep(0, vcount(net))
  positiveBehaviorsReceivedSum = rep(0, vcount(net))
  negativeBehaviorsReceivedSum = rep(0, vcount(net))
  averageEmotionalValenceReceived = rep(0, vcount(net))
  medianEmotionalValenceReceived = rep(0, vcount(net))
  sdEmotionalValenceReceived = rep(0, vcount(net))
  
  positiveBehaviorsExpressedCount = rep(0, vcount(net))
  negativeBehaviorsExpressedCount = rep(0, vcount(net))
  positiveBehaviorsExpressedSum = rep(0, vcount(net))
  negativeBehaviorsExpressedSum = rep(0, vcount(net))
  averageEmotionalValenceExpressed = rep(0, vcount(net))
  medianEmotionalValenceExpressed = rep(0, vcount(net))
  sdEmotionalValenceExpressed = rep(0, vcount(net))
  
  for(v in V(net)) {
    inEdges = E(net)[incident(net, v, mode = "in")]
    outEdges = E(net)[incident(net, v, mode = "out")]
    inWeights = inEdges$weight
    outWeights = outEdges$weight
    
    positiveBehaviorsReceivedWeights = inWeights[inWeights > 0]
    negativeBehaviorsReceivedWeights = inWeights[inWeights < 0]
    positiveBehaviorsExpressedWeights = outWeights[inWeights > 0]
    negativeBehaviorsExpressedWeights = outWeights[inWeights < 0]
      
    positiveBehaviorsReceivedCount[v] = length(positiveBehaviorsReceivedWeights)  
    negativeBehaviorsReceivedCount[v] = length(negativeBehaviorsReceivedWeights)
    positiveBehaviorsReceivedSum[v] = sum(positiveBehaviorsReceivedWeights)
    negativeBehaviorsReceivedSum[v] = sum(negativeBehaviorsReceivedWeights)
    averageEmotionalValenceReceived[v] = mean(inWeights)
    medianEmotionalValenceReceived[v] = median(inWeights)
    sdEmotionalValenceReceived[v] = sd(inWeights)
    
    positiveBehaviorsExpressedCount[v] = length(positiveBehaviorsExpressedWeights)
    negativeBehaviorsExpressedCount[v] = length(negativeBehaviorsExpressedWeights)
    positiveBehaviorsExpressedSum[v] = sum(positiveBehaviorsExpressedWeights)
    negativeBehaviorsExpressedSum[v] = sum(negativeBehaviorsExpressedWeights)
    averageEmotionalValenceExpressed[v] = mean(outWeights)
    medianEmotionalValenceExpressed[v] = median(outWeights)
    sdEmotionalValenceExpressed[v] = sd(outWeights)
  }
  
  V(net)$positiveBehaviorsReceivedCount = positiveBehaviorsReceivedCount
  V(net)$negativeBehaviorsReceivedCount = negativeBehaviorsReceivedCount
  V(net)$positiveBehaviorsReceivedSum = positiveBehaviorsReceivedSum
  V(net)$negativeBehaviorsReceivedSum = negativeBehaviorsReceivedSum
  V(net)$averageEmotionalValenceReceived = averageEmotionalValenceReceived
  V(net)$medianEmotionalValenceReceived = medianEmotionalValenceReceived
  V(net)$sdEmotionalValenceReceived = sdEmotionalValenceReceived
  
  V(net)$positiveBehaviorsExpressedCount = positiveBehaviorsExpressedCount
  V(net)$negativeBehaviorsExpressedCount = negativeBehaviorsExpressedCount
  V(net)$positiveBehaviorsExpressedSum = positiveBehaviorsExpressedSum
  V(net)$negativeBehaviorsExpressedSum = negativeBehaviorsExpressedSum
  V(net)$averageEmotionalValenceExpressed = averageEmotionalValenceExpressed
  V(net)$medianEmotionalValenceExpressed = medianEmotionalValenceExpressed
  V(net)$sdEmotionalValenceExpressed = sdEmotionalValenceExpressed
  
  return(net)
}

net = attachVertexStats(net)
V(net)$size = abs(V(net)$scaledpagerank * 20)

filterEdgesByWeight = function(net, minWeight, maxWeight) {
  return(delete.edges(net, E(net)[weight > minWeight & weight < maxWeight]))
}

getTwoColorsByThreshold = function(values, threshold, colors=c("red", "green")) {
  colors[(values > threshold) + 1]
}

getThreeColorsByThreshold = function(values, lowerThreshold, upperThreshold, colors=c("white", "red", "green")) {
  colorsIndex = rep(1, length(values))
  colorsIndex = colorsIndex + (values < lowerThreshold)
  colorsIndex = colorsIndex + (2*(values > upperThreshold))
  return(colors[colorsIndex])
}

getEdgeColors = function(net, threshold) {
  getTwoColorsByThreshold(E(net)$weight, threshold)
}

# color the nodes 
# red if their scaledpagerank are below a lower threshold of standard deviations
# green if they are above an upper threshold
# otherwise color them white
getVertexColors = function(net, lowerThreshold, upperThreshold) {
  return(getThreeColorsByThreshold(V(net)$scaledpagerank, lowerThreshold, upperThreshold))
}

lowerEdgeWeight = -2.1
upperEdgeWeight = 2.1
lowerVertexWeight = -2
upperVertexWeight = 2
net.sp = filterEdgesByWeight(net, lowerEdgeWeight, upperEdgeWeight)
E(net.sp)$edge.color = getEdgeColors(net.sp, 0);
V(net.sp)$color = getVertexColors(net.sp, lowerVertexWeight, upperVertexWeight)
E(net.sp)$edge.width = abs(E(net.sp)$weight) 

par(mar=c(0,0,0,0))

createCircleLayoutWithoutVertexOverlap = function(g) {
  circlelayout = layout.circle(g)
  curWeight = 0;
  weights = V(g)$size
  totWeight = sum(weights)
  upperRads=0;
  lowerRads=0;
  for(i in 1:nrow(circlelayout)) {
    curWeight = curWeight + weights[i]
    upperRads = 2*pi * (curWeight / totWeight)
    rads = mean(c(lowerRads, upperRads))
    circlelayout[i,] = c(cos(rads), sin(rads))
    lowerRads = upperRads;
  }
  return(circlelayout)
}

circleLayout = createCircleLayoutWithoutVertexOverlap(net.sp)

plot(net.sp,
          edge.arrow.size=.2, 
          edge.curved = .5,
          edge.color = E(net.sp)$edge.color,
          edge.width = E(net.sp)$edge.width,
          vertex.size = V(net.sp)$size,
          vertex.color= adjustcolor(V(net.sp)$color, alpha.f = .5), 
          vertex.frame.color="#555555",
          vertex.label=V(net.sp)$id, 
          vertex.label.color="black",
          vertex.label.cex=.6, 
          layout=circleLayout) 

# save vertices of graph
saveVertices = function(net, fileName) {
  vertexList = lapply(list.vertex.attributes(net),function(x) get.vertex.attribute(net,x))
  vertexDataFrame = cbind.data.frame(vertexList)
  colnames(vertexDataFrame) = list.vertex.attributes(net)
  write.csv(vertexDataFrame, file=fileName)
}

saveVertices(net, "people.csv")

# export behavior dataas gexf so it can be loaded into gephi

# Converts the given igraph object to GEXF format and saves it at the given filepath location
#     g: input igraph object to be converted to gexf format
#     filepath: file location where the output gexf file should be saved
#
# saveAsGEXF = function(g, filepath="converted_graph.gexf")
# {
#   require(igraph)
#   require(rgexf)
#   
#   # gexf nodes require two column data frame (id, label)
#   # check if the input vertices has label already present
#   # if not, just have the ids themselves as the label
#   if(is.null(V(g)$label))
#     V(g)$label <- as.character(V(g))
#   
#   # similarily if edges does not have weight, add default 1 weight
#   if(is.null(E(g)$weight))
#     E(g)$weight <- rep.int(1, ecount(g))
#   
#   nodes <- data.frame(cbind(V(g), V(g)$label))
#   edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
#   
#   # combine all node attributes into a matrix (and take care of & for xml)
#   vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
#   nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
#   
#   # combine all edge attributes into a matrix (and take care of & for xml)
#   eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
#   edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
#   
#   # combine all graph attributes into a meta-data
#   graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
#   
#   # generate the gexf object
#   output <- write.gexf(nodes, edges, 
#                        edgesWeight=E(g)$weight,
#                        edgesAtt = edgesAtt,
#                        nodesAtt = nodesAtt,
#                        defaultedgetype = "directed",
#                        meta=c(list(creator="Will Riley", description="igraph -> gexf converted file", keywords="igraph, gexf, R, rgexf"), graphAtt))
#   
#   print(output, filepath, replace=T)
# }
#saveAsGEXF(net.sp, "output.gexf")
