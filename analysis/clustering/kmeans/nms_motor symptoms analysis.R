#### TODO: Necesita medias normalizadas si se compara con la edad, pd_duration, etc
#### TODO: Mostrar en el heatmap la media, en vez de los centros


#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}

if (!require("cluster")){
  install.packages("cluster", dependencies = TRUE)
  library(cluster)
}

#No-use i think
if (!require("tidyverse")){
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}

if (!require("reshape2")){
  install.packages("reshape2", dependencies = TRUE)
  library(reshape2)
}

#########################################################
### B) Reading in data and preparing it for clustering
#########################################################

d897 <- read.csv("analysis/clustering/kmeans/data/897_motor_nms.csv", comment.char="#")

# Filtering the data frame
d897_nms30 = d897[38:67]

# Filtering the data frame
d897_motor25 = d897[8:32]

#########################################################
### C) Using the clustering algorithm
#########################################################

# first we create two lists of NULL values with size = 11 that represent 11 possible cluster models
# with raising k values
kmeans_nms_models = vector("list", 11)
kmeans_motor_models = vector("list", 11)

# Execute the kmeans algorithm 11 times (k = 2,3,4,...,12) with the NMS features
for(i in 1:length(kmeans_nms_models)){
  kmeans_nms_models[[i]] = kmeans(x = d897_nms30, centers = i+1, iter.max = 30, nstart = 20)
}

# Execute the kmeans algorithm 11 times (k = 2,3,4,...,12) with the SCOPA-MOTOR features
for(i in 1:length(kmeans_motor_models)){
  kmeans_motor_models[[i]] = kmeans(x = d897_motor25, centers = i+1, iter.max = 30, nstart = 20)
}

#########################################################
### D) Preparing clustered data por single-view ANALYSIS
#########################################################

# First we select the columns that interest us for the single-view analysis

# 38:67 -> 30 Non-motor symptoms in order
# 8:32  -> 25 motor symptoms in order
# 3:7   -> country, age, sex, pdonset, durat_pd
# 82    -> cisitot
# 33    -> hy
# 81    -> scmtot
# 77    -> nmstot

filtered_d897_nms = d897[c(38:67, 3:7, 82, 33, 81)]
filtered_d897_motor = d897[c(8:32, 3:7, 82, 33, 77)]

#Then we create a new list where each of its items is a completed data frame with clusters
filtered_d897_nms_withClusters = vector("list", 11)
filtered_d897_motor_withClusters = vector("list", 11)

# NMS: Add its assigned clusters to each data frame's row
for(i in 1:length(filtered_d897_nms_withClusters)){
  filtered_d897_nms_withClusters[[i]] = kmeans_nms_models[[i]]$cluster
}

# SCOPA-MOTOR: Add its assigned clusters to each data frame's row
for(i in 1:length(filtered_d897_motor_withClusters)){
  filtered_d897_motor_withClusters[[i]] = kmeans_motor_models[[i]]$cluster
}

#########################################################
### E) single-view heatmaps (not analysis)
#########################################################

nms_heatmaps = vector("list", 11)

for(i in 1:length(kmeans_nms_models)){
  only_nms_centers_data = kmeans_nms_models[[i]]$centers
  
  # Order the matrix rows in ascending order 
  # (http://stackoverflow.com/questions/10508352/how-to-sort-a-matrix-by-all-columns)
  ordered_heat_map_data = only_nms_centers_data[do.call(order, lapply(1:NCOL(only_nms_centers_data), function(i) only_nms_centers_data[, i])),]
  
  # Name the matrix rows with its new number representing its ascending order
  rownames(ordered_heat_map_data) = 1:nrow(ordered_heat_map_data)
  
  # melt the data matrix to be  able to use it with ggplot
  melted_heat_map_data = melt(ordered_heat_map_data)
  
  # rename the columns of the melted matrix (Var1 -> Non-motor symptoms, Var2 -> Clusters)
  names(melted_heat_map_data) = c("Var1" = "Non-motor symptoms", "Var2"= "Clusters", "value" = "value")
  
  # We draw the specific NMS heatmap for the current set of clusters
  nms_heatmaps[[i]] = ggplot(data = melted_heat_map_data, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value, 1))) +
    scale_fill_gradient(low = "white", high = "red")
}

motor_heatmaps = vector("list", 11)

for(i in 1:length(kmeans_motor_models)){
  only_motor_centers_data = kmeans_motor_models[[i]]$centers
  
  # Order the matrix rows in ascending order 
  # (http://stackoverflow.com/questions/10508352/how-to-sort-a-matrix-by-all-columns)
  ordered_heat_map_data = only_motor_centers_data[do.call(order, lapply(1:NCOL(only_motor_centers_data), function(i) only_motor_centers_data[, i])),]
  
  # Name the matrix rows with its new number representing its ascending order
  rownames(ordered_heat_map_data) = 1:nrow(ordered_heat_map_data)
  
  # melt the data matrix to be  able to use it with ggplot
  melted_heat_map_data = melt(ordered_heat_map_data)
  
  # rename the columns of the melted matrix (Var1 -> SCOPA-MOTOR symptoms, Var2 -> Clusters)
  names(melted_heat_map_data) = c("Var1" = "SCOPA-MOTOR symptoms", "Var2"= "Clusters", "value" = "value")
  
  # We draw the specific SCOPA-Motor heatmap for the current set of clusters
  motor_heatmaps[[i]] = ggplot(data = melted_heat_map_data, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(aes(fill = value)) + 
    geom_text(aes(label = round(value, 1))) +
    scale_fill_gradient(low = "white", high = "blue")
}

#########################################################
### F) Export the heatmaps
#########################################################

