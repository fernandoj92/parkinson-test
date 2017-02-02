x897_nms30_kmeans234 = list(x897_nms_30_kmeans2, x897_nms_30_kmeans3, x897_nms_30_kmeans4)
filtered_nms30_kmeans234 = list()
for(i in 1:length(x897_nms30_kmeans234)){
  filtered_nms30_kmeans234[[i]] = x897_nms30_kmeans234[[i]][c(3:37, 98)]
}