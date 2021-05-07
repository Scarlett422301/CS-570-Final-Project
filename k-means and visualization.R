# load the data
# a list with 514 elements(subjects)
# each element is a 91*55 matrix (in the form of a list)
# 91 is the number of correlation matrix using the sliding window technique
# 55 = choose(11, 2), which contains the unique elements (up diagonal)
# of a 11*11 correlation matrix (11: # of modules/nodes)
load("C:/Users/phadmin/Desktop/CS 570 Project/PNC514_Corrmat.RData")

# libraries
library(reshape)
library(ggplot2)
library(gridExtra)
library(cluster)    
library(factoextra) 

# combine all data as a big matrix
# concatenate the matrices in each element of data by row
data_matrix = rbind(as.matrix(data[[1]]), as.matrix(data[[2]]))
N = length(data) # 514, number of subjects
for (i in 3:N) {
  data_matrix = rbind(data_matrix, as.matrix(data[[i]]))
} # data_matrix: 46774*55 (46774 = 514*91)



########### Decide the initial centroids for k = 3-8 ###############
n = 100 # number of repetitions
t = nrow(as.matrix(data[[1]])) # 91, number of correlation matrix using the sliding window technique
data_subsample = list(NA) # an empty list to store the n subsample

# Step1: sample 3 rows from the 91 rows for each subject,
#        repeat this process n times to get n subsamples
for (i in 1:n) {
  for (j in 1:N) {
    # set a different seed for each subject and in each iteration
    set.seed((i-1)*N + j) 
    # sample 3 rows from the 91 rows for each subject for subject N
    index_i_j = sample(1:t, 3)
    
    # concatenate the sampled rows from each subject
    if(j == 1){
      data_subsample_i = as.matrix(data[[j]])[index_i_j, ] 
    } else{
      data_subsample_i = rbind(data_subsample_i, as.matrix(data[[j]])[index_i_j, ])
    }
    
  }
  # store the subsample from the ith iteration
  data_subsample[[i]] = data_subsample_i # (514*3)*55
}

# Step2: for each subsample, run k-means clustering (random initialization),
# to get the "final" cluster centroids. 
# Then perform k-means clustering for all n "final centroids" (k*n rows)
# The "final" centroids from clustering the pervious centroids are treated as 
# the initial centroids for the complete dataset (data_matrix)
# repeat this step for k = 3-8

k = 3
# an empty list to store the clustering result for the n subsample
cluster_subsample_k3 = list(NA) 
set.seed(424)

for (i in 1:n) {
  # the maximum number of iterations allowed is set to 70
  cluster_subsample_k3[[i]] = kmeans(data_subsample[[i]], centers = k, 
                                     algorithm = "Lloyd", iter.max = 70)
  # cluster_subsample_k3[[i]] stores the list of the clustering result from the ith subsample
}

# k*n rows, concatenate all centroids in cluster_subsample_k3
data_centroids_k3 = cluster_subsample_k3[[1]]$centers
for (i in 2:n) {
  data_centroids_k3 = rbind(data_centroids_k3, cluster_subsample_k3[[i]]$centers)
}

# clustering the centroids matrix
set.seed(425)
cluster_centroids_k3 = 
  kmeans(data_centroids_k3, centers = k, algorithm = "Lloyd")

# the final centroids from clustering the centroids matrix are terated as
# the initial centroids for the complete dataset (data_matrix)
initial_k3 = cluster_centroids_k3$centers



k = 4
# an empty list to store the clustering result for the n subsample
cluster_subsample_k4 = list(NA) 
set.seed(424)

for (i in 1:n) {
  # the maximum number of iterations allowed is set to 70
  cluster_subsample_k4[[i]] = kmeans(data_subsample[[i]], centers = k, 
                                     algorithm = "Lloyd", iter.max = 70)
  # cluster_subsample_k4[[i]] stores the list of the clustering result from the ith subsample
}

# k*n rows, concatenate all centroids in cluster_subsample_k4
data_centroids_k4 = cluster_subsample_k4[[1]]$centers
for (i in 2:n) {
  data_centroids_k4 = rbind(data_centroids_k4, cluster_subsample_k4[[i]]$centers)
}

# clustering the centroids matrix
set.seed(425)
cluster_centroids_k4 = 
  kmeans(data_centroids_k4, centers = k, algorithm = "Lloyd")

# the final centroids from clustering the centroids matrix are terated as
# the initial centroids for the complete dataset (data_matrix)
initial_k4 = cluster_centroids_k4$centers


k = 5
# an empty list to store the clustering result for the n subsample
cluster_subsample_k5 = list(NA) 
set.seed(424)

for (i in 1:n) {
  # the maximum number of iterations allowed is set to 80
  cluster_subsample_k5[[i]] = kmeans(data_subsample[[i]], centers = k, 
                                     algorithm = "Lloyd", iter.max = 80)
  # cluster_subsample_k5[[i]] stores the list of the clustering result from the ith subsample
}

# k*n rows, concatenate all centroids in cluster_subsample_k5
data_centroids_k5 = cluster_subsample_k5[[1]]$centers
for (i in 2:n) {
  data_centroids_k5 = rbind(data_centroids_k5, cluster_subsample_k5[[i]]$centers)
}

# clustering the centroids matrix
set.seed(425)
cluster_centroids_k5 = 
  kmeans(data_centroids_k5, centers = k, algorithm = "Lloyd")

# the final centroids from clustering the centroids matrix are terated as
# the initial centroids for the complete dataset (data_matrix)
initial_k5 = cluster_centroids_k5$centers


k = 6
# an empty list to store the clustering result for the n subsample
cluster_subsample_k6 = list(NA) 
set.seed(424)

for (i in 1:n) {
  # the maximum number of iterations allowed is set to 70
  cluster_subsample_k6[[i]] = kmeans(data_subsample[[i]], centers = k, 
                                     algorithm = "Lloyd", iter.max = 70)
  # cluster_subsample_k6[[i]] stores the list of the clustering result from the ith subsample
}

# k*n rows, concatenate all centroids in cluster_subsample_k6
data_centroids_k6 = cluster_subsample_k6[[1]]$centers
for (i in 2:n) {
  data_centroids_k6 = rbind(data_centroids_k6, cluster_subsample_k6[[i]]$centers)
}

# clustering the centroids matrix
set.seed(425)
cluster_centroids_k6 = 
  kmeans(data_centroids_k6, centers = k, algorithm = "Lloyd", iter.max = 20)

# the final centroids from clustering the centroids matrix are terated as
# the initial centroids for the complete dataset (data_matrix)
initial_k6 = cluster_centroids_k6$centers


k = 7
# an empty list to store the clustering result for the n subsample
cluster_subsample_k7 = list(NA) 
set.seed(424)

for (i in 1:n) {
  # the maximum number of iterations allowed is set to 70
  cluster_subsample_k7[[i]] = kmeans(data_subsample[[i]], centers = k, 
                                     algorithm = "Lloyd", iter.max = 70)
  # cluster_subsample_k7[[i]] stores the list of the clustering result from the ith subsample
}

# k*n rows, concatenate all centroids in cluster_subsample_k7
data_centroids_k7 = cluster_subsample_k7[[1]]$centers
for (i in 2:n) {
  data_centroids_k7 = rbind(data_centroids_k7, cluster_subsample_k7[[i]]$centers)
}

# clustering the centroids matrix
set.seed(425)
cluster_centroids_k7 = 
  kmeans(data_centroids_k7, centers = k, algorithm = "Lloyd", iter.max = 30)

# the final centroids from clustering the centroids matrix are terated as
# the initial centroids for the complete dataset (data_matrix)
initial_k7 = cluster_centroids_k7$centers


k = 8
# an empty list to store the clustering result for the n subsample
cluster_subsample_k8 = list(NA) 
set.seed(424)

for (i in 1:n) {
  # the maximum number of iterations allowed is set to 70
  cluster_subsample_k8[[i]] = kmeans(data_subsample[[i]], centers = k, 
                                     algorithm = "Lloyd", iter.max = 70)
  # cluster_subsample_k8[[i]] stores the list of the clustering result from the ith subsample
}

# k*n rows, concatenate all centroids in cluster_subsample_k8
data_centroids_k8 = cluster_subsample_k8[[1]]$centers
for (i in 2:n) {
  data_centroids_k8 = rbind(data_centroids_k8, cluster_subsample_k8[[i]]$centers)
}

# clustering the centroids matrix
set.seed(425)
cluster_centroids_k8 = 
  kmeans(data_centroids_k8, centers = k, algorithm = "Lloyd")

# the final centroids from clustering the centroids matrix are terated as
# the initial centroids for the complete dataset (data_matrix)
initial_k8 = cluster_centroids_k8$centers


############ Run k-means clustering on the complete data ############
set.seed(426)
cluster_complete_k3 = kmeans(data_matrix, centers = initial_k3,
                             algorithm = "Lloyd", iter.max = 20)

centroids_k3 = cluster_complete_k3$centers

set.seed(426)
cluster_complete_k4 = kmeans(data_matrix, centers = initial_k4,
                             algorithm = "Lloyd", iter.max = 60)

centroids_k4 = cluster_complete_k4$centers

set.seed(426)
cluster_complete_k5 = kmeans(data_matrix, centers = initial_k5,
                             algorithm = "Lloyd", iter.max = 70)

centroids_k5 = cluster_complete_k5$centers

set.seed(426)
cluster_complete_k6 = kmeans(data_matrix, centers = initial_k6,
                             algorithm = "Lloyd", iter.max = 70)

centroids_k6 = cluster_complete_k6$centers

set.seed(426)
cluster_complete_k7 = kmeans(data_matrix, centers = initial_k7,
                             algorithm = "Lloyd", iter.max = 90)

centroids_k7 = cluster_complete_k7$centers

set.seed(426)
cluster_complete_k8 = kmeans(data_matrix, centers = initial_k8,
                             algorithm = "Lloyd", iter.max = 180)

centroids_k8 = cluster_complete_k8$centers


########### elbow method (min within cluster sum of squares) #########

# total within cluster sum of square
df = data.frame(cluster = 3:7, totalwithinss = c(cluster_complete_k3$tot.withinss, cluster_complete_k4$tot.withinss,
                                                 cluster_complete_k5$tot.withinss, cluster_complete_k6$tot.withinss,
                                                 cluster_complete_k7$tot.withinss))
ggplot(data=df, aes(x=cluster, y=totalwithinss, group=1)) +
  geom_line() + geom_point() + theme_bw() + xlab("Number of cluster k") + ylab("Total within-cluster sum of square") 

# proportion of change
df.diffprop = data.frame(cluster = 4:7, diffprop = abs(df$totalwithinss[2:5] - df$totalwithinss[1:4])/df$totalwithinss[1:4])

ggplot(data=df.diffprop, aes(x=cluster, y=abs(diffprop), group=1)) +
  geom_line()+
  geom_point()  

########### visualization of cluster centroids k = 3-8 ###############

# old modname
modname = c("UC","med vis","op vis","lat vis","DMN","CB","SM","Aud","EC","FPL","FPR")
## change order
# 0 1-3 6 7 4 8 9 10 5 orig
# 11 1-3 4 5 6 7 8 9 10 new
modname.new = c("med vis","op vis","lat vis","SM","Aud","DMN","EC","FPL","FPR","CB","UC")
# match old to new
matching = match(modname.new, modname)

# transform vector back to matrix
Ltrinv = function(x, V , d=F){ 
  Y = matrix(0,ncol = V, nrow = V)
  Y[upper.tri(Y,d)] = x
  return(Y + t(Y) - d*diag(diag(Y)))  
}

# plot cluster centriods
for(k in 3:8){
  # get centorids
  centroids = eval(as.symbol(paste0("centroids_k", k)))
  # get size
  size = eval(as.symbol(paste0("cluster_complete_k", k)))$size
  # get proportion
  prop = round(size/dim(data_matrix)[1],2)*100
  # create a list to store plots
  plot_centroids = list()
  for(index in 1:k){
    # orginal
    origin.matrix = Ltrinv(centroids[index,], 11)
    # new
    new.matrix = origin.matrix[matching, matching]
    colnames(new.matrix) = modname.new
    rownames(new.matrix) = modname.new
    # plot the matrices
    longData = melt(new.matrix)
    longData$X1 = factor(longData$X1, levels = modname.new)
    longData$X2 = factor(longData$X2, levels = modname.new)
    plot_centroids[[index]] = ggplot(longData, aes(x = X2, y = X1)) + 
      geom_raster(aes(fill=value)) + 
      scale_fill_gradient2(low="#6E8AE9", mid = "white", high="red") +
      labs(x="", y="", title=paste0(size[index], "(", prop[index], "%)")) +
      theme_bw() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                         axis.text.y=element_blank(),axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(), legend.position="none") 
  }
  # assign
  var.name = paste0("plot_k", k)
  assign(var.name, plot_centroids)
}

# create a null plot
# plot the matrices
nullData = melt(Ltrinv(rep(0, 55), 11))
nullData$X1 = factor(nullData$X1)
nullData$X2 = factor(nullData$X2)
plot_null = ggplot(nullData, aes(x = X2, y = X1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient2(low="white", mid = "white", high="white") +
  labs(x="", y="", title="") +
  theme_bw() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),plot.background=element_blank(),
                     legend.position="none") 

# print out the plots
g = arrangeGrob(plot_k3[[1]], plot_k3[[2]], plot_k3[[3]], plot_null, plot_null, plot_null, plot_null, 
                plot_k4[[1]], plot_k4[[2]], plot_k4[[4]], plot_k4[[3]], plot_null, plot_null, plot_null, 
                plot_k5[[3]], plot_k5[[2]], plot_k5[[5]], plot_k5[[4]], plot_k5[[1]], plot_null, plot_null, 
                plot_k6[[6]], plot_k6[[3]], plot_k6[[1]], plot_k6[[2]], plot_k6[[5]], plot_k6[[4]], plot_null, 
                plot_k7[[5]], plot_k7[[7]], plot_k7[[4]], plot_k7[[3]], plot_k7[[1]], plot_k7[[2]], plot_k7[[6]], 
                # plot_k8[[7]], plot_k8[[3]], plot_k8[[4]], plot_k8[[2]], plot_k8[[5]], plot_k8[[8]], plot_k8[[1]], plot_k8[[6]],
                nrow = 5, ncol = 7)
setwd("/Users/scarlett/OneDrive\ -\ Emory\ University/CS\ 570\ Final\ Project/program\ and\ results")
ggsave(filename = "plot_centroids.png", g, width = 20, height = 15)







