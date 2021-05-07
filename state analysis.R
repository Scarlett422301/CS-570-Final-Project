# state analysis
# libraries
library(ggplot2)
library(ggthemes)

######### plot 6 states based on the cluster result ###########

# old modname
modname = c("UC","med vis","op vis","lat vis","DMN","CB","SM","Aud","EC","FPL","FPR")
## change order
# 0 1-3 6 7 4 8 9 10 5 orig
# 11 1-3 4 5 6 7 8 9 10 new
modname.new = c("med vis","op vis","lat vis","SM","Aud","DMN","EC","FPL","FPR","CB","UC")
# match old to new
matching = match(modname.new, modname)

# extract the 6 centroids from k means clustering
cluster_complete = cluster_complete_k6
centroids = cluster_complete$centers

# plot the centroids using the new order
# transform vector back to matrix
Ltrinv = function(x, V , d=F){ 
  Y = matrix(0,ncol = V, nrow = V)
  Y[upper.tri(Y,d)] = x
  return(Y + t(Y) - d*diag(diag(Y)))  
}

# create a list to store plots
plot_centroids = list()
# get size
size = cluster_complete$size
# get proportion
prop = round(size/dim(data_matrix)[1],2)*100
for(index in 1:6){
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
    labs(x="", y="", title=paste0("State ", index, "  ", size[index], "(", prop[index], "%)")) +
    theme_bw() + theme(axis.line=element_blank(),
                       # axis.text.x=element_blank(),
                       # axis.text.y=element_blank(),
                       axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(), legend.position="none") # 
}

###### calculate prop of occurence for each state accross time ######

# states for each person
state_index = cluster_complete$cluster
state_index.subj = matrix(state_index, nrow = 514, ncol = 91, byrow = T)

# prop of occurence
plot_stateprop = list()
for(index in 1:6){
  prop = apply(state_index.subj, 2, function(x){
    mean(x == index)
  })
  df = data.frame(time = 1:91, proportion = prop*100)
  plot_stateprop[[index]] = ggplot(data=df, aes(x=time, y=proportion, group=1)) + 
    geom_line(color = "blue") + ylim(c(0, 40)) + ylab("Proportion of occurence(%)") + theme_classic() 
}

# aggregate the plots together
df.prop = c()
for(index in 1:6){
  prop = apply(state_index.subj, 2, function(x){
    mean(x == index)
  })
  df.prop = c(df.prop, prop*100)
}
df = data.frame(time = rep(1:91, 6), proportion = df.prop, state = factor(rep(1:6, each = 91)))
ggplot(data=df, aes(x=time, y=proportion, group=state)) + 
  geom_line(aes(color=state)) + ylim(c(0, 40)) + ylab("Proportion of occurence(%)") + theme_classic() 

###### calculate the average transition matrix ##########

# transition matrix
P = matrix(0, ncol = 6, nrow = 6)
for(i in 1:514){
  for(j in 1:90){
    P[state_index.subj[i,j], state_index.subj[i,j+1]] =  P[state_index.subj[i,j], state_index.subj[i,j+1]] + 1
  }
}
# P.avg = P/(514*90) 
P.avg = sweep(P, 1, apply(P, 1, sum), FUN = '/') # large value: high prob of transition 

# calculate stationary state
library(markovchain)
mychain = new("markovchain", states = as.character(1:6), transitionMatrix = P.avg, 
              byrow = TRUE, name = "mychain")
round(steadyStates(mychain), 4)*100

# plot the transition matrix
diag(P.avg) = NA
longData = melt(P.avg)
longData$X1= factor(longData$X1)
longData$X2 = factor(longData$X2)
colnames(longData)[3] = "Probability"
ggplot(longData, aes(X2, X1, fill = Probability)) + scale_fill_gradient(low="#6D3FE8", high="#E8E33F", na.value = "white") +
  geom_tile() + theme_bw() + xlab("To") + ylab("From")

# plot state transition examples
set.seed(123)
index = sample(1:514, 3)

df = data.frame(time = 1:91, state = as.factor(state_index.subj[1,]))
ggplot(data=df, aes(x=time, y=state, group=1)) + 
  geom_line() + theme_wsj(color = "white") + xlab("time")

df = data.frame(time = 1:91, state = as.factor(state_index.subj[6,]))
ggplot(data=df, aes(x=time, y=state, group=1)) + 
  geom_line() + theme_wsj(color = "white") + xlab("time")

df = data.frame(time = 1:91, state = as.factor(state_index.subj[14,]))
ggplot(data=df, aes(x=time, y=state, group=1)) + 
  geom_line() + theme_wsj(color = "white") + xlab("time")
