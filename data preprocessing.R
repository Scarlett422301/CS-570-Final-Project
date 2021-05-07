# library
library(psych)
# import data
load("/Users/scarlett/OneDrive\ -\ Emory\ University/CS\ 570\ Final\ Project/data/PNC515.RData")
# extract the data
data = dat[-135] # 514 subjects, each 120*264 matrices

# node mode(each node's mode, in total 264 nodes)
node.mode = read.table("/Users/scarlett/OneDrive\ -\ Emory\ University/CS\ 570\ Final\ Project/data/roi_id.txt")
node.mode = node.mode$V1
modID = data.frame(id = 0:10, modname = c("UC","med vis","op vis","lat vis","DMN","CB","SM","Aud","EC","FPL","FPR"))

# calculate the mean of the time series based on node  264*120
node.mean = function(timeseries){
  # create a data frame 264 * (120 + node mode)
  temp = data.frame(t(timeseries), mode = node.mode)
  # calculate row mean based on mode
  result = aggregate(temp, list(temp$mode), mean)
  # return a new matrix
  newtimeseries = as.matrix(result[,-c(1,122)])
  # return 
  return(newtimeseries)
}

# sliding window calculation
bin = 30
newdata = list()
Corrmat = matrix(0, nrow = 91, ncol = 11*(11-1)/2)
for(subj in 1:length(data)){
  newtimeseries = node.mean(data[[subj]])
  for(i in 0:90){
    Sigma0 = cor(t(newtimeseries[,(i+1):(i+30)]))
    Corrmat[i+1,] = Sigma0[upper.tri(Sigma0)]
  }
  # fisher z transformation
  corrmat = fisherz(Corrmat)
  # change to Yraw.subj
  newdata[[subj]] = corrmat
}

# save data
data = newdata # list: 514 * (91*55)
save(data, file = "/Users/scarlett/OneDrive\ -\ Emory\ University/CS\ 570\ Final\ Project/data/PNC514_Corrmat.RData")
