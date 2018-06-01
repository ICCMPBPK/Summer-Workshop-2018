

          ###############################################
          # Statistical Machine Learning Summer Workshop
          # 06/01/2018
          # Network Analysis 
          # Instructor: Cen Wu, Guotao Chu and Fei Zhou
          # A special Thanks to Jie Ren 
          # Department of Statistics, Kansas State University
          ##############################################


install.packages("igraph")

rm(list=ls(all=TRUE))
setwd("C:/Users/wucen/Desktop/Workshop/Network")
library("igraph")
source("./Network_N3.R")

#######################
## step1: load the data
#######################
load(file ="./SKCM_GENEs.RData")
Method = load("./coeff_network_56.RData") # Network example
# Method = load("./coeff_lasso_53.RData") # LASSO example
# Method = load("./coeff_network.RData") # your Network result
# Method = load("./coeff_lasso.RData") # your LASSO result

b.graph = b.rgn[-c(1:3)] # -c(1:3) removes coefficients of intercept, age and gender.
data = G

######################################
## step2: compute the adjacency matrix
######################################

adj.full = N.3(data)
subset = (b.graph != 0)
workingSet = data[,subset]
adjacency = adj.full[subset, subset]

###############################
## step3: plot the full network
###############################
par(mfrow=c(1,1))
net0 <- graph.adjacency(adjacency, mode="undirected", weighted=TRUE, diag=FALSE)
V(net0)$color = "skyblue"
plot(net0, vertex.size=10, edge.color="gray40", vertex.label=NA)
# plot(net0, vertex.size=10, vertex.label.cex=0.6, vertex.label.dist=2, edge.color="gray50")

###########################
## step4: plot sub-networks
###########################
par(mfrow=c(1,2))
nets = decompose.graph(net0, mode="weak")
min.genes = 3

largest = 0
for(i in 1: length(nets)){
  counts = length(V(nets[[i]]))
  if(counts >= min.genes){
    cat("subnet: ",i, "\n")
    plot(nets[[i]], vertex.size=10, vertex.label.dist=2, edge.color="gray80", main = paste("sub-network: ",i))
    if(counts>largest){
      largest = counts
      id = i
    }
  }
}
cat("The largest sub-networ has", largest, "genes.\n")

############################
## step5: take a closer look
############################
par(mfrow=c(1,1))
sgw = nets[[id]]
Width <- function(weight) weight^2 * 10;
l.sgw = layout.gem(sgw) # layout.lgl(sgw)

E(sgw)$color = "gray70"
E(sgw)$width = Width(E(sgw)$weight)
E(sgw)$label.color = "black"
V(sgw)$size = 10
V(sgw)$label.cex = 1
V(sgw)$label.dist = 2.5

plot(sgw, layout=l.sgw)

### show weights of the edges

w = signif(E(sgw)$weight, 2)
cutoff = 0.3
w[abs(w)<cutoff] = NA
plot(sgw, edge.label.color="black", edge.label.cex=0.8, edge.label=w, layout=l.sgw, vertex.label=NA)

### delete edges with small weights

for(cutoff in c(0.4, 0.5, 0.6, 0.7)){
  sgw.mod = delete.edges(sgw, which(E(sgw)$weight<cutoff))
  plot(sgw.mod, layout=l.sgw)
  readline(prompt="Press [enter] to continue")
}
