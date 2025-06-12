####Sequential Behavioral Analysis in P. Tenera####

#####Load required packages####
library('igraph')

#####Import data frame####
peri=read.csv2('ptenera_observed_behaviours_simmetric_contests.csv',h=F,stringsAsFactors = T)

#####inspect data frame####
head(peri)
summary(peri)
View(peri)

#remove the first row (column descriptors) and the last two columns (contest descriptors)
peri<-peri[-c(1),-c(1,4)]

#the resulting dataset should have only 2 columns.
View(peri)

#####Generate the observed adjacency matrix####
graph<-graph.data.frame(peri,directed = T) #Command to create a graph from the data frame (first column edge and subsequent behaviour vertex)

#Than I convert this graph into a adjacency matrix, a way to represent a graph with the frequency of each transition values
adjacency.matrix<-get.adjacency(graph,sparse=F)

#if using your own data, the matrix should be n x n for an ethogram with n possible behaviours
#In the adjacency matrix each cell represents the number of times one behavior (each row) transitions to another (each column) (similar to Green & Patek, 2018). 

adjacency.matrix #the vertical behaviors are preceding, and in the horizontal the subsequent behaviors


####Network figure#######

#save matrices for each of the low and high quantile data
low<-matrix(quantiles[,1],nrow=11,byrow=F)
high<-matrix(quantiles[,2],nrow=11,byrow=F)

#rename the low and high quantile datasets to match row and column names of the observed adjacency matrix
colnames(low)<-colnames(adjacency.matrix)
rownames(low)<-colnames(adjacency.matrix)
colnames(high)<-colnames(adjacency.matrix)
rownames(high)<-colnames(adjacency.matrix)

##### Calculating transitional probabilities from the observed dataset ####

#pull out the transitional probability values from the observed matrix
#divide each cell by the row sum
trans.prob<-round(adjacency.matrix/rowSums(adjacency.matrix),2) 
trans.prob[is.nan(trans.prob)]<-0 #replace any NaN values with 0
trans.prob

#simplify the dataset to keep only those cell values where adjacency matrix is greater or equal to high quantile
#keep the original transitional probabilities, i.e. before simplification
#Transitions observed that have a greater probabilty than expected
keep_high<-ifelse(adjacency.matrix>=high,trans.prob,0) 

# Calculate row sums (total counts for each behavior)
behavior_frequency <- colSums(adjacency.matrix)


#####Calculating frequency of each behavior (for the figure)####

#Intruder approach
31/(31+212+157+89+175+70+83+90+105+15+31) #31/1058
#3%
#Frequency of Cospecific identification
157/(212+157+89+175+70+83+90+105+15)
#16%
#Frequency of Bounce
83/(212+157+89+175+70+83+90+105+15)
#8%
#Frequency of Oviposition site probing
90/(212+157+89+175+70+83+90+105+15)
#9%
#Frequency of Pursuit Flight
212/(212+157+89+175+70+83+90+105+15)
#21%
#Frequency of Hover
105/(212+157+89+175+70+83+90+105+15)
#10%
#Frequency of Pursuit Flight Escalated
175/(212+157+89+175+70+83+90+105+15)
#16%
#Frequency of Circle Chase
89/(212+157+89+175+70+83+90+105+15)
#9%
#Frequency of Physical Agression
70/(212+157+89+175+70+83+90+105+15)
#7%
#Frequency of Out of sight
15/(212+157+89+175+70+83+90+105+15)
#1%
#Frequency of End of the contest
31/(31+212+157+89+175+70+83+90+105+15+31)
#3%



####Evaluating the presence of modules in the network####

#Package of network to define modules
library(bipartite)

#computeModules function takes a bipartite weighted graph and computes modules by applying Newman's modularity measure in a bipartite weighted version to it. 
#The value of the index indicate the level in which the matrix is organized in modules

mod<-computeModules(adjacency.matrix, steps=10E7) ; mod
#Index value ---> 0.25

#Generating the plot
tiff('Figure Module Simmetric Contests.tif',  w=3200 , h=2400,res=600)
par(mar=c(4,4,2,2))
plotModuleWeb(mod)
dev.off()

#Finding the most modular result of the network, with the command which re-runs the algorithm several times. 

mod1<-metaComputeModules(adjacency.matrix, N=10, steps=10E7) ; mod1@likelihood #Value of Likelihood

#Print the information of each potential modules (the behaviors and the subsequent transitions)
printoutModuleInformation(mod1)


#After that, we compare the value of the index of our matrix with the null distribution.
#For that, we generated random matrices with the same frequency of the behaviors that we observed and generate a Newman's modularity index measure of these matrix. After all randomization, its create a null distribution with all the value of the modularity index.

nulls <- nullmodel(adjacency.matrix, N=10000, method="r2d") #command to create the null distribution

#ATENTION ----> This can take a while to run <---- 
modules.nulls <- sapply(nulls, computeModules, step=10E7) #command to create a list of the modularity index and other information of each matrix of the null distribution
like.nulls <- sapply(modules.nulls, function(x) x@likelihood) #create a list only with the modules values (likelihood)

#We considered that the observed transitions had modules if our modularity index occurred equal or lower than 5% of seen in the null distribuition 
quantile(unlist(like.nulls), 0.95) 


# Calculate the mean null modularity value
mean_null_modularity <- mean(like.nulls)

# Output the mean null modularity value
print(mean_null_modularity)

