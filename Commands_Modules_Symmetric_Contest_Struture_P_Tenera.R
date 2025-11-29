####Sequential Behavioral Analysis in P. Tenera####

#####Load required packages####
library('igraph')

#####Import data frame####
peri=read.csv2('ptenera_observed_behaviours_simmetric_contests.csv',h=F,stringsAsFactors = T)

#####data frame inspection####
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

#The matrix should be n x n for an ethogram with n possible behaviours
#In the adjacency matrix each cell represents the number of times one behavior (each row) transitions to another (each column) (similar to Green & Patek, 2018). 

adjacency.matrix #the vertical behaviors are preceding, and in the horizontal the subsequent behaviors

#####Evaluating the modularity in the observed matrix####

#Load packages
library(igraph)


#Create directed graph
g <- graph_from_adjacency_matrix(adjacency.matrix, mode = "directed", diag = FALSE)
# Observed modularity with Infomap method
comm_obs <- cluster_infomap(g)

#Infomap optimizes the search and function algorithm known as the mapequation using a modified and extended Louvain search algorithm (Farage et al., 2021). Specifically, the algorithm finds the partition that best compresses a description of flows on the network.
#After that we use this output of cluster/subset identification to orient the modularity calculation, inserting it as a membership vector. Membership refereed to the subset of behaviors of each module
obs_mod  <- modularity(g, membership(comm_obs))

  
#####Creating the reference model for calculate the mean modularity by chance####

# Configuration model - maintains exact degree sequence
  create_null_model_config <- function(adjacency.matrix) {
    adjacency.matrix <- as.matrix(adjacency.matrix)
    n <- nrow(adjacency.matrix)
    
    if (n != ncol(adjacency.matrix)) {
      stop("Adjacency matrix must be square")
    }
    
    out_degree <- rowSums(adjacency.matrix, na.rm = TRUE) #Maintain the sum of rows
    in_degree <- colSums(adjacency.matrix, na.rm = TRUE) #Maintain the sum of columns
    
    # Create stub lists
    out_stubs <- rep(1:n, times = out_degree) #Replicate the sum of the rows in each matrix
    in_stubs <- rep(1:n, times = in_degree) #Replicate the sum of the columns in each matrix
    
    if (length(out_stubs) != length(in_stubs)) {
      stop("Total out-degree must equal total in-degree")
    } #Stop if rows do not match column number
    
    # Shuffle connections
    in_stubs_shuffled <- sample(in_stubs) 
    
    # Create null matrix (maintaining the number of rows and column)
    null.matrix <- matrix(0, nrow = n, ncol = n)
    
    if (!is.null(rownames(adjacency.matrix)) && length(rownames(adjacency.matrix)) == n) {
      rownames(null.matrix) <- rownames(adjacency.matrix)
    }
    if (!is.null(colnames(adjacency.matrix)) && length(colnames(adjacency.matrix)) == n) {
      colnames(null.matrix) <- colnames(adjacency.matrix)
    }
    
    # Assign connections (edges/transitions)
    for (i in 1:length(out_stubs)) {
      from <- out_stubs[i]
      to <- in_stubs_shuffled[i]
      null.matrix[from, to] <- null.matrix[from, to] + 1
    }
    
    return(null.matrix)
  }

####Comparing modularities####
print(paste("Observed modularity:", round(obs_mod, 4)))
print(paste("Number of communities:", max(membership(comm_obs))))

#####Creating the modularity of reference (null) model####
null.matrix.test <- create_null_model_config(adjacency.matrix)
g_null_test <- graph_from_adjacency_matrix(null.matrix.test, mode = "directed", diag = FALSE)
comm_null_test <- cluster_infomap(g_null_test)
null_mod_test <- modularity(g_null_test, membership(comm_null_test))

#Print results  
print(paste("Test null modularity:", round(null_mod_test, 4)))

###Setting parameters to creating the reference model

n_permutations <- 10000  # We maintain the highest value of permutations
method <- "config"      # Use configuration model to generate 10000 reference models

#Initialize vector to store null modularities
null_modularities <- numeric(n_permutations)
  
# Start the loop
start_time <- Sys.time()
  
for (i in 1:n_permutations) {
    # Create null model
    null.matrix <- create_null_model_config(adjacency.matrix)
    
    # Calculate modularity for null model
    g_null <- graph_from_adjacency_matrix(null.matrix, mode = "directed", diag = FALSE)
    comm_null <- cluster_infomap(g_null)
    null_modularities[i] <- modularity(g_null, membership(comm_null))
    
    # Print progress every 100 iterations
    if (i %% 100 == 0) {
      print(paste("Completed", i, "of", n_permutations, "permutations"))
    }
  }
end_time <- Sys.time()

#Calculating test statistics  
# Remove any NA values
null_modularities <- null_modularities[!is.na(null_modularities)]
valid_permutations <- length(null_modularities)
  
# Calculate p-value and statistics
p_value <- sum(null_modularities >= obs_mod) / valid_permutations
mean_null <- mean(null_modularities)
sd_null <- sd(null_modularities)
z_score <- (obs_mod - mean_null) / sd_null
  
#### Printing results ####
cat("\n=== MODULARITY SIGNIFICANCE TEST RESULTS ===\n")
cat("Observed modularity:", round(obs_mod, 4), "\n")
cat("Mean null modularity:", round(mean_null, 4), "\n")
cat("SD null modularity:", round(sd_null, 4), "\n")
cat("Z-score:", round(z_score, 4), "\n")
cat("P-value:", round(p_value, 4), "\n")
cat("Valid permutations:", valid_permutations, "out of", n_permutations, "\n")
  
#RESULT: Modularity is not significantly different from chance (p >= 0.05)
# Simple histogram

#Observed modularity: - 0.03
#Mean null modularity: -0.07
#SD null modularity: 0.03
#Z-score: 1.2
#P-value: 0.10


########Creating the network diagram#########

#####Creating the transitional probability values####

# These permutations preserve:
# - row sums = frequency each behavior precedes another
# - column sums = frequency each behavior follows another
# Thus preventing biologically impossible transitions
# (e.g., transitions into Intruder approach or out of Loser flew away)


library(vegan)

set.seed(123)

randomized <- permatfull(
  m = adjacency.matrix,
  fix = "both",      # fixes row and column sums
  shuffle = "both",
  times = 1000       # number of null models
)

#Save the generated matrices
randomized_matrices <- randomized$perm

# Convert list of 1000 matrices to 11×11×1000 array
randomized_matrices <- simplify2array(randomized_matrices)
dim(randomized_matrices)   # should be 11 11 1000


#####Computing quantiles for each behavioral transition ######

# Getting 2.5% and 97.5% quantiles for each cell across random permutations
quant_array <- apply(
  randomized_matrices,
  c(1,2),
  quantile,
  probs = c(0.025, 0.975)
)

# Reshaping matrices
quantiles <- matrix(quant_array, ncol = 2, byrow = FALSE)
colnames(quantiles) <- c("lowCI_2.5%", "highCI_97.5%")

low  <- matrix(quantiles[,1], nrow = 11, byrow = FALSE)
high <- matrix(quantiles[,2], nrow = 11, byrow = FALSE)


#Renaming the low and high quantile datasets to match row and column names of the observed adjacency matrix
colnames(low)<-colnames(adjacency.matrix)
rownames(low)<-colnames(adjacency.matrix)
colnames(high)<-colnames(adjacency.matrix)
rownames(high)<-colnames(adjacency.matrix)

# Calculating transitional probabilities from the observed dataset

#For this, first we calculed the transtions probabibily of our observed matrix, dividing each cell number of our observed column by row sum. After that, we isolated the transitions that occurs more than expected by chance.

#pulling out the transitional probability values from the observed matrix
trans.prob<-round(adjacency.matrix/rowSums(adjacency.matrix),2) 
trans.prob[is.nan(trans.prob)]<-0 #replace any NaN values with 0
trans.prob

#To maintain the frequency of each behavior, we isolated the transition that occurs more than expected. For this, we simplify the data set to keep only those cell values where adjancecy matrix is greater or equal to high quantile of the null matrix.

#keep the original transitional probabilities, i.e. before simplification
#Transitions observed that have a greater probabilty than expected
keep_high<-ifelse(adjacency.matrix>=high,trans.prob,0) 

#Calculating frequency of each behavior (nodes size in the figure)
# Calculate row sums (total counts for each behavior)

behavior_frequency <- colSums(adjacency.matrix)
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

