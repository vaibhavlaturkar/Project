
#########################################################################################
################################## / Segmenatation / ####################################
#######################################################################################



### We need 8 clusters 
## The parameter was set to 8, since eight ( 2× 2× 2 ) possible combinations of inputs (RFM) can be obtained by assigning high or low, according to the average R (F, M) value of a cluster being less than or greater than the overall average R (F, M). 


### Elbow method 

#Concept:

#Recall that, the basic idea behind partitioning methods, such as k-means clustering, is to define clusters such that the total intra-cluster variation (known as total within-cluster variation or total within-cluster sum of square) is minimized:

# minimize(???k=1kW(Ck))minimize(???k=1kW(Ck)),

#Where CkCk is the kthkth cluster and W(Ck)W(Ck) is the within-cluster variation.

# The total within-cluster sum of square (wss) measures the compactness of the clustering and we want it to be as small as possible.

# We will check optimal cluster by checking different evalution techique used in clustering


# K MEAN 

colnames(customers_q1)
# We will take Recency,Frequency & Monetary_Value for clustering 
customers_seg <- customers_q1[,2:4]
head(customers_seg)

# First we will find optimal number of cluster to determine using Elbow method and decide the number of no.of cluster to be involved in our analysis



set.seed(123)
# Compute and plot wss for k = 2 to k = 20
k.max <- 10 # Maximal number of clusters
data <- scale(customers_seg)

dataM <- as.matrix(data)

data <- dist(data)
wss <- sapply(1:k.max,function(k){kmeans(data,k,nstart=10)$tot.withinss})

plot(1:k.max, wss,type="b", pch = 19, frame = FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")
abline(v = 4, lty =2)

# By elbow method it is showing 3 cluster are optimal

#We can use  elbow method is implemented in factoextra package and can be easily computed using the function fviz_nbclust()

library(factoextra)
library(fpc)
library(cluster)


## K-MEANS clustering
fviz_nbclust(data, kmeans, method = "wss")+geom_vline(xintercept = 8, linetype = 2)
# TSS = 380000



## Hierarchical clustering
fviz_nbclust(data, hcut, method = "wss") +geom_vline(xintercept = 8, linetype = 2)
# TSS = 450000

# single
fviz_nbclust(data, hcut, method = "wss",hc_method = "single") +geom_vline(xintercept = 8, linetype = 2)
# TSS = 1472000


# complete
fviz_nbclust(data, hcut, method = "wss",hc_method = "complete") +geom_vline(xintercept = 8, linetype = 2)
# TSS = 500000

# average
fviz_nbclust(data, hcut, method = "wss",hc_method = "average") +geom_vline(xintercept = 8, linetype = 2)
# TSS = 500000  

# ward.D
fviz_nbclust(data, hcut, method = "wss",hc_method = "ward.D") +geom_vline(xintercept = 8, linetype = 2)
# TSS =  460000

# ward.D2
fviz_nbclust(data, hcut, method = "wss",hc_method = "ward.D2") +geom_vline(xintercept = 8, linetype = 2)
# TSS = 450000


## K mediods PAM
fviz_nbclust(data, pam, method = "wss") +geom_vline(xintercept = 8, linetype = 2)
# TSS =  430000



## DBSCAN But it will form no cluster for object which are away from dense area

###  Average silhouette method  

# CONCEPT:
# The average silhouette approach we'll be described comprehensively in the chapter cluster validation statistics. Briefly, it measures the quality of a clustering. That is, it determines how well each object lies within its cluster. A high average silhouette width indicates a good clustering.

#Average silhouette method computes the average silhouette of observations for different values of k. The optimal number of clusters k is the one that maximize the average silhouette over a range of possible values for k (Kaufman and Rousseeuw [1990]).

require(cluster)
## K-MEANS clustering
fviz_nbclust(data, kmeans, method = "silhouette")
# k = 8 has silhouette width  0.34


## PAM clustering
fviz_nbclust(data, pam, method = "silhouette")
# k = 8 has silhouette width is 0.30



## Hierarchical clustering

#Complete Method
fviz_nbclust(data, hcut, method = "silhouette",hc_method = "complete")
# k = 8 has  silhouette width is 0.24

#Average Method
fviz_nbclust(data, hcut, method = "silhouette",hc_method = "average")
# k = 8 has silhouette width is 0.21

#Single Method
fviz_nbclust(data, hcut, method = "silhouette",hc_method = "single")
# k = 8 has silhouette width is -0.1

#Ward.D Method
fviz_nbclust(data, hcut, method = "silhouette",hc_method = "ward.D")
# k = 8 has silhouette width is 0.24

#Ward.D2 Method
fviz_nbclust(data, hcut, method = "silhouette",hc_method = "ward.D2")
#  k = 8 has highest silhouette width  0.26


##########  Conclusions about elbow and silhouette methods

# Three cluster solutions are suggested using k-means, PAM and hierarchical clustering in combination with the elbow method.

# The average silhouette method gives two cluster solutions using k-means ,Hierarchical clustering method (kmean,single,average,ward.D2) algorithms. and silhouette method returns 3 clusters using PAM,Hierarchical clustering method ward.D


#The disadvantage of elbow and average silhouette methods is that, they measure a global clustering characteristic only. A more sophisticated method is to use the gap statistic which provides a statistical procedure to formalize the elbow/silhouette heuristic in order to estimate the optimal number of clusters.


######################### Gap Statistic method ########################

# Concept:

# The gap statistic has been published by R. Tibshirani, G. Walther, and T. Hastie (Standford University, 2001). The approach can be applied to any clustering method (K-means clustering, hierarchical clustering, .).

# The gap statistic compares the total within intracluster variation for different values of k with their expected values under null reference distribution of the data, i.e. a distribution with no obvious clustering.

# Function clusGap() in cluster package  can be used to estimate the number of clusters in the data by applying the gap statistic



library(cluster)

data1 <- customers_seg

# K-MEAN clustering
gap_stat <- clusGap(data1, FUN = kmeans, nstart = 10,K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
# Plot gap statistic
fviz_gap_stat(gap_stat)        # Gap Statistic = 0.42



## PAM clusteriNG
gap_stat_pam <- clusGap(data1, FUN = pam,K.max = 10, B = 50)
# Plot gap statistic
fviz_gap_stat(gap_stat_pam)    # Gap Statistic = 0.41


## Hierarchical clustering
set.seed(123)
gap_stat_h <- clusGap(data1, FUN = hcut,K.max = 10, B = 50)
# Plot gap statistic
fviz_gap_stat(gap_stat_h)      # Gap Statistic = 0.42

# Single
gap_stat_s <- clusGap(data1, FUN = hcut,method="single",K.max = 10, B = 50)
fviz_gap_stat(gap_stat_s)      # Gap Statistic = 0.43

# Complete
gap_stat_c <- clusGap(data1, FUN = hcut,method="complete",K.max = 10, B = 50)
fviz_gap_stat(gap_stat_c)      # Gap Statistic = 0.43


# Average
gap_stat_a <- clusGap(data1, FUN = hcut,method="average",K.max = 10, B = 50)
fviz_gap_stat(gap_stat_a)      # Gap Statistic = 0.44

# Ward-D
gap_stat_w <- clusGap(data1, FUN = hcut,method="ward.D",K.max = 10, B = 50)
fviz_gap_stat(gap_stat_w)      # Gap Statistic = 0.43

# Ward-D2
gap_stat_wd <- clusGap(data1, FUN = hcut,method="ward.D2",K.max = 10, B = 50)
fviz_gap_stat(gap_stat_w)      # Gap Statistic = 0.42





####################### NbClust Package ###############################

library(NbClust)

# A Package providing 30 indices for determining the best number of clusters

# Overview of NbClust package:
#As mentioned in the introduction of this article, many indices have been proposed in the literature for determining the optimal number of clusters in a partitioning of a data set during the clustering process.

#NbClust package, published by Charrad et al., 2014, provides 30 indices for determining the relevant number of clusters and proposes to users the best clustering scheme from the different results obtained by varying all combinations of number of clusters, distance measures, and clustering methods.

#An important advantage of NbClust is that the user can simultaneously computes multiple indices and determine the number of clusters in a single function call.

#The indices provided in NbClust package includes the gap statistic, the silhouette method and 28 other indices described comprehensively in the original paper of Charrad et al., 2014.

# Compute all the 30 indices


nb <- NbClust(data1, distance = "euclidean", min.nc = 2,max.nc = 10, method = "complete", index ="all")

# Print the result
nb

# Plot
fviz_nbclust(nb) + theme_minimal()


# _______________________________________________________________
# Algorithms     | Elbow Method | Silhouette Method| G-Statistic |
# ---------------------------------------------------------------
# Kmean          |    380000    |      0.34        |    0.42     |
# Kmean++        |       938    |      0.35        |    0.56     |
# PAM            |    430000    |      0.30        |    0.41     |
# Hclust-Single  |   1472000    |      -0.1        |    0.43     |
#       -Complete|    500000    |      0.24        |    0.43     |     
#       -Average |    500000    |      0.21        |    0.44     |
#       -Ward-D  |    460000    |      0.24        |    0.43     |
#       -Ward-D2 |    450000    |      0.26        |    0.42     |
# ---------------|--------------|------------------|-------------|


## As per the above graph K-Means is best algorithm for our analysis because Total Within Sum of Square 380000 which low with comparsion to other clusters, Average Silhouette Method is 0.34 which is high in Silhouette Method and G-Statistic is 0.42 is average.



## K-Means++

library(LICORS)

head(data)

data1 <- data.matrix(data[,c(1,2)])
data1

head(data)

dataM <- as.matrix(data1)

set.seed(123)
kmeanplus <- kmeanspp(dataM,k = 8)

kmeanplus$tot.withinss # 937.606


gap_stat_k <- clusGap(dataM, FUN = kmeanspp,K.max = 10, B = 50)
fviz_gap_stat(gap_stat_k)      # Gap Statistic = 0.56

fviz_nbclust(dataM, kmeanspp, method = "silhouette")
# Avg Width is 0.34


customers_seg$Cluster <- kmeanplus$cluster
customers_seg$RFM_Value <- customers_q1$RFM_Value
customers_seg$R <- customers_q1$Rec_Score
customers_seg$F <- customers_q1$Freq_Score
customers_seg$M <- customers_q1$Mon_Score
customers_seg$CustomerID <- customers_q1$CustomerID


fg <- customers_seg %>%
  group_by(RFM_Value) %>%
  summarise(Count=n())
  

write.csv(customers_seg,"seg4.csv")


df <- customers_seg %>%
  group_by(RFM_Value)


library(dplyr)
Final.Cluster.Analysis <- customers_seg %>%
  group_by(Cluster) %>%
  summarise(Re=round(mean(Recency)),R=mean(R),Freq=round(mean(Frequency)),F=mean(F),Mone=round(mean(Monetary_Value)),M=mean(M),Count=n())

library(ggplot2)
ggplot(customers_seg,aes(Recency,Monetary_Value,color=as.factor(Cluster)))+geom_point()
ggplot(customers_seg,aes(Recency,Frequency,color=as.factor(Cluster)))+geom_point()

summary(customers_seg)
J <- filter(customers_seg,Cluster == 3)
summary(J)
hist(J$Recency)

# C1  R-Low,F-Low,M-Low     : LLL  
# C2  R-Low,F-High,M-Low    : HHL  
# C3  R-High,F-Low,M-High   : HHL     TOP CUSTOMERS
# C4  R-Low,F-Low,M-High    : HLH
# C5  R-High,F-Low,M-vHigh  : LHL
# C6  R-VHigh,F-Low,M-High  : HLH
# C7  R-High,F-High,M-Low   : LLH
# C8  R-High,F-vHigh,M-Low  : HHH    


# -----------------------------------------------------------------------------------
# Cluster Recency Frequency Monetary_Value  RFM  Count RFM Pattern   Customer Type
# -----------------------------------------------------------------------------------
#   C1     109        10         29         151    57   R??? F??? M???   High Value Spenders
#   C2     111        38          6         156   116    R??? F??? M???   Frequent
#   C3      71        14         17         322   272    R??? F??? M???   First Time
#   C4     112        13         17         146   223    R??? F??? M???   Low Value Spenders
#   C5      51        16         29         456    99    R??? F??? M???   High Value,Not Loyal
#   C6      43        17         17         498   262    R??? F??? M???   Active High Value
#   C7      57        39         11         430   246    R??? F??? M???   Active Low Value
#   C8      58        75         11         444   100    R??? F??? M???   Shopper
# ------------------------------------------------------------------------------------
#   Avg    76.5      27.75     17.12       Total = 1375
# -------------------------------------------------------------------------------------

summary(customers_seg)


############ Next Markow Chain
############ 


