###################-------------------###################
#############                                 ###########
#               Preprosessing the Data                  #
#############                                 ###########
###################-------------------###################

data<-my_data <- read.csv("Dar.csv") # importing the data
rownames(data) <- data[,1]            # Setting rownames

new_data=data[,-1]     # removing the first column because it only serves as our row names

class(new_data)        # checking your data class

dim(new_data)          # getting the dimension of your data

names(new_data)        # getting the column names

str(new_data)          # checking data structure

anyNA(new_data)        # checking for missing values

summary(new_data)      # summary of the data

###################-------------------###################
#############                                 ###########
#                    Cluster Analysis                   #
#############                                 ###########
###################-------------------###################

## standardizing and preparing the dataset
new_data_scaled <- scale(new_data)                # standardizing the variables
new_data_scaled
new_data_scaled.dist <- dist(new_data_scaled)     # distance computation with year as observation
new_data_scaled.distT <- dist(t(new_data_scaled)) # distance computation with year as variables

# Ward, single and average linkage 
ward = hclust(new_data_scaled.distT,method='ward.D2') # Rely on the variance 
single= hclust(new_data_scaled.distT,method='single') # Rely on the distance
average = hclust(new_data_scaled.distT,method='average') # Rely on the average

## Plotting the wards, single and Average method
plot(ward, main = 'Wards Method',xlab =" ", sub ="")
rect.hclust(ward, k =3, border = 'red') ## selecting three clusters
plot(single, main = 'Single Linkage',xlab =" ", sub ="")
rect.hclust(single, k =3, border = 'red') ## selecting three clusters
plot(average, main = 'Average Linkage',xlab =" ", sub ="")
rect.hclust(average, k =3, border = 'red') ## selecting three clusters

