library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dbplyr)
library(NbClust)

set.seed(123)

# Read file: either defence-stats.csv or attack-stats.csv
df <- read.csv(file="defence-stats.csv") 

# Get rid null values
df <- na.omit(df)

# Group by teams
df <- df %>% group_by(team)  %>%summarise(across(everything(), list(mean)))

# Set rownames
df <- column_to_rownames(df, var = "team")
df <- scale(df)

#Do WSS to detect best k
fviz_nbclust(df, kmeans, method = "wss")

#Do NBclust to get best k
nb <- NbClust(df, method = "kmeans", distance = "euclidean", min.nc = 2, max.nc = 10, index = "all")

#DO silhouette to detect best k
fviz_nbclust(df, kmeans, method = "silhouette")

# compute gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Plot clusterings
final <- kmeans(df, 3, nstart = 25)
fviz_cluster(final, data = df)
