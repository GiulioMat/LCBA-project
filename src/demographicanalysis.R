library(lattice)
library(cluster)
library(ggplot2)
library(gridExtra)

# load data
df <- read.csv("Laptop_Research_Survey.csv")
df <- df[,c(1,10,11,12,13)]
df <- unique(df)
df <- df[-1]



# plot demographic data
p_1 <- ggplot(as.data.frame(prop.table(table(df$Age))), aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  labs(x = "Age", y = "Frequency") +
  theme(legend.position="none")


p_2 <- ggplot(as.data.frame(prop.table(table(df$Gender))), aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  labs(x = "Gender", y = "Frequency") +
  theme(legend.position="none")

p_3 <- ggplot(as.data.frame(prop.table(table(df$Work))), aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  labs(x = "Work", y = "Frequency") +
  theme(legend.position="none")

p_4 <- ggplot(as.data.frame(prop.table(table(df$Education))), aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  labs(x = "Education", y = "Frequency") +
  theme(legend.position="none")


lay <- rbind(c(1,1,2,2,3,3),
             c(NA,4,4,4,4,NA))

grid.arrange(p_1, p_2, p_3, p_4, layout_matrix = lay, top = "Demographic Data")



# process for clustering
for(i in unique(df$Age)){
  name <- paste0("age_", i)
  df[name] <- ifelse((df$Age == i), 1, 0)
}

df$male <- ifelse((df$Gender == "male"), 1, 0)

df$bachelor <- ifelse((df$Education == "Bachelor's degree"), 1, 0)
df$master <- ifelse((df$Education == "Master of Science"), 1, 0)
df$phd <- ifelse((df$Education == "Doctor of Philosophy"), 1, 0)

df$part_time <- ifelse((df$Work == "Yes, part-time"), 1, 0)
df$full_time <- ifelse((df$Work == "Yes, full-time"), 1, 0)



# perform clustering
data_for_clustering <- df[, 5:ncol(df)]

min_clusters <- 2
max_clusters <- 20

evaluation_vector <- NULL 

for(num_clusters in min_clusters:max_clusters) {
  try_clustering <- pam(data_for_clustering, k = num_clusters, metric = "manhattan", stand = TRUE, pamonce=5)
  evaluation_vector <- rbind(evaluation_vector, 
                             data.frame(num_clusters, 
                                        average_silhouette_width = try_clustering$silinfo$avg.width))
}        

plot(evaluation_vector$num_clusters, evaluation_vector$average_silhouette_width)
grid()
abline(h=0.5, col="red")



# select 5 as number of clusters
five_cluster_solution <- pam(data_for_clustering, k = 5,
                            metric = "manhattan", stand = TRUE, pamonce=5)
plot(five_cluster_solution)

df$cluster <- five_cluster_solution$clustering
df$cluster <- factor(df$cluster, labels=c("A", "B", "C", "D", "E"))

# look at demographics across the clusters/segments
with(df, table(cluster, Gender))

with(df, table(cluster, Age)) 

with(df, table(cluster, Education))

with(df, table(cluster, Work))



# create table containing most frequent attribute for each cluster
clustering_results <- NULL

for(i in unique(df$cluster)){
  temp <- df[df$cluster == i,]
  clustering_results <- rbind(clustering_results,
                              data.frame(cluster=i,
                                         gender=names(which.max(table(temp$Gender))),
                                         age=names(which.max(table(temp$Age))), 
                                         education=names(which.max(table(temp$Education))),
                                         work=names(which.max(table(temp$Work))),
                                         size=nrow(temp)))
}

print(clustering_results)


