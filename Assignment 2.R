library(tidyverse)
library(cluster)
library(factoextra)
library(corrplot)

url <- "https://raw.githubusercontent.com/rashakil-ds/Public-Datasets/main/country-data.csv"
df <- read.csv(url)

df_clean <- drop_na(df)


set.seed(42) 
df_clean <- df_clean %>% slice_sample(n = 80) 


rownames(df_clean) <- df_clean$country

df_num <- df_clean %>% select(-country)
df_scaled <- scale(df_num)

cor_matrix <- cor(df_num)
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, mar=c(0,0,1,0))

dist_matrix <- dist(df_scaled, method = "euclidean")
hc_model <- hclust(dist_matrix, method = "ward.D2")

clusters <- cutree(hc_model, k = 3)
df_clean$Cluster <- as.factor(clusters)

# Increased text size (cex = 0.8) since we have fewer labels now
fviz_dend(hc_model, k = 3, cex = 0.8, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, rect = TRUE, 
          main = "Global Country Development Dendrogram ")

# Increased labelsize to 9 for better readability
fviz_cluster(list(data = df_scaled, cluster = clusters),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             ellipse.type = "convex", repel = TRUE, labelsize = 9, 
             show.clust.cent = FALSE, main = "Hierarchical Clustering")

cluster_profiles <- df_clean %>%
  select(-country) %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean))

print(cluster_profiles)