# Load necessary libraries
library(ggplot2)

# Create a sample dataset
set.seed(123)  # For reproducibility
n <- 200  # Number of customers

# Generating random data for Age, Annual Income, and Spending Score
age <- sample(18:70, n, replace = TRUE)
annual_income <- sample(30000:120000, n, replace = TRUE)
spending_score <- sample(1:100, n, replace = TRUE)

# Combine into a data frame
data <- data.frame(Age = age,
                   Annual.Income = annual_income,
                   Spending.Score = spending_score)

# Inspect the first few rows of the dataset
head(data)

# Select relevant features for clustering
features <- data[, c("Age", "Annual.Income", "Spending.Score")]

# Standardize the features
features_scaled <- scale(features)

# Set seed for reproducibility
set.seed(123)

# Choose the number of clusters
k <- 3  # You can adjust this number based on your needs

# Run K-means clustering
kmeans_result <- kmeans(features_scaled, centers = k, nstart = 25)

# Add the cluster results to the original dataset
data$Cluster <- as.factor(kmeans_result$cluster)

# View the cluster assignments
head(data)

# Visualize the clusters
plot <- ggplot(data, aes(x = Annual.Income, y = Spending.Score, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "K-means Clustering of Mall Customers",
       x = "Annual Income",
       y = "Spending Score") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Print the plot
print(plot)

# Save the plot to a file
ggsave("kmeans_clusters.png", plot)

# Optional: Evaluate the clustering using within-cluster sum of squares
wss <- (nrow(features_scaled) - 1) * sum(apply(features_scaled, 2, var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(features_scaled, centers = i, nstart = 25)$withinss)
}

# Plotting the elbow method
plot(1:15, wss, type = "b", pch = 19, xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares",
     main = "Elbow Method for Optimal k")
