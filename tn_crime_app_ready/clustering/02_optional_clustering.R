# =============================================================
# Optional: District Risk Clustering (k-means)
# =============================================================
# The AI/ML component for the AI lab requirement -- groups districts by
# risk PROFILE (multiple crime categories at once) using an algorithm,
# rather than manually picking rate cutoffs.
#
# Run this AFTER data_prep/01_clean_and_merge.R has produced
# tn_crime_master.csv. Separate from the main app on purpose -- run it
# once, inspect the results, and wire the cluster labels into app.R's
# map coloring only if you want that for your submission.
# =============================================================

library(readr)
library(dplyr)
library(cluster)

master_df <- read_csv("tn_crime_master.csv", show_col_types = FALSE)

features <- master_df %>%
  select(district, murder_rate, women_crime_rate, road_accident_rate, suicide_rate) %>%
  na.omit()

scaled_features <- scale(features %>% select(-district))

wss <- sapply(2:6, function(k) kmeans(scaled_features, centers = k, nstart = 25)$tot.withinss)
plot(2:6, wss, type = "b", xlab = "Number of clusters (k)", ylab = "Total within-cluster SS",
     main = "Elbow plot -- pick k where the drop levels off")

k_chosen <- 4

set.seed(42)
km <- kmeans(scaled_features, centers = k_chosen, nstart = 25)

sil <- silhouette(km$cluster, dist(scaled_features))
cat("Average silhouette width:", round(mean(sil[, 3]), 3), "\n")
cat("(Above ~0.25 is workable; above ~0.5 is strong; below ~0.15, try a different k.)\n")

features$cluster <- km$cluster

cluster_summary <- features %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean), n_districts = n(), .groups = "drop") %>%
  mutate(overall_severity = murder_rate + women_crime_rate + road_accident_rate + suicide_rate) %>%
  arrange(overall_severity)

print(cluster_summary)

severity_rank <- cluster_summary$cluster
label_lookup <- setNames(
  c("Low risk", "Moderate risk", "Elevated risk", "High / mixed risk")[seq_along(severity_rank)],
  severity_rank
)

features <- features %>%
  mutate(risk_cluster = label_lookup[as.character(cluster)])

cat("\nDistricts per risk cluster:\n")
print(features %>% count(risk_cluster))

write_csv(features %>% select(district, cluster, risk_cluster), "tn_crime_clusters.csv")
cat("\nSaved tn_crime_clusters.csv.\n")
