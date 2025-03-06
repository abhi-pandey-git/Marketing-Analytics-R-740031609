##########################################################
# INTEL SMARTWATCH SEGMENTATION & VISUALIZATION IN R
##########################################################

# Libraries
library(readxl)
library(tidyverse)
library(cluster)
library(factoextra)
library(reshape2)
install.packages("fmsb")
library(fmsb)

# Load data
data_survey <- read_excel("SmartWatch Data File.xlsx")

# Select relevant attributes
attrib_data <- data_survey %>%
  select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)

# Standardize
scaled_attrib <- scale(attrib_data)

# Elbow method for clusters
set.seed(123)
wss <- map_dbl(1:6, ~ kmeans(scaled_attrib, .x, nstart=10)$tot.withinss)
plot(1:6, wss, type="b", pch=19,
     xlab="Number of Clusters", ylab="Within-Cluster SS",
     main="Elbow Method")

# K-means with 3 clusters (example)
set.seed(123)
km_3 <- kmeans(scaled_attrib, centers=3, nstart=25)
data_survey$Cluster <- factor(km_3$cluster)



# Mean attribute scores by cluster
summary_table <- data_survey %>%
  group_by(Cluster) %>%
  summarise(across(c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style),
                   mean))

print(summary_table)

# ANOVA checks
aov_results <- lapply(names(attrib_data), function(x){
  res <- aov(as.formula(paste(x, "~ Cluster")), data=data_survey)
  summary(res)
})

# Visualization: bar chart of means
long_df <- summary_table %>%
  pivot_longer(-Cluster, names_to="Attribute", values_to="MeanVal")

ggplot(long_df, aes(x=Attribute, y=MeanVal, fill=Cluster)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  labs(title="Attribute Importance by Segment",
       x="Attribute", y="Mean Rating (1–7)")

# Visualization: box plots
melted_data <- data_survey %>%
  pivot_longer(cols=c(ConstCom,TimelyInf,TaskMgm,DeviceSt,Wellness,Athlete,Style),
               names_to="Attribute", values_to="Value")

ggplot(melted_data, aes(x=Cluster, y=Value, fill=Cluster)) +
  geom_boxplot() +
  facet_wrap(~ Attribute, scales="free") +
  labs(title="Box Plots of Attribute Ratings by Cluster")

