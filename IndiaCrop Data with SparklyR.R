library(sparklyr)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
install.packages("viridis")
library(viridis)

# Connect to Spark
sc <- spark_connect(master = "local")

# Load the dataset
crop_production <- spark_read_csv(sc, name = "crop_production", path = "C:/Users/bimbo/Downloads/archive (7)/crop_production.csv", header = TRUE, infer_schema = TRUE)

# Preview the data
print(head(crop_production))


# Summary of the dataset
summary_data <- sdf_describe(crop_production)
summary_data

# Total production by crop and season
total_production_by_crop_season <- crop_production %>%
  group_by(Crop, Season) %>%
  summarise(Total_Production = sum(Production, na.rm = TRUE)) %>%
  arrange(Crop, desc(Total_Production)) %>%
  collect()

# Display the results
print(total_production_by_crop_season)

# Identify the top 10 crops by total production
top_crops_total <- total_production_by_crop_season %>%
  group_by(Crop) %>%
  summarise(Total_Production = sum(Total_Production, na.rm = TRUE)) %>%
  top_n(10, Total_Production) %>%
  pull(Crop)

# Filter the data for top 10 crops by total production
filtered_total_production <- total_production_by_crop_season %>%
  filter(Crop %in% top_crops_total)

# Plot total production by crop and season for top 10 crops with log scale
ggplot(filtered_total_production, aes(x = reorder(Crop, -Total_Production), y = Total_Production, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10(labels = comma) +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Production by Top 10 Crops and Season (Log Scale)", 
       x = "Crop", y = "Total Production (Log Scale)")



# Average production by crop and season
average_production_by_crop_season <- crop_production %>%
  group_by(Crop, Season) %>%
  summarise(Average_Production = mean(Production, na.rm = TRUE)) %>%
  arrange(Crop, desc(Average_Production)) %>%
  collect()

# Display the results
print(average_production_by_crop_season)


# Total production by state 
total_production_by_state <- crop_production %>%
  group_by(State_Name) %>%
  summarise(Total_Production = sum(Production, na.rm = TRUE)) %>%
  arrange(desc(Total_Production)) %>%
  collect()

ggplot(total_production_by_state, aes(x = reorder(State_Name, -Total_Production), 
                                      y = Total_Production, fill = State_Name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Production by State (Log Scale)", 
       x = "State", y = "Total Production (Log Scale)")

# Identify the top 10 crops by total production
top_crops_total <- total_production_by_crop_season %>%
  group_by(Crop) %>%
  summarise(Total_Production = sum(Total_Production, na.rm = TRUE)) %>%
  top_n(10, Total_Production) %>%
  pull(Crop)

# Display the top 10 crops by total production
top_10_crops <- total_production_by_crop_season %>%
  group_by(Crop) %>%
  summarise(Total_Production = sum(Total_Production, na.rm = TRUE)) %>%
  arrange(desc(Total_Production)) %>%
  slice(1:10)

print(top_10_crops)


#analyse impact of season on crop

# List of specific crops to analyze
specific_crops <- c("Rice", "Wheat", "Maize", "Soyabean")

# Filter data for specific crops
filtered_crops <- crop_production %>%
  filter(Crop %in% specific_crops) %>%
  collect()
# Total production by specific crops and season
total_production_specific_crops <- filtered_crops %>%
  group_by(Crop, Season) %>%
  summarise(Total_Production = sum(Production, na.rm = TRUE)) %>%
  arrange(Crop, desc(Total_Production))

# Display the results
print(total_production_specific_crops)

# Plot total production by specific crops and season
ggplot(total_production_specific_crops, aes(x = Season, y = Total_Production, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d() +
  facet_wrap(~ Crop, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Production by Season for Specific Crops", x = "Season", y = "Total Production")


# Average production by specific crops and season
average_production_specific_crops <- filtered_crops %>%
  group_by(Crop, Season) %>%
  summarise(Average_Production = mean(Production, na.rm = TRUE)) %>%
  arrange(Crop, desc(Average_Production))

print(average_production_specific_crops)

# Plot average production by specific crops and season
ggplot(average_production_specific_crops, aes(x = Season, y = Average_Production, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d() +
  facet_wrap(~ Crop, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Production by Season for Specific Crops", x = "Season", y = "Average Production")


# Plot heatmap for total production by crop and season
ggplot(total_production_specific_crops, aes(x = Season, y = Crop, fill = Total_Production)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Total Production by Crop and Season", x = "Season", y = "Crop", fill = "Total Production")

# Plot  -heatmap for total production by crop and season with labels
ggplot(total_production_specific_crops, aes(x = Season, y = Crop, fill = Total_Production)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::comma(Total_Production)), color = "black", size = 3) +
  scale_fill_viridis_c(option = "plasma", labels = comma, na.value = "grey90") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Total Production by Crop and Season", x = "Season", y = "Crop", fill = "Total Production")


#spark disconnect
spark_disconnect(sc)
