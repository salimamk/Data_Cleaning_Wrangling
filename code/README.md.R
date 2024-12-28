# Install and load required packages
install.packages(c("dplyr", "tidyr", "lubridate", "stringr", "ggplot2"))
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

# Load the dataset
data(ToothGrowth)
View(ToothGrowth)

#Explore the dataset
# View first few rows
head(ToothGrowth)

# Summary statistics
summary(ToothGrowth)

# Structure of the dataset
str(ToothGrowth)

#Column Names
colnames(ToothGrowth)

#Handle Missing Values
# Check for missing values
sum(is.na(ToothGrowth))

# If missing values were present, we could drop or impute them.
# Example: Removing rows with NA values
ToothGrowth_clean <- ToothGrowth %>% drop_na()

# Alternatively, if you wanted to impute missing values, you could use a package like `tidyr::replace_na()`


# Rename columns for clarity
ToothGrowth_clean <- ToothGrowth_clean %>%
  rename(
    ToothLength = len,
    Supplement = supp,
    DoseAmount = dose
  )

# View updated data
head(ToothGrowth_clean)


#Filter and Subset the data (filter for 0.5mg dose)

# Filter for dose of 0.5
ToothGrowth_filtered <- ToothGrowth_clean %>%
  filter(DoseAmount == 0.5)

# View filtered data
head(ToothGrowth_filtered)

#Create New Variables
# Create a new categorical column for dose levels
ToothGrowth_clean <- ToothGrowth_clean %>%
  mutate(DoseCategory = case_when(
    DoseAmount == 0.5 ~ "Low",
    DoseAmount == 1.0 ~ "Medium",
    DoseAmount == 2.0 ~ "High"
  ))

# View updated data with the new variable
head(ToothGrowth_clean)

#Summarize the data- calculate the average tooth length by supplement type.
# Group by supplement and calculate mean tooth length
ToothGrowth_summary <- ToothGrowth_clean %>%
  group_by(Supplement) %>%
  summarize(AverageToothLength = mean(ToothLength, na.rm = TRUE))

# View summary data
print(ToothGrowth_summary)

#Reshape the data
# Pivot the data into wide format
ToothGrowth_wide <- ToothGrowth_clean %>%
  pivot_wider(names_from = Supplement, values_from = ToothLength)

# View reshaped data
head(ToothGrowth_wide)

#Visualize the data

# Create the plot
plot <- ggplot(ToothGrowth_clean, aes(x = DoseAmount, y = ToothLength, color = Supplement)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tooth Length by Dose Amount and Supplement Type", x = "Dose Amount", y = "Tooth Length") +
  theme_minimal()

plot

# Save the plot in the 'figures' directory
ggsave("figures/plot_tooth_growth.png", plot, width = 8, height = 6)

#Final Dataset
# Save the cleaned dataset to a CSV file
final_dataset<-write.csv(ToothGrowth_clean, "ToothGrowth_cleaned.csv", row.names = FALSE)
