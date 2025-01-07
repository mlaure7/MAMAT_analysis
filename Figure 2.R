install.packages("ggplot")
install.packages("tidyr")
# Load necessary library
# Load necessary libraries
library(ggplot2)
library(tidyr)

# Load the data
file_path <- "enrollment.csv"  # Replace with your actual file path
data <- read.csv(file_path, sep = ";")

# Reshape the data into a long format for easier plotting
data_long <- data %>%
  pivot_longer(
    cols = starts_with("Year"),
    names_to = "Quarter",
    values_to = "Enrollment"
  )

# Rename the first column for clarity
colnames(data_long)[1] <- "Category"

# Convert "Enrollment" to numeric and clean up missing values
data_long$Enrollment <- as.numeric(data_long$Enrollment)
data_long <- na.omit(data_long)

# Filter data for young adults
data_young <- data_long %>%
  filter(Category %in% c(
    "Combined target enrollment - young adults",
    "Combined actual enrollment - young adults"
  ))

# Filter data for older adults
data_older <- data_long %>%
  filter(Category %in% c(
    "Combined target enrollment - older adults",
    "Combined actual enrollment - older adults"
  ))

# Plot for Young Adults
plot_young <- ggplot(data_young, aes(x = Quarter, y = Enrollment, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("magenta", "blue")) +
  labs(
    title = "Enrollment: Actual vs. Target (Young Adults)",
    x = "Quarter",
    y = "Enrollment",
    color = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    legend.position = "bottom",  # Place the legend below the plot
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Plot for Older Adults
plot_older <- ggplot(data_older, aes(x = Quarter, y = Enrollment, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("orange", "green")) +
  labs(
    title = "Enrollment: Actual vs. Target (Older Adults)",
    x = "Quarter",
    y = "Enrollment",
    color = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray"),
    legend.position = "bottom",  # Place the legend below the plot
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Print the plots
print(plot_young)
print(plot_older)
