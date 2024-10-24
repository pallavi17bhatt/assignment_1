#install.packages("ggplot2")

# Load required libraries
library(dplyr)
library(ggplot2)

# Load the data
data <- read.csv("Online_Retail_full_copy.csv")

# Filter out non-finite quantities and quantities greater than 1000
cleaned_data <- data %>%
  filter(Country %in% c("Spain", "Netherlands", "Belgium", "France", "Germany"), !is.na(Country), Quantity > 0, is.finite(Quantity)) %>%  # Keep only positive finite quantities
  filter(Quantity <= 100)  # Filter out quantities greater than 1000 for this plot

# Histogram with a color gradient based on Quantity
histogram_for_frequency_vs_quantity <- ggplot(cleaned_data, aes(x = Quantity, fill = ..count..)) +
  geom_histogram(binwidth = 5,boundary=0,color = "black", alpha = 0.7, linewidth=0.3) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  scale_y_continuous(breaks=seq(0, 10000, by = 1000), labels = scales::comma)+
  scale_fill_gradient(low = "blue", high = "grey") +  # Color gradient from blue to red
  labs(title = "Histogram of Quantities Sold (Gradient Coloring)", x = "Quantity", y = "Frequency") +
  theme_classic() +
  theme(
    # panel.background = element_rect(fill = "lightyellow", color = NA),  # Set the plot area background
    plot.background = element_rect(fill = "lightblue", color = "black"),  # Set the overall plot background
    axis.text = element_text(color = "black"),          # Change axis text to black
    axis.title = element_text(color = "black"),         # Change axis title to black
    plot.title = element_text(color = "black", face = "bold")  # Change plot title to black
  )
ggsave("histogram_for_frequency_vs_quantity.png", plot = histogram_for_frequency_vs_quantity, width = 8, height = 6)


# Summary statistics for Quantity
summary_stats <- cleaned_data %>%
  summarise(
    Mean = mean(Quantity, na.rm = TRUE),
    Median = median(Quantity, na.rm = TRUE),
    StdDev = sd(Quantity, na.rm = TRUE),
    IQR = IQR(Quantity, na.rm = TRUE),
    Min = min(Quantity, na.rm = TRUE),
    Max = max(Quantity, na.rm = TRUE),
    Q1 = quantile(Quantity, 0.25, na.rm = TRUE),
    Q3 = quantile(Quantity, 0.75, na.rm = TRUE),
    )

summary_stats
# Assume you have a dataframe called summary_stats
write.csv(summary_stats, "summary_statistics.csv", row.names = FALSE)


# Create plot1 a box-and-whisker plot for Quantity across different countries
boxplot_for_quantity_sold_vs_country <- ggplot(cleaned_data, aes(x = Country, y = Quantity, fill = Country)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#4682B4", "#FF7F50", "#3CB371", "#DAA520", "#6A5ACD")) +
  labs(title = "Boxplot of Quantity Sold across Countries", x = "Country", y = "Quantity Sold") +
  scale_y_continuous(breaks=seq(0, 100, by = 10), labels = scales::comma)+
  
  theme_classic()+
  theme(
    #panel.background = element_rect(fill = "lightyellow", color = NA),  # Set the plot area background
    plot.background = element_rect(fill = "lightblue", color = "black"),  # Set the overall plot background
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(color = "black"),  # Set y-axis text to black
  )+
  stat_summary(fun = median, geom = "text", aes(label = paste0("Median: ", round(..y.., 2))), vjust = 1.5, color = "#FFFFFF", size=2) +
  stat_summary(fun = function(x) quantile(x, 0.25), geom = "text", aes(label = paste0("Q1: ", round(..y.., 2))), vjust = 1.5, hjust = 1.5, size=2) +
  stat_summary(fun = function(x) quantile(x, 0.75), geom = "text", aes(label = paste0("Q3: ", round(..y.., 2))), vjust = -1.5, hjust = 1.5, size=2)

ggsave("boxplot_for_quantity_sold_vs_country.png", plot = boxplot_for_quantity_sold_vs_country, width = 8, height = 6)






