data <- data_filtered[which(data_filtered$visit==2),]
#data <- data_filtered
# Create an empty dataframe
cor_df <- data.frame(
  Variable_X = character(),
  Variable_Y = character(),
  Correlation = numeric(),
  P_Value = numeric(),
  CI_left = numeric(),
  CI_right = numeric(),
  stringsAsFactors = FALSE
)

# Specify columns for correlation
selected_columns_x <- c("CPZE", "ZVM", "ZDS", "ZMT", "ZVF", "ZSC", "ZToL", "Comp Z")
selected_columns_y <- c("CPZE",  'Total score SAS', 'Total score PANSS', "akathisia", "wrist rigidity", "head rotation", "elbow rigidity", "gait", "THF dose")
#
selected_columns_x <- c("CPZE", 'Total score SAS',  "akathisia", "wrist rigidity", "head rotation", "elbow rigidity", "gait")
selected_columns_y <- c("CPZE",'Total score PANSS', "THF dose", "Positive scale", "Negative scale", "negative", "excitment", "cognitive", "positive", "depression", "age")

# Loop through each combination of variables
for (var_x in selected_columns_x) {
  for (var_y in selected_columns_y) {
    cat(var_x, var_y)
    # Calculate Spearman correlation coefficients and p-values
    cor_result <-corci(data[[var_x]], data[[var_y]], method = "spearman")
    cor_result
    # Check if p-value is less than 0.05
    if (!is.na(cor_result$p.value) & cor_result$p.value < 0.05) {
      # Add the results to the dataframe
      cor_df <- rbind(cor_df, data.frame(
        Variable_X = var_x,
        Variable_Y = var_y,
        Correlation = cor_result$estimate,
        P_Value = cor_result$p.value,
        CI_left = cor_result$conf.int[1],
        CI_right = cor_result$conf.int[2]
      ))
    }
  }
}
library(plotly)
library(bootcorci)
# Print the dataframe with significant results

ggplot(cor_df, aes(x = Variable_Y, y = Variable_X, fill = Correlation, label = round(Correlation, 2))) +
  geom_tile() +
   geom_text(
    aes(label = paste(round(Correlation, 2), "\n", "[", round(CI_left, 2), ", ", round(CI_right, 2), "]")),
    size = 3,
    color = "black"
  ) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = NA) +
  labs(
    title = "Spearman Correlation Heatmap (p-value < 0.05)",
    x = "",
    y = ""
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#install.packages("devtools")
#devtools::install_github("GRousselet/bootcorci")
