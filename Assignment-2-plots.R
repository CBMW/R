# Visualisation of age difference between islands
attach(combinedIslandData)  # Attach the dataset to access variables directly

# Create a boxplot comparing Age distributions between the islands
boxplot(Age ~ Island,
        main = "Age Distribution Between Providence and Ironbard",
        xlab = "Island",
        ylab = "Age",
        col = c("skyblue", "orange"),
        border = "black",
        notch = TRUE,
        las = 1)
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")

# Add mean points
means <- tapply(Age, Island, mean)  # Calculate means for each island
points(1:length(means), means, col = "red", pch = 18, cex = 1.5)  # Add red diamond points


# Add grid lines for better visibility
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Visualisation of income comparison between islands
attach(combinedIslandData)  # Attach the dataset to access variables directly

# Create a boxplot comparing Income distributions between the islands
boxplot(Income ~ Island,
        data = combinedIslandData,
        main = "Income Distribution Between Providence and Ironbard",
        xlab = "Island",
        ylab = "Income($)",
        col = c("skyblue", "orange"),
        border = "black",
        notch = TRUE,
        las = 1)

# Add mean points
means <- tapply(combinedIslandData$Income, combinedIslandData$Island, mean, na.rm = TRUE)  # Calculate means for each island, ignoring NA values
points(1:length(means), means, col = "red", pch = 18, cex = 1.5)  # Add red diamond points

# Add grid lines for better visibility
grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")




detach(combinedIslandData)  # Detach the dataset after use
detach()
