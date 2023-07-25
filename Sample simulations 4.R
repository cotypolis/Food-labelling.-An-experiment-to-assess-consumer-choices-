#Sample simulations
#Constanza Avalos
#21/07/23

#Excercise 2: 
#generates samples with different sample sizes: 50, 100, 300 and 500
#In each sample, 50% of the participants are randomly assigned to either treatment A or B.
#In each sample, it simulates randomized choices. 
#Each participant must answer 15 cereal choice tasks.
#In the tasks, subjects choose from a list of 9 kinds of cereal. 
#The value is the cereals are: 1 = 474, 2=453, 3=424, 4=398, 5=392, 6=380, 7=374, 8=360, 9=0
#Plot the density means and standard deviation of the sample sizes by treatment groups.


# Set seed for reproducibility
set.seed(123)

# Define cereal values
cereal_values <- c(474, 453, 424, 398, 392, 380, 374, 360, 0)

# Generate samples
sample_sizes <- c(50, 100, 300, 500, 800, 1000)
treatments <- c("A", "B") # A= Coarse and B= Detailed labels
samples <- list()

for (i in 1:length(sample_sizes)) {
  sample_size <- sample_sizes[i]
  sample <- data.frame(participant = 1:(2*sample_size),
                       treatment = rep(treatments, each = sample_size),
                       task1 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task2 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task3 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task4 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task5 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task6 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task7 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task8 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task9 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task10 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task11 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task12 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task13 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task14 = sample(cereal_values, 2*sample_size, replace = TRUE),
                       task15 = sample(cereal_values, 2*sample_size, replace = TRUE))
  samples[[i]] <- sample
}

# Calculate means and standard deviations by treatment group
means <- list()
sds <- list()
vars <- list()

for (i in 1:length(samples)) {
  sample <- samples[[i]]
  means[[i]] <- aggregate(sample[, 3:17], by = list(sample$treatment), mean)
  sds[[i]] <- aggregate(sample[, 3:17], by = list(sample$treatment), sd)
  vars[[i]] <- aggregate(sample[, 3:17], by = list(sample$treatment), var)
}

# Plot density means and standard deviation of the sample sizes by treatment groups
par(mfrow = c(2, 2))
for (i in 1:length(samples)) {
  sample_size <- sample_sizes[i]
  mean_A <- means[[i]][1, 2:16]
  mean_B <- means[[i]][2, 2:16]
  sd_A <- sds[[i]][1, 2:16]
  sd_B <- sds[[i]][2, 2:16]
  var_A <- vars[[i]][1, 2:16]
  var_B <- vars[[i]][2, 2:16]
  mean_A_num <- as.numeric(mean_A)
  mean_B_num <- as.numeric(mean_B)
  sd_A_num <- as.numeric(sd_A)
  sd_B_num <- as.numeric(sd_B)
  var_A_num <- as.numeric(var_A)
  var_B_num <- as.numeric(var_B)
  plot(density(mean_A_num), main = paste0("Sample Size = ", sample_size, ", Treatment groups"), xlab = "Mean", ylab = "Density")
  lines(density(mean_B_num), col = "red")
  legend("topright", legend = c("A", "B"), col = c("black", "red"), lty = 1)
  plot(density(sd_A_num), main = paste0("Sample Size = ", sample_size, ", Treatment groups"), xlab = "Standard deviation", ylab = "Density")
  lines(density(sd_B_num), col = "red")
  legend("topright", legend = c("A", "B"), col = c("black", "red"), lty = 1)
}

# Summary means and sd by sample sizes

print(means[[1]])
print(means[[2]])
print(means[[3]])
print(means[[4]])
print(means[[5]])
print(means[[6]])

print(sds[[1]])
print(sds[[2]])
print(sds[[3]])
print(sds[[4]])
print(sds[[5]])
print(sds[[6]])
