student_work <- c(0, 0, 0, 0, 0, 0.25, 0.75, 0.75, 1, 1.25, 6)

# Number of boostrap replications
B <- 500

# Compute the length of vector
n <- length(student_work)

# Confidence level
alpha <- 0.05

# Initialisation of 
boot_mean <- rep(NA, B)

# Step 1
for (i in 1:B){
  # Step 2
  student_work_star <- student_work[sample(1:n, replace = TRUE)]
  
  # Step 3
  boot_mean[i] <- mean(student_work_star)
}

# Step 4
quantile(boot_mean, c(alpha/2, 1 - alpha/2))
