install.packages(c("e1071", "nortest", "MVN", "corrplot", "MASS"))

library(e1071)
library(nortest)
library(MVN)
library(corrplot)
library(MASS)

#Q1. Import, identify missing codes and check data types (3 marks)
# Number of records and variables
dim(groundwater)

# Identify missing-value codes
sapply(groundwater, function(x) unique(as.character(x)))

# Recode missing values
groundwater[] <- lapply(groundwater, function(x) {
  x <- as.character(x)
  x[x %in% c("-", "NA", "N/A", "NULL", "")] <- NA
  x
})

# Convert seven response variables to numeric
vars <- c("pH", "TDS", "Ca", "Mg", "Cl", "NO3", "F")

groundwater[vars] <- lapply(groundwater[vars], as.numeric)

# Check data types
str(groundwater)


#Q2. Check and remove duplicate records (2 marks)
# Display duplicate records
groundwater[duplicated(groundwater), ]

# Remove duplicates
groundwater <- groundwater[!duplicated(groundwater), ]

# Number of records after removal
nrow(groundwater)


#Q3. Identify physically impossible values (2 marks)
# Define physically valid ranges
limits <- list(
  pH = c(0, 14),
  TDS = c(0, Inf),
  Ca = c(0, Inf),
  Mg = c(0, Inf),
  Cl = c(0, Inf),
  NO3 = c(0, Inf),
  F = c(0, Inf)
)

# Find and report impossible values
for (v in vars) {
  bad <- which(groundwater[[v]] < limits[[v]][1] |
                 groundwater[[v]] > limits[[v]][2])
  
  if (length(bad) > 0) {
    print(data.frame(
      Sample_ID = groundwater$Sample_ID[bad],
      Variable = v,
      Value = groundwater[[v]][bad]
    ))
    
    groundwater[[v]][bad] <- NA
  }
}

#Q4. Missing-value analysis (2 marks)
# Missing values by variable
colSums(is.na(groundwater))

# Missing values by aquifer
aggregate(
  lapply(groundwater[vars], function(x) as.numeric(is.na(x))),
  by = list(Aquifer = groundwater$Aquifer),
  FUN = sum
)

# Overall percentage of missing cells
mean(is.na(groundwater)) * 100

# Number of complete records
sum(complete.cases(groundwater))

# Records with more than one missing value
groundwater[rowSums(is.na(groundwater)) > 1, ]

# Missing data pattern
mice::md.pattern(groundwater)


#Q5. Median imputation within each aquifer (3 marks)
# Calculate medians for each aquifer
medians <- aggregate(
  groundwater[vars],
  by = list(Aquifer = groundwater$Aquifer),
  FUN = median,
  na.rm = TRUE
)

# Display median table
print(medians)

# Impute missing values using aquifer-specific medians
for (v in vars) {
  for (a in unique(groundwater$Aquifer)) {
    i <- which(groundwater$Aquifer == a & is.na(groundwater[[v]]))
    
    groundwater[[v]][i] <- medians[medians$Aquifer == a, v]
  }
}

# Save cleaned dataset
write.csv(groundwater, "groundwater_clean.csv", row.names = FALSE)

# Check remaining missing values
colSums(is.na(groundwater))


#Q6. Descriptive statistics for each aquifer (2 marks)

result <- do.call(rbind, lapply(split(groundwater, groundwater$Aquifer),
                                function(d) {
                                  data.frame(
                                    Variable = vars,
                                    Mean = sapply(d[vars], mean),
                                    Median = sapply(d[vars], median),
                                    SD = sapply(d[vars], sd),
                                    Minimum = sapply(d[vars], min),
                                    Maximum = sapply(d[vars], max),
                                    CV = sapply(d[vars], sd) / sapply(d[vars], mean) * 100,
                                    Skewness = sapply(d[vars], e1071::skewness),
                                    Kurtosis = sapply(d[vars], e1071::kurtosis)
                                  )
                                }
))

print(result)


#Q7. Side-by-side boxplots and outlier identification (2 marks)

par(mfrow = c(2, 4))

for (v in vars) {
  boxplot(groundwater[[v]] ~ groundwater$Aquifer,
          main = v, xlab = "Aquifer",
          ylab = v, col = c("skyblue", "orange"))
}

par(mfrow = c(1, 1))

# Identify outlying Sample_IDs using 1.5 × IQR
for (v in vars) {
  for (a in unique(groundwater$Aquifer)) {
    
    d <- groundwater[groundwater$Aquifer == a, ]
    out <- boxplot.stats(d[[v]])$out
    
    cat("\nVariable:", v, "| Aquifer:", a, "\n")
    print(d$Sample_ID[d[[v]] %in% out])
  }
}


#Q8. Correlation matrix and heatmap (2 marks)

# Correlation matrix
R <- cor(groundwater[vars])

print(round(R, 3))

# Install package (only first time)
install.packages("corrplot")

# Load package
library(corrplot)

# Correlation heatmap
corrplot(R, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black")


#Q9. Shapiro-Wilk and Kolmogorov-Smirnov normality tests (4 marks)

normality <- do.call(rbind, lapply(unique(groundwater$Aquifer),
                                   function(a) {
                                     
                                     d <- groundwater[groundwater$Aquifer == a, vars]
                                     
                                     do.call(rbind, lapply(vars, function(v) {
                                       
                                       x <- d[[v]]
                                       
                                       data.frame(
                                         Aquifer = a,
                                         Variable = v,
                                         Shapiro_p = shapiro.test(x)$p.value,
                                         KS_p = nortest::lillie.test(x)$p.value
                                       )
                                     }))
                                   }
))

print(normality)

# Identify variables failing normality at 5% significance
subset(normality, Shapiro_p < 0.05 | KS_p < 0.05)


#QQ PLOTS
par(mfrow = c(2, 4))

for (a in unique(groundwater$Aquifer)) {
  for (v in vars) {
    x <- groundwater[groundwater$Aquifer == a, v][[1]]
    
    qqnorm(x, main = paste(a, v))
    qqline(x, col = "red")
  }
}

par(mfrow = c(1, 1))

#Q10. Multivariate normality using Mardia's test (2 marks)
# Load package
library(MVN)

# Alluvial
mvn(data = groundwater[groundwater$Aquifer == "Alluvial", vars],
    mvn_test = "mardia")

# Hard-rock
mvn(data = groundwater[groundwater$Aquifer == "Hard-rock", vars],
    mvn_test = "mardia")


#Q11. Hotelling's T² test for Hard-rock aquifer (4 marks)

# Select Hard-rock samples and four variables
d <- groundwater[groundwater$Aquifer == "Hard-rock",
                 c("Ca", "Mg", "Cl", "F")]

n <- nrow(d)
p <- ncol(d)

# Hypothesized mean
mu0 <- c(55, 24, 50, 1)

# Sample mean and covariance
xbar <- colMeans(d)
S <- cov(d)

# Hotelling's T-squared
T2 <- as.numeric(n * t(xbar - mu0) %*%
                   solve(S) %*% (xbar - mu0))

# Equivalent F statistic
Fstat <- ((n - p) / (p * (n - 1))) * T2

# Critical value and p-value
Fcrit <- qf(0.95, p, n - p)
pvalue <- pf(Fstat, p, n - p, lower.tail = FALSE)

# Results
cat("T-squared =", T2,
    "\nF statistic =", Fstat,
    "\nDegrees of freedom =", p, ",", n-p,
    "\nCritical value =", Fcrit,
    "\nP-value =", pvalue)

#Q12. PCA and eigenvalues (3 marks)

# PCA using correlation matrix
pca <- prcomp(groundwater[vars], scale. = TRUE)

# Eigenvalues
eigenvalues <- pca$sdev^2

# Proportion of variance
proportion <- eigenvalues / sum(eigenvalues)

# Cumulative proportion
cumulative <- cumsum(proportion)

# Results table
pca_table <- data.frame(
  Component = paste0("PC", 1:7),
  Eigenvalue = eigenvalues,
  Proportion = proportion,
  Cumulative = cumulative
)

print(pca_table)


#Q13. Scree plot and component selection (3 marks)

plot(eigenvalues, type = "b",
     main = "Scree Plot",
     xlab = "Principal Components",
     ylab = "Eigenvalues",
     pch = 19)

abline(h = 1, col = "red", lty = 2)

#Q14. LDA classification and confusion matrix (3 marks)

# Load package
library(MASS)

# Fit LDA model
lda_model <- MASS::lda(
  Aquifer ~ .,
  data = groundwater[, c("Aquifer", vars)],
  prior = c(0.5, 0.5)
)

# Classify training samples
pred <- predict(lda_model)$class

# Confusion matrix
confusion <- table(
  Actual = groundwater$Aquifer,
  Predicted = pred
)

print(confusion)

# Apparent error rate
APER <- 1 - sum(diag(confusion)) / sum(confusion)

cat("APER =", APER)


#Q15. Classify six newly drilled wells (3 marks)

# New observations
new_wells <- data.frame(
  pH = c(7.33, 7.13, 7.65, 7.34, 7.73, 7.32),
  TDS = c(524, 469, 403, 612, 477, 702),
  Ca = c(70.7, 55.6, 44.2, 66.9, 55.5, 82.6),
  Mg = c(27.7, 22.3, 26.2, 34.3, 20.5, 43.1),
  Cl = c(60.0, 60.8, 47.6, 49.1, 60.1, 77.8),
  NO3 = c(8.8, 12.4, 8.4, 11.9, 13.8, 19.9),
  F = c(0.89, 1.66, 1.44, 0.82, 0.82, 0.40)
)

rownames(new_wells) <- paste0("N", 1:6)

# Predict aquifer type
new_prediction <- predict(lda_model, newdata = new_wells)

# Display classification
data.frame(
  Well = rownames(new_wells),
  Predicted_Aquifer = new_prediction$class
)
