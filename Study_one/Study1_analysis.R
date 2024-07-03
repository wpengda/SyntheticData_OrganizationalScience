# Studty one GATB synthetic data

#### Packages
library(ggplot2)
library(moments)
library(digest) # For hash functions
library(tidyverse)
library(boot)

#### Read dataset
original_data <- read.csv("data/original_data.csv")
selected_columns <- c("SEX","ETHGP","G", "V", "N", "S", "P", "Q", "K", "F", "M", "CRFIN")
original_data <- original_data[selected_columns]
synthetic_data <- read.csv("data/synthetic_data.csv")

#### Original data
sex_counts <- table(original_data$SEX)
sex_proportions <- prop.table(sex_counts)

print("SEX Group Proportions:")
print(sex_proportions)

ethgp_counts <- table(original_data$ETHGP)
ethgp_proportions <- prop.table(ethgp_counts)

print("ETHGP Group Proportions:")
print(ethgp_proportions)





#### Overlapped sample test
#  Convert rows of data to hash values
row_to_hash <- function(row) {
  digest(paste0(row, collapse = ""), algo = "md5")
}

# converts each row of the dataset to a hash value and returns the hash set
hash_dataset <- function(dataset) {
  sapply(1:nrow(dataset), function(i) row_to_hash(dataset[i, ]))
}

# Hash the two datasets
hashed_original <- hash_dataset(original_data)
hashed_synthetic <- hash_dataset(synthetic_data)

# Compute the intersection
overlapping_hashes <- intersect(hashed_original, hashed_synthetic)

# Calculate the percentage of overlap
overlap_ratio <- length(overlapping_hashes) / length(hashed_original)

# print
print(paste("Overlap Ratio:", overlap_ratio))






#### Descriptive table
# create an empty data table
summary_table <- data.frame(Variable=character(),
                            Min=numeric(), 
                            Max=numeric(),
                            Median=numeric(),
                            Mean=numeric(),
                            SD=numeric(),
                            Cohens_d=numeric(),  
                            stringsAsFactors=FALSE)

# all variables
variables <- c("G", "V", "N", "S", "P", "Q", "K", "F", "M", "CRFIN")

# loop
for(v in variables) {
  original_summary <- summary(original_data[[v]])
  synthetic_summary <- summary(synthetic_data[[v]])
  mean_original <- mean(original_data[[v]], na.rm = TRUE)
  mean_synthetic <- mean(synthetic_data[[v]], na.rm = TRUE)
  sd_original <- sd(original_data[[v]], na.rm = TRUE)
  sd_synthetic <- sd(synthetic_data[[v]], na.rm = TRUE)
  n_original <- sum(!is.na(original_data[[v]]))
  n_synthetic <- sum(!is.na(synthetic_data[[v]]))
  pooled_sd <- sqrt(((n_original - 1) * sd_original^2 + (n_synthetic - 1) * sd_synthetic^2) / (n_original + n_synthetic - 2))
  cohens_d <- (mean_original - mean_synthetic) / pooled_sd
  
  summary_table <- rbind(summary_table, 
                         data.frame(Variable=paste(v, "original", sep="_"),
                                    Min=original_summary["Min."][[1]], 
                                    Max=original_summary["Max."][[1]],
                                    Median=median(original_data[[v]], na.rm = TRUE),
                                    Mean=mean_original,
                                    SD=sd_original,
                                    Cohens_d=NA),  # Placeholder for Cohens_f in original data
                         data.frame(Variable=paste(v, "synthetic", sep="_"),
                                    Min=synthetic_summary["Min."][[1]], 
                                    Max=synthetic_summary["Max."][[1]],
                                    Median=median(synthetic_data[[v]], na.rm = TRUE),
                                    Mean=mean_synthetic,
                                    SD=sd_synthetic,
                                    Cohens_d=cohens_d))
}

# check table
print(summary_table)







#### Correlation
variables <- c("G", "V", "N", "S", "P", "Q", "K", "F", "M", "CRFIN")

original_data_selected <- original_data[, variables]
synthetic_data_selected <- synthetic_data[, variables]

cor_matrix_original <- cor(original_data_selected, use="complete.obs", method="pearson")
cor_matrix_synthetic <- cor(synthetic_data_selected, use="complete.obs", method="pearson")

cor_difference <- cor_matrix_original - cor_matrix_synthetic

cor_matrix_original
cor_matrix_synthetic

# mean difference cor
mean_difference <- mean(abs(cor_difference), na.rm = TRUE)

# print
print(mean_difference)



cor_difference_vector <- as.vector(abs(cor_difference))
cor_difference_vector_no_diag <- cor_difference_vector[cor_difference_vector != 0]
hist(cor_difference_vector_no_diag,
     main="Absolute Correlation Difference Histogram",
     xlab="Absolute Correlation Difference",
     ylab="Frequency",
     col="blue",
     border="black")





#### Regression
original_data$type <- 'Original'
synthetic_data$type <- 'Synthetic'
combined_data <- rbind(original_data, synthetic_data)

ggplot(combined_data, aes(x=V, y=CRFIN, color=type)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", se = FALSE, aes(group=type)) +
  theme_minimal() +
  labs(title = "Scatter Plot of CRFIN vs V with Fit Lines", x = "V", y = "CRFIN") +
  scale_color_manual(values=c("Original"="blue", "Synthetic"="red"))

## Regression result
# Higher order distributions of 3 or more columns are not included.
# Very high order similarity may have an adverse effect on the synthetic data. 

variables <- c("G", "V", "N", "S", "P", "Q", "K", "F", "M")

results_df <- data.frame(
  Variable = character(),
  Dataset = character(),
  Beta = numeric(),
  P_Value = numeric(),
  R_Squared = numeric(),
  stringsAsFactors = FALSE
)

for (var in variables) {
  model_original <- lm(paste0("CRFIN ~ ", var), data=original_data)
  summary_model_original <- summary(model_original)
  
  model_synthetic <- lm(paste0("CRFIN ~ ", var), data=synthetic_data)
  summary_model_synthetic <- summary(model_synthetic)
  
  results_df <- rbind(results_df, data.frame(
    Variable = var,
    Dataset = "Original",
    Beta = summary_model_original$coefficients[2, 1],
    P_Value = summary_model_original$coefficients[2, 4],
    R_Squared = summary_model_original$r.squared
  ))
  
  results_df <- rbind(results_df, data.frame(
    Variable = var,
    Dataset = "Synthetic",
    Beta = summary_model_synthetic$coefficients[2, 1],
    P_Value = summary_model_synthetic$coefficients[2, 4],
    R_Squared = summary_model_synthetic$r.squared
  ))
}

delta_results <- data.frame(
  Variable = rep(variables, each = 1),
  Delta_Beta = numeric(length(variables)),
  Delta_R_Squared = numeric(length(variables)),
  stringsAsFactors = FALSE
)

for (var in variables) {
  original_beta <- results_df$Beta[results_df$Variable == var & results_df$Dataset == "Original"]
  synthetic_beta <- results_df$Beta[results_df$Variable == var & results_df$Dataset == "Synthetic"]
  original_r_squared <- results_df$R_Squared[results_df$Variable == var & results_df$Dataset == "Original"]
  synthetic_r_squared <- results_df$R_Squared[results_df$Variable == var & results_df$Dataset == "Synthetic"]
  
  delta_results$Delta_Beta[delta_results$Variable == var] <- synthetic_beta - original_beta
  delta_results$Delta_R_Squared[delta_results$Variable == var] <- synthetic_r_squared - original_r_squared
}

results_df <- merge(results_df, delta_results, by = "Variable")

results_df$P_Value <- ifelse(results_df$P_Value < 0.001, "< .001",
                             ifelse(results_df$P_Value < 0.01, "< .01",
                                    ifelse(results_df$P_Value < 0.05, "< .05", ">= .05")))

print(results_df)








#### kurtosis and skewness
summary_table <- data.frame(Variable=character(),
                            Kurtosis=numeric(),
                            Skewness=numeric(),
                            stringsAsFactors=FALSE)
variables <- c("G", "V", "N", "S", "P", "Q", "K", "F", "M", "CRFIN")

for(v in variables) {
  kurtosis_original <- kurtosis(original_data[[v]], na.rm = TRUE)
  kurtosis_synthetic <- kurtosis(synthetic_data[[v]], na.rm = TRUE)
  skewness_original <- skewness(original_data[[v]], na.rm = TRUE)
  skewness_synthetic <- skewness(synthetic_data[[v]], na.rm = TRUE)
  
  summary_table <- rbind(summary_table, 
                         data.frame(Variable=paste(v, "original", sep="_"),
                                    Kurtosis=kurtosis_original,
                                    Skewness=skewness_original),
                         data.frame(Variable=paste(v, "synthetic", sep="_"),
                                    Kurtosis=kurtosis_synthetic,
                                    Skewness=skewness_synthetic))
}

print(summary_table)









#### SEX
# define function
calculate_stats_general <- function(data, filter_column, filter_values, data_label) {
  # variable list 
  variables <- c("G", "V", "N", "S", "P", "Q", "K", "F", "M", "CRFIN")
  
  # lappy~
  stats_list <- lapply(filter_values, function(value) {
    # filter for specific data
    filtered_data <- data[data[[filter_column]] == value, ]
    
    # initialize the table
    summary_table <- data.frame()
    
    # for loop for each variable
    for(v in variables) {
      # extract data
      variable_data <- filtered_data[[v]]
      
      # min, max, median, mean...
      summary_table <- rbind(summary_table, 
                             data.frame(DataType=data_label,
                                        Group=value,
                                        Variable=v,
                                        Min=min(variable_data, na.rm = TRUE), 
                                        Max=max(variable_data, na.rm = TRUE),
                                        Median=median(variable_data, na.rm = TRUE),
                                        Mean=mean(variable_data, na.rm = TRUE),
                                        SD=sd(variable_data, na.rm = TRUE)))
    }
    
    return(summary_table)
  })
  
  # combine all result
  do.call(rbind, stats_list)
}

## SEX
# Use functions for original and synthetic data
stats_original_sex <- calculate_stats_general(original_data, "SEX", c(1, 2), "Original")
stats_synthetic_sex <- calculate_stats_general(synthetic_data, "SEX", c(1, 2), "Synthetic")

# combine
combined_stats_sex <- rbind(stats_original_sex, stats_synthetic_sex)

# print
print(combined_stats_sex)

# calculate cohens d
cohens_d <- function(mean1, mean2, sd1, sd2) {
  pooled_sd <- sqrt((1055*sd1^2 + 1055*sd2^2) / (1056+1056+2))
  d <- (mean1 - mean2) / pooled_sd
  return(d)
}

calculate_cohens_d_within_type <- function(data) {
  results <- data.frame(DataType = character(), 
                        Variable = character(), 
                        Cohens_d = numeric(), 
                        stringsAsFactors = FALSE)
  
  for (data_type in unique(data$DataType)) {
    for (variable in unique(data$Variable)) {
      sub_data <- data[data$DataType == data_type & data$Variable == variable,]
      if(nrow(sub_data) == 2) {
        d_value <- cohens_d(sub_data$Mean[1], sub_data$Mean[2], 
                            sub_data$SD[1], sub_data$SD[2])
        results <- rbind(results, data.frame(DataType = data_type, 
                                             Variable = variable, 
                                             Cohens_d = d_value))
      }
    }
  }
  
  return(results)
}

cohens_d_results_within_type <- calculate_cohens_d_within_type(combined_stats_sex)
print(cohens_d_results_within_type)


################################################################################################


## ETHGP
stats_original_race <- calculate_stats_general(original_data, "ETHGP", c(1, 2), "Original")
stats_synthetic_race <- calculate_stats_general(synthetic_data, "ETHGP", c(1, 2), "Synthetic")

# combine
combined_stats_race <- rbind(stats_original_race, stats_synthetic_race)

# print
print(combined_stats_race)

# calculate cohens d
cohens_d_results_within_type <- calculate_cohens_d_within_type(combined_stats_race)
print(cohens_d_results_within_type)

























