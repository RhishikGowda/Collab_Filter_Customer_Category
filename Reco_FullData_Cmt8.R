# Temporal Holdout Split Based Model for Full Data
library(tidyr)
library(dplyr)
library(ggplot2)
library(proxy)

# Relevant features for the level-8 model (Highest Granularity)
fulldata_lvl8 <- merged_data %>%
  select(acct_nbr, invc_dt, ship_qty, cmt_nm_lvl_8_nm)

#############################################################################################################################

# Function for temporal split within each customer's data (train/test, 80:20)
temporal_split_per_customer <- function(df) {
  df %>%
    arrange(acct_nbr, invc_dt) %>%
    group_by(acct_nbr) %>%
    mutate(row_id = row_number(), 
           is_train = row_id <= 0.8 * n()) %>%
    ungroup() %>%
    split(.$is_train)
}

split_data_lvl8 <- temporal_split_per_customer(fulldata_lvl8)
train_data_lvl8 <- split_data_lvl8[["TRUE"]]
test_data_lvl8 <- split_data_lvl8[["FALSE"]]
# splitting data while maintaining temporal structure while also ensuring that customers present in test are also in train
# considering the initial 80% of customer purchases for train and later 20% purchases for test
# this may still not preserve seasonality so a more sensitive rolling window might be required
# single transactions are always placed in test (0.8 * 1 Transaction = rounded down to 0, rather than 1)

# Counting Unique customers in each split
cat("Unique customers in fulldata_lvl8:", n_distinct(fulldata_lvl8$acct_nbr), "\n") #8665
cat("Unique customers in train_data_lvl8:", n_distinct(train_data_lvl8$acct_nbr), "\n") #7884
cat("Unique customers in test_data_lvl8:", n_distinct(test_data_lvl8$acct_nbr), "\n") #8665

#############################################################################################################################

# Single Transaction Customers to be removed from test data (skews evaluation metrics)
customer_transaction_counts_lvl8 <- fulldata_lvl8 %>%
  group_by(acct_nbr) %>%
  summarise(transaction_count = n())  # finding transaction count per customer

customers_with_multiple_transactions_lvl8 <- customer_transaction_counts_lvl8 %>%
  filter(transaction_count > 1) %>%
  pull(acct_nbr)  # filtering for customer with >1 transaction

test_data_lvl8 <- test_data_lvl8 %>%
  filter(acct_nbr %in% customers_with_multiple_transactions_lvl8)  # overwriting test data with only multi-transaction customers

cat("Unique customers in updated test_data_lvl8:", n_distinct(test_data_lvl8$acct_nbr), "\n") #7884 unique customers

#############################################################################################################################

# Log transformation and normalization for `ship_qty`
train_data_lvl8 <- train_data_lvl8 %>%
  mutate(ship_qty_log = log1p(ship_qty),
         norm_ship_qty = (ship_qty_log - min(ship_qty_log)) / 
           (max(ship_qty_log) - min(ship_qty_log)))  # normalizing to [0, 1]

# Creating the customer-item matrix at lvl 8
customer_item_matrix_lvl8 <- train_data_lvl8 %>%
  group_by(acct_nbr, cmt_nm_lvl_8_nm) %>%
  summarise(total_qty = sum(norm_ship_qty, na.rm = TRUE)) %>%  # aggregating normalized ship_qty
  pivot_wider(names_from = cmt_nm_lvl_8_nm, values_from = total_qty, values_fill = 0)  # pivot to wide format

# Converting to matrix format for similarity calculations
customer_item_matrix_lvl8 <- as.data.frame(customer_item_matrix_lvl8) #checking for dataframe format
rownames(customer_item_matrix_lvl8) <- customer_item_matrix_lvl8$acct_nbr  #setting rownames to customer IDs
customer_item_matrix_lvl8 <- customer_item_matrix_lvl8[, -1]  #removing customer ID column
customer_item_matrix_lvl8 <- as.matrix(customer_item_matrix_lvl8)  #converting to matrix format

#Cosine Similarity Calculations (7884 x 7884 Matrix = 62M Elements)
similarity_matrix_lvl8 <- as.matrix(simil(customer_item_matrix_lvl8, method = "cosine"))
diag(similarity_matrix_lvl8) <- 1  # setting diagonal values to 1

#############################################################################################################################

# Recommend 'top_n' new items (categories not yet purchased) 
# (set k = number of similar customers to be taken into consideration)

recommend_new_items_lvl8 <- function(customer_id, similarity_matrix, customer_item_matrix, top_n = 10, k = 500) {
  similar_customers <- similarity_matrix[customer_id, ]
  similar_customers <- sort(similar_customers, decreasing = TRUE) #similarity scores for customer
  similar_customers <- similar_customers[names(similar_customers) != customer_id] #excluding target cust from similar custs
  top_similar_customers <- names(head(similar_customers, k)) #selecting top k similar customers
  recommendations <- colSums(customer_item_matrix[top_similar_customers, , drop = FALSE]) # sum of item values for the top k similar customers
  
  customer_items <- customer_item_matrix[customer_id, ]
  recommendations <- recommendations[customer_items == 0] #excluding items the target customer has already purchased
  
  recommended_items <- sort(recommendations, decreasing = TRUE)
  return(names(head(recommended_items, top_n))) # sorting recommendations by score
}

# Recommend growth items (categories where similar customers bought more) 
recommend_growth_lvl8 <- function(customer_id, similarity_matrix, customer_item_matrix, top_n = 10, k = 500) {
  similar_customers <- similarity_matrix[customer_id, ]
  similar_customers <- sort(similar_customers, decreasing = TRUE) #similarity scores for customer
  similar_customers <- similar_customers[names(similar_customers) != customer_id] #excluding target customer
  top_similar_customers <- names(head(similar_customers, k)) #top k customers
  recommendations <- colSums(customer_item_matrix[top_similar_customers, , drop = FALSE]) #sum of item values
  
  customer_items <- customer_item_matrix[customer_id, ]
  recommendations <- recommendations - customer_items #finding growth potential by subtracting customer's current item values
  
  recommendations <- recommendations[recommendations > 0] #retaining only positive growth items
  
  recommended_items <- sort(recommendations, decreasing = TRUE)
  top_recommendations <- head(recommended_items, top_n) #sorting recommendations by growth potential
  
  return(data.frame(Category = names(top_recommendations), Score = top_recommendations)) #returning recommendations as a data frame
}

#############################################################################################################################

# Generating recommendations for all test customers
all_recommendations_lvl8 <- lapply(rownames(customer_item_matrix_lvl8), function(customer_id) {
  
  new_items <- recommend_new_items_lvl8(customer_id, similarity_matrix_lvl8, customer_item_matrix_lvl8, top_n = 10)
  if (length(new_items) == 0) new_items <- NA # getting a list of top 10 new item recommendations
  
  
  growth_result <- recommend_growth_lvl8(customer_id, similarity_matrix_lvl8, customer_item_matrix_lvl8, top_n = 10)
  if (nrow(growth_result) == 0) {
    growth_categories <- NA
    growth_scores <- NA
  } else {
    growth_categories <- growth_result$Category
    growth_scores <- growth_result$Score
  } #getting a list of top 10 growth item recommendations
  
  # Create separate data frames for each recommendation type
  new_items_df <- data.frame(
    Customer_ID = customer_id,
    New_Item_Recommendation = new_items,
    Growth_Item_Recommendation = NA,
    Growth_Score = NA
  )
  
  growth_items_df <- data.frame(
    Customer_ID = customer_id,
    New_Item_Recommendation = NA,
    Growth_Item_Recommendation = growth_categories,
    Growth_Score = growth_scores
  )
  
  # Combine the new and growth recommendations without expanding
  combined_df <- bind_rows(new_items_df, growth_items_df)
  
  return(combined_df)
})

# Combine results into a single data frame
all_recommendations_df_lvl8 <- do.call(rbind, all_recommendations_lvl8)

# Preview the recommendations
print(head(all_recommendations_df_lvl8))

#############################################################################################################################
# Evaluation Metrics: Mean Average Precision (MAP) and Mean Reciprocal Rank (MRR)
evaluate_model_with_map_mrr_lvl8 <- function(test_data, recommendations) {
  # Initialize variables to store results
  avg_precisions <- c()
  reciprocal_ranks <- c()
  
  # Loop through each customer in the test set
  unique_customers <- unique(test_data$acct_nbr)
  
  for (customer_id in unique_customers) {
    # Actual items for the customer in the test set
    actual_items <- test_data %>%
      filter(acct_nbr == customer_id) %>%
      pull(cmt_nm_lvl_8_nm)
    
    # Predicted items (new items only) for the customer from the recommendations
    predicted_items <- recommendations %>%
      filter(Customer_ID == customer_id) %>%
      pull(New_Item_Recommendation)
    predicted_items <- predicted_items[!is.na(predicted_items)]  # Remove NA values
    
    # Calculate Average Precision for this customer
    precision_at_k <- c()
    relevant_found <- 0
    
    for (k in seq_along(predicted_items)) {
      if (predicted_items[k] %in% actual_items) {
        relevant_found <- relevant_found + 1
        precision_at_k <- c(precision_at_k, relevant_found / k)  # Precision at this rank
      }
    }
    
    if (length(precision_at_k) > 0) {
      avg_precisions <- c(avg_precisions, mean(precision_at_k))  # Average precision for this customer
    } else {
      avg_precisions <- c(avg_precisions, 0)  # No relevant items in predictions
    }
    
    # Calculate Reciprocal Rank for this customer
    first_relevant_position <- which(predicted_items %in% actual_items)[1]
    if (!is.na(first_relevant_position)) {
      reciprocal_ranks <- c(reciprocal_ranks, 1 / first_relevant_position)
    } else {
      reciprocal_ranks <- c(reciprocal_ranks, 0)  # No relevant items found
    }
  }
  
  # Calculate MAP and MRR
  MAP <- mean(avg_precisions)
  MRR <- mean(reciprocal_ranks)
  
  # Return results
  list(MAP = MAP, MRR = MRR)
}

# Evaluate the model for fulldata_lvl8
results_lvl8 <- evaluate_model_with_map_mrr_lvl8(test_data_lvl8, all_recommendations_df_lvl8)

# Print results
print(results_lvl8)

#############################################################################################################################

# Function to calculate Precision, Recall, and Accuracy
evaluate_precision_recall_accuracy_lvl8 <- function(test_data, recommendations) {
  # Initialize variables to store results
  precisions <- c()
  recalls <- c()
  accuracies <- c()
  
  # Loop through each customer in the test set
  unique_customers <- unique(test_data$acct_nbr)
  
  for (customer_id in unique_customers) {
    # Actual items for the customer in the test set
    actual_items <- test_data %>%
      filter(acct_nbr == customer_id) %>%
      pull(cmt_nm_lvl_8_nm)
    
    # Predicted items (new items only) for the customer from the recommendations
    predicted_items <- recommendations %>%
      filter(Customer_ID == customer_id) %>%
      pull(New_Item_Recommendation)
    predicted_items <- predicted_items[!is.na(predicted_items)]  # Remove NA values
    
    # Calculate True Positives, False Positives, False Negatives
    true_positives <- sum(predicted_items %in% actual_items)
    false_positives <- sum(!predicted_items %in% actual_items)
    false_negatives <- sum(!actual_items %in% predicted_items)
    
    # Precision: True Positives / (True Positives + False Positives)
    precision <- ifelse((true_positives + false_positives) > 0, 
                        true_positives / (true_positives + false_positives), 
                        NA)
    precisions <- c(precisions, precision)
    
    # Recall: True Positives / (True Positives + False Negatives)
    recall <- ifelse((true_positives + false_negatives) > 0, 
                     true_positives / (true_positives + false_negatives), 
                     NA)
    recalls <- c(recalls, recall)
    
    # Accuracy: True Positives / Total Items (Actual + Predicted)
    total_items <- length(unique(c(actual_items, predicted_items)))
    accuracy <- ifelse(total_items > 0, true_positives / total_items, NA)
    accuracies <- c(accuracies, accuracy)
  }
  
  # Calculate mean of each metric, excluding NAs
  Precision <- mean(precisions, na.rm = TRUE)
  Recall <- mean(recalls, na.rm = TRUE)
  Accuracy <- mean(accuracies, na.rm = TRUE)
  
  # Return results
  list(Precision = Precision, Recall = Recall, Accuracy = Accuracy)
}

# Evaluate the model for fulldata_lvl8
results_lvl8_normal <- evaluate_precision_recall_accuracy_lvl8(test_data_lvl8, all_recommendations_df_lvl8)

# Print results
print(results_lvl8_normal)

#############################################################################################################################

# Low Evaluation Metrics - Justification 

sparsity <- sum(customer_item_matrix_lvl8 == 0) / (nrow(customer_item_matrix_lvl8) * ncol(customer_item_matrix_lvl8))
cat("Sparsity of the matrix:", sparsity, "\n") #over 99% sparsity in the matrix


recommendation_coverage <- mean(!is.na(all_recommendations_df_lvl8$New_Item_Recommendation))
cat("Recommendation Coverage:", recommendation_coverage, "\n") #only 52% of test customers covered

#############################################################################################################################
library(proxy)
library(RSpectra)  # For SVD
#############################################################################################################################
# Preparing SVD-Enhanced Similarity for Level-8 Data

# Check dimensions of the matrix
matrix_rows_lvl8 <- nrow(customer_item_matrix_lvl8)
matrix_cols_lvl8 <- ncol(customer_item_matrix_lvl8)

# Ensure latent factors are less than the smaller dimension of the matrix
latent_factors_lvl8 <- 150  # Number of latent factors
if (latent_factors_lvl8 >= min(matrix_rows_lvl8, matrix_cols_lvl8)) {
  stop("'latent_factors_lvl8' must be less than the smaller dimension of the matrix.")
}

# Perform Truncated SVD on the customer-item matrix
svd_result_lvl8 <- svds(customer_item_matrix_lvl8, k = latent_factors_lvl8)

# Extract components
U_lvl8 <- svd_result_lvl8$u  # Left singular vectors
D_lvl8 <- diag(svd_result_lvl8$d)  # Singular values (diagonal matrix)
V_lvl8 <- svd_result_lvl8$v  # Right singular vectors

# Reconstruct the matrix with reduced dimensionality
approx_customer_item_matrix_lvl8 <- U_lvl8 %*% D_lvl8 %*% t(V_lvl8)
rownames(approx_customer_item_matrix_lvl8) <- rownames(customer_item_matrix_lvl8)
colnames(approx_customer_item_matrix_lvl8) <- colnames(customer_item_matrix_lvl8)

# Compute cosine similarity on the SVD-enhanced matrix
similarity_matrix_lvl8_svd <- as.matrix(simil(approx_customer_item_matrix_lvl8, method = "cosine"))
diag(similarity_matrix_lvl8_svd) <- 1  # Ensure diagonal is 1

#############################################################################################################################
# Recommend 'top_n' new items (categories not yet purchased) using SVD
recommend_new_items_lvl8_svd <- function(customer_id, similarity_matrix, customer_item_matrix, top_n = 10, k = 500) {
  similar_customers <- similarity_matrix[customer_id, ]
  similar_customers <- sort(similar_customers, decreasing = TRUE)
  similar_customers <- similar_customers[names(similar_customers) != customer_id]
  top_similar_customers <- names(head(similar_customers, k))
  recommendations <- colSums(customer_item_matrix[top_similar_customers, , drop = FALSE])
  customer_items <- customer_item_matrix[customer_id, ]
  recommendations <- recommendations[customer_items == 0]  # Recommendations only for items not yet purchased
  recommended_items <- sort(recommendations, decreasing = TRUE)
  return(names(head(recommended_items, top_n)))
}

#############################################################################################################################
# Recommend growth items (categories where similar customers bought more) using SVD
#recommend_growth_lvl8_svd_sum <- function(customer_id, similarity_matrix, customer_item_matrix, top_n = 10, k = 500) {
  #similar_customers <- similarity_matrix[customer_id, ]
  #similar_customers <- sort(similar_customers, decreasing = TRUE)
  #similar_customers <- similar_customers[names(similar_customers) != customer_id]
  #top_similar_customers <- names(head(similar_customers, k))
  #recommendations <- colSums(customer_item_matrix[top_similar_customers, , drop = FALSE])
  #customer_items <- customer_item_matrix[customer_id, ]
  #recommendations <- recommendations - customer_items  # Growth potential
  #recommendations <- recommendations[recommendations > 0]
  #recommended_items <- sort(recommendations, decreasing = TRUE)
  #top_recommendations <- head(recommended_items, top_n)
  #return(data.frame(Category = names(top_recommendations), Score = top_recommendations))
#}

#Averaging 500 customers instead of aggregating 
recommend_growth_lvl8_svd <- function(customer_id, similarity_matrix, customer_item_matrix, top_n = 10, k = 500) {
  # Step 1: Extract similarity scores for the target customer
  similar_customers <- similarity_matrix[customer_id, ]
  similar_customers <- sort(similar_customers, decreasing = TRUE) # Sort by similarity
  similar_customers <- similar_customers[names(similar_customers) != customer_id] # Exclude the target customer
  top_similar_customers <- names(head(similar_customers, k))
  recommendations <- colMeans(customer_item_matrix[top_similar_customers, , drop = FALSE], na.rm = TRUE)
  customer_items <- customer_item_matrix[customer_id, ]
  recommendations <- recommendations - customer_items # Growth potential
  recommendations <- recommendations[recommendations > 0]
  recommended_items <- sort(recommendations, decreasing = TRUE)
  top_recommendations <- head(recommended_items, top_n)
  
  # Return the recommendations as a data frame
  return(data.frame(Category = names(top_recommendations), Score = top_recommendations))
}

#############################################################################################################################
# Generate recommendations for all test customers using SVD
all_recommendations_lvl8_svd <- lapply(rownames(approx_customer_item_matrix_lvl8), function(customer_id) {
  
  new_items <- recommend_new_items_lvl8_svd(customer_id, similarity_matrix_lvl8_svd, customer_item_matrix_lvl8, top_n = 10)
  if (length(new_items) == 0) new_items <- NA
  
  growth_result <- recommend_growth_lvl8_svd(customer_id, similarity_matrix_lvl8_svd, customer_item_matrix_lvl8, top_n = 10)
  if (nrow(growth_result) == 0) {
    growth_categories <- NA
    growth_scores <- NA
  } else {
    growth_categories <- growth_result$Category
    growth_scores <- growth_result$Score
  }
  
  # Create separate data frames for each recommendation type
  new_items_df <- data.frame(
    Customer_ID = customer_id,
    New_Item_Recommendation = new_items,
    Growth_Item_Recommendation = NA,
    Growth_Score = NA
  )
  
  growth_items_df <- data.frame(
    Customer_ID = customer_id,
    New_Item_Recommendation = NA,
    Growth_Item_Recommendation = growth_categories,
    Growth_Score = growth_scores
  )
  
  # Combine the new and growth recommendations without expanding
  combined_df <- bind_rows(new_items_df, growth_items_df)
  
  return(combined_df)
})

# Combine results into a single data frame
all_recommendations_df_lvl8_svd <- do.call(rbind, all_recommendations_lvl8_svd)

# Preview the recommendations
print(head(all_recommendations_df_lvl8_svd))

#############################################################################################################################
# Evaluation Metrics: MAP and MRR for SVD
results_lvl8_svd <- evaluate_model_with_map_mrr_lvl8(test_data_lvl8, all_recommendations_df_lvl8_svd)
print(results_lvl8_svd)

#############################################################################################################################
# Precision, Recall, and Accuracy for SVD
results_lvl8_normal_svd <- evaluate_precision_recall_accuracy_lvl8(test_data_lvl8, all_recommendations_df_lvl8_svd)
print(results_lvl8_normal_svd)

#############################################################################################################################

# Sparsity and Recommendation Coverage
sparsity_lvl8 <- sum(customer_item_matrix_lvl8 == 0) / (nrow(customer_item_matrix_lvl8) * ncol(customer_item_matrix_lvl8))
cat("Sparsity of the matrix:", sparsity_lvl8, "\n")

recommendation_coverage_lvl8_svd <- mean(!is.na(all_recommendations_df_lvl8_svd$New_Item_Recommendation))
cat("Recommendation Coverage:", recommendation_coverage_lvl8_svd, "\n")

#############################################################################################################################

#Non-SVD: top_n = 10; k = 150 Customers; MAP = 0.13; MRR = 0.14; P =  5.1%; R = 4.6%; A = 2%
#         top_n = 10; k = 500 Customers; MAP = 0.133; MRR = 0.142; P =  5.3%; R = 4.8%; A = 2.2%
#         top_n = 10; k = 750 Customers; MAP = 0.131; MRR = 0.140; P =  5.24%; R = 4.61%; A = 2.15%
#         top_n = 10; k = 1000 Customers; MAP = 0.1306; MRR = 0.139; P =  5.2%; R = 4.9%; A = 2.2%

#SVD: top_n = 25; lf = 50; k = 500 Customers; MAP = 0.1272137; MRR = 0.1548414; P =  4.10%; R = 8.62%; A = 2.33%
# First relevant reco is falling around 5th or 6th reco - out of 25 and clustered around the 10th-15th reco

#SVD: top_n = 10; lf = 150; k = 500 Customers; MAP = 0.1333374; MRR = 0.1440022; P =  5.40%; R = 4.87%; A = 2.20%

#############################################################################################################################

# Function to get recommendations for a specific customer using Non-SVD similarity matrix
get_customer_recommendations <- function(customer_id, similarity_matrix, customer_item_matrix, top_n = 10, k = 500) {
  # Recommend new items
  new_items <- recommend_new_items_lvl8(customer_id, similarity_matrix, customer_item_matrix, top_n = top_n, k = k)
  
  # Recommend growth items
  growth_result <- recommend_growth_lvl8(customer_id, similarity_matrix, customer_item_matrix, top_n = top_n, k = k)
  
  if (nrow(growth_result) == 0) {
    growth_categories <- NA
    growth_scores <- NA
  } else {
    growth_categories <- growth_result$Category
    growth_scores <- growth_result$Score
  }
  
  # Combine the recommendations into a data frame
  recommendations <- data.frame(
    Customer_ID = customer_id,
    New_Item_Recommendation = ifelse(length(new_items) > 0, paste(new_items, collapse = ", "), NA),
    Growth_Item_Recommendation = ifelse(!is.na(growth_categories), paste(growth_categories, collapse = ", "), NA),
    Growth_Score = ifelse(!is.na(growth_scores), paste(round(growth_scores, 2), collapse = ", "), NA)
  )
  
  return(recommendations)
}

# Function to get recommendations for a specific customer using SVD-enhanced similarity matrix
get_customer_recommendations_svd <- function(customer_id, similarity_matrix, customer_item_matrix, top_n = 10, k = 500) {
  # Recommend new items
  new_items <- recommend_new_items_lvl8_svd(customer_id, similarity_matrix, customer_item_matrix, top_n = top_n, k = k)
  
  # Recommend growth items
  growth_result <- recommend_growth_lvl8_svd(customer_id, similarity_matrix, customer_item_matrix, top_n = top_n, k = k)
  
  if (nrow(growth_result) == 0) {
    growth_categories <- NA
    growth_scores <- NA
  } else {
    growth_categories <- growth_result$Category
    growth_scores <- growth_result$Score
  }
  
  # Combine the recommendations into a data frame
  recommendations <- data.frame(
    Customer_ID = customer_id,
    New_Item_Recommendation = ifelse(length(new_items) > 0, paste(new_items, collapse = ", "), NA),
    Growth_Item_Recommendation = ifelse(!is.na(growth_categories), paste(growth_categories, collapse = ", "), NA),
    Growth_Score = ifelse(!is.na(growth_scores), paste(round(growth_scores, 2), collapse = ", "), NA)
  )
  
  return(recommendations)
}

# Example usage for a specific customer 
customer_id <- "576563008"
customer_id <- "264594004"

# For Non-SVD recommendations
recommendations_non_svd <- get_customer_recommendations(
  customer_id = customer_id,
  similarity_matrix = similarity_matrix_lvl8,
  customer_item_matrix = customer_item_matrix_lvl8,
  top_n = 10,
  k = 500
)
print(recommendations_non_svd)

# For SVD-enhanced recommendations
recommendations_svd <- get_customer_recommendations_svd(
  customer_id = customer_id,
  similarity_matrix = similarity_matrix_lvl8_svd,
  customer_item_matrix = customer_item_matrix_lvl8,
  top_n = 10,
  k = 500
)
print(recommendations_svd)


#############################################################################################################################

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Aggregate the growth scores for each category
top_categories <- all_recommendations_df_lvl8_svd %>%
  filter(!is.na(Growth_Item_Recommendation)) %>%  # Remove rows with NA growth recommendations
  group_by(Growth_Item_Recommendation) %>%
  summarise(Total_Growth_Score = sum(as.numeric(Growth_Score), na.rm = TRUE)) %>%  # Summing growth scores
  arrange(desc(Total_Growth_Score)) %>%  # Sort in descending order
  slice_head(n = 3)  # Select the top 3 categories

# Plot the top 3 categories as a bar chart
ggplot(top_categories, aes(x = reorder(Growth_Item_Recommendation, Total_Growth_Score), 
                           y = Total_Growth_Score, fill = "Healthcare Blue")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Healthcare Blue" = "#0073C2")) +  # Custom healthcare blue color
  labs(
    title = "Top 3 Highest Growing Categories",
    x = " ",
    y = "Growth Score"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    plot.title = element_text(size = 14, face = "bold")
  )

#############################################################################################################################

#Nitrile Examination Gloves (total ship_qty and new_net_sales)
gloves_totals <- merged_data %>%
  filter(cmt_nm_lvl_8_nm == "Nitrile Examination Gloves") %>%
  summarise(
    Total_Ship_Qty = sum(ship_qty, na.rm = TRUE),
    Total_New_Net_Sales = sum(new_net_sales, na.rm = TRUE)
  )

print(gloves_totals) #avg price = $20.12

#For Wipers
wipers_totals <- merged_data %>%
  filter(cmt_nm_lvl_8_nm == "General Purpose Wipers") %>%
  summarise(
    Total_Ship_Qty = sum(ship_qty, na.rm = TRUE),
    Total_New_Net_Sales = sum(new_net_sales, na.rm = TRUE)
  )

print(wipers_totals) #avg price = $11.50

#For Pipets
pipets_totals <- merged_data %>%
  filter(cmt_nm_lvl_8_nm == "Plastic Serological Pipets") %>%
  summarise(
    Total_Ship_Qty = sum(ship_qty, na.rm = TRUE),
    Total_New_Net_Sales = sum(new_net_sales, na.rm = TRUE)
  )
print(pipets_totals) #avg price = $87.76

#############################################################################################################################

# Step 1: Extract log transformation parameters from training data
min_ship_qty_log <- min(train_data_lvl8$ship_qty_log, na.rm = TRUE)
max_ship_qty_log <- max(train_data_lvl8$ship_qty_log, na.rm = TRUE)

# Step 2: Scale Growth Scores for Stability
# Calculate the maximum growth score to scale down values
max_growth_score <- max(all_recommendations_df_lvl8_svd$Growth_Score, na.rm = TRUE)

# Function to reverse log transformation with scaling
reverse_transform_scaled <- function(growth_score, max_score, min_log, max_log) {
  scaled_score <- growth_score / max_score  # Scale growth score to [0, 1]
  log_transformed <- scaled_score * (max_log - min_log) + min_log  # Reverse normalization
  original_value <- exp(log_transformed) - 1  # Reverse log transformation
  return(original_value)
}

# Step 3: Apply inverse transformations to calculate `Estimated_Ship_Qty`
lvl8_est_qty <- all_recommendations_df_lvl8_svd %>%
  filter(!is.na(Growth_Item_Recommendation) & !is.na(Growth_Score)) %>%  # Retain rows with growth scores
  mutate(
    Estimated_Ship_Qty = reverse_transform_scaled(
      Growth_Score,
      max_growth_score,
      min_ship_qty_log,
      max_ship_qty_log
    )
  )

# Step 4: Apply Weighting and Cap Recommendations
# Add weights based on rank and cap the number of recommendations to the top 3 per customer
all_recommendations_weighted <- lvl8_est_qty %>%
  group_by(Customer_ID) %>%
  mutate(
    Rank = row_number(),  # Rank recommendations for each customer
    Weight = 1 / Rank,    # Assign weights inversely proportional to rank
    Weighted_Estimated_Ship_Qty = Estimated_Ship_Qty * Weight  # Apply weights
  ) %>%
  slice_max(Weighted_Estimated_Ship_Qty, n = 3)  # Retain top 3 recommendations per customer

# Step 5: Aggregate Weighted and Capped Estimates by Category
estimated_growth_ship_qty <- all_recommendations_weighted %>%
  group_by(Growth_Item_Recommendation) %>%
  summarise(
    Total_Weighted_Estimated_Ship_Qty = sum(Weighted_Estimated_Ship_Qty, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Weighted_Estimated_Ship_Qty))

# Step 6: Normalize Growth Estimates by Historical Sales
# Calculate historical sales for each category
historical_sales <- train_data_lvl8 %>%
  group_by(cmt_nm_lvl_8_nm) %>%
  summarise(Historical_Sales = sum(ship_qty, na.rm = TRUE))

# Join historical sales with growth estimates and normalize
final_growth_estimates <- estimated_growth_ship_qty %>%
  left_join(historical_sales, by = c("Growth_Item_Recommendation" = "cmt_nm_lvl_8_nm")) %>%
  mutate(
    Normalized_Estimated_Ship_Qty = (Total_Weighted_Estimated_Ship_Qty / 
                                       sum(Total_Weighted_Estimated_Ship_Qty)) * 
      sum(Historical_Sales, na.rm = TRUE)
  ) %>%
  arrange(desc(Normalized_Estimated_Ship_Qty))

# Step 7: Apply Capping to Prevent Over-Inflated Estimates
# Cap growth at 75% of historical sales
final_growth_estimates <- final_growth_estimates %>%
  mutate(
    Capped_Normalized_Estimated_Ship_Qty = pmin(Normalized_Estimated_Ship_Qty, 0.75 * Historical_Sales)
  )

# Step 8: Adjust Growth with Conversion Rate
conversion_rate <- 0.1
final_growth_estimates <- final_growth_estimates %>%
  mutate(
    Adjusted_Estimated_Ship_Qty = Capped_Normalized_Estimated_Ship_Qty * conversion_rate
  )

# Print the final table with adjusted growth estimates
print(final_growth_estimates)

#############################################################################################################################

# Growth Estimates for Top Customer by Purchase Counts & Revenue
top_customer_growth_estimate_data <- merged_data %>%
  select(acct_nbr, invc_dt, ship_qty, cmt_nm_lvl_8_nm,new_net_sales)

customer_summary <- top_customer_growth_estimate_data %>%
  group_by(acct_nbr) %>%
  summarise(
    Total_Revenue = sum(new_net_sales, na.rm = TRUE),  # Sum of revenue
    Total_Purchases = sum(ship_qty, na.rm = TRUE)     # Sum of quantities purchased
  )

top_spender <- customer_summary %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 1)  # Get the top customer by revenue

top_buyer <- customer_summary %>%
  arrange(desc(Total_Purchases)) %>%
  slice_head(n = 1)  # Get the top customer by purchase counts

cat("Customer with highest revenue:\n")
print(top_spender)

cat("\nCustomer with highest number of purchases:\n")
print(top_buyer)

#############################################################################################################################

# Step 1: Define the reverse transformation function
reverse_transform_scaled <- function(growth_score, max_score, min_log, max_log) {
  # Scale down the growth score relative to the maximum growth score
  scaled_score <- growth_score / max_score  
  # Reverse normalization
  log_transformed <- scaled_score * (max_log - min_log) + min_log  
  # Reverse log transformation
  original_value <- exp(log_transformed) - 1  
  return(original_value)
}

# Step 2: Input parameters
# Replace with actual values for `min_ship_qty_log`, `max_ship_qty_log`, and `max_growth_score` from your dataset
min_ship_qty_log <- min(train_data_lvl8$ship_qty_log, na.rm = TRUE)  # Minimum log-transformed ship_qty
max_ship_qty_log <- max(train_data_lvl8$ship_qty_log, na.rm = TRUE)  # Maximum log-transformed ship_qty
max_growth_score <- max(all_recommendations_df_lvl8_svd$Growth_Score, na.rm = TRUE)  # Max growth score

# Step 3: Input your growth score(s)
# Example: Replace these values with the actual growth scores you want to calculate for
input_growth_scores <- c(0.12, 0.09, 0.08)  # Example growth scores for specific categories

# Step 4: Calculate the corresponding ship_qty for each input growth score
calculated_ship_qty <- sapply(input_growth_scores, function(score) {
  reverse_transform_scaled(score, max_growth_score, min_ship_qty_log, max_ship_qty_log)
})

# Step 5: Create a result data frame for easy interpretation
result <- data.frame(
  Growth_Score = input_growth_scores,
  Estimated_Ship_Qty = calculated_ship_qty
)

# Print the results
print(result)

##########
#Nitrile Examination Gloves (total ship_qty and new_net_sales)
filtered_pips <- merged_data %>%
  filter(cmt_nm_lvl_8_nm == "Sharps Disposal Containers") %>%
  summarise(
    Total_Ship_Qty = sum(ship_qty, na.rm = TRUE),
    Total_New_Net_Sales = sum(new_net_sales, na.rm = TRUE)
  )



# Visualizations for Top Categories and Customers
library(ggplot2)
ggplot(top_categories, aes(x = reorder(Growth_Item_Recommendation, -Total_Growth_Score), 
                           y = Total_Growth_Score, fill = "Healthcare Blue")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Healthcare Blue" = "#0073C2")) +  # Custom healthcare blue color
  labs(
    title = " ",
    x = " ",  # Keep x-axis label blank
    y = "Growth Score"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.text.x = element_text(angle = 0, hjust = 1, size = 24),  # Rotate and increase x-axis label size
    axis.text.y = element_text(size = 28),  # Increase y-axis label size
    axis.title.x = element_text(size = 24),  # Increase x-axis title size
    axis.title.y = element_text(size = 24),  # Increase y-axis title size
    plot.title = element_text(size = 18, face = "bold")  # Increase plot title size
  )


top_categories <- data.frame(
  Category = c(
    "Filtered Universal Standard Pipette Tips",
    "Chromatographic Acetonitrile",
    "Sharps Disposal Containers"
  ),
  Growth_Score = c(0.12, 0.09, 0.08)
)

ggplot(top_categories, aes(x = reorder(Category, -Growth_Score), y = Growth_Score, fill = "Healthcare Blue")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("Healthcare Blue" = "#0073C2")) +  # Custom healthcare blue color
  labs(
    title = " ",
    x = " ",
    y = "Growth Score"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove gridlines
    axis.text.x = element_text(angle = 45, hjust = 1, size = 24),  # Rotate and increase x-axis label size
    axis.text.y = element_text(size = 28),  # Increase y-axis label size
    axis.title.x = element_text(size = 24),  # Increase x-axis title size
    axis.title.y = element_text(size = 24),  # Increase y-axis title size
    plot.title = element_text(size = 18, face = "bold")  # Increase plot title size
  )





