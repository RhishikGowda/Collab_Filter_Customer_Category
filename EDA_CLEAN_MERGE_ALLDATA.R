#install.packages("tidyr")
#install.packages("ggcorrplot")
#install.packages("readr")
#install.packages("stringr")

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggcorrplot)
library(readr)
library(stringr)

acct <- read_csv("file/path/redacted/for/privacy/acct.csv")
prod <- read_csv("file/path/redacted/for/privacy,prod.csv")
ih <- read_csv("file/path/redacted/for/privacy/ih.csv")

# Checking for duplicates in prod
duplicates <- prod %>%
  group_by(sku_anonymized) %>%
  summarise(count = n()) %>%
  filter(count > 1)

sum(duplicates$count) #42700 duplicate values - but each duplicate SKU has a unique "prod_nm_en_anonymized" 
# then, each ROW will be unique in merged_data below, even though SKU is duplicated various times

# Removing duplicate SKU's for Prod (DataDict. notes it is a unique value) (can't be removed later)
prod <- anti_join(prod, duplicates, by = "sku_anonymized")

# Joining 'acct' and 'ih' on 'acct_nbr'
join_acct_ih <- left_join(acct, ih, by = "acct_nbr") #yields null values for all except cust_seg = 1
prefinal_join <- filter(join_acct_ih, cust_seg_cd == 1) # remove all except cust_seg = 1 
final_join <- left_join(prefinal_join, prod, by = "sku_anonymized")
head(final_join) # has NA values for CMT because not all products in IH are present in PROD
# example: 7448 exists in IH, but PROD only has 74480, and many other prods with 7448 IN IT (xyz7448abc)
# it could be because 7448 could be used as main prod code - and alphanumerics added to it later for subcategories
# could mess with temporal pattern since 70K+ orders with date info are being dropped

# Unique and null counts for each column
column_stats <- function(data) {
  stats <- data.frame(
    Column = character(),
    Unique_Values = integer(),
    Null_Values = integer(),
    stringsAsFactors = FALSE
  )
  
  for (col_name in names(data)) {
    num_unique <- length(unique(data[[col_name]]))
    num_na <- sum(is.na(data[[col_name]]))
    stats <- rbind(stats, data.frame(Column = col_name, Unique_Values = num_unique, Null_Values = num_na))
  }
  
  return(stats)
}

stats_result <- column_stats(final_join)
print(stats_result)


# Cmt_names have an extra "NA-NOT Mapped to CMT" value in it that needs to be removed 
unique_values_lvl_2_nm <- unique(final_join$cmt_nm_lvl_2_nm)
print(unique_values_lvl_2_nm) #Two types of NA values
unique_values_lvl_2_cd <- unique(final_join$cmt_nm_lvl_2_cd)
print(unique_values_lvl_2_cd) #NA values only

# Number of unique categories in each CMT level
unique_values_lvl_3_nm <- unique(final_join$cmt_nm_lvl_3_nm)
print(unique_values_lvl_3_nm) #75 categories

unique_values_lvl_4_nm <- unique(final_join$cmt_nm_lvl_4_nm)
print(unique_values_lvl_4_nm) #618 categories

unique_values_lvl_7_nm <- unique(final_join$cmt_nm_lvl_7_nm)
print(unique_values_lvl_7_nm) #3466 categories

unique_values_lvl_8_nm <- unique(final_join$cmt_nm_lvl_8_nm)
print(unique_values_lvl_8_nm) #3485 categories (Includes NA and starts at row 2 so technically 3483 unique values)


# COUNT how many "NA-NOT Mapped To CMT" and "NA" in 'cmt_nm_lvl_2_nm'
count_na_not_mapped <- sum(final_join$cmt_nm_lvl_2_nm == "NA-NOT MAPPED TO CMT", na.rm = TRUE)
count_na_values_nm <- sum(is.na(final_join$cmt_nm_lvl_2_nm))
count_na_values_cd <- sum(is.na(final_join$cmt_nm_lvl_2_cd))
cat("Count of 'NA-NOT Mapped To CMT' in 'cmt_nm_lvl_2_nm':", count_na_not_mapped, "\n")
cat("Count of 'NA' values in 'cmt_nm_lvl_2_nm':", count_na_values_nm, "\n")
cat("Count of 'NA' values in 'cmt_nm_lvl_2_cd':", count_na_values_cd, "\n")
# 86  + 73609 (NA Values in cmt_nm) = 73523 (NA Values in cmt_cd)

#Removing all NA-MAPPED To CMT and NA Values
merged_data <- final_join %>%
  filter(!(cmt_nm_lvl_3_nm == "NA-NOT MAPPED TO CMT" | is.na(cmt_nm_lvl_3_nm)))

# Data Summary for merged_data
stats_result_merged <- column_stats(merged_data)
print(stats_result_merged) #all null values dropped

summary(merged_data)
str(merged_data)
merged_data$invc_dt <- as.Date(as.character(merged_data$invc_dt), format = "%Y%m%d") #changing date to date
merged_data <- merged_data[order(merged_data$invc_dt), ]


#Keep for records/Final Model Train-Test
write.csv(merged_data, "file/path/redacted/for/privacy", row.names = FALSE)

#############################################################################################################################

# Testing for values below $1 in merged_data (Free Samples - indicative of temporal trend or remove?)
below_one_dollar <- sum(merged_data$new_net_sales < 1, na.rm = TRUE)
cat("Number of values below $1 in 'new_net_sales':", below_one_dollar, "\n") #3040 values below a dollar

# Histogram for sales under $100
filtered_data <- merged_data %>%
  filter(new_net_sales < 100)

ggplot(filtered_data, aes(x = new_net_sales)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of new_net_sales (under $100)",
       x = "new_net_sales",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Looks like there ~5000 rows with sales < $8
# Choosing not to drop the values assuming sampling of prod indicates some interest for that product at that time (purchase counts)
# Will be dropped for revenue subset analysis
###########################################################################################################################
###########################################################################################################################

#Sampling with temporal structure intact (50% Data Split)
min_date <- min(merged_data$invc_dt, na.rm = TRUE)
max_date <- max(merged_data$invc_dt, na.rm = TRUE)
midpoint_date <- as.Date(mean(c(min_date, max_date))) # ~6months

sample_1 <- merged_data %>%
  filter(invc_dt <= midpoint_date)
range(sample_1$invc_dt) #days we have data on 

sample_2 <- merged_data %>%
  filter(invc_dt > midpoint_date)
range(sample_2$invc_dt) #other 6 months


###########################################################################################################################
###########################################################################################################################

# Summarize the total 'new_net_sales' per day
sales_by_date <- merged_data %>%
  group_by(invc_dt) %>%
  summarise(total_sales = sum(new_net_sales, na.rm = TRUE))
print(sales_by_date)

# Create the line plot with only the first, midpoint, and last dates on the x-axis
ggplot(sales_by_date, aes(x = invc_dt, y = total_sales)) +
  geom_line(color = "blue", size = 1) +  # Line for total sales
  geom_vline(xintercept = as.numeric(midpoint_date), linetype = "dotted", color = "red", size = 1) +  # Vertical dotted line for midpoint
  annotate("text", x = min_date + (midpoint_date - min_date) / 2, y = max(sales_by_date$total_sales) * 0.9, 
           label = "Sample 1", size = 8, color = "black") +  # Label for Sample 1
  annotate("text", x = midpoint_date + (max_date - midpoint_date) / 2, y = max(sales_by_date$total_sales) * 0.9, 
           label = "Sample 2", size = 8, color = "black") +  # Label for Sample 2
  labs(title = " ", 
       x = " ", 
       y = "Total Sales") +
  scale_x_date(breaks = c(min_date, midpoint_date, max_date), date_labels = "%Y-%m-%d") +  # Only first, midpoint, and last dates
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  # Rotate x-axis labels, increase font size
        axis.text.y = element_text(size = 18),  # Increase y-axis text font size
        axis.title.y = element_text(size = 22),  # Increase y-axis title size
        panel.grid = element_blank(),  # Remove all gridlines
        axis.line = element_line(color = "black"),  # Add axis lines for both x and y axes
        axis.ticks = element_line(color = "black"))  # Add axis ticks for x and y axes
#############################################################################################################################






















