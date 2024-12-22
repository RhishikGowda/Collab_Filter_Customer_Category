# Collab_Filter_Customer_Category
A recommendation model in RStudio to help recommend sales teams' what categories to focus on next by using matrix factorization (cosine similarity), SVD and a customer-category matrix. 
Includes 3 datasets - invoice history, account, and product information that were merged and cleaned to form "merged_data.csv". Data dictionary also provided. Only column names shared. 
The code uses 4 columns - customer account number; ship qty; customer category; and invoice date, arranged chronlogically and split with temporal structure intact for test-train (80-20)
All single transaction customers removed (48% of total dataset) as they lack data to for latent factors or any discernable patterns in customer behavior. 
Recommendation model code uses "ship_qty" as a "score" in the customer-category matrix to find similar customers (similarity matrix) and most popular categories. 
Ship_qty scores log transformed and normalized to handle outliers and prevent large skews in model recommendation. Customer-category and similarity matrix calculated with these scores.
"Top_n" notifies number of recommendations the model needs to output. "K" notifies how many similar customers were considered in making category recommendations.

Two Customer recommendation functions created to find: 
      1. Finding the "top_n" most likely new categories customers will buy next, disregards already purchased categories. 
      2. Finding the "top_n" growth categroes where customer spending is expected to grow, disregards negative growth categories. Output = log_normalized ship qty of expected growth. 

SVD applied with customer latent factors, where "lf" = 50 currently but can be changed to tweak model performance. Model is applied on test data set. 
Evaluation: top_n = 10; lf = 150; k = 500 Customers; MAP = 0.1333374; MRR = 0.1440022; Precision =  5.40%; Recall = 4.87%; Accuracy = 2.20% 

Challenges and further improvements: 
      1. Improve MAP, MRR and Recall. Sparse matrix usually produce low evaluation metrics, which is acceptable as long as the MAP and MRR are robust. 
      2. Ship qty stores individual units sold or bulk units sold for the same product in the same column. i.e. 10 boxes of gloves (1000 gloves) and 10 gloves are both stored as 10. 
         Skews model heavily and produces inaccurate predictions. Possible solution - replace ship_qty with "purchased = Yes or No" binary model or "revenue". 
      3. Cold start problem - around 48% of all customers have under 4 transcation and were ignored in the model recommendations. 
