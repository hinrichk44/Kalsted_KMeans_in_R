# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----


library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 CUSTOMER TRENDS ----
# - GOAL: Mine Customer Purchase History for similarity to other "like" customers
# - TECHNIQUES: K-Means Clustering, UMAP 2D Projection


# 1.1 Get Customer Trends ----
# Aggregating purchasing trends to customer & products is typically the way to go
customer_trends_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, price, model, category_1, category_2, frame_material, quantity) %>%
    group_by(bikeshop_name, price, model, category_1, category_2, frame_material) %>%
    summarize(quantity_purchased = sum(quantity)) %>%
    ungroup() %>%
    group_by(bikeshop_name) %>%
    # Here we are getting the proportion of total purchased
    mutate(prop_of_total = quantity_purchased / sum(quantity_purchased)) %>%
    ungroup()




# 1.2 Convert to User-Item Format (e.g. Customer-Product) ----


# Here we are creating customer product buying habits
# For example, the customer bikeshop Albuquerque Cycles purchased almost 2% of all Bad Habit 1 bicycles
customer_product_tbl <- customer_trends_tbl %>%
    select(bikeshop_name, model, prop_of_total) %>%
    # We are filling in all missing values with a 0
    spread(key = model, value = prop_of_total, fill = 0)




# 2.0 MODELING: K-MEANS CLUSTERING ----


# 2.1 Performing K-Means ----


kmeans_obj <- customer_product_tbl %>%
    # Here we are selecting all columns EXCEPT bikeshop_name, hence the negative sign
    # We want all numeric columns for now
    select(-bikeshop_name) %>%
    # Here we are initializing the k-means clustering algorithm
    # We are starting with three clusters (centers = 3)
    # nstart: K-Means picks a random starting point and then iteratively finds the best location for the centers
    # Choosing nstart > 1 ensures a higher likelihood that a good center is found
    kmeans(centers = 3, nstart = 100)


# Centers output is the matrix of cluster locations in k x m_items space
kmeans_obj$centers


# Cluster output is the group that each product is assigned to for each product
kmeans_obj$cluster




# 2.2 Tidying a K-Means Object ----


# Here we are seeing the location of the clusters, the size of the clusters, and the within sum of squares
# Size meaning how many data points are in each cluster. Looks like 3-6-21 for clusters 1-2-3
broom::tidy(kmeans_obj) %>%
    glimpse()


# glance() returns overall summary metrics for the model
# Remember that Total Within Sum of Squares will be the metric we use to help determine what number of clusters to use
broom::glance(kmeans_obj)


# Here we are adding a column named .cluster that shows which cluster each data point is in
broom::augment(kmeans_obj, customer_product_tbl) %>%
    select(bikeshop_name, .cluster)




# 2.3 How many centers (customer groups) to use? ----


# Step one is to create a function that can be iterated 
# We will call the function kmeans_mapper and it will take in an argument "centers" which will have a default value of three
kmeans_mapper <- function(centers = 3) {
    
    customer_product_tbl %>%
        select(-bikeshop_name) %>%
        kmeans(centers = centers, nstart = 100)
}


# Here we will test out our function on one iteration
3 %>% kmeans_mapper() %>% glance()


# Here we are creating a simple tibble which we will use to map our function we created just above
# It will have a column named "centers" that is numbered 1-15
# Step two is to implement purr row-wise
# So, you can click on kmeans_mapped_tbl in the environment. 
# It will have a column for centers (1-15)
# It will have a column "k_means" which shows which cluster each data point is in
# It will have a column "glance" that gives the summary statistics for that amount of clusters
kmeans_mapped_tbl <- tibble(centers = 1:15) %>%
    # Here we are adding a "k_means" column to the tibble
    mutate(k_means = centers %>% map(kmeans_mapper)) %>%
    # Here we are adding a "glance" column to the tibble
    mutate(glance = k_means %>% map(glance))


# Here we are unnesting the glance column in our kmeans_mapped_tbl
kmeans_mapped_tbl %>%
    unnest(glance) %>%
    select(centers, tot.withinss)




# 2.4 Scree Plot ----


# The Scree Plot will help us determine how many centers to use
# Four looks like the optimal amount of clusters since going from four to five doesn't seem to decrease very much
kmeans_mapped_tbl %>%
    unnest(glance) %>%
    select(centers, tot.withinss) %>%
    ggplot(aes(centers, tot.withinss)) +
    geom_point(color = "#2c3e50", size = 4) +
    geom_line(color = "#2c3e50", size = 1) +
    # The ggrepel function "repels" the label so the label goes off the dot instead of covering the dot
    ggrepel::geom_label_repel(aes(label = centers), color = "#2c3e50") +
    theme_tq() +
    labs(
        title = "Scree Plot",
        subtitle = "Measures the distance that each of the customers are from the closest K-Means center"
    )




# 3.0 VISUALIZATION: UMAP ----


# 3.1 Use UMAP to get 2-D Projection ----


# UMAP is a high performance dimension reduction where we can visualize customer segments
# The Scree Plot helped us determine the amount of clusters. The UMAP will help us see which customers are in which cluster
# UMAP is a user-item matrix after dimension reduction to a 2D space
# config provides settings that can be overwritten like number of neighbors, number of components, distance measurement and more
umap_obj <- customer_product_tbl %>%
    select(-bikeshop_name) %>%
    umap()


# Here we are returning a matrix of values from the UMAP object
umap_results_tbl <- umap_obj$layout %>%
    # This will convert the matrix into a tibble. It's a 30x2 dataframe
    as_tibble() %>%
    # This will make the column names x and y
    set_names(c("x", "y")) %>%
    # Here we are combining x, y, and the bikeshop_name column from the customer_product_tbl
    # We now basically have what we need to plot the UMAP
    bind_cols(customer_product_tbl %>% select(bikeshop_name))


# Here is a visualization of the UMAP results
umap_results_tbl %>%
    ggplot(aes(x,y)) +
    geom_point() +
    geom_label_repel(aes(label = bikeshop_name), size = 3)




# 3.2 Use K-Means to Add Cluster Assignments ----


# Here we are pulling out data from the 4th cluster
kmeans_4_obj <- kmeans_mapped_tbl %>%
    # We are pulling out the list of k-means objects in the k_means column
    pull(k_means) %>%
    # Pluck allows us to extract an element from a list using a number.
    # Similar to slice for lists
    # We are plucking the 4th cluster since that was the ideal amount
    pluck(4)


#
kmeans_4_clusters_tbl <- kmeans_4_obj %>% 
    augment(customer_product_tbl) %>%
    select(bikeshop_name, .cluster)


# Here we are combining the umap and k-means results
umap_kmeans_4_results_tbl <- umap_results_tbl %>%
    left_join(kmeans_4_clusters_tbl)




# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----


# We will use the cluster assignment to do color coding for us
umap_kmeans_4_results_tbl %>%
    mutate(label_text =  str_glue("Customer: {bikeshop_name}
                                  Cluster: {.cluster}")) %>%
    ggplot(aes(x, y, color = .cluster)) +
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Customer Segmentation: 2D Projection",
        subtitle = "UMAP 2D projection with k-means cluster assignment",
        caption = "Conclusion: Four customer segments identified using two different algorithms"
    ) +
    theme(legend.position = "none")




# 4.0 ANALYZE PURCHASING TRENDS ----


# Let's bin the prices we have in the customer_trends_tbl
customer_trends_tbl %>%
    pull(price) %>%
    # quantile() returns values for binning numeric vectors by user assigned probabilities
    # The value at 0-percentile is 415, at 25th-percentile is 1950, at 75th-percentile is 5330 and at 100th-percentile is 12,790
    quantile()


# Let's bin the prices we have in the customer_trends_tbl
customer_trends_tbl %>%
    pull(price) %>%
    # We are doing what we did above, but changing the bin ranges
    # The value at 0-percentile is 415, at 33rd-percentile is 2240, at 66th-percentile is 4260 and at 100th-percentile is 12,790
    quantile(probs = c(0, 0.33, 0.66, 1))


# We know which customers are in which cluster due to the visualizations we did above
# However, we still don't know exactly what each customer is buying
cluster_trends_tbl <- customer_trends_tbl %>%
    # Here we are joining cluster assignments by bikeshop name
    left_join(umap_kmeans_4_results_tbl) %>%
    # Here we are creating a new column called "price_bin" that will categorize prices based on the binning we did just above
    mutate(price_bin = case_when(
        price <= 2240 ~ "low",
        price <= 4260 ~ "medium",
        TRUE ~ "high"
    )) %>%
    # Here we are rearranging columns so .cluster comes first, then model, then all columns containing "price"
    # After that it's category_1 through quantity_purchased 
    select(.cluster, model, contains("price"), category_1:quantity_purchased) %>%
    # Here we are aggregating quantity purchased by cluster and product attributes
    # group_by_at is a scoped variant of group_by that allows us to select columns with tidy_select helpers like contains()
    group_by_at(.vars = vars(.cluster:frame_material)) %>%
    summarize(total_quantity = sum(quantity_purchased)) %>%
    # You should have a 378 x 8 tibble right now
    ungroup() %>%
    # We are going to normalize the data by calculating proportion of total
    group_by(.cluster) %>%
    mutate(prop_of_total = total_quantity / sum(total_quantity)) %>%
    ungroup()


# Here we are taking an in-depth look at customers in cluster 1
# Looks like mostly high and medium price bins
cluster_trends_tbl %>%
    # Here we are selecting only customers from cluster 1
    filter(.cluster == 1) %>%
    # Here we are arranging them by proportion of total in descending order
    arrange(desc(prop_of_total)) %>%
    mutate(cum_prop = cumsum(prop_of_total)) 


# Here we are creating a function that will help us analyze each cluster more efficiently 
# We will call the function "get_clusters"
get_clusters <- function(cluster = 1) {
    
    cluster_trends_tbl %>%
        
        filter(.cluster == cluster) %>%
        
        # Here we are arranging them by proportion of total in descending order
        arrange(desc(prop_of_total)) %>%
        
        mutate(cum_prop = cumsum(prop_of_total))
    
}


# Now we are applying our function to analyze customers in cluster 2
# Still looks like mostly high and medium price bin customers
get_clusters(cluster = 2)


# Now we are applying our function to analyze customers in cluster 3
# Looks like these customers are in the low to medium price bin
get_clusters(cluster = 3)


# Now we are applying our function to analyze customers in cluster 4
# Similar to cluster 3 these are low to medium price bin customers 
get_clusters(cluster = 4)




# Now we are going to update our UMAP visualization, but with adding context to the clusters


# First we will create the necessary cluster labels for our UMAP
cluster_label_tbl <- tibble(
    .cluster = 1:4,
    .cluster_label = c(
        "High/Medium Price, Road, Carbon Frame",
        "High/Medium Price, Mountain, Carbon Frame",
        "Low/Medium Price, Mountain, Aluminum Frame",
        "Low/Medium Price, Road, Aluminum/Carbon Frame")
) %>%
    # Numeric data cannot be directly converted to a factor
    # You must convert to a character first
    mutate(.cluster = as_factor(as.character(.cluster)))


# Here we are joining the cluster labels with our umap tbl
# So we have: x, y, bikeshop_name, .cluster, and cluster_label
umap_kmeans_4_results_tbl %>%
    left_join(cluster_label_tbl)


# Here is the new and updated UMAP visualization
umap_kmeans_4_results_tbl %>%
    left_join(cluster_label_tbl) %>%
    mutate(label_text =  str_glue("Customer: {bikeshop_name}
                                  Cluster: {.cluster}
                                  {.cluster_label}")) %>%
    ggplot(aes(x, y, color = .cluster)) +
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Customer Segmentation: 2D Projection",
        subtitle = "UMAP 2D projection with k-means cluster assignment",
        caption = "Conclusion: Four customer segments identified using two different algorithms"
    ) +
    theme(legend.position = "none")