# Load necessary libraries
library(dendextend)
library(corrplot)
library(proxy)
library(flexclust)

# Create a directory for storing the results
clusters_dir <- "./clusters"
dir.create(clusters_dir)

# Create a subdirectory for comparison results
comparison_dir <- "Comparisons"
dir.create(paste0(clusters_dir,"/",comparison_dir))

# Define a function to analyze the input matrix
analyze_matrix <- function(my_matrix) {
  
  # Convert the input matrix to a regular matrix format
  asmatrix = as.matrix(my_matrix)
  
  # Get the name of the input matrix
  matrix_name <- deparse(substitute(my_matrix))
  
  # Create a subdirectory for storing results for the current matrix
  dir.create(paste0(clusters_dir, "/", matrix_name))
  
  # Perform hierarchical clustering on the input matrix
  # using the complete linkage method and Euclidean distance
  doc_names <- rownames(my_matrix)
  doc_count <- length(doc_names)
  legend <- paste(
    paste(
      "d",
      1:length(doc_names),
      sep = ""
    ),
    doc_names,
    sep = " -> "
  )
  clusters_pattern <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3)
  cols <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink", "dodgerblue", "darkmagenta", "khaki", "darkolivegreen")
  cols_pattern <- c()
  for (doc in 1:doc_count) {
    cols_pattern[doc] <- cols[clusters_pattern[doc]]
  }
  names(clusters_pattern) <- doc_names
  names(cols_pattern) <- doc_names
  
  # Calculate the pairwise distance matrix between rows of the input matrix
  dist_matrix_1 <- dist(asmatrix, method = "euclidean")
  
  # Apply the hierarchical clustering algorithm to the distance matrix
  h_clust_1 <- hclust(dist_matrix_1, method = "complete")

  # This section of the code generates plots and saves them in a specified directory
  
  # Set up a plot file name and output directory
  # Plotting the dendrogram of hierarchical clustering using the complete linkage method
  # Saving the plot to a file in PNG format
  plot_file <- paste(
    clusters_dir,
    "/",
    matrix_name,
    "/",
    "dend_",
    matrix_name,
    "_base.png",
    sep = ""
  )
  png(plot_file, height = 600)
  plot(h_clust_1)
  dev.off()
  
  # Create a bar plot of the dendrogram height
  barplot(h_clust_1$height, names.arg = (doc_count-1):1)
  
  # Convert the dendrogram into a dendrogram object
  dend <- as.dendrogram(h_clust_1)
  
  # Set the name attribute of the dendrogram object to the matrix name
  attr(dend, "name") <- paste0(matrix_name)
  
  # Find the optimal number of clusters using the Calinski-Harabasz criterion
  clust_caunt <- find_k(dend)$k
  
  # Set up a plot file name for the dendrogram with a rainbow pattern and output directory
  # Plotting the dendrogram with branches colored according to the optimal number of clusters
  # Saving the plot to a file in PNG format
  plot_file <- paste(
    clusters_dir,
    "/",
    matrix_name,
    "/",
    "dend_",
    matrix_name,
    "_rainbow_pattern.png",
    sep = ""
  )
  png(plot_file, height = 600)
  par(mai = c(4,1,1,1))
  cols_dend <- color_branches(
    dend,
    k = clust_caunt
  )
  plot(cols_dend)
  dev.off()
  plot_file <- paste(
    clusters_dir,
    "/",
    matrix_name,
    "/",
    "dend_",
    matrix_name,
    "_label_pattern.png",
    sep = ""
  )
  png(plot_file, height = 600)
  
  # set margin size for plot
  par(mai = c(4,1,1,1))
  
  # color dendrogram based on the pattern of the clusters
  cols_dend <- color_branches(
    dend,
    col = cols_pattern[dend %>% labels]
  )
  
  # plot dendrogram with colored branches
  plot(cols_dend)
  dev.off()
  
  # cut dendrogram into clusters based on the optimal k value
  clust_1 <- cutree(h_clust_1, k=clust_caunt)
  
  # create a binary matrix to show the membership of each document in the clusters
  clust_matrix_1 <- matrix(0, doc_count, clust_caunt)
  rownames(clust_matrix_1) <- doc_names
  for (doc in 1:doc_count) {
    clust_matrix_1[doc, clust_1[doc]] <- 1
  }
  
  # save the matrix plot
  plot_file <- paste(
    clusters_dir,
    "/",
    matrix_name,
    "/",
    "matrix_",
    matrix_name,
    sep = ""
  )
  png(plot_file)
  corrplot(clust_matrix_1)
  dev.off()
  
  # return the dendrogram
  return(dend)
}


# Load the ggplot2 library
library(ggplot2)

# Define a function to analyze K-means clustering
k_means_analyze <- function(my_matrix, my_centers) {
  
  # Convert the input matrix to a data frame
  asmatrix <- as.matrix(my_matrix)
  
  # Get the name of the input matrix
  matrix_name <- deparse(substitute(my_matrix))
  
  # Perform K-means clustering
  kmeans <- kmeans(my_matrix, centers = my_centers)
  
  # Add a column to the data frame with the assigned cluster labels
  plot_data <- data.frame(x = asmatrix[,1], y = asmatrix[,2], cluster = as.factor(kmeans$cluster), titles = rownames(asmatrix))
  
  # Generate a scatter plot of the data with color-coded clusters and titles for each point
  ggplot(plot_data, aes(x = x, y = y, color = cluster)) +
    geom_point() +
    geom_text(aes(label = titles), vjust = -0.5) +
    labs(x = "X", y = "Y", title = "Cluster Plot")
  
  # Save the plot to a graphics file
  plot_file <- paste(
    clusters_dir, # directory where the file will be saved
    "/", # forward slash separator
    matrix_name, # name of the input matrix
    "/", # forward slash separator
    my_centers, # number of centers used for K-means clustering
    "kmeans.png", # file name extension
    sep = "" # no separator needed
  )
  ggsave(plot_file)
}



# This function generates a plot of the Fowlkes-Mallows index for two dendrograms
# dend_1: the first dendrogram
# dend_2: the second dendrogram
plot_FMIndex <- function(dend_1, dend_2) {
  
  # Extract the names of the dendrograms
  matrix_1_name <- attr(dend_1, "name")
  matrix_2_name <- attr(dend_2, "name")
  
  # Define the output file name
  file_name <- paste(
    clusters_dir,
    "/",
    comparison_dir,
    "/",
    "FM_Index_",
    matrix_1_name,
    "_",
    matrix_2_name,
    sep = ""
  )
  
  # Create a PNG file to save the plot
  png(file_name)
  
  # Generate the plot using the Bk_plot function
  Bk_plot(
    dend_1,
    dend_2,
    add_E = FALSE,
    rejection_line_asymptotic = FALSE,
    main = paste("Indeks Fawlkes'a - Mallows'a dla dendogramów", matrix_1_name, matrix_2_name),
    cex.main = 0.8
  )
  
  # Close the PNG file
  dev.off()
}

dend_1 = analyze_matrix(dtm_tf_bounds1)                       
dend_2 = analyze_matrix(dtm_tf_bounds2)
dend_3 = analyze_matrix(dtm_tfidf_bounds1)                       
dend_4 = analyze_matrix(dtm_tfidf_bounds2)

k_means_analyze(dtm_tf_bounds1, 5)
k_means_analyze(dtm_tf_bounds1, 10)
k_means_analyze(dtm_tf_bounds1, 2)

k_means_analyze(dtm_tf_bounds2, 5)
k_means_analyze(dtm_tf_bounds2, 10)
k_means_analyze(dtm_tf_bounds2, 2)


plot_FMIndex(dend_1, dend_2)
plot_FMIndex(dend_1, dend_3)
plot_FMIndex(dend_1, dend_4)
plot_FMIndex(dend_2, dend_3)
plot_FMIndex(dend_2, dend_4)
plot_FMIndex(dend_3, dend_4)