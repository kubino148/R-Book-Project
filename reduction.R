# Function to generate PCA and LSA graphs for given matrix
# Input: matrix - frequency matrix
generate_PCA_LSA_graphs <- function(matrix) {
  
  # Load library
  library(lsa)
  
  # Prepare data
  legend <- paste(
    paste(
      "d",
      1:length(rownames(matrix)),
      sep = ""
    ),
    rownames(matrix),
    sep = " -> "
  )
  
  options(scipen = 5)
  
  # Perform PCA analysis
  pca_model <- prcomp(matrix)
  
  x <- pca_model$x[,1]
  y <- pca_model$x[,2]
  
  pca_plot_file <- paste0("reduction_graphs/pca_", deparse(substitute(matrix)), ".png")
  png(pca_plot_file)
  
  # Generate PCA plot
  plot(
    x,
    y,
    main = paste0("Principal Component Analysis ", deparse(substitute(matrix))),
    xlab = "PC1",
    ylab = "PC2",
    col = "purple",
    pch = 16
  )
  # Add text labels to data points
  text(
    x,
    y,
    paste(
      "d",
      1:length(rownames(matrix)),
      sep = ""
    ),
    col = "purple",
    pos = 1
  )
  # Add legend
  legend(
    "top",
    legend,
    cex = 0.6,
    text.col = "purple"
  )
  dev.off()
  
  # Perform LSA analysis
  # Singular Value Decomposition
  lsa_model <- lsa(matrix)
  
  coord_docs <- lsa_model$dk %*% diag(lsa_model$sk)
  coord_terms <- lsa_model$tk %*% diag(lsa_model$sk)
  terms_importance <- diag(
    lsa_model$tk %*% diag(lsa_model$sk) %*% t(diag(lsa_model$sk)) %*% t(lsa_model$tk)
  )
  important_terms <- names(
    tail(
      sort(terms_importance),
      30
    )
  )
  current_terms <- important_terms
  x1 <- coord_docs[,1]
  y1 <- coord_docs[,2]
  x2 <- coord_terms[current_terms,1]
  y2 <- coord_terms[current_terms,2]
  
  lsa_plot_file <- paste0("reduction_graphs/lsa_", deparse(substitute(matrix)), ".png")
  png(lsa_plot_file)
  
  # Generate LSA plot
  plot(
    x1,
    y1,
    main = paste0("Latent Semantic Analysis ", deparse(substitute(matrix))),
    xlab = "SD1",
    ylab = "SD2",
    col = "purple",
    pch = 16
  )
  # Add text labels to data points
  text(
    x1,
    y1,
    paste(
      "d",
      1:length(rownames(matrix)),
      sep = ""
    ),
    col = "purple",
    pos = 1
  )
  # Add important terms as magenta points and labels
  points(
    x2, 
    y2,
    pch = 15,
    col = "magenta"
  )
  text(
    x2,
    y2,
    rownames(coord_terms[current_terms,]),
    col = "magenta"
  )
  # Add legend
  legend(
    "topleft",
    legend,
    cex = 0.6,
    text.col = "purple"
  )
  dev.off()
}

# Load and execute
source_file = "./frequency_matrix.R"
source(source_file)

generate_PCA_LSA_graphs(dtm_tf_bounds1)
generate_PCA_LSA_graphs(tdm_tf_bounds1)

generate_PCA_LSA_graphs(dtm_tf_bounds1)
generate_PCA_LSA_graphs(dtm_tf_bounds1)

generate_PCA_LSA_graphs(dtm_tfidf_bounds1)
generate_PCA_LSA_graphs(dtm_tfidf_bounds1)

generate_PCA_LSA_graphs(dtm_tfidf_bounds2)
generate_PCA_LSA_graphs(dtm_tfidf_bounds2)
