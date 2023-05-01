generate_PCA_LSA_graphs <- function(matrix) {
  
  # wczytanie bibliotek
  library(lsa)
  
  # przygotowanie danych
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
  
  # analiza głównyh składowych
  pca_model <- prcomp(matrix)
  
  x <- pca_model$x[,1]
  y <- pca_model$x[,2]

  pca_plot_file <- paste0("reduction_graphs/pca_", deparse(substitute(matrix)), ".png")
  png(pca_plot_file)
  
  plot(
    x,
    y,
    main = paste0("Analiza głównych składowych ", deparse(substitute(matrix))),
    xlab = "PC1",
    ylab = "PC2",
    col = "purple",
    pch = 16
    
  )
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
  legend(
    "top",
    legend,
    cex = 0.6,
    text.col = "purple"
  )
  dev.off()
  
  # analiza ukrytych wymiarów semantycznych
  # dekompozycja wg wartości osobliwych
  lsa_model <- lsa(matrix)
  
  coord_docs <- lsa_model$dk%*%diag(lsa_model$sk)
  coord_terms <- lsa_model$tk%*%diag(lsa_model$sk)
  terms_importance <- diag(
    lsa_model$tk%*%diag(lsa_model$sk)%*%t(diag(lsa_model$sk))%*%t(lsa_model$tk)
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

  plot(
    x1,
    y1,
    main = paste0("Analiza ukrytych wymiarów semantycznych ", deparse(substitute(matrix))),
    xlab = "SD1",
    ylab = "SD2",
    col = "purple",
    pch = 16
    
  )
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
  legend(
    "topleft",
    legend,
    cex = 0.6,
    text.col = "purple"
  )
  dev.off()
}
# wczytanie i wykonanie skryptu frequency_matrix.R
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
