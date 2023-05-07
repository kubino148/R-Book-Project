library(wordcloud)

source_file = "./frequency_matrix.R"
source(source_file)

# Stworzenie folderu 'words_phrases'
words_phrases_dir <- "./words_phrases"
if (!dir.exists(words_phrases_dir)) {
  dir.create(words_phrases_dir)
}

create_wordclouds <- function(matrix, matrix_name) {
  # Tworzenie folderu o nazwie macierzy wewnątrz folderu 'words_phrases'
  output_dir <- paste0(words_phrases_dir, "/", matrix_name)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  for (doc_no in 1:length(corpus)) {
    cloud_file <- paste(
      output_dir,
      paste(corpus[[doc_no]]$meta$id, ".png", sep = ""),
      sep = "/"
    )
    png(cloud_file)
    par(mai = c(0, 0, 0, 0))
    wordcloud(
      corpus[doc_no],
      max.words = 200,
      colors = brewer.pal(8, "PuOr")
    )
    dev.off()
  }
  
  cloud_file <- paste(
    output_dir,
    "cloud.png",
    sep = "/"
  )
  png(cloud_file)
  par(mai = c(0, 0, 0, 0))
  wordcloud(
    corpus,
    max.words = 200,
    colors = brewer.pal(8, "PuOr")
  )
  dev.off()
}

# Przykład użycia funkcji:
create_wordclouds(dtm_tf_bounds1, "dtm_tf_bounds_1")
create_wordclouds(dtm_tf_bounds2, "dtm_tf_bounds_2")
create_wordclouds(tdm_tfidf_bounds1, "tdm_tfidf_bounds_1")
create_wordclouds(tdm_tfidf_bounds2, "tdm_tfidf_bounds_2")
