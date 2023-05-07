library(tm)

# Create document Corpus
corpus_dir <- "materialy_processed"
corpus <- VCorpus(
  DirSource(
    corpus_dir,
    "UTF-8",
    "*.txt"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)
cut_extensions <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$", 
    "", 
    meta(document, "id")
  )
  return(document)
}
corpus <- tm_map(corpus, cut_extensions)

# TF matrix with small bounds
tdm_tf_bounds1 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTf,
    bounds = list(
      global = c(2, 8)
    )
  )
)

dtm_tf_bounds1 <- as.DocumentTermMatrix(t(tdm_tf_bounds1))

# TF matrix with big bounds
tdm_tf_bounds2 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTf,
    bounds = list(
      global = c(10, 20)
    )
  )
)

dtm_tf_bounds2 <- as.DocumentTermMatrix(t(tdm_tf_bounds2))

# TF-IDF matrix with small bounds
tdm_tfidf_bounds1 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2, 8)
    )
  )
)

dtm_tfidf_bounds1 <- as.DocumentTermMatrix(t(tdm_tfidf_bounds1))

# TF-IDF matrix with big bounds
tdm_tfidf_bounds2 <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(10, 20)
    )
  )
)

dtm_tfidf_bounds2 <- as.DocumentTermMatrix(t(tdm_tfidf_bounds2))

tdm_tf_bounds1_m <- as.matrix(tdm_tf_bounds1)
tdm_tf_bounds1_df <- as.data.frame(tdm_tf_bounds1_m)

dtm_tf_bounds1_m <- as.matrix(dtm_tf_bounds1)
dtm_tf_bounds1_df <- as.data.frame(dtm_tf_bounds1_m)

tdm_tf_bounds2_m <- as.matrix(tdm_tf_bounds2)
tdm_tf_bounds2_df <- as.data.frame(tdm_tf_bounds2_m)

dtm_tf_bounds2_m <- as.matrix(dtm_tf_bounds2)
dtm_tf_bounds2_df <- as.data.frame(dtm_tf_bounds2_m)

tdm_tfidf_bounds1_m <- as.matrix(tdm_tfidf_bounds1)
tdm_tfidf_bounds1_df <- as.data.frame(tdm_tfidf_bounds1_m)

dtm_tfidf_bounds1_m <- as.matrix(dtm_tfidf_bounds1)
dtm_tfidf_bounds1_df <- as.data.frame(dtm_tfidf_bounds1_m)

tdm_tfidf_bounds2_m <- as.matrix(tdm_tfidf_bounds2)
tdm_tfidf_bounds2_df <- as.data.frame(tdm_tfidf_bounds2_m)

dtm_tfidf_bounds2_m <- as.matrix(dtm_tfidf_bounds2)
dtm_tfidf_bounds2_df <- as.data.frame(dtm_tfidf_bounds2_m)

tdm_tf_bounds1_matrix_file <- "./matrix_files/tdm_tf_bounds1.csv"
write.table(
  tdm_tf_bounds1_m, 
  tdm_tf_bounds1_matrix_file, 
  sep = ";", 
  dec = ",", 
  col.names = NA
)

tdm_tf_bounds2_matrix_file <- "./matrix_files/tdm_tf_bounds2.csv"
write.table(
  tdm_tf_bounds2_m, 
  tdm_tf_bounds2_matrix_file, 
  sep = ";", 
  dec = ",", 
  col.names = NA
)

tdm_tfidf_bounds1_matrix_file <- "./matrix_files/tdm_tfidf_bounds1.csv"
write.table(
  tdm_tf_bounds1_m, 
  tdm_tfidf_bounds1_matrix_file, 
  sep = ";", 
  dec = ",", 
  col.names = NA
)

tdm_tfidf_bounds2_matrix_file <- "./matrix_files/tdm_tfidf_bounds2.csv"
write.table(
  tdm_tf_bounds2_m, 
  tdm_tfidf_bounds2_matrix_file, 
  sep = ";", 
  dec = ",", 
  col.names = NA
)
