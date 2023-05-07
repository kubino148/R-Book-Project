# wczytanie bibliotek
library(topicmodels)

# wczytanie i wykonanie skryptu frequency_matrix.R
source_file = "./frequency_matrix.R"
source(source_file)

if (!dir.exists("./topics")) {
  dir.create("./topics")
}

perform_LDA <- function(dtm, dtm_name, topics_count = 4) {
  # Tworzenie katalogu na wyniki
  output_dir <- paste0("./topics/topics_", topics_count, "_", dtm_name)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Analiza ukrytej alokacji Dirichlet'a
  lda_model <- LDA(
    dtm,
    topics_count,
    method = "Gibbs",
    control = list(
      burnin = 2000,
      thin = 100,
      iter = 3000
    )
  )
  results <- posterior(lda_model)
  cols <- c("purple", "turquoise", "orange", "lightskyblue", "darkseagreen", "hotpink", "dodgerblue", "darkmagenta", "khaki", "darkolivegreen")
  
  # Prezentacja tematów
  for (topic_no in 1:topics_count) {
    topic_file <- paste(
      output_dir,
      paste("Temat", topic_no, ".png"),
      sep = "/"
    )
    topic <- tail(sort(results$terms[topic_no,]), 20)
    png(topic_file)
    par(mai = c(1, 2, 1, 1))
    barplot(
      topic,
      horiz = T,
      las = 1,
      main = paste("Temat", topic_no),
      xlab = "Prawdopodobieństwo",
      col = cols[topic_no]
    )
    dev.off()
  }
  
  # Prezentacja dokumentów
  plot_file <- paste(
    output_dir,
    "Dokumenty.png",
    sep = "/"
  )
  png(plot_file, width = 880)
  par(mai = c(1, 4, 1, 1))
  barplot(
    t(results$topics),
    horiz = T,
    las = 1,
    main = "Dokumenty",
    xlab = "Prawdopodobieństwo",
    col = cols,
    xlim = c(0, 1)
  )
  dev.off()
  
  return(list(lda_model = lda_model, results = results))
}

perform_LDA(dtm_tf_bounds1, "macierz1", 4)
perform_LDA(dtm_tf_bounds1, "macierz1", 5)
perform_LDA(dtm_tf_bounds1, "macierz1", 10)
perform_LDA(dtm_tf_bounds2, "macierz2", 4)
perform_LDA(dtm_tf_bounds2, "macierz2", 5)
perform_LDA(dtm_tf_bounds2, "macierz2", 10)
# perform_LDA(tdm_tf_bounds1, "macierz3", 4)
# perform_LDA(tdm_tf_bounds1, "macierz3", 5)
# perform_LDA(tdm_tf_bounds1, "macierz3", 10)
