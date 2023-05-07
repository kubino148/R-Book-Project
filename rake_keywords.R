library(stringr)

rake_dir <- function(stop_words_file = "./stopwords_pl.txt", phrase_length = 2, word_min_length = 3, word_max_length = Inf, min_word_occurrences = 1, min_phrase_occurrences = 1, min_keyword_score = 1) {
  # Wczytanie pliku ze stop words
  stop_words <- readLines(stop_words_file)
  
  # Utworzenie folderu 'rake_words'
  words_dir <- "./rake_keywords"
  if (!dir.exists(words_dir)) {
    dir.create(words_dir)
  }
  
  # Przetwarzanie każdego pliku tekstowego w folderze 'materialy'
  files <- list.files("./materialy")
  for (file in files) {
    # Wczytanie tekstu z pliku
    text <- readLines(paste0("./materialy/", file), encoding = "UTF-8")
    text <- str_to_lower(text)
    text <- str_replace_all(text, "[^[:alnum:] ]", " ")
    
    # Przygotowanie wyniku dla danego pliku
    file_result <- list()
    
    # Podobne jak w funkcji rake
    words <- str_split(text, " ")[[1]]
    word_counts <- table(words)
    words <- names(word_counts)
    words <- words[!words %in% stop_words]
    words <- words[nchar(words) >= word_min_length & nchar(words) <= word_max_length]
    
    phrases <- list()
    for (i in phrase_length) {
      phrases[[i]] <- unlist(lapply(seq_along(words), function(j) paste(words[j:min(j + i - 1, length(words))], collapse = " ")))
    }
    phrases <- unlist(phrases)
    
    phrase_counts <- table(phrases)
    phrases <- names(phrase_counts)
    phrases <- phrases[nchar(phrases) >= word_min_length * phrase_length]
    phrases <- phrases[phrase_counts[phrases] >= min_phrase_occurrences]
    words <- words[word_counts[words] >= min_word_occurrences]
    
    scores <- rep(0, length(phrases))
    for (i in seq_along(phrases)) {
      words_in_phrase <- unlist(str_split(phrases[i], " "))
      for (j in seq_along(words_in_phrase)) {
        scores[i] <- scores[i] + word_counts[words_in_phrase[j]] - 1
      }
      scores[i] <- scores[i] / (length(words_in_phrase) - 1)
    }
    
    file_result$keywords <- c(words, phrases[scores >= min_keyword_score])
    
    # Zapisanie pliku tekstowego z kluczowymi słowami dla danego pliku
    words_file <- paste0(words_dir, "/", str_remove(file, "\\.txt"), "_keywords.txt")
    writeLines(file_result$keywords, words_file)
  }
}

rake_dir()
