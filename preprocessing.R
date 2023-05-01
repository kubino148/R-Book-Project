# załadowanie bibliotek
library(tm)
library(hunspell)

# utworzenie korpusu dokumentów
corpus_dir <- "./materialy"
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

# dodatkowe funkcje transformujące
paste_paragraphs <- function(text){
  paste(text, collapse = " ")
}
remove_char <- content_transformer(
  function(text, char) gsub(char, "", text)
)
cut_extensions <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$", 
    "", 
    meta(document, "id")
  )
  return(document)
}

# wstępne przetwarzanie
corpus <- tm_map(corpus, content_transformer(paste_paragraphs))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
stoplist_file <- "./stopwords_pl.txt"
stoplist <- readLines(stoplist_file, encoding = "UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, remove_char, intToUtf8(8722))
corpus <- tm_map(corpus, remove_char, intToUtf8(190))
corpus <- tm_map(corpus, stripWhitespace)

# lematyzacja
polish <- dictionary("./dicts/pl_PL")
lemmatize <- function(text){
  parsed_text_vec <- unlist(hunspell_parse(text, dict = polish))
  lemmatized_text_vec <- hunspell_stem(parsed_text_vec, polish)
  for (t in 1:length(lemmatized_text_vec)) {
    if(length(lemmatized_text_vec[[t]]) == 0) { 
      lemmatized_text_vec[t] <- parsed_text_vec[t]
    }
    if(length(lemmatized_text_vec[[t]])  > 1) {
      lemmatized_text_vec[t] <- lemmatized_text_vec[[t]][1]
    }
    lemmatized_text <- paste(lemmatized_text_vec, collapse = " ")
  }
  return(lemmatized_text)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))

corpus <- tm_map(corpus, cut_extensions)

# eksport przetworzonego korpusu do plików tekstowych
preprocessed_dir <- "./materialy_processed"
dir.create(preprocessed_dir)
writeCorpus(corpus, preprocessed_dir)
