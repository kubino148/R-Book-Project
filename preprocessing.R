# Load necessary libraries
library(tm) # for text mining operations
library(hunspell) # for lemmatization using dictionaries

# Create a corpus of documents
corpus_dir <- "./materialy"
corpus <- VCorpus(
  DirSource(
    corpus_dir, # directory containing text documents
    "UTF-8", # encoding of text documents
    "*.txt" # filter for text documents
  ),
  readerControl = list(
    language = "pl_PL" # set language to Polish
  )
)

# Define additional functions for corpus transformation
paste_paragraphs <- function(text){
  paste(text, collapse = " ") # concatenate paragraphs of a document
}
remove_char <- content_transformer(
  function(text, char) gsub(char, "", text) # remove specified characters from text
)
cut_extensions <- function(document){
  meta(document, "id") <- gsub(
    "\\.txt$", # remove the extension .txt from document IDs
    "", 
    meta(document, "id")
  )
  return(document)
}

# Perform initial preprocessing on the corpus
corpus <- tm_map(corpus, content_transformer(paste_paragraphs)) # concatenate paragraphs
corpus <- tm_map(corpus, removeNumbers) # remove numbers
corpus <- tm_map(corpus, removePunctuation) # remove punctuation
corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lowercase
stoplist_file <- "./stopwords_pl.txt" # file containing stop words for Polish language
stoplist <- readLines(stoplist_file, encoding = "UTF-8") # read stop words from file
corpus <- tm_map(corpus, removeWords, stoplist) # remove stop words
corpus <- tm_map(corpus, remove_char, intToUtf8(8722)) # remove specified character
corpus <- tm_map(corpus, remove_char, intToUtf8(190)) # remove specified character
corpus <- tm_map(corpus, stripWhitespace) # remove extra whitespace

# Perform lemmatization on the corpus using a Polish dictionary
polish <- dictionary("./dicts/pl_PL") # load Polish dictionary
lemmatize <- function(text){
  parsed_text_vec <- unlist(hunspell_parse(text, dict = polish)) # parse the text and retrieve the base forms of words
  lemmatized_text_vec <- hunspell_stem(parsed_text_vec, polish) # retrieve the stem of each word using the dictionary
  for (t in 1:length(lemmatized_text_vec)) {
    if(length(lemmatized_text_vec[[t]]) == 0) { 
      lemmatized_text_vec[t] <- parsed_text_vec[t] # if a word has no stem, use its base form
    }
    if(length(lemmatized_text_vec[[t]])  > 1) {
      lemmatized_text_vec[t] <- lemmatized_text_vec[[t]][1] # if a word has multiple stems, use the first one
    }
    lemmatized_text <- paste(lemmatized_text_vec, collapse = " ") # concatenate the lemmatized words into a single string
  }
  return(lemmatized_text)
}
corpus <- tm_map(corpus, content_transformer(lemmatize)) # apply the lemmatization function

corpus <- tm_map(corpus, cut_extensions) # remove file extensions from document IDs

# Export processed corpus to text files
preprocessed_dir <- "./materialy_processed"
dir.create(preprocessed_dir)
writeCorpus(corpus, preprocessed_dir)
