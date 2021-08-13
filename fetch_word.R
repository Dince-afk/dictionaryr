library(tidyverse)
library(httr)
library(jsonlite)

# The fetchWord function that's actually going to fetch dictionary answer
fetchWord = function(word) {
  
  # Download API data for word
  response = GET(
    paste("https://api.dictionaryapi.dev/api/v2/entries/en_US/",
          # Insert word parameter
          word,
          sep = "")
  )
  print(response$status_code)
  if (response$status_code == 200L) {
    
    data = fromJSON(rawToChar(response$content))
    
    # def can hold several definitions. 
    def = paste(
      # Type of word: "Verb, Noun, Adjective"
      data$meanings[[1]]$partOfSpeech, 
      # New line:
      "\n\n",
      # Definition:
      "Definition: ",
      data$meanings[[1]]$definitions[[1]]$definition,
      # New line:
      "\n\n",
      # Synonyms:
      "Synonyms: ",
      paste(data$meanings[[1]]$definitions[[1]]$synonyms[[1]], collapse = ", "),
      # New Line:
      "\n\n",
      # Example:
      "Example: ",
      data$meanings[[1]]$definitions[[1]]$example, 
      sep = ""
    )
    
    def = paste(def, collapse = "\n\n-------------\n\n")
    return(def)
  } else {
    print("Word fetching failed.")
    return(NA)
  }
}

# Testing -----------------------------------------------------------------

# Function test 1: 
fetchWord(word = "abide")

# Not found test:
fetchWord(word = "aggrandizement") 

# Several elements (definitions) in one.
fetchWord(word = "able") 

# Working loop! ---- 

# Each loop is limited to 250 words. The API allows 250 request at once. After
# more than that it throws error 429.
i = 1
lim = 251
for (word in voc_ls$word[i:100000]) {
  if (i <= lim) {
    print(word)
    voc_ls$definition[i] = fetchWord(word = word)
    i = i+1
    print(i)
  }
}


