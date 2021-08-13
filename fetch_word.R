library(tidyverse)
library(httr)
library(jsonlite)

# Loading Vocab Data ----
getwd()
setwd("/Data")

# Load data ----
voc = read_tsv("Vocabulary.tsv", col_names = F)

# Rename columns
voc = voc %>%
  rename(english = 1, defintion = 2)

# Fill up all definition values with NA
voc$defintion = NA

# Store as list ----
voc_ls = list(word = voc$english, definition = voc$defintion)

# Functions ---------------------------------------------------------------

# Set word used in function for test purposes
word = "able"

# The fetchWord function that's actually going to fetch dictionary answer
fetchWord = function(word) {
  
  # Download API data for word.
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

# From voc_ls to voc table ----
voc = tibble(word = voc_ls$word, definition = voc_ls$definition)

# How many non NA rows?
sum(!is.na(voc$definition))

# Add zwischenschritte 
# final_voca %>% 
#   add_row() %>% 
#   view()

# Save voc with words where only NA defintion
write_csv(voc, "voc-with-na-words.csv", col_names = F)

# Remove any rows with any NAs in them: ----
nrow(voc)
voc = voc %>% na.exclude()
nrow(voc)

# Finally saving onto csv file. ----
write_csv(voc, "voc.csv", col_names = F)






# Words API ---------------------------------------------------------------


library(tidyverse)
library(httr)
library(jsonlite)

# Loading Vocab Data ----
getwd()
setwd("Working Directory")

# Load data ----
voc = read_delim("vocabulary.tsv", col_names = T)

# Rename columns
voc = voc %>%
  rename(german = 1, defintion = 2)

# Fill up all definition values with NA
voc$defintion = NA

# Store as list ----
voc_ls = list(german = voc$german, definition = voc$defintion)


# Functions ---------------------------------------------------------------

library(httr)

url <- "https://wordsapiv1.p.rapidapi.com/words/incredible/definitions"

response <- VERB("GET", url,
                 add_headers(x_rapidapi-key = "df47ef04a6msha8ec9624cc842edp108531jsn13ee12225b98",
                             x_rapidapi-host = "wordsapiv1.p.rapidapi.com"),
                 content_type("application/octet-stream"))

content(response, "text")



# Set word used in function for test purposes
word = "able"

# The fetchWord function that's actually going to fetch dictionary answer
fetchWord = function(word) {
  
  # Download API data for word.
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

# From voc_ls to voc table ----
voc = tibble(word = voc_ls$word, definition = voc_ls$definition)

# How many non NA rows?
sum(!is.na(voc$definition))

# Add zwischenschritte 
# final_voca %>% 
#   add_row() %>% 
#   view()

# Save voc with words where only NA defintion
write_csv(voc, "voc-with-na-words.csv", col_names = F)

# Remove any rows with any NAs in them: ----
nrow(voc)
voc = voc %>% na.exclude()
nrow(voc)

# Finally saving onto csv file. ----
write_csv(voc, "voc.csv", col_names = F)


