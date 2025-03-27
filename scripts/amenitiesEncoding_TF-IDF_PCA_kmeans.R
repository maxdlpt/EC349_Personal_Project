#-------------------------------------------------------------------------------
#                               AMENITIES ENCODING
#                   using TF-IDF, PCA, and k-means clustering 
#-------------------------------------------------------------------------------
library(tidyverse)
library(tm)
library(cluster)
library(factoextra)

#------------------------ CREATE UNIQUE AMENITIES LIST -------------------------
amenities <- df_base %>%
  mutate(amenities = str_remove_all(amenities, "\\[|\\]|\"")) %>%
  separate_rows(amenities, sep = ", ") %>%
  group_by(amenities) %>%
  summarise(freq_count = n()) %>%
  ungroup()

write_csv(amenities, "unique_amenities.csv")


#--------------------------- COUNT AMENITY FREQUENCY ---------------------------
amenities_counts <- df_base %>%
  mutate(amenities = str_remove_all(amenities, "\\[|\\]|\"")) %>%
  separate_rows(amenities, sep = ", ") %>%
  group_by(amenities) %>%
  summarise(freq_count = n()) %>%
  ungroup()

amenities <- amenities %>%
  left_join(amenities_counts, by = "amenities")

head(amenities)


#---------------------------------- TF-IDF -------------------------------------
amenities <- read_csv("unique_amenities.csv")
head(amenities)

clean_text <- function(text) {
  text <- gsub("\\\\u[0-9A-Fa-f]{4}", "", text) # remove unicode sequences
  text <- tolower(text)  # Convert to lowercase
  text <- gsub("[[:punct:]]", " ", text)  # Remove punctuation
  text <- gsub("[0-9]", "", text)  # Remove numbers
  text <- gsub("\\s+", " ", text)  # Remove extra whitespace
  return(trimws(text))  # Trim leading/trailing spaces
}

amenities$amenities_clean <- sapply(amenities$amenities, clean_text)
amenities_corpus <- Corpus(VectorSource(amenities$amenities_clean))

# Preprocess the text
amenities_corpus <- amenities_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

# Convert to a document-term matrix (DTM)
dtm <- DocumentTermMatrix(amenities_corpus)
amenities_matrix <- as.matrix(dtm)

# Compute TF-IDF values
tfidf <- weightTfIdf(dtm)
tfidf_matrix <- as.matrix(tfidf)

#----------------------------------- PCA ---------------------------------------
tfidf_matrix <- scale(tfidf_matrix)

pca <- prcomp(tfidf_matrix, scale. = TRUE)
reduced_data <- pca$x[, 1:100]  

#--------------------------------- K-MEANS -------------------------------------
set.seed(123)
k <- 50 # of clusters
km_res <- kmeans(reduced_data, centers = k, nstart = 25)

fviz_cluster(list(data = as.data.frame(reduced_data), cluster = km_res$cluster))


#------------------------------ SAVE CLUSTERS ----------------------------------
amenities$cluster <- km_res$cluster
head(amenities)
write_csv(amenities, "clustered_amenities.csv")
