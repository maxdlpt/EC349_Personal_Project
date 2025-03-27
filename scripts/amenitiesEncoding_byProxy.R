#-------------------------------------------------------------------------------
#                             AMENITIES ENCODING
#                                  by proxy
#-------------------------------------------------------------------------------
library(tidyverse)
library(text2vec)



#---------------------------------- SETUP --------------------------------------
# Separate and Count Unique Amenities
df_amenities <- df_base %>%
  mutate(amenities = str_remove_all(amenities, "\\[|\\]|\"")) %>%
  separate_rows(amenities, sep = ", ") %>%
  group_by(amenities) %>%
  summarise(freq_count = n(), .groups = "drop")

# Set Frequency & Cosine Similarity freq_threshold
freq_threshold <- 100
cos_sim_threshold <- 0.50 

df_frequent <- df_amenities %>% filter(freq_count >= freq_threshold)
df_rare     <- df_amenities %>% filter(freq_count < freq_threshold)



#-------------------------------- TOKENISE -------------------------------------
clean_text <- function(txt) {
  txt %>%
    tolower() %>%
    str_replace_all("\\\\u[0-9A-Fa-f]{4}", "") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("[[:digit:]]", " ") %>%
    str_squish()
}

df_amenities <- df_amenities %>%
  mutate(amenity_clean = clean_text(amenities))

token_list <- space_tokenizer(df_amenities$amenity_clean)



#------------------------- TRAIN GLOVE EMBEDDINGS ------------------------------
it <- itoken(token_list, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

glove_model <- GlobalVectors$new(rank = 50, x_max = 10, learning_rate = 0.15)
w_main <- glove_model$fit_transform(tcm, n_iter = 20)
w_context <- glove_model$components
word_vectors <- w_main + t(w_context)



#-------------------- COMPUTE UNIQUE AMENITIES EMBEDDINGS ----------------------
get_phrase_embedding <- function(tokens, embedding_mat) {
  valid_tokens <- tokens[tokens %in% rownames(embedding_mat)]
  if (length(valid_tokens) == 0) return(rep(0, ncol(embedding_mat)))
  colMeans(embedding_mat[valid_tokens, , drop = FALSE])
}

emb_matrix <- t(vapply(token_list, get_phrase_embedding, numeric(ncol(word_vectors)), word_vectors))

df_amenities <- df_amenities %>%
  mutate(row_id = row_number())

emb_frequent <- emb_matrix[df_amenities$freq_count >= freq_threshold, , drop = FALSE]
emb_rare     <- emb_matrix[df_amenities$freq_count < freq_threshold, , drop = FALSE]

df_frequent <- df_amenities %>% filter(freq_count >= freq_threshold)
df_rare     <- df_amenities %>% filter(freq_count < freq_threshold)



#------------------- MATCH RARE AMENITIES TO CLOSEST PROXY ---------------------
cosine_sim <- function(a, b) {
  if (all(a == 0) || all(b == 0)) return(0)
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

# Get index and max similarity
match_info <- vapply(seq_len(nrow(emb_rare)), function(i) {
  sims <- apply(emb_frequent, 1, function(fvec) cosine_sim(emb_rare[i, ], fvec))
  best_idx <- which.max(sims)
  best_sim <- sims[best_idx]
  if (is.na(best_sim) || best_sim < cos_sim_threshold) {
    return(c(idx = NA_integer_, sim = NA_real_))
  }
  c(idx = best_idx, sim = best_sim)
}, numeric(2))

# Assign proxy amenities
df_rare$proxy_amenity <- ifelse(
  is.na(match_info["idx", ]),
  "other",
  df_frequent$amenities[match_info["idx", ]]
)

# Self-mapping for frequent
df_frequent$proxy_amenity <- df_frequent$amenities



#------------------------------- SAVE RESULTS ----------------------------------
df_amenities_encoding_key <- bind_rows(df_frequent, df_rare) %>%
  select(amenities, freq_count, proxy_amenity)
