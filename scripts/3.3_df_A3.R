#-------------------------------------------------------------------------------
#                             CREATE & PREP DF_A3
#                             feature engineering
#-------------------------------------------------------------------------------
library(tidyverse)
library(stringr)

#----------------------------- `bathrooms_text` --------------------------------
df_base <- df_base %>%
  mutate(bathrooms_is_shared = as.factor(
    if_else(str_detect(tolower(as.character(bathrooms_text)), "shared"), 
            "t",
            "f"
    )))

summary(df_base$bathrooms_is_shared)



#----------------------------- `host_location` ---------------------------------
df_base <- df_base %>%
  mutate(host_location = case_when(
    str_detect(tolower(as.character(host_location)), "london") ~ "in London",
    str_detect(tolower(as.character(host_location)), "united kingdom") ~ "in UK",
    !is.na(host_location) ~ "outside UK",
    TRUE ~ NA_character_
  )) %>%
  mutate(host_location = as.factor(host_location))
summary(df_base$host_location)



#--------------------------- `host_verifications` ------------------------------
df_base <- df_base %>%
  mutate(host_nContact_options = str_count(host_verifications, "\\w+"))
summary(df_base$host_nContact_options)



#----------------------------- `property_type` ---------------------------------
# Is it shared?
df_base <- df_base %>%
  mutate(property_is_shared = as.factor(
    if_else(str_detect(tolower(as.character(property_type)), "shared|room"), 
            "t",
            "f"
    )))

summary(df_base$property_is_shared)

# reduce factor cardinality
df_base <- df_base %>%
  mutate(property_type_simplified = case_when(
    str_detect(tolower(property_type), 
               "apartment|apt|condo|loft|unit|flat|aparthotel|place") ~ "apartment",
    str_detect(tolower(property_type), 
               "home|house|townhouse|bungalow|villa|cottage|cabin|chalet|guesthouse|hut") ~ "house",
    str_detect(tolower(property_type), 
               "hotel|hostel|suite|boutique|bed and breakfast") ~ "hotel",
    TRUE ~ "other"
  )) %>%
  mutate(property_type_simplified = as.factor(property_type_simplified))
summary(df_base$property_type_simplified)



#-------------------------------- `amenities` ----------------------------------
df_base <- df_base %>%
  mutate(
    amenities_clean = str_remove_all(amenities, "\\[|\\]|\""),
    amenities_count = if_else(
      str_trim(amenities_clean) == "", 0L, str_count(amenities_clean, ",") + 1L))
summary(df_base$amenities_count)

source(file.path(getwd(),"scripts", "amenitiesEncoding_byProxy.R"))
source(file.path(getwd(),"scripts", "amenitiesScoring.R"))  
summary(df_base_scored$amenities_score)



#-------------------------------- CREATE df_A3 ---------------------------------
df_A3 <- df_base_scored %>%
  select(-c(bathrooms_text,
            host_verifications,
            property_type,
            amenities,
            amenities_clean,
            neighbourhood_cleansed
            ))



#------------------------------ MISSING VALUES ---------------------------------

find_na_ratios(df_A3)

df_A3 <- df_A3 %>%
  filter(if_all(where(is.factor), ~ !is.na(.))) %>%
  filter(if_all(where(is.numeric), ~ !is.na(.)))

find_na_ratios(df_A3)



#------------------------------- RUN ANALYSIS ----------------------------------
A3_results <- run_analysis(df_A3, plot_cv = FALSE, show_prog = TRUE)
