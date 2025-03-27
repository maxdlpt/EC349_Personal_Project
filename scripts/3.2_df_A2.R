#-------------------------------------------------------------------------------
#                             CREATE & PREP DF_A2
#                              minimal cleaning
#-------------------------------------------------------------------------------
library(tidyverse)


#----------------------------- CLEAN EASY VARIABLES ----------------------------
df_base <- df_base %>% 
  mutate(host_response_rate = as.numeric(gsub("\\%", "", as.character(host_response_rate)))/100,
         host_acceptance_rate = as.numeric(gsub("\\%", "", as.character(host_acceptance_rate)))/100)

df_base <- df_base %>%
  mutate(has_availability = as.factor(ifelse(as.character(has_availability) == "", 
                                             "f", as.character(has_availability))))


df_base <- df_base %>%
  
  mutate(first_review = as.Date(as.character(first_review), format = "%Y-%m-%d"),
         last_review = as.Date(as.character(last_review), format = "%Y-%m-%d"),
         host_since = as.Date(as.character(host_since), format = "%Y-%m-%d"),
         last_scraped = as.Date(as.character(last_scraped), format = "%Y-%m-%d"))%>% 
  
  mutate(first_review = as.numeric(last_scraped - first_review),
         last_review = as.numeric(last_scraped - last_review),
         host_since = as.numeric(last_scraped - host_since)) %>%
  
  rename(nDays_since_first_review = first_review, 
         nDays_since_last_review = last_review,
         host_since_nDays = host_since)

summary(df_base$host_response_rate)
summary(df_base$host_acceptance_rate)
summary(df_base$has_availability)
summary(df_base$nDays_since_first_review)
summary(df_base$nDays_since_last_review)
summary(df_base$host_since_nDays)



#-------------------------------- CREATE DF_A2 ---------------------------------

df_A2 <- df_base %>%
  select(-c(bathrooms_text,
            host_location,
            host_verifications,
            property_type,
            amenities
  ))

#------------------------------ MISSING VALUES ---------------------------------

find_na_ratios(df_A2)

df_A2 <- df_A2 %>%
  filter(if_all(where(is.factor), ~ !is.na(.))) %>%
  filter(if_all(where(is.numeric), ~ !is.na(.)))

find_na_ratios(df_A2)

#------------------------------- RUN ANALYSIS ----------------------------------
A2_results <- run_analysis(df_A2, plot_cv = FALSE, show_prog = TRUE)
