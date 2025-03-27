#-------------------------------------------------------------------------------
#                             CREATE & PREP DF_A1
#                                 no cleaning
#-------------------------------------------------------------------------------
library(tidyverse)

# Create
df_A1 <- df_base %>%
  select(-c(host_acceptance_rate,
            host_response_rate,
            has_availability,
            first_review,
            last_review,
            host_since,
            bathrooms_text,
            host_location,
            host_verifications,
            property_type,
            amenities
  ))

#------------------------------ MISSING VALUES ---------------------------------

find_na_ratios <- function(df) {
  df %>%
    summarise(across(everything(), ~mean(is.na(.)), .names = "{.col}")) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "NA_ratio") %>%
    filter(NA_ratio > 0) %>%
    arrange(desc(NA_ratio))
}

find_na_ratios(df_A1)

df_A1 <- df_A1 %>%
  filter(if_all(where(is.factor), ~ !is.na(.))) %>%
  filter(if_all(where(is.numeric), ~ !is.na(.)))

find_na_ratios(df_A1)

#------------------------------- RUN ANALYSIS ----------------------------------
A1_results <- run_analysis(df_A1, plot_cv = FALSE, show_prog = TRUE)
