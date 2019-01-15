library(tidyverse)
library(readxl)
library(writexl)
library(countrycode)

# Load original data ------------------------------------------------------

# This is from https://darinchristensen.com/replication/JoD_Replication.zip
dcjw_raw <- read_excel("data_original/DCJW_NGO_Laws.xlsx")

# This was hand-typed by Andrew Heiss
dcjw_meta <- read_csv("data_original/dcjw_questions.csv")

# Mini data frame indicating if barriers should be ignored in indexes
dcjw_barriers_ignore <- dcjw_meta %>%
  select(question, ignore_in_index)


# Clean and wrangle data --------------------------------------------------

dcjw <- dcjw_raw %>% 
  # Get rid of extra columns
  select(-contains("source"), -contains("burden"), 
         -contains("subset"), -Coder, -Date) %>%
  # Make data long, with row per country-regulation
  gather(key, value, -Country) %>%
  # Split key column into two, since lots of the questions end with "_year", etc.
  separate(key, c("question", "var_name"), 4) %>%
  # Get rid of rows without countries
  drop_na(Country) %>% 
  # Get rid of the leading _ in "_year" entries
  mutate(var_name = ifelse(var_name == "", "value", gsub("_", "", var_name))) %>%
  # Make data wide so that there's a year column
  spread(var_name, value) %>%
  # Get rid of rows where year is missing and regulation was not imposed
  filter(!(is.na(year) & value == 0)) %>%
  # Make these columns numbers
  mutate_at(vars(value, year), funs(as.integer)) %>% 
  # If year is missing but some regulation exists, assume it has always already
  # existed (since 1950, arbitrarily)
  mutate(year = ifelse(is.na(year), 1950, year))

# Find every combination of country, year, and regulation
dcjw_panel_skeleton <- dcjw %>% 
  expand(Country, question, year = min(year, na.rm = TRUE): 2015)

# Bring actual values into full panel skeleton
dcjw_panel_everything <- dcjw %>%
  right_join(dcjw_panel_skeleton, by = c("Country", "question", "year")) %>%
  group_by(Country, question) %>%
  # Bring most recent legislation forward
  mutate(value = zoo::na.locf(value, na.rm = FALSE)) %>%
  ungroup() %>%
  # Set defaults for columns that aren't all NA
  mutate(value_fixed = ifelse(is.na(value), 0L, value)) %>% 
  select(-value) %>%
  spread(question, value_fixed) %>%
  ungroup()


# Start making a country-year-barrier panel
dcjw_panel_barriers_skeleton <- dcjw_panel_skeleton %>%
  mutate(question_cat = as.integer(substr(question, 3, 3))) %>%
  left_join(dcjw_meta, by = c("question", "question_cat")) %>%
  distinct(Country, year, barrier)

# Clean up and recode/reorder lots of the possible values
dcjw_panel_barriers <- dcjw_panel_everything %>%
  gather(question, value, -Country, -year) %>%
  mutate(question_cat = as.integer(substr(question, 3, 3))) %>%
  left_join(dcjw_meta, by = c("question", "question_cat")) %>% 
  # Make an index for each type of barrier
  mutate(value = as.numeric(value)) %>%
  mutate(value = case_when(
    # Reverse values for associational rights
    question == "q_1a" & value == 0 ~ 1,
    question == "q_1a" & value == 1 ~ 0,
    question == "q_1b" & value == 0 ~ 1,
    question == "q_1b" & value == 1 ~ 0,
    # Reverse value for q_2c
    question == "q_2c" & value == 0 ~ 1,
    question == "q_2c" & value == 1 ~ 0,
    # Recode 0-2 questions as 0-1
    question == "q_3e" & value == 1 ~ 0.5,
    question == "q_3e" & value == 2 ~ 1,
    question == "q_3f" & value == 1 ~ 0.5,
    question == "q_3f" & value == 2 ~ 1,
    question == "q_4a" & value == 1 ~ 0.5,
    question == "q_4a" & value == 2 ~ 1,
    TRUE ~ value
  )) %>% 
  # Ignore neutral indexes, like basic registration requirements
  mutate(value_restrictive = ifelse(ignore_in_index, 0, value)) %>%
  group_by(Country, year, barrier) %>% 
  # Add up all the barriers for each country year
  # Use a floor of zero to account for negative values
  summarize(all = sum(value, na.rm = TRUE),
            restrictive = sum(value_restrictive, na.rm = TRUE)) %>%
  mutate_at(vars(all, restrictive),
            funs(ifelse(. < 0, as.integer(0), .))) %>%
  # Join with full possible panel
  right_join(dcjw_panel_barriers_skeleton,
             by = c("Country", "barrier", "year")) %>%
  gather(temp, value, all, restrictive) %>%
  unite(temp1, barrier, temp, sep = "_") %>%
  # Take "_restrictive" out of the variable names. 
  # "entry" = all restrictive entry laws, "entry_all" = all entry laws
  mutate(temp1 = str_replace_all(temp1, "_restrictive", "")) %>% 
  spread(temp1, value) %>%
  ungroup() %>%
  # Calculate total barriers
  mutate(barriers_total = advocacy + entry + funding)

# Phew. Almost done. Merge the barrier panel in with the full barrier panel,
# resulting in a country-year-barrier panel + indexes for restrictiveness across
# types of regulations
dcjw_full <- dcjw_panel_everything %>% 
  left_join(dcjw_panel_barriers, by = c("Country", "year")) %>% 
  # Lop off the ancient observations
  filter(year > 1980) %>% 
  # Rename q_* variables
  rename_(.dots = setNames(dcjw_meta$question, 
                           dcjw_meta$question_clean)) %>% 
  # Add country-related variables
  mutate(Country = countrycode(Country, "country.name", "country.name"),
         cowcode = countrycode(Country, "country.name", "cown",
                               custom_match = c(Serbia = 340,
                                                Vietnam = 816)),
         iso3 = countrycode(Country, "country.name", "iso3c",
                            custom_match = c(Kosovo = "KXX"))) %>%
  select(country_name = Country, iso3, cowcode, year, everything())


# Save this stuff ---------------------------------------------------------

write_csv(dcjw_full, "data_clean/dcjw_clean.csv")
write_xlsx(dcjw_full, "data_clean/dcjw_clean.xlsx")
