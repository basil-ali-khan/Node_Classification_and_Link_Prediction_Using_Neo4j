library(tidyverse)  # Data manipulation
library(janitor)    # Clean column names
library(stringr)    # String operations

# Set working directory (update to your path)
# setwd("path/to/your/data")

# ---------------------------------------------
# 1. Load Data
# ---------------------------------------------

# Load CSV files
journal <- read.csv("journal.csv", stringsAsFactors = FALSE)
paper_journal <- read.csv("cleaned_paper_journal.csv", stringsAsFactors = FALSE)

# Clean column names (standardize to snake_case)
journal <- clean_names(journal)
paper_journal <- clean_names(paper_journal)

# ---------------------------------------------
# 2. Handle Missing Values in journal.csv
# ---------------------------------------------

# Impute missing journal_publisher with "Unknown", drop missing journal_name
journal <- journal %>%
  mutate(journal_publisher = ifelse(is.na(journal_publisher) | journal_publisher == "", "Unknown", journal_publisher)) %>%
  filter(!is.na(journal_name) & journal_name != "")

# ---------------------------------------------
# 3. Standardize journal_publisher in journal.csv
# ---------------------------------------------

# Standardize publisher names: use the first significant name
journal <- journal %>%
  mutate(journal_publisher = str_to_lower(journal_publisher)) %>%
  mutate(journal_publisher = case_when(
    str_detect(journal_publisher, "^routledge") ~ "Routledge",
    str_detect(journal_publisher, "^sage") ~ "Sage",
    str_detect(journal_publisher, "^oxford university press") ~ "Oxford University Press",
    str_detect(journal_publisher, "^cambridge university press") ~ "Cambridge University Press",
    str_detect(journal_publisher, "^wiley") ~ "Wiley",
    str_detect(journal_publisher, "^springer") ~ "Springer",
    str_detect(journal_publisher, "^elsevier") ~ "Elsevier",
    str_detect(journal_publisher, "^carfax") ~ "Carfax",
    TRUE ~ str_to_title(journal_publisher)  # Keep others as-is, but title case
  ))

# ---------------------------------------------
# 4. Handle Missing Values in cleaned_paper_journal.csv
# ---------------------------------------------

# Drop rows with missing paper_id or journal_name
paper_journal <- paper_journal %>%
  filter(!is.na(paper_id) & paper_id != "" & 
           !is.na(journal_name) & journal_name != "")

# ---------------------------------------------
# 5. Standardize journal_name in cleaned_paper_journal.csv
# ---------------------------------------------

# Ensure journal_name in paper_journal matches the standardized journal_name in journal
paper_journal <- paper_journal %>%
  mutate(journal_name = str_trim(journal_name)) %>%
  left_join(select(journal, journal_name), by = "journal_name") %>%
  filter(!is.na(journal_name))  # Keep only matches with journal.csv

# ---------------------------------------------
# 6. Export Cleaned Data
# ---------------------------------------------

write.csv(journal, "cleaned_journal(2).csv", row.names = FALSE)
write.csv(paper_journal, "cleaned_paper_journal(2).csv", row.names = FALSE)


library(tidyverse)  # Data manipulation
library(janitor)    # Clean column names
library(stringr)    # String operations

# Set working directory (update to your path)
# setwd("path/to/your/data")

# ---------------------------------------------
# 1. Load Data
# ---------------------------------------------

# Load CSV files
journal <- read.csv("cleaned_journal(2).csv", stringsAsFactors = FALSE)
paper_journal <- read.csv("cleaned_paper_journal(2).csv", stringsAsFactors = FALSE)

# Clean column names (standardize to snake_case)
journal <- clean_names(journal)
paper_journal <- clean_names(paper_journal)

# ---------------------------------------------
# 2. Standardize journal_publisher in cleaned_journal(2).csv
# ---------------------------------------------

# Standardize publisher names: fix Routledge, Sage, and truncate at Ltd/Inc/Center/Association
journal <- journal %>%
  # Convert to lowercase for matching
  mutate(journal_publisher = str_to_lower(journal_publisher)) %>%
  # Remove emails and URLs
  mutate(journal_publisher = str_replace_all(journal_publisher, "\\S+@\\S+", "")) %>%
  mutate(journal_publisher = str_replace_all(journal_publisher, "\\S+\\.org|\\.co\\.uk|\\.com", "")) %>%
  # Standardize publisher names
  mutate(journal_publisher = case_when(
    str_detect(journal_publisher, "^routledge") ~ "routledge",
    str_detect(journal_publisher, "^taylor and francis") ~ "routledge",
    str_detect(journal_publisher, "^carfax") ~ "routledge",
    str_detect(journal_publisher, "^sage") ~ "sage publications inc",
    str_detect(journal_publisher, "^oxford university press") ~ "oxford university press",
    str_detect(journal_publisher, "^cambridge university press") ~ "cambridge university press",
    str_detect(journal_publisher, "^wiley") ~ "john wiley and sons ltd",
    str_detect(journal_publisher, "^springer") ~ "springer",
    str_detect(journal_publisher, "^elsevier") ~ "elsevier ltd",
    str_detect(journal_publisher, "^blackwell") ~ "blackwell publishing ltd",
    str_detect(journal_publisher, "^american psychological association") ~ "american psychological association",
    str_detect(journal_publisher, "^asian and scalabrini migration center") ~ "asian and scalabrini migration center",
    str_detect(journal_publisher, "^kluwer") ~ "kluwer academic publishers",
    TRUE ~ journal_publisher
  )) %>%
  # Truncate after Ltd/Inc (case-insensitive)
  mutate(journal_publisher = str_replace(journal_publisher, "(?i)(ltd|inc).*", "\\1")) %>%
  # Truncate after Center for Scalabrini Migration Center
  mutate(journal_publisher = str_replace(journal_publisher, "(?i)(asian and scalabrini migration center).*", "\\1")) %>%
  # Truncate after Association for American Psychological Association
  mutate(journal_publisher = str_replace(journal_publisher, "(?i)(american psychological association).*", "\\1")) %>%
  # Convert to title case for consistency
  mutate(journal_publisher = str_to_title(journal_publisher))

# ---------------------------------------------
# 3. Standardize and Impute journal_publisher in cleaned_paper_journal(2).csv
# ---------------------------------------------

# Standardize publisher names and handle missing values
paper_journal <- paper_journal %>%
  # Impute missing journal_publisher with "Unknown"
  mutate(journal_publisher = ifelse(is.na(journal_publisher) | journal_publisher == "", "Unknown", journal_publisher)) %>%
  # Convert to lowercase for matching
  mutate(journal_publisher = str_to_lower(journal_publisher)) %>%
  # Remove emails and URLs
  mutate(journal_publisher = str_replace_all(journal_publisher, "\\S+@\\S+", "")) %>%
  mutate(journal_publisher = str_replace_all(journal_publisher, "\\S+\\.org|\\.co\\.uk|\\.com", "")) %>%
  # Standardize publisher names to match cleaned_journal(2).csv
  mutate(journal_publisher = case_when(
    str_detect(journal_publisher, "^routledge") ~ "routledge",
    str_detect(journal_publisher, "^taylor and francis") ~ "routledge",
    str_detect(journal_publisher, "^carfax") ~ "routledge",
    str_detect(journal_publisher, "^sage") ~ "sage publications inc",
    str_detect(journal_publisher, "^oxford university press") ~ "oxford university press",
    str_detect(journal_publisher, "^cambridge university press") ~ "cambridge university press",
    str_detect(journal_publisher, "^wiley") ~ "john wiley and sons ltd",
    str_detect(journal_publisher, "^springer") ~ "springer",
    str_detect(journal_publisher, "^elsevier") ~ "elsevier ltd",
    str_detect(journal_publisher, "^blackwell") ~ "blackwell publishing ltd",
    str_detect(journal_publisher, "^american psychological association") ~ "american psychological association",
    str_detect(journal_publisher, "^asian and scalabrini migration center") ~ "asian and scalabrini migration center",
    str_detect(journal_publisher, "^kluwer") ~ "kluwer academic publishers",
    TRUE ~ journal_publisher
  )) %>%
  # Truncate after Ltd/Inc (case-insensitive)
  mutate(journal_publisher = str_replace(journal_publisher, "(?i)(ltd|inc).*", "\\1")) %>%
  # Truncate after Center for Scalabrini Migration Center
  mutate(journal_publisher = str_replace(journal_publisher, "(?i)(asian and scalabrini migration center).*", "\\1")) %>%
  # Truncate after Association for American Psychological Association
  mutate(journal_publisher = str_replace(journal_publisher, "(?i)(american psychological association).*", "\\1")) %>%
  # Convert to title case for consistency
  mutate(journal_publisher = str_to_title(journal_publisher))

# ---------------------------------------------
# 4. Export Updated Data to the Same Files
# ---------------------------------------------

write.csv(journal, "cleaned_journal(2).csv", row.names = FALSE)
write.csv(paper_journal, "cleaned_paper_journal(2).csv", row.names = FALSE)