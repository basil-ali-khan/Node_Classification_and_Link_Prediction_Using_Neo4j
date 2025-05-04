# R Script to Clean Bibliographic Dataset for Node Classification/Link Prediction
# Date: April 23, 2025

# Install and load required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")
if (!require(stringr)) install.packages("stringr")
if (!require(fuzzyjoin)) install.packages("fuzzyjoin")
if (!require(lubridate)) install.packages("lubridate")

library(tidyverse)  # Data manipulation
library(janitor)    # Clean column names
library(stringr)    # String operations
library(fuzzyjoin)  # Fuzzy matching
library(lubridate)  # Date handling

# ---------------------------------------------
# 1. Load Data
# ---------------------------------------------

# Load CSV files
journal <- read.csv("journal.csv", stringsAsFactors = FALSE)
topic <- read.csv("topic.csv", stringsAsFactors = FALSE)
paper <- read.csv("paper.csv", stringsAsFactors = FALSE)
paper_reference <- read.csv("paper_reference.csv", stringsAsFactors = FALSE)
paper_topic <- read.csv("paper_topic.csv", stringsAsFactors = FALSE)
paper_journal <- read.csv("paper_journal.csv", stringsAsFactors = FALSE)

# Clean column names (standardize to snake_case)
journal <- clean_names(journal)
topic <- clean_names(topic)
paper <- clean_names(paper)
paper_reference <- clean_names(paper_reference)
paper_topic <- clean_names(paper_topic)
paper_journal <- clean_names(paper_journal)

# ---------------------------------------------
# 2. Handle Missing Values in paper.csv and paper_journal.csv
# ---------------------------------------------

# paper.csv: Impute missing citation_count with 0, impute missing strings with "Unknown", 
# drop rows with missing paper_id, paper_year, or journal_date
paper <- paper %>%
  mutate(paper_citation_count = ifelse(is.na(paper_citation_count), 0, paper_citation_count)) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.) | . == "", "Unknown", .))) %>%
  filter(!is.na(paper_id) & paper_id != "" & 
           !is.na(paper_year) & paper_year != "" & 
           !is.na(journal_date) & journal_date != "")

# paper_journal.csv: Drop rows with missing paper_id or journal_name
paper_journal <- paper_journal %>%
  filter(!is.na(paper_id) & paper_id != "" & 
           !is.na(journal_name) & journal_name != "")

# ---------------------------------------------
# 3. Address Issues in journal.csv
# ---------------------------------------------
#missing vals
# Impute missing journal_publisher with "Unknown", drop rows with missing journal_name
journal <- journal %>%
  mutate(journal_publisher = ifelse(is.na(journal_publisher) | journal_publisher == "", "Unknown", journal_publisher)) %>%
  filter(!is.na(journal_name) & journal_name != "")

# Issue: Duplicates in journal_name (e.g., "Patterns of Prejudice", "Ethnic & Racial Studies")
# Action: Merge duplicates by consolidating journal_publisher
journal <- journal %>%
  group_by(journal_name) %>%
  summarise(journal_publisher = paste(unique(journal_publisher), collapse = "; ")) %>%
  ungroup()


# Fuzzy matching to standardize publisher names (e.g., "Routledge" vs "Routledge Journals")
unique_publishers <- unique(journal$journal_publisher)
journal <- journal %>%
  mutate(journal_publisher = sapply(journal_publisher, function(x) {
    if (is.na(x) || x == "") return("Unknown")
    matches <- stringdist::stringdist(x, unique_publishers)
    if (min(matches) < 5) unique_publishers[which.min(matches)] else x
  }))

# ---------------------------------------------
# 4. Address Issues in topic.csv
# ---------------------------------------------

# Issue: Potential duplicates in topic_name (e.g., "Money" with Topic_ID 21808 and 21809)
# Action: Keep the first occurrence based on topic_id
topic <- topic %>%
  distinct(topic_id, .keep_all = TRUE) %>%
  distinct(topic_name, .keep_all = TRUE)

# Issue: Inconsistent topic_name formatting (title case vs lowercase)
# Action: Standardize to title case
topic <- topic %>%
  mutate(topic_name = str_to_title(str_trim(topic_name)))

# ---------------------------------------------
# 6. Address Issues in paper_reference.csv
# ---------------------------------------------

# Issue: Unmatched referenced_paper_id (e.g., "paper180540", "paper7908")
# Action: Validate against paper.csv and remove invalid references
paper_reference <- paper_reference %>%
  filter(referenced_paper_id %in% paper$paper_id)

# ---------------------------------------------
# 7. Address Issues in paper_topic.csv
# ---------------------------------------------

# Issue: Unmatched topic_id (e.g., "330", "193")
# Action: Validate against topic.csv and remove invalid entries
paper_topic <- paper_topic %>%
  filter(topic_id %in% topic$topic_id)

# Issue: Unmatched paper_id (e.g., "5b16e84fb", "e446c5770")
# Action: Validate against paper.csv and remove invalid entries
paper_topic <- paper_topic %>%
  filter(paper_id %in% paper$paper_id)

# ---------------------------------------------
# 8. Address Issues in paper_journal.csv
# ---------------------------------------------

# Issue: Unmatched journal_name (e.g., "Journal of Sociology")
# Action: Validate against journal.csv and remove invalid entries
paper_journal <- paper_journal %>%
  filter(journal_name %in% journal$journal_name)

# Issue: Unmatched paper_id (e.g., "5b16e84fb", "e446c5770")
# Action: Validate against paper.csv and remove invalid entries
paper_journal <- paper_journal %>%
  filter(paper_id %in% paper$paper_id)

# ---------------------------------------------
# 9. Export Cleaned Data
# ---------------------------------------------

write.csv(journal, "cleaned_journal.csv", row.names = FALSE)
write.csv(topic, "cleaned_topic.csv", row.names = FALSE)
write.csv(paper, "cleaned_paper.csv", row.names = FALSE)
write.csv(paper_reference, "cleaned_paper_reference.csv", row.names = FALSE)
write.csv(paper_topic, "cleaned_paper_topic.csv", row.names = FALSE)
write.csv(paper_journal, "cleaned_paper_journal.csv", row.names = FALSE)