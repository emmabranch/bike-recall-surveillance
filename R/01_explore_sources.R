# 01_explore_sources.R
# Purpose: download both recall sources and see what fields they contain,
# and roughly how many records mention bikes. Exploration only.

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(tibble)

dir.create("data-raw", showWarnings = FALSE)

# Broad first-pass pattern. Deliberately over-inclusive: the case definition
# in step 2 will remove exercise bikes, dirt bikes, helmets, and so on.
bike_pattern <- regex("\\b(e-?bikes?|bicycles?|bikes?|tricycles?|cycling)\\b",
                      ignore_case = TRUE)

# ---- 1. US CPSC recalls API --------------------------------------------------
cpsc_url <- "https://www.saferproducts.gov/RestWebServices/Recall?format=json"

request(cpsc_url) |>
  req_user_agent("bike-recall-surveillance (independent research project)") |>
  req_timeout(600) |>
  req_perform(path = "data-raw/cpsc_recalls_raw.json")

cpsc <- fromJSON("data-raw/cpsc_recalls_raw.json", simplifyDataFrame = TRUE) |>
  as_tibble()

cat("\n========== CPSC ==========\n")
cat("Records:", nrow(cpsc), "\n")
cat("Fields:\n"); print(names(cpsc))
cpsc_dates <- as.Date(substr(cpsc$RecallDate, 1, 10))
cat("Recall date range:", format(range(cpsc_dates, na.rm = TRUE)), "\n")

cpsc_hits <- cpsc |>
  filter(str_detect(coalesce(Title, ""), bike_pattern) |
           str_detect(coalesce(Description, ""), bike_pattern))

cat("Records mentioning bikes (broad pattern):", nrow(cpsc_hits), "\n")
cat("  ...of which mention exercise/stationary:",
    sum(str_detect(cpsc_hits$Title, regex("exercise|stationary", ignore_case = TRUE)),
        na.rm = TRUE), "\n")

set.seed(2026)
cat("\nRandom sample of 20 CPSC titles:\n")
cpsc_hits |>
  slice_sample(n = min(20, nrow(cpsc_hits))) |>
  mutate(line = paste0(substr(RecallDate, 1, 10), "  ", Title)) |>
  pull(line) |>
  cat(sep = "\n")

# ---- 2. Health Canada recalls and safety alerts ------------------------------
hc_url <- paste0("https://recalls-rappels.canada.ca/sites/default/files/",
                 "opendata-donneesouvertes/HCRSAMOpenData.csv")

request(hc_url) |>
  req_user_agent("bike-recall-surveillance (independent research project)") |>
  req_timeout(600) |>
  req_perform(path = "data-raw/hc_recalls_raw.csv")

hc <- read_csv("data-raw/hc_recalls_raw.csv", show_col_types = FALSE)

cat("\n========== HEALTH CANADA ==========\n")
cat("Records:", nrow(hc), "\n")
cat("Fields and first values:\n")
glimpse(hc)

hc_hits <- hc |>
  filter(if_any(where(is.character), ~ str_detect(coalesce(.x, ""), bike_pattern)))

cat("\nRecords mentioning bikes (broad pattern):", nrow(hc_hits), "\n")
cat("\nFive sample bike records:\n")
set.seed(2026)
hc_hits |> slice_sample(n = min(5, nrow(hc_hits))) |> glimpse()