# 02_profile_candidates.R
# Purpose: profile bike-related candidate records in both sources so we can
# write a case definition. Saves candidate lists to data-interim/.

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(rvest)

dir.create("data-interim", showWarnings = FALSE)

bike_pattern <- regex("\\b(e-?bikes?|bicycles?|bikes?|tricycles?|cycling)\\b",
                      ignore_case = TRUE)

# Helper: collapse one field of a nested data frame column into a single string
collapse_field <- function(x, field) {
  map_chr(x, function(d) {
    if (is.data.frame(d) && nrow(d) > 0 && field %in% names(d)) {
      vals <- unique(na.omit(as.character(d[[field]])))
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
    } else {
      NA_character_
    }
  })
}

# ============================================================================
# 1. CPSC
# ============================================================================
cpsc <- fromJSON("data-raw/cpsc_recalls_raw.json", simplifyDataFrame = TRUE) |>
  as_tibble()

cpsc_hits <- cpsc |>
  filter(str_detect(coalesce(Title, ""), bike_pattern) |
           str_detect(coalesce(Description, ""), bike_pattern)) |>
  mutate(recall_date = as.Date(substr(RecallDate, 1, 10)))

cat("\n========== CPSC: nested field structure ==========\n")
list_cols <- c("Products", "Inconjunctions", "Injuries", "Hazards",
               "Remedies", "RemedyOptions", "ManufacturerCountries")
for (col in list_cols) {
  first <- detect(cpsc_hits[[col]], function(d) is.data.frame(d) && nrow(d) > 0)
  cat(sprintf("%-22s", col), ":",
      if (is.null(first)) "(always empty in hits)" else paste(names(first), collapse = ", "),
      "\n")
}

cpsc_hits <- cpsc_hits |>
  mutate(
    product_type     = collapse_field(Products, "Type"),
    product_category = collapse_field(Products, "CategoryID"),
    product_names    = collapse_field(Products, "Name"),
    units            = collapse_field(Products, "NumberOfUnits"),
    hazard_text      = collapse_field(Hazards, "Name"),
    hazard_type      = collapse_field(Hazards, "HazardType"),
    injury_text      = collapse_field(Injuries, "Name"),
    mfr_country      = collapse_field(ManufacturerCountries, "Country"),
    has_inconjunction = map_lgl(Inconjunctions, function(d) is.data.frame(d) && nrow(d) > 0)
  )

cat("\n--- Product Type (top 20) ---\n")
print(count(cpsc_hits, product_type, sort = TRUE), n = 20)

cat("\n--- Product CategoryID (top 15) ---\n")
print(count(cpsc_hits, product_category, sort = TRUE), n = 15)

cat("\n--- Hazard Type (top 15) ---\n")
print(count(cpsc_hits, hazard_type, sort = TRUE), n = 15)

cat("\n--- Bike hits by 5-year period ---\n")
cpsc_hits |>
  mutate(period = paste0(floor(as.numeric(format(recall_date, "%Y")) / 5) * 5, "s")) |>
  count(period) |>
  print(n = 15)

cat("\n--- Joint recalls (Inconjunctions not empty):",
    sum(cpsc_hits$has_inconjunction), "of", nrow(cpsc_hits), "---\n")
cat("First three Inconjunctions entries:\n")
cpsc_hits$Inconjunctions[cpsc_hits$has_inconjunction] |> head(3) |> str()

cat("\n--- Five example Injuries texts ---\n")
cpsc_hits |> filter(!is.na(injury_text)) |> slice_head(n = 5) |>
  pull(injury_text) |> str_trunc(250) |> cat(sep = "\n\n")

cpsc_hits |>
  select(RecallNumber, recall_date, Title, product_type, product_category,
         product_names, units, hazard_text, hazard_type, injury_text,
         mfr_country, has_inconjunction, URL, Description) |>
  write_csv("data-interim/cpsc_candidates.csv")

# ============================================================================
# 2. HEALTH CANADA
# ============================================================================
hc <- read_csv("data-raw/hc_recalls_raw.csv", show_col_types = FALSE)

hc_hits_all <- hc |>
  filter(str_detect(coalesce(Title, ""), bike_pattern) |
           str_detect(coalesce(Product, ""), bike_pattern))

cat("\n========== HEALTH CANADA ==========\n")
cat("--- Organization among bike hits ---\n")
print(count(hc_hits_all, Organization, sort = TRUE))

hc_hits <- hc_hits_all |> filter(Organization == "Consumer product safety")
cat("\nConsumer product safety bike hits:", nrow(hc_hits), "\n")

cat("\n--- Category ---\n")
print(count(hc_hits, Category, sort = TRUE), n = 20)

cat("\n--- Issue (top 20) ---\n")
print(count(hc_hits, Issue, sort = TRUE), n = 20)

cat("\n--- Last updated year x Archived ---\n")
hc_hits |>
  count(year = as.numeric(format(`Last updated`, "%Y")), Archived) |>
  print(n = 30)

cat("\nTitles starting 'Archived -':",
    sum(str_detect(hc_hits$Title, "^Archived")), "\n")

write_csv(hc_hits, "data-interim/hc_candidates.csv")

# ============================================================================
# 3. Peek at three Health Canada recall pages
# ============================================================================
cat("\n========== HEALTH CANADA DETAIL PAGES ==========\n")
set.seed(2026)
sample_urls <- bind_rows(
  hc_hits |> filter(Archived == 1) |> slice_sample(n = 1),
  hc_hits |> filter(Archived == 0) |> slice_sample(n = 2)
) |> pull(URL)

for (u in sample_urls) {
  cat("\n------------------------------------------------------------\n")
  cat(u, "\n------------------------------------------------------------\n")
  page <- request(u) |>
    req_user_agent("bike-recall-surveillance (independent research project)") |>
    req_timeout(60) |>
    req_perform() |>
    resp_body_string() |>
    read_html()
  page |> html_element("main") |> html_text2() |> str_squish() |>
    str_trunc(2500) |> cat("\n")
  Sys.sleep(2)  # be polite to the server
}

cat("\nCandidate files written to data-interim/\n")