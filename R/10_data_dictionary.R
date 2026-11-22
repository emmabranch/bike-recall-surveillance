# 10_data_dictionary.R
# Purpose: write a data dictionary (descriptions + profile computed from the
# data) and a Parquet copy of the public dataset.
# Reads:  data/bike_recalls.csv
# Writes: data/data_dictionary.csv, data/bike_recalls.parquet

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(arrow)

recalls <- read_csv("data/bike_recalls.csv",
                    col_types = cols(linked_cpsc_id = col_character(), .default = col_guess()),
                    guess_max = 5000)

# ---------------------------------------------------------------------------
# 1. Descriptions (one row per column)
# ---------------------------------------------------------------------------
dict_text <- tribble(
  ~variable, ~description,
  "record_id", "Unique record ID. 'US-' plus the CPSC recall number, or 'CA-' plus the Health Canada identification (RA) number. Where two Canadian notices share an RA number, '-NID' plus the open data NID is appended.",
  "event_id", "Recall event ID. Joint US and Canadian notices share one event ID (EV-US- plus the CPSC recall number). Use for combined counts across both countries.",
  "source", "Regulator that published the notice: US CPSC or Health Canada.",
  "country", "Country of the regulator: United States or Canada.",
  "recall_date", "Date the recall was announced. CPSC: RecallDate. Health Canada: starting date (pages before 2022) or original published date (2022 onward), read from the recall page.",
  "recall_year", "Year of recall_date.",
  "in_trend_window", "TRUE if the notice falls in the trend analysis window (US from 2000, Canada from 2008). Earlier notices are kept but not used in trend charts.",
  "title", "Notice title as published. The Health Canada 'Archived -' prefix is removed.",
  "url", "Link to the official recall notice.",
  "case_status", "Certainty under case definition v1.3: confirmed or probable. This dataset contains cases only.",
  "case_reason", "ID of the rule that set the case status (see methods/case_definition_rules.csv). 'MANUAL:' marks a documented manual override.",
  "product_form", "What was recalled: Complete bicycle, Complete e-bike, Component, Accessory, E-bike battery, charger, or power part, or Unclear.",
  "is_ebike_related", "TRUE if the recalled product is an e-bike (including recalls that mix regular and electric bikes), an e-bike battery, charger, or power part, or a component recalled from e-bikes. Accessories made for e-bikes, such as e-bike racks, are not counted.",
  "possible_emoto", "TRUE if e-bike related and the text describes no pedals, a throttle-only design, a speed above 28 mph or 32 km/h, or power above 750 W (US) or 500 W (Canada). Rarely triggered: notices seldom state these specifications.",
  "emoto_evidence", "Text evidence behind possible_emoto.",
  "failed_component", "The part whose failure creates the hazard. For part-only recalls, the recalled part. Compliance problems with no failing part (for example lead paint) are Other or unclear.",
  "components_mentioned", "All component category IDs mentioned in the product name or hazard text, separated by semicolons.",
  "hazard_group", "Main injury mechanism: Fall or crash, Fire or burn, Laceration, or Other.",
  "hazard_detail", "Detailed hazard: fall_crash, fire_burn, laceration, entrapment_pinch, choking_ingestion, chemical_lead, electrical_shock, or other_injury.",
  "hazards_mentioned", "All hazard categories mentioned in the title, issue, or hazard text, separated by semicolons.",
  "incidents_reported", "Number of incidents, reports, complaints, or claims stated in the notice, as reported by the firm or regulator. Extracted from text. NA if no number is stated. Health Canada counts usually refer to Canada only.",
  "injuries_reported", "Number of injuries stated in the notice. Extracted from text. NA if no number is stated. 0 where the notice says no injuries were reported.",
  "death_mentioned", "TRUE if the incident text mentions a death, after removing standard 'serious injury or death' risk wording. Review before use: may still reflect risk language rather than reported deaths.",
  "deaths_reported", "Number of deaths, where stated as a number.",
  "units_recalled", "Number of units recalled or sold, parsed from units_text (first number stated). CPSC figures are usually US units. Health Canada figures are units sold in Canada.",
  "units_text", "Units text as published.",
  "remedy_us", "CPSC remedy options (for example Refund, Repair, Replace). Not available for Health Canada notices.",
  "origin_primary", "First country of manufacture named in the notice.",
  "origin_countries", "All countries of manufacture named, separated by semicolons.",
  "linked_cpsc_id", "For Health Canada notices linked to a CPSC notice, the CPSC recall number.",
  "link_method", "How a Health Canada notice was linked to CPSC: url, date_and_name, or manual. NA if not linked.",
  "hazard_text", "Hazard description from the notice (CPSC Hazards field, or the Health Canada 'Hazard identified' or 'Issue' section).",
  "description_text", "Product description from the notice."
)

undocumented <- setdiff(names(recalls), dict_text$variable)
orphaned     <- setdiff(dict_text$variable, names(recalls))
if (length(undocumented) > 0 || length(orphaned) > 0) {
  stop("Dictionary out of sync. Undocumented: ", paste(undocumented, collapse = ", "),
       " | Documented but missing: ", paste(orphaned, collapse = ", "))
}

# ---------------------------------------------------------------------------
# 2. Profile computed from the data
# ---------------------------------------------------------------------------
profile_col <- function(nm) {
  x <- recalls[[nm]]
  nd <- n_distinct(x, na.rm = TRUE)
  values <- if (all(is.na(x))) {
    "(all missing)"
  } else if (inherits(x, "Date")) {
    paste(format(min(x, na.rm = TRUE)), "to", format(max(x, na.rm = TRUE)))
  } else if (is.numeric(x)) {
    paste("range", format(min(x, na.rm = TRUE), big.mark = ","),
          "to", format(max(x, na.rm = TRUE), big.mark = ","))
  } else if (nd <= 12) {
    paste(sort(unique(na.omit(as.character(x)))), collapse = "; ")
  } else {
    "free text or identifier"
  }
  tibble(variable = nm,
         type = class(x)[1],
         pct_complete = round(100 * mean(!is.na(x)), 1),
         n_distinct = nd,
         values = values)
}

profile <- map(names(recalls), profile_col) |> list_rbind()

dictionary <- profile |>
  left_join(dict_text, by = "variable") |>
  select(variable, type, description, values, pct_complete, n_distinct)

write_csv(dictionary, "data/data_dictionary.csv")
write_parquet(recalls, "data/bike_recalls.parquet")

# ---------------------------------------------------------------------------
# 3. Report
# ---------------------------------------------------------------------------
cat("\n========== DATA DICTIONARY ==========\n")
cat("Rows in dataset:", nrow(recalls), "| columns:", ncol(recalls), "\n\n")
dictionary |>
  transmute(variable, type, pct_complete, values = str_trunc(values, 70)) |>
  print(n = 50, width = Inf)

check <- read_parquet("data/bike_recalls.parquet")
cat("\nParquet check: rows", nrow(check), "| recall_date class:", class(check$recall_date)[1], "\n")
cat("\nWrote data/data_dictionary.csv and data/bike_recalls.parquet\n")