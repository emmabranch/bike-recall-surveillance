# 05_apply_case_definition.R
# Purpose: harmonise CPSC and Health Canada candidates, apply the case
# definition (methods/case-definition.md), and link joint recalls.
# Reads:  data-raw/cpsc_recalls_raw.json, data-interim/hc_pages_parsed.csv
# Writes: data-interim/candidates_with_status.csv,
#         methods/case_definition_rules.csv

library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)

dir.create("methods", showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
rx  <- function(p) regex(p, ignore_case = TRUE)
has <- function(text, p) str_detect(coalesce(text, ""), rx(p))

collapse_field <- function(x, field) {
  map_chr(x, function(d) {
    if (is.data.frame(d) && nrow(d) > 0 && field %in% names(d)) {
      vals <- unique(na.omit(as.character(d[[field]])))
      vals <- vals[vals != ""]
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
    } else {
      NA_character_
    }
  })
}

norm_url <- function(u) {
  u |> str_to_lower() |> str_trim() |> str_remove("^https?://") |> str_remove("/$")
}

# ---------------------------------------------------------------------------
# Patterns
# ---------------------------------------------------------------------------
bike_words        <- "\\b(e-?bikes?|bicycles?|bikes?|tricycles?|trikes?|cycling|pedelecs?)\\b"
strict_bike_words <- "\\b(bicycles?|e-?bikes?|electric bikes?|pedelecs?)\\b"
component_words   <- paste0(
  "\\b(derailleurs?|cranks?(sets?|arms?)?|chainrings?|handlebars?|stems?|",
  "seat ?posts?|saddles?|forks?|pedals?|hubs?|rims?|spokes?|disc brakes?|",
  "brake (levers?|calipers?|rotors?)|quick[- ]release|bottom brackets?|headsets?)\\b")

bicycle_code_pattern  <- "(^|; )Bicycles (&|and) "
excluded_type_pattern <- paste0("Helmet|Headgear|Exercise|Child Carriers|Baby Carriers|",
                                "Motorcycles|Dirt Bikes|Scooters|Mini bikes|Toy Ride-on|Lawn")

exclusion_rules <- tribble(
  ~rule_id,               ~pattern,                                                          ~description,
  "EX01_helmet",          "\\b(helmets?|headgear)\\b",                                       "Helmets and head protection",
  "EX02_exercise",        "\\b(exercise|stationary|spin(ning)? bikes?|indoor cycl\\w*|fitness bikes?)\\b",
  "Exercise, stationary, and indoor bikes",
  "EX03_motorized",       "\\b(dirt ?bikes?|mini ?bikes?|pit ?bikes?|motorcycles?|motorbikes?|mopeds?|motocross|ATVs?|off-road vehicles?|go-?karts?)\\b",
  "Motorized two-wheelers that are not e-bikes",
  "EX04_micromobility",   "\\b(scooters?|scoot|hoverboards?|self-balancing|unicycles?|skateboards?|morfboard)\\b",
  "E-scooters, hoverboards, and other micromobility",
  "EX05_child_passenger", "\\b(child (bicycle |bike )?(carriers?|seats?)|baby carriers?|(bicycle|bike|child) trailers?|trailers? (and )?conversion kits?|strollers?)\\b",
  "Child carriers, child seats, trailers, and strollers",
  "EX06_toy_ride_on",     "\\b(ride-?ons?|toys?|toddler tricycles?|big wheel)\\b|\\btricycles?\\b.*\\b(toddlers?|child|children|kids?)\\b|\\b(toddlers?|child|children|kids?)('s)?\\b.*\\btricycles?\\b",
  "Toy tricycles and ride-on toys",
  "EX07_homonym",         "\\b(horses?|equestrian|riding saddles?|saddle cinch(es)?|crossbows?|gaming|wireless headsets?|ear force|earbuds?|lawn|trimmers?|edgers?)\\b",
  "Unrelated products sharing a bicycle word"
)

inclusion_rules <- tribble(
  ~rule_id,                   ~level,       ~description,
  "EX00_cpsc_product_type",   "excluded",   "CPSC product type is out of scope and not a bicycle category",
  "PROB02_mixed_product",     "probable",   "Exclusion pattern matches but a bicycle or e-bike is also named",
  "CONF01_cpsc_bicycle_code", "confirmed",  "CPSC product type is a bicycle category",
  "CONF02_bike_named",        "confirmed",  "Bicycle, e-bike, or tricycle named in title or product name",
  "CONF03_component_named",   "confirmed",  "Bicycle component named, with bicycle context",
  "PROB01_description_only",  "probable",   "Bicycle mentioned only in the description",
  "NC01_no_bike_reference",   "not_case",   "No qualifying bicycle reference"
)

first_match <- function(text, rules) {
  text <- coalesce(text, "")
  hits <- map(rules$pattern, function(p) str_detect(text, rx(p)))
  m <- do.call(cbind, hits)
  apply(m, 1, function(r) {
    i <- which(r)
    if (length(i) > 0) rules$rule_id[i[1]] else NA_character_
  })
}

# Title + product name, with the retailer name "Toys R Us" removed so it
# does not trigger the toy exclusion
name_text_of <- function(title, product) {
  paste(coalesce(title, ""), coalesce(product, "")) |>
    str_remove_all(rx("toys\\s*[\"']?\\s*r\\s*[\"']?\\s*us"))
}

# ---------------------------------------------------------------------------
# 1. CPSC candidates (keyword, description, or bicycle product type)
# ---------------------------------------------------------------------------
cpsc_raw <- fromJSON("data-raw/cpsc_recalls_raw.json", simplifyDataFrame = TRUE) |>
  as_tibble()

cpsc_all <- cpsc_raw |>
  mutate(
    recall_date    = as.Date(substr(RecallDate, 1, 10)),
    product_type   = collapse_field(Products, "Type"),
    product_names  = collapse_field(Products, "Name"),
    units_text     = collapse_field(Products, "NumberOfUnits"),
    hazard_text    = collapse_field(Hazards, "Name"),
    injury_text    = collapse_field(Injuries, "Name"),
    remedy_text    = collapse_field(RemedyOptions, "Option"),
    origin_text    = collapse_field(ManufacturerCountries, "Country"),
    joint_url      = collapse_field(Inconjunctions, "URL")
  )

cpsc_cand <- cpsc_all |>
  mutate(found_by = case_when(
    has(Title, bike_words) | has(product_names, bike_words) ~ "keyword_title_product",
    has(Description, bike_words)                            ~ "keyword_description",
    str_detect(coalesce(product_type, ""), "Bicycle")       ~ "product_type_only",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(found_by))

cpsc_h <- cpsc_cand |>
  transmute(
    source            = "US CPSC",
    source_id         = as.character(RecallNumber),
    recall_date,
    title             = Title,
    product_text      = product_names,
    description_text  = Description,
    hazard_text,
    injury_text,
    units_text,
    remedy_text,
    origin_text,
    cpsc_product_type = product_type,
    url               = URL,
    joint_url,
    found_by
  )

# ---------------------------------------------------------------------------
# 2. Health Canada candidates, linked to CPSC by URL
# ---------------------------------------------------------------------------
links <- map2(cpsc_h$source_id, cpsc_h$joint_url, function(id, u) {
  if (is.na(u)) return(NULL)
  tibble(linked_cpsc_id = id, url_norm = norm_url(str_split(u, ";\\s*")[[1]]))
}) |>
  list_rbind() |>
  distinct(url_norm, .keep_all = TRUE)

hc <- read_csv("data-interim/hc_pages_parsed.csv", show_col_types = FALSE)

hc_h <- hc |>
  transmute(
    source           = "Health Canada",
    source_id        = hc_id,
    recall_date,
    title            = str_remove(Title, "^Archived\\s*-\\s*"),
    product_text     = coalesce(Product, summary_product),
    description_text = product_description,
    hazard_text      = hazard_identified,
    units_text       = number_sold,
    origin_text      = place_of_origin,
    hc_category      = Category,
    hc_issue         = coalesce(Issue, summary_issue),
    hc_brand         = brand,
    hc_joint_flag    = joint_with_cpsc,
    time_period_sold,
    url              = URL,
    found_by,
    url_norm         = norm_url(URL)
  ) |>
  left_join(links, by = "url_norm") |>
  select(-url_norm)

# ---------------------------------------------------------------------------
# 3. Apply the case definition
# ---------------------------------------------------------------------------
combined <- bind_rows(cpsc_h, hc_h) |>
  mutate(
    record_id       = paste0(if_else(source == "US CPSC", "US-", "CA-"), source_id),
    event_id        = case_when(
      source == "US CPSC"     ~ paste0("EV-US-", source_id),
      !is.na(linked_cpsc_id)  ~ paste0("EV-US-", linked_cpsc_id),
      TRUE                    ~ paste0("EV-CA-", source_id)
    ),
    name_text       = name_text_of(title, product_text),
    excl_rule       = first_match(name_text, exclusion_rules),
    mixed           = excl_rule %in% c("EX03_motorized", "EX04_micromobility") &
      has(name_text, strict_bike_words),
    bike_code       = str_detect(coalesce(cpsc_product_type, ""), bicycle_code_pattern),
    type_excluded   = !bike_code & str_detect(coalesce(cpsc_product_type, ""), excluded_type_pattern),
    bike_named      = has(name_text, bike_words),
    component_named = has(name_text, component_words),
    bike_in_desc    = has(description_text, bike_words),
    bike_context    = bike_code | has(hc_category, "sports") | bike_in_desc,
    case_reason     = case_when(
      type_excluded                    ~ "EX00_cpsc_product_type",
      !is.na(excl_rule) & !mixed       ~ excl_rule,
      mixed                            ~ "PROB02_mixed_product",
      bike_code                        ~ "CONF01_cpsc_bicycle_code",
      bike_named                       ~ "CONF02_bike_named",
      component_named & bike_context   ~ "CONF03_component_named",
      bike_in_desc                     ~ "PROB01_description_only",
      TRUE                             ~ "NC01_no_bike_reference"
    ),
    case_status     = case_when(
      str_starts(case_reason, "EX")   ~ "excluded",
      str_starts(case_reason, "CONF") ~ "confirmed",
      str_starts(case_reason, "PROB") ~ "probable",
      TRUE                            ~ "not_case"
    ),
    is_case         = case_status %in% c("confirmed", "probable"),
    in_trend_window = if_else(source == "US CPSC",
                              recall_date >= as.Date("2000-01-01"),
                              recall_date >= as.Date("2008-01-01"))
  )

write_csv(combined, "data-interim/candidates_with_status.csv")

bind_rows(
  exclusion_rules |> mutate(level = "excluded"),
  inclusion_rules
) |>
  select(rule_id, level, description, pattern) |>
  arrange(rule_id) |>
  write_csv("methods/case_definition_rules.csv")

# ---------------------------------------------------------------------------
# 4. Report
# ---------------------------------------------------------------------------
set.seed(2026)
show_titles <- function(df, n) {
  df |>
    slice_sample(n = min(n, nrow(df))) |>
    transmute(line = paste0("[", if_else(source == "US CPSC", "US", "CA"), " | ",
                            case_reason, "] ", str_trunc(title, 95))) |>
    pull(line) |>
    cat(sep = "\n")
}

cat("\n========== CPSC candidates by route in ==========\n")
print(count(cpsc_h, found_by))

cat("\n========== STATUS BY SOURCE ==========\n")
print(table(combined$case_status, combined$source))

cat("\n========== REASON BY SOURCE ==========\n")
print(table(combined$case_reason, combined$source))

cat("\n========== JOINT RECALL LINKING ==========\n")
cat("Health Canada notices linked to a CPSC record:",
    sum(!is.na(combined$linked_cpsc_id)), "\n")
cat("Health Canada notices saying 'joint' but not linked:",
    sum(combined$hc_joint_flag & is.na(combined$linked_cpsc_id), na.rm = TRUE), "\n")

pairs <- combined |>
  filter(source == "Health Canada", !is.na(linked_cpsc_id)) |>
  select(event_id, ca_status = case_status, ca_title = title) |>
  left_join(combined |> filter(source == "US CPSC") |>
              select(event_id, us_status = case_status),
            by = "event_id")
cat("\nStatus agreement in linked pairs (rows = US, columns = Canada):\n")
print(table(US = pairs$us_status, Canada = pairs$ca_status))

cat("\nDiscordant linked pairs:\n")
pairs |>
  filter(us_status != ca_status) |>
  transmute(line = paste0("US ", us_status, " / CA ", ca_status, ": ",
                          str_trunc(ca_title, 90))) |>
  pull(line) |>
  cat(sep = "\n")

cat("\n========== CASES PER YEAR IN TREND WINDOW (notices) ==========\n")
cases <- combined |> filter(is_case, in_trend_window)
print(table(year = format(cases$recall_date, "%Y"), cases$source))
cat("\nDistinct recall events (both countries combined):",
    n_distinct(cases$event_id), "\n")

cat("\n========== SPOT CHECKS ==========\n")
cat("\n-- 15 probable --\n")
show_titles(filter(combined, case_status == "probable"), 15)
cat("\n-- 25 excluded --\n")
show_titles(filter(combined, case_status == "excluded"), 25)
cat("\n-- 10 not a case --\n")
show_titles(filter(combined, case_status == "not_case"), 10)
cat("\n-- 15 confirmed via component rule (CONF03) --\n")
show_titles(filter(combined, case_reason == "CONF03_component_named"), 15)