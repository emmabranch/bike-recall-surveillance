# 05_apply_case_definition.R  (case definition v1.3)
# Purpose: harmonise CPSC and Health Canada candidates, link joint recalls,
# apply the case definition (methods/case-definition.md), apply manual overrides.
# Reads:  data-raw/cpsc_recalls_raw.json, data-interim/hc_pages_parsed.csv,
#         methods/manual_overrides.csv, methods/manual_links.csv
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

component_words <- paste0(
  "\\b(derailleurs?|cranks?(sets?|arms?)?|chainrings?|chains?|cassettes?|freewheels?|",
  "handlebars?|stems?|seat ?posts?|saddles?|forks?|suspension|shocks?|frames?|framesets?|",
  "wheels?|wheelsets?|hubs?|rims?|spokes?|tires?|tyres?|inner tubes?|",
  "brakes?|brake (levers?|calipers?|rotors?)|shifters?|shift levers?|",
  "pedals?|quick[- ]release|bottom brackets?|headsets?)\\b")

ebike_part_words <- "\\b(batter(y|ies)|battery packs?|chargers?|motors?|controllers?|conversion kits?|throttles?)\\b"

accessory_words <- paste0(
  "\\b(locks?|lights?|lamps?|headlights?|taillights?|pumps?|bags?|panniers?|bells?|",
  "computers?|racks?|carriers?|hooks?|hoists?|brackets?|mounts?|kickstands?|fenders?|mudguards?|",
  "baskets?|bottle cages?|trainers?|stands?|reflectors?|grips?|straps?)\\b")

bicycle_code_pattern  <- "(^|; )Bicycles (&|and) "
excluded_type_pattern <- paste0("Helmet|Headgear|Exercise|Child Carriers|Baby Carriers|",
                                "Motorcycles|Dirt Bikes|Scooters|Mini bikes|Toy Ride-on|Lawn")

exclusion_rules <- tribble(
  ~rule_id, ~pattern, ~description,
  "EX01_helmet",
  "\\b(helmets?|headgear)\\b",
  "Helmets and head protection",
  "EX02_exercise",
  "\\b(exercise|stationary|spin(ning)? bikes?|indoor cycl\\w*|fitness bikes?|fitness equipment|treadmills?|ellipticals?)\\b",
  "Exercise, stationary, and indoor bikes, and fitness equipment",
  "EX03_motorized",
  "\\b(dirt ?bikes?|mini ?bikes?|pit ?bikes?|snow ?bikes?|snowmx|motorcycles?|motorbikes?|mopeds?|motocross|ATVs?|off-road vehicles?|go-?karts?)\\b",
  "Motorized two-wheelers that are not e-bikes",
  "EX04_micromobility",
  "\\b(scooters?|scoot|hoverboards?|self-balancing|unicycles?|skateboards?|morfboard)\\b",
  "E-scooters, hoverboards, and other micromobility",
  "EX05_child_passenger",
  "\\b(child (bicycle |bike )?(carriers?|seats?)|baby carriers?|(bicycle|bike|child) trailers?|trailers? (and )?conversion kits?|strollers?)\\b",
  "Child carriers, child seats, trailers, and strollers",
  "EX06_toy_ride_on",
  "\\b(ride-?on (toys?|vehicles?|cars?)|ride-?ons|toys?|toddler tricycles?|big wheel)\\b|\\btricycles?\\b.*\\b(toddlers?|child|children|kids?)\\b|\\b(toddlers?|child|children|kids?)('s)?\\b.*\\btricycles?\\b",
  "Toy tricycles and ride-on toys",
  "EX07_homonym",
  "\\b(horses?|equestrian|riding saddles?|saddle cinch(es)?|crossbows?|firearms?|guns?|gaming|wireless headsets?|ear force|earbuds?|lawn|trimmers?|edgers?)\\b",
  "Unrelated products sharing a bicycle word (never overrides an explicit bicycle word)",
  "EX08_maintenance_chemical",
  "\\b(degreasers?|lubricants?|lubes?|chain oils?|cleaners?|solvents?|polish(es)?|sealants?)\\b",
  "Maintenance chemicals: degreasers, lubricants, cleaners, sealants"
)

inclusion_rules <- tribble(
  ~rule_id,                   ~level,       ~description,
  "EX00_cpsc_product_type",   "excluded",   "CPSC product type is out of scope and not a bicycle category",
  "PROB02_mixed_product",     "probable",   "Motorized or micromobility pattern matches but a bicycle or e-bike is also named",
  "CONF01_cpsc_bicycle_code", "confirmed",  "CPSC product type is a bicycle category",
  "CONF02_bike_named",        "confirmed",  "Bicycle, e-bike, or tricycle named in product phrase or product name",
  "CONF03_component_named",   "confirmed",  "Bicycle component named, with bicycle context",
  "PROB01_part_or_accessory", "probable",   "E-bike part or bicycle accessory named, with bicycle context",
  "PROB03_sports_context",    "probable",   "Bicycle mentioned only in the description of a Health Canada sports and fitness notice",
  "NC02_incidental_mention",  "not_case",   "Bicycle mentioned only in the description, no bicycle product named",
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

firm_prefix <- "^.*?\\b(recalls|recall (of|to [a-z]+))\\s+"
product_phrase_of <- function(title) str_remove(coalesce(title, ""), rx(firm_prefix))

name_text_of <- function(title, product) {
  paste(product_phrase_of(title), coalesce(product, "")) |>
    str_remove_all(rx("toys\\s*[\"']?\\s*r\\s*[\"']?\\s*us"))
}

stop_tokens <- c("recall", "recalls", "recalled", "due", "hazard", "hazards", "risk",
                 "fall", "crash", "injury", "injuries", "serious", "death", "the", "and",
                 "for", "with", "from", "sold", "bike", "bikes", "bicycle", "bicycles",
                 "cycle", "cycling", "cpsc", "announce", "announces", "inc", "llc", "ltd",
                 "corp", "usa", "canada", "model", "models", "certain", "various",
                 "potential", "posing", "poses", "pose", "can", "may", "alert", "expanded")

tok <- function(x) {
  t <- str_split(str_replace_all(str_to_lower(coalesce(x, "")), "[^a-z0-9]+", " "), " ")[[1]]
  unique(t[nchar(t) >= 3 & !t %in% stop_tokens])
}

jacc <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(0)
  length(intersect(a, b)) / length(union(a, b))
}

# ---------------------------------------------------------------------------
# 1. CPSC candidates
# ---------------------------------------------------------------------------
cpsc_raw <- fromJSON("data-raw/cpsc_recalls_raw.json", simplifyDataFrame = TRUE) |>
  as_tibble()

cpsc_all <- cpsc_raw |>
  mutate(
    recall_date   = as.Date(substr(RecallDate, 1, 10)),
    product_type  = collapse_field(Products, "Type"),
    product_names = collapse_field(Products, "Name"),
    units_text    = collapse_field(Products, "NumberOfUnits"),
    hazard_text   = collapse_field(Hazards, "Name"),
    injury_text   = collapse_field(Injuries, "Name"),
    remedy_text   = collapse_field(RemedyOptions, "Option"),
    origin_text   = collapse_field(ManufacturerCountries, "Country"),
    joint_url     = collapse_field(Inconjunctions, "URL")
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
# 2. Health Canada candidates, stage 1 linking by URL
# ---------------------------------------------------------------------------
url_links <- map2(cpsc_h$source_id, cpsc_h$joint_url, function(id, u) {
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
  left_join(url_links, by = "url_norm") |>
  mutate(link_method = if_else(!is.na(linked_cpsc_id), "url", NA_character_)) |>
  select(-url_norm)

combined <- bind_rows(cpsc_h, hc_h) |>
  mutate(
    record_id      = paste0(if_else(source == "US CPSC", "US-", "CA-"), source_id),
    product_phrase = product_phrase_of(title),
    name_text      = name_text_of(title, product_text)
  )

# ---------------------------------------------------------------------------
# 3. Stage 2 and 3 linking: date and name similarity, then manual links
# ---------------------------------------------------------------------------
us_pool <- combined |>
  filter(source == "US CPSC", !is.na(recall_date)) |>
  transmute(us_id = source_id, us_date = recall_date, us_title = title,
            us_tokens = map(name_text, tok))

already_linked <- na.omit(combined$linked_cpsc_id)

hc_unlinked <- combined |>
  filter(source == "Health Canada", is.na(linked_cpsc_id),
         coalesce(hc_joint_flag, FALSE), !is.na(recall_date))

cand_matches <- map(seq_len(nrow(hc_unlinked)), function(i) {
  r <- hc_unlinked[i, ]
  pool <- us_pool |>
    filter(!us_id %in% already_linked,
           abs(as.numeric(us_date - r$recall_date)) <= 14)
  if (nrow(pool) == 0) {
    return(tibble(record_id = r$record_id, best_id = NA_character_,
                  score = 0, runner_up = 0))
  }
  a <- tok(r$name_text)
  s <- map_dbl(pool$us_tokens, function(b) jacc(a, b))
  o <- order(s, decreasing = TRUE)
  tibble(record_id = r$record_id, best_id = pool$us_id[o[1]],
         score = s[o[1]], runner_up = if (length(o) > 1) s[o[2]] else 0)
}) |>
  list_rbind()

if (nrow(cand_matches) == 0) {
  cand_matches <- tibble(record_id = character(), best_id = character(),
                         score = numeric(), runner_up = numeric())
}

accepted <- cand_matches |>
  filter(!is.na(best_id), score >= 0.25, score > runner_up) |>
  group_by(best_id) |>
  slice_max(score, n = 1, with_ties = FALSE) |>
  ungroup()

manual_links_path <- "methods/manual_links.csv"
if (!file.exists(manual_links_path)) {
  write_csv(tibble(record_id = character(), cpsc_id = character(), note = character()),
            manual_links_path)
}
manual_links <- read_csv(manual_links_path, col_types = cols(.default = col_character()))

accepted <- bind_rows(
  accepted |>
    filter(!record_id %in% manual_links$record_id) |>
    mutate(method = "date_and_name"),
  manual_links |>
    transmute(record_id, best_id = cpsc_id, score = NA_real_,
              runner_up = NA_real_, method = "manual")
)

combined <- combined |>
  left_join(accepted |> select(record_id, sec_id = best_id, link_score = score,
                               sec_method = method),
            by = "record_id") |>
  mutate(
    link_method    = case_when(!is.na(link_method) ~ link_method,
                               !is.na(sec_id)      ~ sec_method,
                               TRUE                ~ NA_character_),
    linked_cpsc_id = coalesce(linked_cpsc_id, sec_id),
    event_id       = case_when(
      source == "US CPSC"    ~ paste0("EV-US-", source_id),
      !is.na(linked_cpsc_id) ~ paste0("EV-US-", linked_cpsc_id),
      TRUE                   ~ paste0("EV-CA-", source_id)
    )
  ) |>
  select(-sec_id, -sec_method)

# ---------------------------------------------------------------------------
# 4. Apply the case definition
# ---------------------------------------------------------------------------
combined <- combined |>
  mutate(
    excl_rule          = first_match(name_text, exclusion_rules),
    strict_named       = has(name_text, strict_bike_words),
    homonym_overridden = excl_rule %in% "EX07_homonym" & strict_named,
    mixed              = excl_rule %in% c("EX03_motorized", "EX04_micromobility") & strict_named,
    excl_effective     = if_else(homonym_overridden | mixed, NA_character_, excl_rule),
    bike_code          = str_detect(coalesce(cpsc_product_type, ""), bicycle_code_pattern),
    type_excluded      = !bike_code & str_detect(coalesce(cpsc_product_type, ""), excluded_type_pattern),
    bike_named         = has(name_text, bike_words),
    component_named    = has(name_text, component_words),
    part_or_acc        = has(name_text, ebike_part_words) | has(name_text, accessory_words),
    bike_in_desc       = has(description_text, bike_words),
    bike_context       = bike_code | has(hc_category, "sports") | bike_in_desc,
    case_reason        = case_when(
      type_excluded                              ~ "EX00_cpsc_product_type",
      !is.na(excl_effective)                     ~ excl_effective,
      mixed                                      ~ "PROB02_mixed_product",
      bike_code                                  ~ "CONF01_cpsc_bicycle_code",
      bike_named                                 ~ "CONF02_bike_named",
      component_named & bike_context             ~ "CONF03_component_named",
      part_or_acc & bike_context                 ~ "PROB01_part_or_accessory",
      bike_in_desc & has(hc_category, "sports")  ~ "PROB03_sports_context",
      bike_in_desc                               ~ "NC02_incidental_mention",
      TRUE                                       ~ "NC01_no_bike_reference"
    ),
    case_status        = case_when(
      str_starts(case_reason, "EX")   ~ "excluded",
      str_starts(case_reason, "CONF") ~ "confirmed",
      str_starts(case_reason, "PROB") ~ "probable",
      TRUE                            ~ "not_case"
    )
  )

# ---------------------------------------------------------------------------
# 5. Manual overrides (documented case review)
# ---------------------------------------------------------------------------
override_path <- "methods/manual_overrides.csv"
if (!file.exists(override_path)) {
  write_csv(tibble(record_id = character(), case_status = character(),
                   case_reason = character(), note = character()),
            override_path)
}
overrides <- read_csv(override_path, col_types = cols(.default = col_character()))

combined <- combined |>
  left_join(overrides |> rename(ov_status = case_status, ov_reason = case_reason,
                                ov_note = note),
            by = "record_id") |>
  mutate(
    case_status     = coalesce(ov_status, case_status),
    case_reason     = if_else(!is.na(ov_status),
                              paste0("MANUAL: ", coalesce(ov_reason, "")), case_reason),
    is_case         = case_status %in% c("confirmed", "probable"),
    in_trend_window = if_else(source == "US CPSC",
                              recall_date >= as.Date("2000-01-01"),
                              recall_date >= as.Date("2008-01-01"))
  )

write_csv(combined, "data-interim/candidates_with_status.csv")

bind_rows(
  exclusion_rules |> mutate(level = "excluded"),
  inclusion_rules,
  tibble(rule_id = c("COMPONENT_TERMS", "EBIKE_PART_TERMS", "ACCESSORY_TERMS"),
         level = "term_list",
         description = c("Bicycle component terms", "E-bike part terms", "Bicycle accessory terms"),
         pattern = c(component_words, ebike_part_words, accessory_words))
) |>
  select(rule_id, level, description, pattern) |>
  arrange(rule_id) |>
  write_csv("methods/case_definition_rules.csv")

# ---------------------------------------------------------------------------
# 6. Report
# ---------------------------------------------------------------------------
set.seed(2026)
show_titles <- function(df, n) {
  if (nrow(df) == 0) { cat("(none)\n"); return(invisible()) }
  df |>
    slice_sample(n = min(n, nrow(df))) |>
    transmute(line = paste0(record_id, " | ", case_reason, " | ", str_trunc(title, 85))) |>
    pull(line) |>
    cat(sep = "\n")
}

cat("\n========== JOINT RECALL LINKING ==========\n")
print(count(filter(combined, source == "Health Canada"), link_method))
cat("Canadian notices saying 'joint' still unlinked:",
    sum(combined$hc_joint_flag & is.na(combined$linked_cpsc_id), na.rm = TRUE), "\n")

pairs <- combined |>
  filter(source == "Health Canada", !is.na(linked_cpsc_id)) |>
  select(event_id, ca_status = case_status, ca_title = title) |>
  left_join(combined |> filter(source == "US CPSC") |>
              select(event_id, us_status = case_status),
            by = "event_id")
cat("\nStatus agreement in linked pairs (rows = US, columns = Canada):\n")
print(table(US = pairs$us_status, Canada = pairs$ca_status))

cat("\n========== STATUS BY SOURCE ==========\n")
print(table(combined$case_status, combined$source))

cat("\n========== REASON BY SOURCE ==========\n")
print(table(combined$case_reason, combined$source))

cases <- combined |> filter(is_case, in_trend_window)
cat("\nDistinct recall events in trend window:", n_distinct(cases$event_id), "\n")

cat("\n-- Maintenance chemicals excluded (EX08) --\n")
show_titles(filter(combined, case_reason == "EX08_maintenance_chemical"), 20)

cat("\n-- 10 confirmed --\n")
show_titles(filter(combined, case_status == "confirmed"), 10)