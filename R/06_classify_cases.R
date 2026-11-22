# 06_classify_cases.R  (classification v1.1)
# Purpose: classify each case by product form, failed component, and hazard;
# extract incidents, injuries, deaths, units, origin, remedy; flag possible e-motos.
# Reads:  data-interim/candidates_with_status.csv
# Writes: data/bike_recalls.csv, methods/classification_rules.csv

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)

dir.create("data", showWarnings = FALSE)

rx  <- function(p) regex(p, ignore_case = TRUE)
has <- function(text, p) str_detect(coalesce(text, ""), rx(p))

cand <- read_csv("data-interim/candidates_with_status.csv",
                 col_types = cols(source_id = col_character(),
                                  linked_cpsc_id = col_character(),
                                  .default = col_guess()),
                 guess_max = 5000)

cases <- cand |> filter(is_case)

# ---------------------------------------------------------------------------
# Rule tables (order matters: earlier rows win ties)
# ---------------------------------------------------------------------------
component_rules <- tribble(
  ~category,          ~label,                      ~pattern,
  "ebike_electrical", "E-bike electrical system",  "\\b(batter(y|ies)|battery packs?|lithium|cells?|chargers?|charging|motors?|controllers?|wiring|wire harness(es)?|wires?|connectors?|displays?|throttles?|electrical|power suppl(y|ies)|accelerat\\w*)\\b",
  "fork",             "Fork",                      "\\b(forks?|steerers?|steerer tubes?|steer tubes?|steer tube clamps?|steering tubes?|fork crowns?|crown races?)\\b",
  "frame",            "Frame",                     "\\b(frames?|framesets?|top ?tubes?|down ?tubes?|head ?tubes?|seat tubes?|seat ?stays?|chain ?stays?|rear triangles?|welds?|swing ?arms?|rear shocks?|shock mounts?|linkages?|dropouts?|folding hinges?|frame hinges?)\\b",
  "stem_handlebar",   "Stem or handlebar",         "\\b(stems?|stem raisers?|handlebars?|handle bars?|handleposts?|handle posts?|steering posts?|aero ?bars?|bar ends?|steering columns?|headsets?)\\b",
  "brakes",           "Brakes",                    "\\b(brakes?|braking|foot ?brakes?|hand ?brakes?|coaster brakes?|calipers?|brake levers?|brake pads?|brake cables?|brake hoses?|rotors?|master cylinders?)\\b",
  "wheels_hubs",      "Wheels, hubs, or tires",    "\\b(wheels?|wheelsets?|rims?|spokes?|hubs?|axles?|quick[- ]releases?|skewers?|thru[- ]axles?|tires?|tyres?|inner tubes?|valves?|freehubs?)\\b",
  "drivetrain",       "Drivetrain or pedals",      "\\b(cranks?|crank ?arms?|cranksets?|chainrings?|chains?|cassettes?|freewheels?|derailleurs?|derailleur hangers?|bottom brackets?|pedals?|pedal axles?|spindles?|shifters?|shift levers?|gears?|gear hubs?|belts?|belt drives?|sprockets?|cogs?)\\b",
  "seat_seatpost",    "Saddle or seatpost",        "\\b(seat ?posts?|saddles?|seats?|seat clamps?|saddle rails?|seat rails?)\\b",
  "accessory",        "Accessory",                 "\\b(lights?|lamps?|headlights?|taillights?|locks?|bags?|panniers?|racks?|carriers?|hooks?|hitch(es)?|adaptor bars?|adapter bars?|cradles?|pumps?|bells?|fenders?|mudguards?|kickstands?|baskets?|reflectors?|computers?|mounts?|brackets?|hoists?|straps?|bottle cages?|trainers?|flywheels?|grips?|keys?)\\b"
)

structural_cats <- c("fork", "frame", "stem_handlebar", "brakes", "wheels_hubs",
                     "drivetrain", "seat_seatpost")

hazard_rules <- tribble(
  ~category,           ~group,          ~pattern,
  "fire_burn",         "Fire or burn",  "\\b(fire|burns?|burning|overheat\\w*|thermal runaway|smok\\w*|flames?|explo\\w*|ignit\\w*)\\b",
  "fall_crash",        "Fall or crash", "\\b(falls?|falling|crash\\w*|loss of control|lose control|losing control|collisions?|thrown|eject\\w*)\\b",
  "laceration",        "Laceration",    "\\b(lacerat\\w*|cuts?|amputat\\w*|sharp edges?)\\b",
  "entrapment_pinch",  "Other",         "\\b(entrap\\w*|pinch\\w*|crush\\w*|strangulat\\w*|fingertips?)\\b",
  "choking_ingestion", "Other",         "\\b(chok\\w*|ingest\\w*|swallow\\w*|aspirat\\w*)\\b",
  "chemical_lead",     "Other",         "\\b(lead paint|lead content|lead levels?|excessive lead|lead poisoning|leaded|federal lead|phthalates?|chemical|toxic)\\b",
  "electrical_shock",  "Other",         "\\b(electric(al)? shock|shock hazard|electrocut\\w*)\\b"
)

ebike_pat <- paste0("\\b(e-?bikes?|ebikes?|electric (bicycles?|bikes?|tricycles?|trikes?|cargo bikes?)|",
                    "e-?trikes?|pedal[- ]assist(ed)?|pedelecs?|power[- ]assisted bicycles?|",
                    "electric[- ]assist|motori[sz]ed bicycles?)\\b")

# A bike word used as the product itself: plural, or singular followed by a
# whole boundary word or punctuation ("Bicycle Forks" is NOT a complete bike)
complete_pat <- paste0("\\b(e-?bikes|ebikes|bicycles|bikes|tricycles|trikes|pedelecs)\\b|",
                       "\\b(e-?bike|ebike|bicycle|bike|tricycle|trike|pedelec)\\b",
                       "(?=\\s*(?:$|[(;,:.]|(?:recalled|recall|due|with|equipped|and|sold|from|for|by)\\b))")

# Phrases describing what a part was sold on or for, removed before deciding
# whether the product itself is a whole bike
fitted_to_pat <- "\\s+(?:(?:sold|used|installed|fitted|found|offered)\\s+)?(?:on|for|in)\\s+.*$"

generic_parts_pat <- "\\b(components?|parts?)\\b"

foot_brake_pat <- "\\b(foot ?brakes?|coaster brakes?)\\b"

death_pat <- "\\b(died|deaths?|fatal\\w*|killed)\\b"

# ---------------------------------------------------------------------------
# Matching helpers
# ---------------------------------------------------------------------------
first_mention <- function(text, rules) {
  text <- coalesce(text, "")
  pos <- vapply(rules$pattern, function(p) {
    s <- str_locate(text, rx(p))[, "start"]
    ifelse(is.na(s), Inf, s)
  }, numeric(length(text)))
  pos <- matrix(pos, nrow = length(text))
  apply(pos, 1, function(r) if (all(is.infinite(r))) NA_character_ else rules$category[which.min(r)])
}

last_mention <- function(text, rules) {
  text <- coalesce(text, "")
  pos <- vapply(rules$pattern, function(p) {
    map_dbl(str_locate_all(text, rx(p)), function(m) if (nrow(m) == 0) -Inf else max(m[, "start"]))
  }, numeric(length(text)))
  pos <- matrix(pos, nrow = length(text))
  apply(pos, 1, function(r) if (all(is.infinite(r))) NA_character_ else rules$category[which.max(r)])
}

priority_match <- function(text, rules) {
  text <- coalesce(text, "")
  hit <- vapply(rules$pattern, function(p) str_detect(text, rx(p)), logical(length(text)))
  hit <- matrix(hit, nrow = length(text))
  apply(hit, 1, function(r) if (any(r)) rules$category[which(r)[1]] else NA_character_)
}

all_matches <- function(text, rules) {
  text <- coalesce(text, "")
  hit <- vapply(rules$pattern, function(p) str_detect(text, rx(p)), logical(length(text)))
  hit <- matrix(hit, nrow = length(text))
  apply(hit, 1, function(r) if (any(r)) paste(rules$category[r], collapse = "; ") else NA_character_)
}

# ---------------------------------------------------------------------------
# Extraction helpers
# ---------------------------------------------------------------------------
num_token <- paste0("(\\d[\\d,]*|no|zero|one|two|three|four|five|six|seven|eight|nine|ten|",
                    "eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|",
                    "nineteen|twenty|thirty|forty|fifty)")
word_nums <- c(no = 0, zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6,
               seven = 7, eight = 8, nine = 9, ten = 10, eleven = 11, twelve = 12,
               thirteen = 13, fourteen = 14, fifteen = 15, sixteen = 16, seventeen = 17,
               eighteen = 18, nineteen = 19, twenty = 20, thirty = 30, forty = 40, fifty = 50)

to_num <- function(x) {
  x <- str_to_lower(str_remove_all(x, ","))
  d <- suppressWarnings(as.numeric(x))
  ifelse(!is.na(d), d, unname(word_nums[x]))
}

extract_count <- function(text, after) {
  m <- str_match(coalesce(text, ""), rx(paste0("\\b", num_token, "\\s+", after)))
  to_num(m[, 2])
}

parse_units <- function(x) {
  m <- str_match(str_to_lower(coalesce(x, "")), "(\\d[\\d,]*(\\.\\d+)?)\\s*(million)?")
  n <- suppressWarnings(as.numeric(str_remove_all(m[, 2], ",")))
  ifelse(!is.na(m[, 4]), n * 1e6, n)
}

max_num <- function(text, pat) {
  map_dbl(coalesce(text, ""), function(t) {
    m <- str_match_all(t, rx(pat))[[1]]
    if (nrow(m) == 0) return(NA_real_)
    v <- suppressWarnings(as.numeric(str_remove_all(m[, 2], ",")))
    if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
  })
}

country_pat <- paste0("\\b(China|Taiwan|Vietnam|Viet Nam|Cambodia|Thailand|Indonesia|Malaysia|",
                      "India|Bangladesh|Philippines|Sri Lanka|Korea|Japan|United States|USA|",
                      "Canada|Mexico|Italy|Germany|France|Spain|Portugal|Netherlands|Switzerland|",
                      "Austria|Czech Republic|Poland|United Kingdom|England|UK|Sweden|Denmark|",
                      "Hungary|Romania|Bulgaria|Turkey|Israel|Australia|New Zealand)\\b")

norm_country <- function(x) {
  x <- str_to_lower(x)
  case_when(
    x %in% c("usa", "united states") ~ "United States",
    x %in% c("uk", "england", "united kingdom") ~ "United Kingdom",
    x %in% c("viet nam", "vietnam") ~ "Vietnam",
    TRUE ~ str_to_title(x)
  )
}

extract_countries <- function(text) {
  map_chr(str_extract_all(coalesce(text, ""), rx(country_pat)), function(v) {
    if (length(v) == 0) NA_character_ else paste(unique(norm_country(v)), collapse = "; ")
  })
}

# ---------------------------------------------------------------------------
# 1. Hazard (first pass)
# ---------------------------------------------------------------------------
cases <- cases |>
  mutate(
    title_due_to        = str_match(coalesce(title, ""), rx("due to (.*)$"))[, 2],
    hazard_primary_text = paste(coalesce(title_due_to, ""), coalesce(hc_issue, "")),
    hazard_all_text     = paste(title, hazard_primary_text, coalesce(hazard_text, "")),
    hazard_detail       = coalesce(priority_match(hazard_primary_text, hazard_rules),
                                   priority_match(hazard_all_text, hazard_rules),
                                   "other_injury"),
    hazards_mentioned   = all_matches(hazard_all_text, hazard_rules)
  )

# ---------------------------------------------------------------------------
# 2. Product form and failed component
# ---------------------------------------------------------------------------
cases <- cases |>
  mutate(
    head_text = str_remove(coalesce(product_phrase, ""),
                           rx("\\s+(due to|recalled|recall|because|posing|poses|pose|sold|announced|reannounced)\\b.*$")),
    head_core = str_remove(head_text, rx(fitted_to_pat)),
    head_cat  = coalesce(last_mention(head_core, component_rules),
                         last_mention(product_text, component_rules)),
    term_in_head = !is.na(last_mention(head_core, component_rules)) | has(head_core, generic_parts_pat),
    is_complete  = has(head_core, complete_pat) | (!term_in_head & has(name_text, complete_pat)),
    is_ebike_any = has(name_text, ebike_pat) | has(description_text, ebike_pat),
    product_form = case_when(
      is_complete & is_ebike_any                      ~ "Complete e-bike",
      is_complete                                     ~ "Complete bicycle",
      head_cat == "ebike_electrical"                  ~ "E-bike battery, charger, or power part",
      head_cat == "accessory"                         ~ "Accessory",
      !is.na(head_cat)                                ~ "Component",
      has(head_core, generic_parts_pat)               ~ "Component",
      TRUE                                            ~ "Unclear"
    ),
    is_ebike_related = product_form %in% c("Complete e-bike", "E-bike battery, charger, or power part") |
      is_ebike_any,
    hazard_comp_cat = first_mention(hazard_text, component_rules),
    name_comp_cat   = first_mention(name_text, component_rules),
    failed_cat = case_when(
      hazard_detail == "chemical_lead"                              ~ "other",
      product_form %in% c("Component", "Accessory",
                          "E-bike battery, charger, or power part") ~ coalesce(head_cat, "other"),
      has(hazard_text, foot_brake_pat)                              ~ "brakes",
      !is.na(hazard_comp_cat)                                       ~ hazard_comp_cat,
      !is.na(name_comp_cat)                                         ~ name_comp_cat,
      TRUE                                                          ~ "other"
    ),
    failed_component = coalesce(component_rules$label[match(failed_cat, component_rules$category)],
                                "Other or unclear"),
    components_mentioned = all_matches(paste(name_text, coalesce(hazard_text, "")), component_rules)
  )

# ---------------------------------------------------------------------------
# 3. Hazard (second pass): generic injury with a structural failure, or the
#    old CPSC "Vehicle Accident" code, is a fall or crash
# ---------------------------------------------------------------------------
cases <- cases |>
  mutate(
    hazard_detail = if_else(
      hazard_detail == "other_injury" &
        (has(hazard_text, "vehicle accident") | failed_cat %in% structural_cats),
      "fall_crash", hazard_detail),
    hazard_group = coalesce(hazard_rules$group[match(hazard_detail, hazard_rules$category)], "Other")
  )

# ---------------------------------------------------------------------------
# 4. Incidents, injuries, deaths, units, origin
# ---------------------------------------------------------------------------
cases <- cases |>
  mutate(
    incident_text = if_else(source == "US CPSC", coalesce(injury_text, hazard_text), hazard_text),
    none_reported = has(incident_text, "^\\s*none reported"),
    incidents_reported = case_when(
      none_reported ~ 0,
      TRUE ~ extract_count(incident_text,
                           "(\\S+\\s+){0,2}?(reports?|incidents?|complaints?|claims?)")
    ),
    injuries_reported = case_when(
      none_reported ~ 0,
      has(incident_text, "\\bno (\\S+\\s+){0,3}injur") ~ 0,
      TRUE ~ extract_count(incident_text, "(\\S+\\s+){0,4}?injur(y|ies)")
    ),
    death_text      = str_remove_all(coalesce(incident_text, ""), rx("(serious )?injur(y|ies) or death")),
    death_mentioned = has(death_text, death_pat),
    deaths_reported = extract_count(death_text, "(\\S+\\s+){0,2}?deaths?"),
    units_recalled   = parse_units(units_text),
    origin_countries = extract_countries(origin_text),
    origin_primary   = str_extract(origin_countries, "^[^;]+")
  )

# ---------------------------------------------------------------------------
# 5. Possible e-moto flag
# ---------------------------------------------------------------------------
cases <- cases |>
  mutate(
    emoto_text = paste(title, coalesce(description_text, ""), coalesce(hazard_text, "")),
    max_mph    = max_num(emoto_text, "(\\d+(?:\\.\\d+)?)\\s*(?:mph|miles per hour)"),
    max_kmh    = max_num(emoto_text, "(\\d+(?:\\.\\d+)?)\\s*(?:km/h|kph|km per hour|kilometres per hour|kilometers per hour)"),
    max_watts  = max_num(emoto_text, "(\\d[\\d,]*)\\s*-?\\s*(?:w|watts?)\\b"),
    no_pedals  = has(emoto_text, "\\b(no pedals|without pedals|does not have pedals|do not have pedals|throttle[- ]only)\\b"),
    watt_limit = if_else(source == "US CPSC", 750, 500),
    possible_emoto = is_ebike_related & (
      no_pedals |
        coalesce(max_mph, 0) > 28 |
        coalesce(max_kmh, 0) > 32 |
        coalesce(max_watts, 0) > watt_limit
    ),
    emoto_evidence = if_else(possible_emoto,
                             str_squish(paste(
                               if_else(no_pedals, "no pedals or throttle-only;", ""),
                               if_else(coalesce(max_mph, 0) > 28, paste0(max_mph, " mph;"), ""),
                               if_else(coalesce(max_kmh, 0) > 32, paste0(max_kmh, " km/h;"), ""),
                               if_else(coalesce(max_watts, 0) > watt_limit, paste0(max_watts, " W;"), "")
                             )),
                             NA_character_)
  )

# ---------------------------------------------------------------------------
# 6. Write the public dataset
# ---------------------------------------------------------------------------
bike_recalls <- cases |>
  transmute(
    record_id, event_id, source,
    country = if_else(source == "US CPSC", "United States", "Canada"),
    recall_date,
    recall_year = as.integer(format(recall_date, "%Y")),
    in_trend_window,
    title, url,
    case_status, case_reason,
    product_form, is_ebike_related, possible_emoto, emoto_evidence,
    failed_component, components_mentioned,
    hazard_group, hazard_detail, hazards_mentioned,
    incidents_reported, injuries_reported, death_mentioned, deaths_reported,
    units_recalled, units_text,
    remedy_us = remedy_text,
    origin_primary, origin_countries,
    linked_cpsc_id, link_method,
    hazard_text, description_text
  ) |>
  arrange(desc(recall_date))

write_csv(bike_recalls, "data/bike_recalls.csv")

bind_rows(
  component_rules |> transmute(dimension = "failed_component", category, label, pattern),
  hazard_rules    |> transmute(dimension = "hazard", category, label = group, pattern),
  tibble(dimension = "product_form",
         category = c("complete", "ebike", "fitted_to", "generic_parts"),
         label = c("Bike word used as the product", "E-bike terms",
                   "Phrase removed before the complete-bike check", "Generic parts wording"),
         pattern = c(complete_pat, ebike_pat, fitted_to_pat, generic_parts_pat)),
  tibble(dimension = "special", category = c("foot_brake", "death"),
         label = c("Missing foot brake or coaster brake codes as Brakes", "Death mentioned"),
         pattern = c(foot_brake_pat, death_pat))
) |>
  write_csv("methods/classification_rules.csv")

# ---------------------------------------------------------------------------
# 7. Report
# ---------------------------------------------------------------------------
cat("\n========== CASES ==========\n")
print(count(bike_recalls, source))

cat("\n========== PRODUCT FORM ==========\n")
print(table(bike_recalls$product_form, bike_recalls$source))

cat("\n========== FAILED COMPONENT ==========\n")
print(table(bike_recalls$failed_component, bike_recalls$source))

cat("\n========== HAZARD GROUP ==========\n")
print(table(bike_recalls$hazard_group, bike_recalls$source))

set.seed(2026)
show_cases <- function(df, n) {
  if (nrow(df) == 0) { cat("(none)\n"); return(invisible()) }
  df |>
    slice_sample(n = min(n, nrow(df))) |>
    transmute(line = paste0(
      record_id, " | ", str_trunc(title, 80),
      "\n   form: ", product_form, " | component: ", failed_component,
      " | hazard: ", hazard_detail,
      "\n   hazard text: ", str_trunc(coalesce(hazard_text, ""), 170))) |>
    pull(line) |>
    cat(sep = "\n")
}

cat("\n========== 15 'OTHER OR UNCLEAR' COMPONENT ==========\n")
show_cases(filter(cases, failed_component == "Other or unclear"), 15)