# 03_fetch_hc_pages.R
# Purpose: build a sensitive candidate list of Health Canada bike-related
# recalls, fetch each recall page once (cached), and parse structured fields.

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(rvest)

Sys.setlocale("LC_TIME", "C")  # so "May 31, 2011" parses regardless of locale

bike_pattern <- regex("\\b(e-?bikes?|bicycles?|bikes?|tricycles?|cycling|pedelecs?)\\b",
                      ignore_case = TRUE)

# Component terms that might appear without the word "bike"
component_pattern <- regex(paste0(
  "\\b(derailleurs?|cranks?(sets?|arms?)?|chainrings?|handlebars?|stems?|",
  "seat ?posts?|saddles?|forks?|pedals?|hubs?|rims?|spokes?|disc brakes?|",
  "brake (levers?|calipers?|rotors?)|quick[- ]release|bottom brackets?|headsets?)\\b"),
  ignore_case = TRUE)

norm_url <- function(u) {
  u |> str_to_lower() |> str_trim() |> str_remove("^https?://") |> str_remove("/$")
}

# ---------------------------------------------------------------------------
# 1. Candidate list (three routes in, recorded in found_by)
# ---------------------------------------------------------------------------
hc  <- read_csv("data-raw/hc_recalls_raw.csv", show_col_types = FALSE)
cps <- hc |> filter(Organization == "Consumer product safety")

cand_keyword <- cps |>
  filter(str_detect(coalesce(Title, ""), bike_pattern) |
           str_detect(coalesce(Product, ""), bike_pattern)) |>
  mutate(found_by = "keyword")

cand_component <- cps |>
  filter(str_detect(coalesce(Category, ""), regex("sports|outdoor|other", ignore_case = TRUE)),
         str_detect(coalesce(Title, ""), component_pattern) |
           str_detect(coalesce(Product, ""), component_pattern)) |>
  anti_join(cand_keyword, by = "NID") |>
  mutate(found_by = "component_term")

# Health Canada URLs that CPSC lists as joint recalls (bike-related CPSC hits)
cpsc <- fromJSON("data-raw/cpsc_recalls_raw.json", simplifyDataFrame = TRUE) |>
  as_tibble()

cpsc_linked_urls <- cpsc |>
  filter(str_detect(coalesce(Title, ""), bike_pattern) |
           str_detect(coalesce(Description, ""), bike_pattern)) |>
  pull(Inconjunctions) |>
  map(function(d) if (is.data.frame(d) && "URL" %in% names(d)) d$URL else character(0)) |>
  unlist() |>
  unique()

cand_linked <- cps |>
  filter(norm_url(URL) %in% norm_url(cpsc_linked_urls)) |>
  anti_join(bind_rows(cand_keyword, cand_component), by = "NID") |>
  mutate(found_by = "cpsc_link_only")

hc_to_fetch <- bind_rows(cand_keyword, cand_component, cand_linked)

cat("\n========== CANDIDATES ==========\n")
cat("Found by keyword:              ", nrow(cand_keyword), "\n")
cat("Found by component term only:  ", nrow(cand_component), "\n")
cat("Found only via CPSC joint link:", nrow(cand_linked), "\n")
cat("Total pages to fetch:          ", nrow(hc_to_fetch), "\n")

linked_in_hc <- norm_url(cpsc_linked_urls) %in% norm_url(cps$URL)
cat("\nCPSC joint-recall URLs:", length(cpsc_linked_urls),
    "| present in HC open data:", sum(linked_in_hc),
    "| not found in HC open data:", sum(!linked_in_hc), "\n")

linked_found_by_search <- norm_url(cpsc_linked_urls[linked_in_hc]) %in%
  norm_url(c(cand_keyword$URL, cand_component$URL))
cat("Of those present, found by our HC search:", sum(linked_found_by_search),
    "of", sum(linked_in_hc), "\n")

# ---------------------------------------------------------------------------
# 2. Fetch pages (cached: re-running skips pages already saved)
# ---------------------------------------------------------------------------
page_dir <- "data-raw/hc_pages"
dir.create(page_dir, recursive = TRUE, showWarnings = FALSE)

fetch_page <- function(nid, url) {
  path <- file.path(page_dir, paste0(nid, ".html"))
  if (file.exists(path) && file.size(path) > 1000) return("cached")
  result <- tryCatch({
    request(url) |>
      req_user_agent("bike-recall-surveillance (independent research project)") |>
      req_timeout(60) |>
      req_retry(max_tries = 3) |>
      req_perform(path = path)
    "ok"
  }, error = function(e) {
    if (file.exists(path)) file.remove(path)
    paste("error:", conditionMessage(e))
  })
  Sys.sleep(1.5)  # be polite to the server
  result
}

cat("\nFetching pages...\n")
hc_to_fetch$fetch_status <- map2_chr(hc_to_fetch$NID, hc_to_fetch$URL, fetch_page,
                                     .progress = TRUE)
cat("\nFetch status:\n")
print(count(hc_to_fetch, status = str_trunc(fetch_status, 60)))

# ---------------------------------------------------------------------------
# 3. Parse each page
# ---------------------------------------------------------------------------
section_heads <- c("Product description", "Hazard identified", "Number sold",
                   "Number Sold", "Time period sold", "Place of origin",
                   "Companies", "Images", "What you should do",
                   "Additional information", "Background")

grab_section <- function(txt, label) {
  stops <- paste(setdiff(section_heads, label), collapse = "|")
  pat <- paste0(label, "\\s*:?\\s*(.*?)\\s*(?=", stops, "|$)")
  out <- str_match(txt, regex(pat, dotall = TRUE))[, 2]
  ifelse(is.na(out) | out == "", NA_character_, out)
}

grab <- function(txt, pattern) str_match(txt, pattern)[, 2]

parse_page <- function(nid) {
  path <- file.path(page_dir, paste0(nid, ".html"))
  if (!file.exists(path)) return(tibble(NID = nid))
  html <- read_html(path)
  node <- html_element(html, "main")
  if (inherits(node, "xml_missing")) node <- html_element(html, "body")
  txt <- node |> html_text2() |> str_squish()
  
  summary_match <- str_match(txt, "Summary Product:\\s*(.*?)\\s*Issue:\\s*(.*?)\\s*What to do:")
  
  tibble(
    NID              = nid,
    hc_id            = grab(txt, "Identification number:\\s*(\\S+)"),
    starting_date    = as.Date(grab(txt, "Starting date:\\s*([A-Z][a-z]+ \\d{1,2}, \\d{4})"),
                               format = "%B %d, %Y"),
    posting_date     = as.Date(grab(txt, "Posting date:\\s*([A-Z][a-z]+ \\d{1,2}, \\d{4})"),
                               format = "%B %d, %Y"),
    page_last_updated = as.Date(grab(txt, "Last updated:\\s*(\\d{4}-\\d{2}-\\d{2})")),
    summary_product  = summary_match[, 2],
    summary_issue    = summary_match[, 3],
    product_description = grab_section(txt, "Product description"),
    hazard_identified   = grab_section(txt, "Hazard identified"),
    number_sold      = coalesce(grab_section(txt, "Number sold"),
                                grab_section(txt, "Number Sold")),
    time_period_sold = grab_section(txt, "Time period sold"),
    place_of_origin  = grab_section(txt, "Place of origin"),
    joint_with_cpsc  = str_detect(txt, regex("joint recall|consumer product safety commission|US CPSC",
                                             ignore_case = TRUE)),
    page_text        = txt
  )
}

cat("\nParsing pages...\n")
parsed <- map(hc_to_fetch$NID, parse_page, .progress = TRUE) |> list_rbind()

hc_parsed <- hc_to_fetch |>
  select(NID, found_by, Title, URL, Product, Issue, Category, `Last updated`,
         Archived, fetch_status) |>
  left_join(parsed, by = "NID") |>
  mutate(hc_recall_date = coalesce(starting_date, posting_date))

write_csv(hc_parsed, "data-interim/hc_parsed.csv")

# ---------------------------------------------------------------------------
# 4. Quality report
# ---------------------------------------------------------------------------
cat("\n========== PARSE COMPLETENESS (% non-missing) ==========\n")
fields <- c("hc_id", "starting_date", "posting_date", "page_last_updated",
            "summary_issue", "product_description", "hazard_identified",
            "number_sold", "time_period_sold", "place_of_origin")
completeness <- sapply(hc_parsed[fields], function(x) round(100 * mean(!is.na(x)), 1))
print(completeness)

cat("\nRecall date range:", format(range(hc_parsed$hc_recall_date, na.rm = TRUE)), "\n")
cat("Pages mentioning a joint recall with CPSC:", sum(hc_parsed$joint_with_cpsc, na.rm = TRUE), "\n")

cat("\nRecall date vs CSV 'Last updated' (archived records):\n")
hc_parsed |>
  filter(Archived == 1, !is.na(hc_recall_date)) |>
  summarise(n = n(),
            same_day = sum(hc_recall_date == `Last updated`),
            median_gap_days = median(as.numeric(`Last updated` - hc_recall_date))) |>
  print()

cat("\nRecalls by year (recall date):\n")
hc_parsed |> count(year = format(hc_recall_date, "%Y")) |> print(n = 40)

cat("\nThree parsed examples:\n")
set.seed(2026)
hc_parsed |>
  filter(!is.na(hazard_identified)) |>
  slice_sample(n = 3) |>
  transmute(Title, found_by, hc_recall_date,
            hazard = str_trunc(hazard_identified, 200),
            number_sold = str_trunc(number_sold, 80),
            place_of_origin, joint_with_cpsc) |>
  glimpse()

cat("\nComponent-term-only titles (check for noise):\n")
hc_parsed |> filter(found_by == "component_term") |> pull(Title) |>
  head(25) |> cat(sep = "\n")