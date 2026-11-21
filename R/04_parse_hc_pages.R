# 04_parse_hc_pages.R
# Purpose: parse saved Health Canada recall pages. Handles two page layouts:
#   pre_2022     "Starting date: May 31, 2011", labels with colons
#   2022_onward  "Original published date: 2026-10-08", labels on own lines
# Reads:  data-interim/hc_parsed.csv (candidate list from 03), data-raw/hc_pages/
# Writes: data-interim/hc_pages_parsed.csv

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(rvest)

Sys.setlocale("LC_TIME", "C")
page_dir <- "data-raw/hc_pages"

candidates <- read_csv("data-interim/hc_parsed.csv", show_col_types = FALSE) |>
  select(NID, found_by, Title, URL, Product, Issue, Category,
         `Last updated`, Archived, fetch_status)

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------
grab <- function(txt, pattern) str_match(txt, pattern)[, 2]

safe_date_iso <- function(x) as.Date(str_extract(x, "\\d{4}-\\d{2}-\\d{2}"))

safe_date_long <- function(x) as.Date(x, format = "%B %d, %Y")

joint_pattern <- regex("joint recall|consumer product safety commission|US CPSC",
                       ignore_case = TRUE)

# ---------------------------------------------------------------------------
# Layout 1: pre-2022 pages (work on whitespace-squished text)
# ---------------------------------------------------------------------------
old_heads <- c("Product description", "Hazard identified",
               "Number sold", "Number Sold",
               "Time period sold", "Time Period Sold",
               "Place of origin", "Place of Origin",
               "Companies", "Images", "What you should do",
               "Additional information", "Background", "Details")

grab_section <- function(txt, label) {
  stops <- paste(setdiff(old_heads, label), collapse = "|")
  pat <- paste0(label, "\\s*:?\\s*(.*?)\\s*(?=", stops, "|$)")
  out <- str_match(txt, regex(pat, dotall = TRUE))[, 2]
  ifelse(is.na(out) | out == "", NA_character_, out)
}

parse_old <- function(txt_raw) {
  txt <- str_squish(txt_raw)
  sm  <- str_match(txt, "Summary Product:\\s*(.*?)\\s*Issue:\\s*(.*?)\\s*What to do:")
  starting <- safe_date_long(grab(txt, "Starting date:\\s*([A-Z][a-z]+ \\d{1,2}, \\d{4})"))
  posting  <- safe_date_long(grab(txt, "Posting date:\\s*([A-Z][a-z]+ \\d{1,2}, \\d{4})"))
  tibble(
    template            = "pre_2022",
    hc_id               = grab(txt, "Identification number:\\s*(\\S+)"),
    recall_date         = coalesce(starting, posting),
    recall_date_source  = case_when(!is.na(starting) ~ "starting_date",
                                    !is.na(posting)  ~ "posting_date",
                                    TRUE ~ NA_character_),
    page_last_updated   = safe_date_iso(grab(txt, "Last updated:\\s*(\\d{4}-\\d{2}-\\d{2})")),
    brand               = NA_character_,
    summary_product     = sm[, 2],
    summary_issue       = sm[, 3],
    product_description = grab_section(txt, "Product description"),
    hazard_identified   = grab_section(txt, "Hazard identified"),
    number_sold         = coalesce(grab_section(txt, "Number sold"),
                                   grab_section(txt, "Number Sold")),
    time_period_sold    = coalesce(grab_section(txt, "Time period sold"),
                                   grab_section(txt, "Time Period Sold")),
    place_of_origin     = coalesce(grab_section(txt, "Place of origin"),
                                   grab_section(txt, "Place of Origin"))
  )
}

# ---------------------------------------------------------------------------
# Layout 2: 2022-onward pages (work line by line)
# ---------------------------------------------------------------------------
# Text between the nth line equal to `start` and the next line in `stops`
section_lines <- function(lines, start, stops, occurrence = 1, sep = " ") {
  idx <- which(lines == start)
  if (length(idx) < occurrence) return(NA_character_)
  i <- idx[occurrence]
  stop_idx <- which(lines %in% stops & seq_along(lines) > i)
  j <- if (length(stop_idx) > 0) min(stop_idx) - 1 else length(lines)
  if (j <= i) return(NA_character_)
  out <- str_squish(lines[(i + 1):j])
  out <- out[out != ""]
  if (length(out) == 0) NA_character_ else paste(out, collapse = sep)
}

# First non-empty line after a label
next_line <- function(lines, label) {
  idx <- which(lines == label)
  if (length(idx) == 0 || idx[1] == length(lines)) return(NA_character_)
  rest <- lines[(idx[1] + 1):length(lines)]
  rest <- rest[rest != ""]
  if (length(rest) == 0) NA_character_ else rest[1]
}

parse_new <- function(txt_raw) {
  lines <- str_split(txt_raw, "\n")[[1]] |> str_trim()
  tibble(
    template            = "2022_onward",
    hc_id               = next_line(lines, "Identification number"),
    recall_date         = safe_date_iso(grab(txt_raw, "Original published date:\\s*(\\d{4}-\\d{2}-\\d{2})")),
    recall_date_source  = "original_published_date",
    page_last_updated   = safe_date_iso(next_line(lines, "Last updated")),
    brand               = section_lines(lines, "Brand(s)", "Last updated", sep = "; "),
    summary_product     = section_lines(lines, "Product", "Issue"),
    summary_issue       = section_lines(lines, "Issue", "What to do", sep = "; "),
    product_description = section_lines(lines, "Affected products", "Issue"),
    hazard_identified   = section_lines(lines, "Issue", "What you should do", occurrence = 2),
    number_sold         = section_lines(lines, "Number Sold",
                                        c("Time Period Sold", "Place of Origin", "Details")),
    time_period_sold    = section_lines(lines, "Time Period Sold",
                                        c("Place of Origin", "Details")),
    place_of_origin     = section_lines(lines, "Place of Origin", c("Details", "Companies"))
  )
}

# ---------------------------------------------------------------------------
# Parse every saved page, choosing the layout automatically
# ---------------------------------------------------------------------------
parse_page <- function(nid) {
  path <- file.path(page_dir, paste0(nid, ".html"))
  if (!file.exists(path)) return(tibble(NID = nid, template = "missing_page"))
  html <- read_html(path)
  node <- html_element(html, "main")
  if (inherits(node, "xml_missing")) node <- html_element(html, "body")
  txt_raw <- html_text2(node)
  
  out <- if (str_detect(txt_raw, "Original published date:")) parse_new(txt_raw) else parse_old(txt_raw)
  
  out |>
    mutate(NID             = nid,
           joint_with_cpsc = str_detect(txt_raw, joint_pattern),
           page_text       = str_squish(txt_raw),
           .before = 1)
}

parsed <- map(candidates$NID, parse_page, .progress = TRUE) |> list_rbind()

hc_pages_parsed <- candidates |> left_join(parsed, by = "NID")
write_csv(hc_pages_parsed, "data-interim/hc_pages_parsed.csv")

# ---------------------------------------------------------------------------
# Quality report
# ---------------------------------------------------------------------------
cat("\n========== COMPLETENESS BY LAYOUT (% non-missing) ==========\n")
fields <- c("hc_id", "recall_date", "summary_issue", "product_description",
            "hazard_identified", "number_sold", "time_period_sold", "place_of_origin")
hc_pages_parsed |>
  group_by(template) |>
  summarise(n = n(), across(all_of(fields), ~ round(100 * mean(!is.na(.x))))) |>
  print(width = Inf)

cat("\nnumber_sold values that ran on into the next section:",
    sum(str_detect(hc_pages_parsed$number_sold, "Time Period|Place of"), na.rm = TRUE), "\n")

cat("\nRecall date range:", format(range(hc_pages_parsed$recall_date, na.rm = TRUE)), "\n")
cat("Joint recall with CPSC mentioned:", sum(hc_pages_parsed$joint_with_cpsc, na.rm = TRUE), "\n")

cat("\nCSV 'Last updated' differs from recall date (2022_onward pages):",
    hc_pages_parsed |>
      filter(template == "2022_onward") |>
      summarise(sum(`Last updated` != recall_date, na.rm = TRUE)) |>
      pull(), "\n")

cat("\nRecalls by year:\n")
hc_pages_parsed |> count(year = format(recall_date, "%Y")) |> print(n = 40)

cat("\nRecords still without a recall date:\n")
hc_pages_parsed |>
  filter(is.na(recall_date)) |>
  transmute(NID, template, Title = str_trunc(Title, 70), URL) |>
  print(width = Inf)

cat("\nTwo 2022-onward examples:\n")
set.seed(2026)
hc_pages_parsed |>
  filter(template == "2022_onward") |>
  slice_sample(n = 2) |>
  transmute(Title, recall_date, brand, summary_issue,
            hazard  = str_trunc(hazard_identified, 200),
            number_sold = str_trunc(number_sold, 100),
            time_period_sold, place_of_origin, joint_with_cpsc) |>
  glimpse()