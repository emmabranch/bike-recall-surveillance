# R/site_setup.R
# Shared setup for every page of the website: loads the public dataset and
# works out the numbers quoted in the text. Each page runs source() on this.

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(here)

recalls <- read_csv(here("data", "bike_recalls.csv"),
                    col_types = cols(linked_cpsc_id = col_character(), .default = col_guess()),
                    guess_max = 5000)

this_year      <- as.integer(format(Sys.Date(), "%Y"))
last_full_year <- this_year - 1
fmt <- function(x) format(round(x), big.mark = ",")
pct <- function(x) paste0(round(100 * x), "%")

# Notices in each country's trend window (US from 2000, Canada from 2008)
trend <- recalls |> filter(in_trend_window)

# One row per recall event. Joint notices share an event; the US notice is
# used for the event's date and categories, because it is the more detailed.
events <- trend |>
  arrange(event_id, desc(source == "US CPSC"), recall_date) |>
  group_by(event_id) |>
  summarise(
    recall_date      = min(recall_date),
    countries        = paste(sort(unique(country)), collapse = " and "),
    product_form     = first(product_form),
    failed_component = first(failed_component),
    hazard_group     = first(hazard_group),
    is_ebike_related = any(is_ebike_related),
    .groups = "drop"
  ) |>
  mutate(recall_year = as.integer(format(recall_date, "%Y")))

# Headline numbers
n_events       <- nrow(events)
n_notices      <- nrow(trend)
first_year     <- min(events$recall_year)
share_fall     <- mean(events$hazard_group == "Fall or crash")
top_component  <- events |> count(failed_component, sort = TRUE) |>
  filter(failed_component != "Other or unclear") |> slice(1)
share_top_comp <- top_component$n / n_events

ebike_window <- function(y1, y2) sum(events$is_ebike_related & events$recall_year >= y1 & events$recall_year <= y2)
ebike_recent <- ebike_window(last_full_year - 2, last_full_year)
ebike_before <- ebike_window(last_full_year - 5, last_full_year - 3)

ebike_fire_share <- with(filter(events, is_ebike_related), mean(hazard_group == "Fire or burn"))
other_fire_share <- with(filter(events, !is_ebike_related), mean(hazard_group == "Fire or burn"))

n_joint <- sum(str_detect(events$countries, " and "))

# Hero background: one stripe per year since 2000, brighter in years with
# more recall events (both countries combined)
hero_stripes <- function(events, last_full_year) {
  per_year <- events |>
    filter(recall_year >= 2000, recall_year <= last_full_year) |>
    count(recall_year) |>
    complete(recall_year = 2000:last_full_year, fill = list(n = 0))
  pal <- scales::col_numeric(c("#14283a", "#30321f", "#6b5f00", "#e3c600", "#ffe94d"),
                             domain = range(per_year$n))
  rects <- paste0(sprintf('<rect x="%d" y="0" width="1" height="1" fill="%s"/>',
                          seq_len(nrow(per_year)) - 1, pal(per_year$n)),
                  collapse = "")
  sprintf('<div class="hero-bg"><svg viewBox="0 0 %d 1" preserveAspectRatio="none" aria-hidden="true">%s</svg></div>',
          nrow(per_year), rects)
}
