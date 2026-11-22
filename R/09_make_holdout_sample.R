# 09_make_holdout_sample.R
# Purpose: draw an independent holdout sample (records not in the development
# sample) for a second, domain-expert rater. Rules are frozen before this runs.
# Reads:  data/bike_recalls.csv, data-interim/candidates_with_status.csv,
#         data-interim/validation_key.csv
# Writes: validation/holdout_sample.xlsx, data-interim/holdout_key.csv

library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(openxlsx)

set.seed(20261122)
dir.create("validation", showWarnings = FALSE)

out_path <- "validation/holdout_sample.xlsx"
if (file.exists(out_path)) {
  stop("holdout_sample.xlsx already exists. Not overwriting, to protect labels.")
}

cand <- read_csv("data-interim/candidates_with_status.csv",
                 col_types = cols(source_id = col_character(),
                                  linked_cpsc_id = col_character(),
                                  .default = col_guess()),
                 guess_max = 5000)

recalls <- read_csv("data/bike_recalls.csv",
                    col_types = cols(linked_cpsc_id = col_character(), .default = col_guess()),
                    guess_max = 5000)

dev_ids <- read_csv("data-interim/validation_key.csv", show_col_types = FALSE)$record_id

# ---------------------------------------------------------------------------
# 1. Strata and sample (excluding development sample)
# ---------------------------------------------------------------------------
frame_all <- bind_rows(
  recalls |>
    transmute(record_id,
              stratum = if_else(is_ebike_related, "case_ebike", "case_other")),
  cand |>
    filter(!is_case) |>
    transmute(record_id, stratum = "noncase")
)

pop <- count(frame_all, stratum, name = "N")

frame <- frame_all |> filter(!record_id %in% dev_ids)

n_target <- c(case_ebike = 8, case_other = 26, noncase = 6)

sampled <- frame |>
  group_by(stratum) |>
  group_modify(function(d, k) slice_sample(d, n = min(n_target[[k$stratum]], nrow(d)))) |>
  ungroup() |>
  left_join(pop, by = "stratum") |>
  group_by(stratum) |>
  mutate(n = n(), weight = N / n) |>
  ungroup() |>
  mutate(val_id = sprintf("H%03d", sample(n()))) |>
  arrange(val_id)

stopifnot(!any(sampled$record_id %in% dev_ids))

write_csv(sampled, "data-interim/holdout_key.csv")

# ---------------------------------------------------------------------------
# 2. Blind labelling sheet
# ---------------------------------------------------------------------------
label_df <- sampled |>
  select(val_id, record_id) |>
  left_join(cand |> select(record_id, source, recall_date, title, product_text,
                           description_text, hazard_text, url),
            by = "record_id") |>
  transmute(
    val_id,
    source,
    recall_date,
    title,
    product_name = str_trunc(coalesce(product_text, ""), 300),
    description  = str_trunc(coalesce(description_text, ""), 1500),
    hazard_text  = str_trunc(coalesce(hazard_text, ""), 1000),
    url,
    is_case          = NA_character_,
    product_form     = NA_character_,
    failed_component = NA_character_,
    hazard_group     = NA_character_,
    notes            = NA_character_
  )
class(label_df$url) <- "hyperlink"

lists <- list(
  is_case = c("Yes", "No"),
  product_form = c("Complete bicycle", "Complete e-bike", "Component", "Accessory",
                   "E-bike battery, charger, or power part", "Unclear"),
  failed_component = c("Frame", "Fork", "Stem or handlebar", "Brakes", "Wheels, hubs, or tires",
                       "Drivetrain or pedals", "Saddle or seatpost",
                       "E-bike electrical system", "Accessory", "Other or unclear"),
  hazard_group = c("Fall or crash", "Fire or burn", "Laceration", "Other")
)

guide <- tribble(
  ~field, ~how_to_label,
  "About", "This is an independent check of an automated classification of public bicycle and e-bike recall notices (US CPSC and Health Canada). Please label each row from the notice text alone. Open the URL if you need more detail. There are no right answers to guess: label what you, as an engineer, would conclude.",
  "is_case", "Yes if the product is in scope: bicycles, e-bikes, balance bikes, adult tricycles and e-trikes, bicycle components, e-bike batteries, chargers, and conversion kits, and bicycle accessories. No for helmets and clothing, exercise or stationary bikes, motorized two-wheelers (dirt bikes, mini bikes, mopeds), scooters and hoverboards, child carriers, child seats and trailers, toy tricycles and ride-on toys, maintenance chemicals (lubricants, degreasers), and products that only mention bicycles in passing. If No, leave the other columns blank.",
  "product_form", "What was recalled. Complete bicycle if whole bikes were recalled, even if only one part is faulty. Complete e-bike if any e-bikes are included in the recall, even mixed with regular bikes. Component if a part was recalled on its own, or was sold both on bikes and separately. Accessory for lights, locks, pumps, bags, racks, carriers, fenders, mounts, bells, and similar. E-bike battery, charger, or power part if those were recalled on their own.",
  "Component vs accessory", "A component is needed to ride the bike, or carries load or controls it: frame, fork, stem, handlebar (including bar ends, aerobars, and extensions), brakes, wheels, hubs, tires, quick releases, cranks, pedals, chain, derailleurs, saddle, seatpost. An accessory is added to the bike or used with it, and the bike works without it.",
  "failed_component", "The part whose failure creates the hazard. For a complete bike with a cracking fork, choose Fork. For a part recalled on its own, choose that part. E-bike electrical system covers batteries, chargers, motors, controllers, wiring, and displays. A missing or non-compliant foot brake counts as Brakes. Lead paint and other compliance problems with no failing part count as Other or unclear.",
  "hazard_group", "The main injury mechanism. Fall or crash covers loss of control, ejection, and parts breaking while riding. Fire or burn covers overheating batteries and chargers. Laceration covers cuts and sharp edges. Other covers lead, choking, entrapment, shock, and anything else. If a notice lists both fall and fire, choose the one caused by the main failure. If they are genuinely equal, choose Fire or burn.",
  "notes", "Optional. Note anything ambiguous, or anything the categories handle badly."
)

wb <- createWorkbook()
addWorksheet(wb, "Label here")
addWorksheet(wb, "Guide")
addWorksheet(wb, "Lists")

for (i in seq_along(lists)) {
  writeData(wb, "Lists", tibble(!!names(lists)[i] := lists[[i]]), startCol = i)
}

header_style <- createStyle(textDecoration = "bold", fgFill = "#1F3A5F", fontColour = "#FFFFFF",
                            wrapText = TRUE, valign = "top")
body_style   <- createStyle(wrapText = TRUE, valign = "top")
input_style  <- createStyle(wrapText = TRUE, valign = "top", fgFill = "#FFF7D6")

writeData(wb, "Label here", label_df, headerStyle = header_style)

n_rows <- nrow(label_df)
rows   <- 2:(n_rows + 1)
input_cols <- match(c("is_case", "product_form", "failed_component", "hazard_group", "notes"),
                    names(label_df))

addStyle(wb, "Label here", body_style, rows = rows, cols = 1:ncol(label_df), gridExpand = TRUE)
addStyle(wb, "Label here", input_style, rows = rows, cols = input_cols, gridExpand = TRUE)

list_cols <- c(is_case = "A", product_form = "B", failed_component = "C", hazard_group = "D")
for (nm in names(list_cols)) {
  col_letter <- list_cols[[nm]]
  n_items <- length(lists[[nm]])
  dataValidation(wb, "Label here",
                 cols = match(nm, names(label_df)), rows = rows,
                 type = "list",
                 value = sprintf("'Lists'!$%s$2:$%s$%d", col_letter, col_letter, n_items + 1))
}

setColWidths(wb, "Label here", cols = 1:ncol(label_df),
             widths = c(8, 12, 11, 40, 30, 70, 55, 18, 10, 22, 24, 16, 30))
setRowHeights(wb, "Label here", rows = rows, heights = 150)
freezePane(wb, "Label here", firstActiveRow = 2, firstActiveCol = 5)

writeData(wb, "Guide", guide, headerStyle = header_style)
addStyle(wb, "Guide", body_style, rows = 2:(nrow(guide) + 1), cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Guide", cols = 1:2, widths = c(22, 120))

saveWorkbook(wb, out_path)

# ---------------------------------------------------------------------------
# 3. Report (no rule outputs)
# ---------------------------------------------------------------------------
cat("\n========== HOLDOUT SAMPLE ==========\n")
sampled |>
  distinct(stratum, N, n, weight) |>
  mutate(weight = round(weight, 2)) |>
  print()
cat("\nOverlap with development sample:", sum(sampled$record_id %in% dev_ids), "\n")
cat("Total rows:", n_rows, "\n")
cat("Workbook written to", out_path, "\n")
cat("Rule outputs are hidden in data-interim/holdout_key.csv.\n")