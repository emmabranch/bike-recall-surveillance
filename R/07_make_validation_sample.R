# 07_make_validation_sample.R
# Purpose: draw a stratified random validation sample and write a blind
# labelling workbook (no rule outputs shown).
# Reads:  data/bike_recalls.csv, data-interim/candidates_with_status.csv
# Writes: validation/validation_sample.xlsx (labelling workbook)
#         data-interim/validation_key.csv (hidden rule outputs and weights)

library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(openxlsx)

set.seed(20261121)
dir.create("validation", showWarnings = FALSE)

out_path <- "validation/validation_sample.xlsx"
if (file.exists(out_path)) {
  stop("validation_sample.xlsx already exists. Not overwriting, to protect your labels.")
}

cand <- read_csv("data-interim/candidates_with_status.csv",
                 col_types = cols(source_id = col_character(),
                                  linked_cpsc_id = col_character(),
                                  .default = col_guess()),
                 guess_max = 5000)

recalls <- read_csv("data/bike_recalls.csv",
                    col_types = cols(linked_cpsc_id = col_character(), .default = col_guess()),
                    guess_max = 5000)

# ---------------------------------------------------------------------------
# 1. Strata and sample
# ---------------------------------------------------------------------------
frame <- bind_rows(
  recalls |>
    transmute(record_id,
              stratum = if_else(is_ebike_related, "case_ebike", "case_other")),
  cand |>
    filter(!is_case) |>
    transmute(record_id, stratum = "noncase")
)

n_target <- c(case_ebike = 15, case_other = 65, noncase = 20)

pop <- count(frame, stratum, name = "N")

sampled <- frame |>
  group_by(stratum) |>
  group_modify(function(d, k) slice_sample(d, n = min(n_target[[k$stratum]], nrow(d)))) |>
  ungroup() |>
  left_join(pop, by = "stratum") |>
  group_by(stratum) |>
  mutate(n = n(), weight = N / n) |>
  ungroup() |>
  mutate(val_id = sprintf("V%03d", sample(n()))) |>
  arrange(val_id)

# ---------------------------------------------------------------------------
# 2. Hidden key: rule outputs, strata, weights
# ---------------------------------------------------------------------------
key <- sampled |>
  left_join(cand |> select(record_id, rule_is_case = is_case, rule_case_reason = case_reason),
            by = "record_id") |>
  left_join(recalls |> select(record_id,
                              rule_product_form = product_form,
                              rule_failed_component = failed_component,
                              rule_hazard_group = hazard_group),
            by = "record_id")

write_csv(key, "data-interim/validation_key.csv")

# ---------------------------------------------------------------------------
# 3. Blind labelling sheet
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
                       "E-bike battery, charger, or motor", "Accessory", "Other or unclear"),
  hazard_group = c("Fall or crash", "Fire or burn", "Laceration", "Other")
)

guide <- tribble(
  ~field, ~how_to_label,
  "General", "Read the title, product name, description, and hazard text. Open the URL if you need more. Label what an expert reader would conclude, following methods/case-definition.md. Don't look at the rules' answers.",
  "is_case", "Yes if the product is in scope (bicycle, e-bike, balance bike, adult trike, bicycle component, e-bike battery or charger, or bicycle accessory). No for helmets, exercise bikes, motorized two-wheelers, scooters, child carriers and trailers, toys, maintenance chemicals, and products that only mention bicycles in passing. If No, leave the other columns blank.",
  "product_form", "What was recalled. Complete bicycle or Complete e-bike if whole bikes were recalled, even if only one part is faulty. Component if a part was sold or recalled on its own (fork, crankset, brake). Accessory for lights, locks, racks, bags, pumps, carriers, and similar. E-bike battery, charger, or power part for those sold or recalled on their own.",
  "failed_component", "The part whose failure creates the hazard. For a complete bike with a cracking fork, choose Fork. For a standalone component, choose that component. For lead paint or other compliance problems with no failing part, choose Other or unclear.",
  "hazard_group", "The main injury mechanism. Fall or crash covers loss of control, ejection, and parts that break while riding. Fire or burn covers overheating batteries and chargers. Laceration covers cuts and sharp edges. Other covers lead, choking, entrapment, shock, and anything else.",
  "notes", "Optional. Note anything ambiguous, or anything you think the categories handle badly. These notes are useful for the methods page."
)

wb <- createWorkbook()
addWorksheet(wb, "Label here")
addWorksheet(wb, "Guide")
addWorksheet(wb, "Lists")

# Lists sheet (dropdown sources)
for (i in seq_along(lists)) {
  writeData(wb, "Lists", tibble(!!names(lists)[i] := lists[[i]]), startCol = i)
}

# Label sheet
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

# Guide sheet
writeData(wb, "Guide", guide, headerStyle = header_style)
addStyle(wb, "Guide", body_style, rows = 2:(nrow(guide) + 1), cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Guide", cols = 1:2, widths = c(18, 120))

saveWorkbook(wb, out_path)

# ---------------------------------------------------------------------------
# 4. Report (no rule outputs)
# ---------------------------------------------------------------------------
cat("\n========== VALIDATION SAMPLE ==========\n")
sampled |>
  distinct(stratum, N, n, weight) |>
  mutate(weight = round(weight, 2)) |>
  print()
cat("\nTotal rows to label:", n_rows, "\n")
cat("Workbook written to", out_path, "\n")
cat("Rule outputs are hidden in data-interim/validation_key.csv. Don't open it until you've finished labelling.\n")