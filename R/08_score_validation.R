# 08_score_validation.R  (v1.1)
# Purpose: compare blind manual labels with CURRENT rule outputs, for both the
# original labels and the adjudicated labels. Reports case definition PPV/NPV
# and weighted sensitivity/specificity, field accuracy, kappa, per-category
# precision and recall, and disagreements.
# Reads:  validation/validation_sample.xlsx, validation/adjudication.csv,
#         data-interim/validation_key.csv, data-interim/candidates_with_status.csv,
#         data/bike_recalls.csv
# Writes: validation/validation_results.csv, validation/validation_by_category.csv,
#         validation/disagreements.csv

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(tibble)

set.seed(20261121)

fields_all <- c("is_case", "product_form", "failed_component", "hazard_group")

allowed <- list(
  is_case = c("Yes", "No"),
  product_form = c("Complete bicycle", "Complete e-bike", "Component", "Accessory",
                   "E-bike battery, charger, or power part", "Unclear"),
  failed_component = c("Frame", "Fork", "Stem or handlebar", "Brakes", "Wheels, hubs, or tires",
                       "Drivetrain or pedals", "Saddle or seatpost",
                       "E-bike electrical system", "Accessory", "Other or unclear"),
  hazard_group = c("Fall or crash", "Fire or burn", "Laceration", "Other")
)

# Category renamed in classification v1.1
renamed <- c("E-bike battery, charger, or motor" = "E-bike electrical system")

canon <- function(x, ok) {
  y <- str_squish(as.character(x))
  hit <- !is.na(y) & y %in% names(renamed)
  y[hit] <- unname(renamed[y[hit]])
  out <- ok[match(str_to_lower(y), str_to_lower(ok))]
  out[is.na(y) | y == ""] <- NA_character_
  out
}

# ---------------------------------------------------------------------------
# 1. Labels, key, current rule outputs, adjudications
# ---------------------------------------------------------------------------
raw_labels <- read_excel("validation/validation_sample.xlsx", sheet = "Label here")

labels <- raw_labels |>
  transmute(
    val_id,
    notes = str_squish(as.character(notes)),
    raw_is_case = as.character(is_case),
    raw_product_form = as.character(product_form),
    raw_failed_component = as.character(failed_component),
    raw_hazard_group = as.character(hazard_group),
    lab_is_case          = canon(raw_is_case, allowed$is_case),
    lab_product_form     = canon(raw_product_form, allowed$product_form),
    lab_failed_component = canon(raw_failed_component, allowed$failed_component),
    lab_hazard_group     = canon(raw_hazard_group, allowed$hazard_group)
  )

key <- read_csv("data-interim/validation_key.csv", show_col_types = FALSE) |>
  select(val_id, record_id, stratum, N, n, weight)

cand <- read_csv("data-interim/candidates_with_status.csv",
                 col_types = cols(source_id = col_character(),
                                  linked_cpsc_id = col_character(),
                                  .default = col_guess()),
                 guess_max = 5000) |>
  select(record_id, title, url, rule_is_case = is_case, rule_case_reason = case_reason)

recalls <- read_csv("data/bike_recalls.csv",
                    col_types = cols(linked_cpsc_id = col_character(), .default = col_guess()),
                    guess_max = 5000) |>
  select(record_id,
         rule_product_form = product_form,
         rule_failed_component = failed_component,
         rule_hazard_group = hazard_group)

v <- key |>
  left_join(labels, by = "val_id") |>
  left_join(cand, by = "record_id") |>
  left_join(recalls, by = "record_id")

adj_path <- "validation/adjudication.csv"
adj <- if (file.exists(adj_path)) {
  read_csv(adj_path, col_types = cols(.default = col_character()))
} else {
  tibble(val_id = character(), field = character(), adjudicated_label = character())
}

for (f in fields_all) {
  a <- adj |> filter(field == f) |> select(val_id, adj_value = adjudicated_label)
  v <- v |>
    left_join(a, by = "val_id") |>
    mutate(!!paste0("adj_", f) := coalesce(canon(adj_value, allowed[[f]]),
                                           .data[[paste0("lab_", f)]])) |>
    select(-adj_value)
}
v <- v |>
  mutate(across(c(adj_product_form, adj_failed_component, adj_hazard_group),
                ~ if_else(adj_is_case == "No", NA_character_, .x)))

# ---------------------------------------------------------------------------
# 2. Label checks
# ---------------------------------------------------------------------------
cat("\n========== LABEL CHECKS ==========\n")
cat("Rows:", nrow(v), "| is_case labelled:", sum(!is.na(v$lab_is_case)),
    "| adjudications applied:", nrow(adj), "\n")
bad <- v |>
  filter((!is.na(raw_is_case) & str_squish(raw_is_case) != "" & is.na(lab_is_case)) |
           (!is.na(raw_product_form) & str_squish(raw_product_form) != "" & is.na(lab_product_form)) |
           (!is.na(raw_failed_component) & str_squish(raw_failed_component) != "" & is.na(lab_failed_component)) |
           (!is.na(raw_hazard_group) & str_squish(raw_hazard_group) != "" & is.na(lab_hazard_group)))
cat("Rows with values not in the lists:", nrow(bad), "\n")

# ---------------------------------------------------------------------------
# 3. Helpers
# ---------------------------------------------------------------------------
wilson <- function(x, n, z = 1.96) {
  if (n == 0) return(c(NA_real_, NA_real_))
  p <- x / n
  d <- 1 + z^2 / n
  centre <- p + z^2 / (2 * n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  c((centre - half) / d, (centre + half) / d)
}

kappa_fn <- function(a, b) {
  lv <- union(unique(a), unique(b))
  t <- table(factor(a, lv), factor(b, lv))
  n <- sum(t)
  po <- sum(diag(t)) / n
  pe <- sum(rowSums(t) * colSums(t)) / n^2
  if (pe == 1) return(NA_real_)
  (po - pe) / (1 - pe)
}

boot_ci <- function(df, fun, B = 2000) {
  groups <- split(seq_len(nrow(df)), df$stratum)
  reps <- replicate(B, {
    idx <- unlist(lapply(groups, function(g) g[sample.int(length(g), length(g), replace = TRUE)]))
    fun(df[idx, , drop = FALSE])
  })
  unname(quantile(reps, c(0.025, 0.975), na.rm = TRUE))
}

pct <- function(x) ifelse(is.na(x), "NA", sprintf("%.0f%%", 100 * x))

fields <- tribble(
  ~field,             ~rule_col,
  "product_form",     "rule_product_form",
  "failed_component", "rule_failed_component",
  "hazard_group",     "rule_hazard_group"
)

# ---------------------------------------------------------------------------
# 4. Scoring function (one label set)
# ---------------------------------------------------------------------------
score_set <- function(v, prefix, set_name) {
  res <- list()
  add <- function(measure, estimate, lower, upper, n, note) {
    res[[length(res) + 1]] <<- tibble(label_set = set_name, measure, estimate, lower, upper, n, note)
  }
  
  d0 <- v |>
    mutate(lab_case = case_when(.data[[paste0(prefix, "is_case")]] == "Yes" ~ TRUE,
                                .data[[paste0(prefix, "is_case")]] == "No"  ~ FALSE,
                                TRUE ~ NA))
  
  cat("\n############", toupper(set_name), "LABELS ############\n")
  
  # Case definition
  cd <- d0 |> filter(!is.na(lab_case))
  cat("\nCase definition confusion table (rows = rules, columns = labels):\n")
  print(table(Rule = cd$rule_is_case, Label = cd$lab_case))
  
  pos <- filter(cd, rule_is_case); neg <- filter(cd, !rule_is_case)
  ppv <- mean(pos$lab_case);  ppv_ci <- wilson(sum(pos$lab_case), nrow(pos))
  npv <- mean(!neg$lab_case); npv_ci <- wilson(sum(!neg$lab_case), nrow(neg))
  sens_fun <- function(d) {
    tp <- sum(d$weight * (d$rule_is_case & d$lab_case))
    fn <- sum(d$weight * (!d$rule_is_case & d$lab_case))
    tp / (tp + fn)
  }
  spec_fun <- function(d) {
    tn <- sum(d$weight * (!d$rule_is_case & !d$lab_case))
    fp <- sum(d$weight * (d$rule_is_case & !d$lab_case))
    tn / (tn + fp)
  }
  sens <- sens_fun(cd); sens_ci <- boot_ci(cd, sens_fun)
  spec <- spec_fun(cd); spec_ci <- boot_ci(cd, spec_fun)
  
  cat("PPV ", pct(ppv), " (", pct(ppv_ci[1]), " to ", pct(ppv_ci[2]), ") | ",
      "NPV ", pct(npv), " (", pct(npv_ci[1]), " to ", pct(npv_ci[2]), ") | ",
      "sensitivity ", pct(sens), " (", pct(sens_ci[1]), " to ", pct(sens_ci[2]), ") | ",
      "specificity ", pct(spec), " (", pct(spec_ci[1]), " to ", pct(spec_ci[2]), ")\n", sep = "")
  
  add("Case definition PPV", ppv, ppv_ci[1], ppv_ci[2], nrow(pos), "Wilson interval, unweighted")
  add("Case definition NPV", npv, npv_ci[1], npv_ci[2], nrow(neg), "Wilson interval, unweighted")
  add("Case definition sensitivity (weighted)", sens, sens_ci[1], sens_ci[2], nrow(cd), "Stratified bootstrap, relative to candidate pool")
  add("Case definition specificity (weighted)", spec, spec_ci[1], spec_ci[2], nrow(cd), "Stratified bootstrap, relative to candidate pool")
  
  # Fields
  both <- d0 |> filter(rule_is_case, lab_case %in% TRUE)
  cat("\nRows where both say case:", nrow(both), "\n")
  
  by_cat <- list()
  dis <- list()
  
  for (i in seq_len(nrow(fields))) {
    f  <- fields$field[i]
    rc <- fields$rule_col[i]
    lc <- paste0(prefix, f)
    
    d <- both |>
      filter(!is.na(.data[[rc]]), !is.na(.data[[lc]])) |>
      mutate(r = .data[[rc]], l = .data[[lc]], agree = r == l)
    
    acc <- mean(d$agree); acc_ci <- wilson(sum(d$agree), nrow(d))
    wacc_fun <- function(x) sum(x$weight * x$agree) / sum(x$weight)
    wacc <- wacc_fun(d); wacc_ci <- boot_ci(d, wacc_fun)
    k <- kappa_fn(d$r, d$l); k_ci <- boot_ci(d, function(x) kappa_fn(x$r, x$l))
    
    cat(sprintf("%-17s accuracy %s (%s to %s) | weighted %s (%s to %s) | kappa %.2f (%.2f to %.2f) | n = %d\n",
                f, pct(acc), pct(acc_ci[1]), pct(acc_ci[2]),
                pct(wacc), pct(wacc_ci[1]), pct(wacc_ci[2]),
                k, k_ci[1], k_ci[2], nrow(d)))
    
    add(paste0(f, " accuracy"), acc, acc_ci[1], acc_ci[2], nrow(d), "Wilson interval, unweighted")
    add(paste0(f, " accuracy (weighted)"), wacc, wacc_ci[1], wacc_ci[2], nrow(d), "Stratified bootstrap")
    add(paste0(f, " kappa"), k, k_ci[1], k_ci[2], nrow(d), "Cohen's kappa, stratified bootstrap")
    
    cats <- sort(union(unique(d$r), unique(d$l)))
    by_cat[[f]] <- map(cats, function(cc) {
      tibble(label_set = set_name, field = f, category = cc,
             n_label = sum(d$l == cc), n_rule = sum(d$r == cc),
             precision = if (sum(d$r == cc) > 0) mean(d$l[d$r == cc] == cc) else NA_real_,
             recall    = if (sum(d$l == cc) > 0) mean(d$r[d$l == cc] == cc) else NA_real_)
    }) |> list_rbind()
    
    dis[[f]] <- d |>
      filter(!agree) |>
      transmute(label_set = set_name, val_id, record_id, stratum, field = f,
                rule = r, label = l, rule_case_reason, title, notes, url)
  }
  
  dis_case <- cd |>
    filter(rule_is_case != lab_case) |>
    transmute(label_set = set_name, val_id, record_id, stratum, field = "is_case",
              rule = if_else(rule_is_case, "case", "not case"),
              label = if_else(lab_case, "case", "not case"),
              rule_case_reason, title, notes, url)
  
  list(results = bind_rows(res),
       by_category = list_rbind(by_cat),
       disagreements = bind_rows(dis_case, list_rbind(dis)))
}

# ---------------------------------------------------------------------------
# 5. Score both label sets
# ---------------------------------------------------------------------------
orig <- score_set(v, "lab_", "original")
adjd <- score_set(v, "adj_", "adjudicated")

cat("\n========== PER-CATEGORY PRECISION AND RECALL (adjudicated) ==========\n")
adjd$by_category |>
  mutate(precision = pct(precision), recall = pct(recall)) |>
  select(-label_set) |>
  print(n = 50, width = Inf)

cat("\n========== REMAINING DISAGREEMENTS (adjudicated, ",
    nrow(adjd$disagreements), ") ==========\n", sep = "")
for (f in unique(adjd$disagreements$field)) {
  cat("\n--", f, "--\n")
  adjd$disagreements |>
    filter(field == f) |>
    transmute(line = paste0(val_id, " | ", record_id, " | rule: ", rule, " | label: ", label,
                            "\n   ", str_trunc(coalesce(title, ""), 90),
                            if_else(is.na(notes), "", paste0("\n   note: ", notes)))) |>
    pull(line) |>
    cat(sep = "\n")
}

# ---------------------------------------------------------------------------
# 6. Save
# ---------------------------------------------------------------------------
bind_rows(orig$results, adjd$results) |>
  mutate(across(c(estimate, lower, upper), ~ round(.x, 3))) |>
  write_csv("validation/validation_results.csv")
bind_rows(orig$by_category, adjd$by_category) |>
  mutate(across(c(precision, recall), ~ round(.x, 3))) |>
  write_csv("validation/validation_by_category.csv")
write_csv(adjd$disagreements, "validation/disagreements.csv")

cat("\nFiles written: validation/validation_results.csv, validation/validation_by_category.csv, validation/disagreements.csv\n")