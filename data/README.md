# Bicycle and e-bike recall dataset

Bicycle, e-bike, and bicycle component recalls published by the US Consumer Product Safety Commission (CPSC) and Health Canada, classified by product form, failed component, and hazard.

This is an independent project using public data. It is not affiliated with or endorsed by CPSC, Health Canada, or any company named in the records.

## Files

| File | Contents |
|---|---|
| `bike_recalls.csv` | One row per recall notice (cases only) |
| `bike_recalls.parquet` | Same data, typed (dates and logicals preserved) |
| `data_dictionary.csv` | Every column: type, description, allowed values, completeness |

## Unit of analysis

- One row is one **recall notice** from one regulator.
- Joint US and Canadian notices share an `event_id`. Count distinct `event_id` values for combined totals, and count rows by `country` for country comparisons.
- For trend analysis, filter to `in_trend_window == TRUE` (US from 2000, Canada from 2008).

## How records were selected and classified

- Case definition: `../methods/case-definition.md` (rules exported to `../methods/case_definition_rules.csv`).
- Classification rules: `../methods/classification_rules.csv`.
- Manual decisions: `../methods/manual_overrides.csv` and `../methods/manual_links.csv`, each with a written reason.
- Validation against blind manual labels: `../validation/`.

## Key caveats

- **Recalls are not injuries.** A recall is a regulatory action, not a measure of harm. There is no sales denominator, so these counts cannot be read as risk.
- **Passive, negotiated reporting.** Most recalls are voluntary and negotiated with the firm. Counts reflect firm reporting, regulator priorities, and enforcement, as well as product safety.
- **Changing regulation.** Canada repealed its federal power-assisted bicycle definition in February 2021. E-bike recall counts in Canada may reflect this jurisdictional change.
- **Text extraction.** Incidents, injuries, units, and origin are extracted from notice text. NA means no number was stated, not zero.
- **Brand names** appear only as published in the official notices. The project does not rank or compare brands.

## Sources and licences

- **US CPSC Recalls API** (saferproducts.gov). US government work, public domain.
- **Health Canada Recalls and Safety Alerts** (open.canada.ca), plus individual recall pages on recalls-rappels.canada.ca. Contains information licensed under the Open Government Licence – Canada.

## Citation

Branch, E. (2026). Bicycle and e-bike recall surveillance dataset (version 1.0) [Data set]. https://github.com/emmabranch/bike-recall-surveillance