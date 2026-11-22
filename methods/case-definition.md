# Case definition

**Version 1.3, 21 November 2026**

This document defines which product recalls count as bicycle, e-bike, and bicycle component recalls in this project. It follows the structure of a public health surveillance case definition: a clear scope, explicit exclusions, and levels of certainty, each tied to a rule that can be checked.

## Unit of analysis

- The unit is one **recall notice**, as published by one regulator.
- Joint notices published by both US CPSC and Health Canada are linked into one **recall event**. Event counts are used for combined totals. Notice counts are used for country comparisons.

## Sources and surveillance periods

| Source | Records | Trend analysis window |
|---|---|---|
| US Consumer Product Safety Commission (CPSC) Recalls API | 1973 onward | 2000 onward |
| Health Canada Recalls and Safety Alerts, consumer products | 2002 onward | 2008 onward |

Records outside the trend window stay in the dataset but are not used in trend charts, because coverage before those years is sparse and inconsistent.

## Product scope

**In scope**

- Bicycles of all kinds: road, mountain, gravel, hybrid, BMX, cargo, folding, and children's bicycles (with or without training wheels)
- Balance bikes
- Adult tricycles and e-trikes
- E-bikes, meaning products marketed as e-bikes or electric bicycles
- E-bike conversion kits, and standalone e-bike batteries, chargers, motors, and controllers
- Bicycle components: frames, forks, suspension, stems, handlebars, seatposts, saddles, brakes, shifters, wheels, hubs, rims, tires, drivetrain parts, pedals, and quick releases
- Bicycle accessories (locks, lights, pumps, bags, bells, computers, racks, carriers, mounts, and storage hoists), coded as a separate "accessory" component so they can be filtered out

**Out of scope**

- Helmets and other protective gear
- Exercise, stationary, and indoor bikes
- Motorized two-wheelers that are not e-bikes: dirt bikes, mini bikes, pit bikes, snow bikes, mopeds, and motorcycles
- Other micromobility: e-scooters, kick scooters, hoverboards, self-balancing devices, unicycles, and skateboards
- Child passenger products: child carriers, bike-mounted child seats, and bicycle trailers
- Toy tricycles, ride-on toys, and tricycle-strollers
- Products that share a word with bicycles but are unrelated, such as horse-riding saddles, crossbow cranks, and gaming headsets
- Products that mention bicycles only in passing, such as clothing with a bicycle print
- Maintenance chemicals such as degreasers, lubricants, cleaners, and sealants

**Borderline electric products.** Some products sold as e-bikes may perform like electric motorcycles. They are included if marketed as e-bikes, and flagged as "possible e-moto" where the recall text describes no pedals, a throttle-only design, or speeds or power above e-bike class limits.

## How text is matched

- Rules match against the **product phrase** (the recall title with any leading "Company X Recalls" removed) plus the product name field. This stops company names such as "X Electric Bikes" from making an unrelated product look like a bicycle.
- Exclusion rules check only the product phrase and product name, not the description. Descriptions often mention bicycles in passing.

## Certainty levels

Rules are applied in order. The first rule that matches decides the record's status.

| Order | Rule | Status | Criterion |
|---|---|---|---|
| 1 | EX00_cpsc_product_type | Excluded | CPSC product type is out of scope and is not a bicycle category |
| 2 | EX01 to EX06, EX08 | Excluded | Product phrase or name matches an exclusion pattern (helmet, exercise, motorized, micromobility, child passenger, toy, maintenance chemical) |
| 3 | EX07_homonym | Excluded | Unrelated product sharing a bicycle word, unless a bicycle or e-bike is explicitly named |
| 4 | PROB02_mixed_product | Probable | A motorized or micromobility pattern matches, but a bicycle or e-bike is also named |
| 5 | CONF01_cpsc_bicycle_code | Confirmed | CPSC product type is "Bicycles & Accessories" or "Bicycles and Bicycle Accessories" |
| 6 | CONF02_bike_named | Confirmed | Product phrase or name names a bicycle, e-bike, or tricycle |
| 7 | CONF03_component_named | Confirmed | A bicycle component is named, with bicycle context (CPSC bicycle code, Health Canada sports category, or bicycle mentioned in the description) |
| 8 | PROB01_part_or_accessory | Probable | An e-bike part or bicycle accessory is named, with bicycle context |
| 9 | PROB03_sports_context | Probable | Bicycle mentioned only in the description of a Health Canada sports and fitness notice |
| 10 | NC02_incidental_mention | Not a case | Bicycle mentioned only in the description, with no bicycle product, part, or accessory named |
| 11 | NC01_no_bike_reference | Not a case | None of the above |

Confirmed and probable records are both counted as cases. Probable cases are reviewed during validation, and results are reported both with and without them.

The full list of patterns is exported to `case_definition_rules.csv` each time the pipeline runs.

## Manual review

A small number of records cannot be settled by rules. These are listed in `manual_overrides.csv` with the record ID, the assigned status, and a written reason. Overridden records are marked `MANUAL:` in the dataset, and the number of overrides is reported on the methods page.

## Linking joint recalls

Linking happens in three stages:

1. **Exact link.** CPSC records list joint Canadian notices in the `Inconjunctions` field as Health Canada URLs. Where a URL matches, the notices are linked.
2. **Date and name link.** For Canadian notices that say "joint recall" but have no URL match, the pipeline looks for CPSC records within 14 days of the Canadian recall date and scores the similarity of their product names (Jaccard similarity on distinctive words, ignoring generic words such as "recall", "bicycle", and "hazard"). A link is accepted if the best score is at least 0.25, is higher than the runner-up, and the CPSC record is not already linked.
3. **Manual link.** Canadian joint notices still unlinked after stage 2 are reviewed by hand against US records for the same company and product. Confirmed pairs are listed in `manual_links.csv` with a written note. Manual links take precedence over automatic matches.

Every link records its method and score. Canadian joint notices with no accepted link are counted as Canadian-only events and reported as unlinked.

## Known limitations of this definition

- Keyword rules depend on how regulators word titles and descriptions. Wording conventions have changed over time.
- Older CPSC records often have no product type, so they depend on keyword rules alone.
- Health Canada titles are shorter than CPSC titles, so more Canadian cases rely on component terms.
- Whether a product is an e-bike or an e-moto cannot always be judged from recall text.
- Date and name linking can miss joint recalls with very different titles in each country, or link the wrong pair when one company has several recalls in the same fortnight. Unlinked joint notices slightly overcount combined events.

## Version history

- **1.3 (21 November 2026):** added EX08 to exclude maintenance chemicals.
- **1.2 (21 November 2026):** added PROB03 for Health Canada sports and fitness notices that mention bicycles only in the description; "ride-on" now matches only toys and vehicles, so the Ride-On brand is not excluded; firearms added to homonyms and fitness equipment to exercise exclusions; added manual link review for joint recalls.
- **1.1 (21 November 2026):** match on product phrase rather than full title; homonym rule cannot override an explicit bicycle word; expanded component, e-bike part, and accessory terms; description-only mentions become "not a case" unless a part or accessory is named; added snow bikes to motorized exclusions; added manual overrides; added date and name linking for joint recalls.
- **1.0 (21 November 2026):** first version.