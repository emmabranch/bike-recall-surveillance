# Case definition

**Version 1.0, 21 November 2026**

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
- Bicycle components: frames, forks, stems, handlebars, seatposts, saddles, brakes, wheels, hubs, rims, tires, drivetrain parts, pedals, and quick releases
- Bicycle accessories (locks, lights, pumps, bags, bells, computers, and racks), coded as a separate "accessory" component so they can be filtered out

**Out of scope**

- Helmets and other protective gear
- Exercise, stationary, and indoor bikes
- Motorized two-wheelers that are not e-bikes: dirt bikes, mini bikes, pit bikes, mopeds, and motorcycles
- Other micromobility: e-scooters, kick scooters, hoverboards, self-balancing devices, unicycles, and skateboards
- Child passenger products: child carriers, bike-mounted child seats, and bicycle trailers
- Toy tricycles, ride-on toys, and tricycle-strollers
- Products that share a word with bicycles but are unrelated, such as horse-riding saddles, crossbow cranks, and gaming headsets

**Borderline electric products.** Some products sold as e-bikes may perform like electric motorcycles. They are included if marketed as e-bikes, and flagged as "possible e-moto" where the recall text describes no pedals, a throttle-only design, or speeds or power above e-bike class limits.

## Certainty levels

Rules are applied in order. The first rule that matches decides the record's status.

| Order | Rule | Status | Criterion |
|---|---|---|---|
| 1 | EX00_cpsc_product_type | Excluded | CPSC product type is an out-of-scope category and is not a bicycle category |
| 2 | EX01 to EX07 | Excluded | Title or product name matches an exclusion pattern (helmet, exercise, motorized, micromobility, child passenger, toy, unrelated homonym) |
| 3 | PROB02_mixed_product | Probable | An exclusion pattern matches, but the title or product name also names a bicycle or e-bike (for example "e-bikes and e-scooters") |
| 4 | CONF01_cpsc_bicycle_code | Confirmed | CPSC product type is "Bicycles & Accessories" or "Bicycles and Bicycle Accessories" |
| 5 | CONF02_bike_named | Confirmed | Title or product name names a bicycle, e-bike, or tricycle |
| 6 | CONF03_component_named | Confirmed | Title or product name names a bicycle component, and there is bicycle context (CPSC bicycle code, Health Canada sports category, or bicycle mentioned in the description) |
| 7 | PROB01_description_only | Probable | Bicycle is mentioned only in the product description |
| 8 | NC01_no_bike_reference | Not a case | None of the above |

Exclusion rules check only the title and product name fields, not the description. Descriptions often mention bicycles in passing ("can be mounted on a bicycle", "wear a helmet"), which would cause false exclusions.

Confirmed and probable records are both counted as cases. Probable cases are reviewed during validation, and results are reported both with and without them.

The full list of patterns is exported to `case_definition_rules.csv` each time the pipeline runs.

## Linking joint recalls

- CPSC records list joint Canadian notices in the `Inconjunctions` field as Health Canada URLs. These are used as an exact linkage key.
- Health Canada notices that say "joint recall" but have no CPSC link are reported as unlinked joint notices and counted as Canadian-only events.

## Known limitations of this definition

- Keyword rules depend on how regulators word titles and descriptions. Wording conventions have changed over time.
- Older CPSC records often have no product type, so they depend on keyword rules alone.
- Health Canada titles are shorter than CPSC titles, so more Canadian cases rely on component terms.
- Whether a product is an e-bike or an e-moto cannot always be judged from recall text.

## Version history

- **1.0 (21 November 2026):** first version.