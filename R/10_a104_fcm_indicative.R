# A.104 ("Substances...used in FCM and packaging", indicative examples) is
# small (4 real data rows after removing one PDF line-wrap artifact) but
# messy enough that it's hand-curated rather than run through a generic
# splitter:
#  - row 2 ("Cooking and baking equipment, coated rubber") and row 3
#    ("Liquid processing equipment Rubber components") each pack two
#    distinct uses into one cell (comma in one, no delimiter at all in the
#    other) -- split into two product rows each, per your call to include
#    A.104 and hand-fix these.
#  - row 3's Substance cell lists five polymers/CAS numbers in a garbled,
#    not-1:1-alignable way ("64706-30-5,64706-30-65 PTFE: 9002-84-0; VDF-
#    co-HFP/FKM#1: 9011-17-0; - - FEP: 25067-11-2 -"); split into five
#    named lines (point 4) using the CAS assignment you gave: FKM
#    64706-30-65, PTFE 9002-84-0, ETFE (no CAS given), FFKM (no CAS
#    given), FEP 25067-11-2.
#  - rows 4-5 in the sheet are a single substance whose name wraps across
#    two physical PDF rows ("...bis(1H,1H,2H,2H-" / "perfluoroalkyl(C8-
#    C18) phosphates [mono-and di-PAP, FT]"); merged into one name here.
# All of this duplicates ground A.105 covers far more completely and
# cleanly -- kept per your call to inspect duplicates later.

a104_polymer_products <- c("Liquid processing equipment", "Rubber components")
a104_polymer_subs <- tribble(
  ~substance_name, ~abbreviation, ~cas_number,
  "Silicone Rubber, fluorinated (fluoroelastomer)", "FKM", "64706-30-65",
  "Polytetrafluoroethylene", "PTFE", "9002-84-0",
  "Ethylene-tetrafluoroethylene copolymer", "ETFE", NA,
  "Perfluoroelastomer", "FFKM", NA,
  "Tetrafluoroethylene-perfluoropropylene copolymer", "FEP", "25067-11-2"
)

a104_special_rows <- expand_grid(product = a104_polymer_products, i = seq_len(nrow(a104_polymer_subs))) %>%
  mutate(
    substance_name = a104_polymer_subs$substance_name[i],
    abbreviation = a104_polymer_subs$abbreviation[i],
    cas_number = a104_polymer_subs$cas_number[i]
  ) %>%
  select(-i) %>%
  mutate(source = "A.104",
         source_text = "A.104 row: Silicone Rubber, fluorinated FKM, fluoroelastomers (1,1-Difluoroethylen-hexafluoropropenpolymer) Ethene, 1,1,2,2-tetrafluoro-, homopolymer (PTFE) Ethylene-tetrafluoroethylene copolymer (ETFE) FKM Perfluoroelastomer (FFKM) Tetrafluoroethylene-perfluoropropylene copolymer (FEP)")

a104_simple_rows <- tribble(
  ~product, ~raw_name, ~raw_abbrev, ~raw_cas,
  "Consumer and industrial cookware",
    "2,3,3,3-tetrafluoro-2-heptafluoropropoxy)-propinoic acid; or perfluoro[2(n-propoxy)propanoic acid]",
    "GenX, HFPO-DA, FRD-903", "13252-13-6",
  "Cooking and baking equipment",
    "Polytetrafluoroethylene; a polymer of: tetrafluoroethylene (TFE)", "PTFE", "9002-84-0",
  "Coated rubber",
    "Polytetrafluoroethylene; a polymer of: tetrafluoroethylene (TFE)", "PTFE", "9002-84-0",
  "Food, non-food and feed packaging",
    "Perfluoroalkyl(C6-C16) phosphates of bis(2-hydroxyethyl)amine or Diethanolamine salts of mono-and bis(1H,1H,2H,2H-perfluoroalkyl(C8-C18) phosphates [mono-and di-PAP, FT])",
    NA, "65530-64-5"
)

a104_simple <- map(seq_len(nrow(a104_simple_rows)), function(i) {
  r <- a104_simple_rows[i, ]
  triple <- process_substance_triple(r$raw_name, r$raw_abbrev, r$raw_cas, "A.104")
  make_row(
    product = r$product, substance_name = triple$substance_name,
    abbreviation = triple$abbreviation, cas_number = triple$cas_number,
    source = "A.104", source_text = paste0("A.104 row: ", r$raw_name),
    substance_synonym = triple$substance_synonym,
    abbreviation_synonym = triple$abbreviation_synonym,
    substance_group = triple$substance_group
  )
})

add_table(bind_rows(a104_simple))
add_table(a104_special_rows)
