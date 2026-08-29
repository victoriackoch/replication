# A.104 ("Substances used in FCM and packaging", indicative examples) is
# small (4 real data rows after removing one PDF line-wrap artifact) but
# messy enough that it's hand-curated rather than run through a generic
# splitter:
#  - row 2 ("Cooking and baking equipment, coated rubber") and row 3
#    ("Liquid processing equipment Rubber components") each pack two
#    distinct uses into one cell (comma in one, no delimiter at all in the
#    other) -- split into two product rows each, per your call to include
#    A.104 and hand-fix these.
#  - row 3's Substance cell itself lists several polymers/CAS numbers in a
#    garbled, not reliably 1:1 alignable way ("64706-30-5,64706-30-65
#    PTFE: 9002-84-0; VDF-co-HFP/FKM#1: 9011-17-0; - - FEP: 25067-11-2 -");
#    kept verbatim as one substance_name with cas_number = NA rather than
#    guessing a name-to-CAS mapping.
#  - rows 4-5 in the sheet are a single substance whose name wraps across
#    two physical PDF rows ("...bis(1H,1H,2H,2H-" / "perfluoroalkyl(C8-
#    C18) phosphates [mono-and di-PAP, FT]"); merged into one name here.
# All of this duplicates ground A.105 covers far more completely and
# cleanly -- flagged for your review against A.105 rather than dropped.

a104_rows <- tribble(
  ~product, ~substance_name, ~abbreviation, ~cas_number,
  "Consumer and industrial cookware",
    "2,3,3,3-tetrafluoro-2-heptafluoropropoxy)-propinoic acid; or perfluoro[2(n-propoxy)propanoic acid]",
    "GenX, HFPO-DA, FRD-903", "13252-13-6",
  "Cooking and baking equipment",
    "Polytetrafluoroethylene; a polymer of: tetrafluoroethylene (TFE)", "PTFE", "9002-84-0",
  "Coated rubber",
    "Polytetrafluoroethylene; a polymer of: tetrafluoroethylene (TFE)", "PTFE", "9002-84-0",
  "Liquid processing equipment",
    "Silicone Rubber, fluorinated FKM, fluoroelastomers (1,1-Difluoroethylen-hexafluoropropenpolymer) Ethene, 1,1,2,2-tetrafluoro-, homopolymer (PTFE) Ethylene-tetrafluoroethylene copolymer (ETFE) FKM Perfluoroelastomer (FFKM) Tetrafluoroethylene-perfluoropropylene copolymer (FEP)",
    NA, NA,
  "Rubber components",
    "Silicone Rubber, fluorinated FKM, fluoroelastomers (1,1-Difluoroethylen-hexafluoropropenpolymer) Ethene, 1,1,2,2-tetrafluoro-, homopolymer (PTFE) Ethylene-tetrafluoroethylene copolymer (ETFE) FKM Perfluoroelastomer (FFKM) Tetrafluoroethylene-perfluoropropylene copolymer (FEP)",
    NA, NA,
  "Food, non-food and feed packaging",
    "Perfluoroalkyl(C6-C16) phosphates of bis(2-hydroxyethyl)amine or Diethanolamine salts of mono-and bis(1H,1H,2H,2H-perfluoroalkyl(C8-C18) phosphates [mono-and di-PAP, FT])",
    NA, "65530-64-5"
) %>%
  mutate(source = "A.104",
         source_text = paste0("A.104 row: ", substance_name))

add_table(a104_rows)
