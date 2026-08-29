# A.17 and A.88 (TULAC / technical textiles: "applied concentrations of
# PFASs") only give a "[%] in the final textile product" free-text cell,
# which mixes a concentration value with (sometimes) a substance mention,
# inconsistently worded and with no reliable delimiter or pattern a
# generic parser could split -- e.g. "<0.1 C6 SCFP in final textile
# product", "100 of membrane is PTFE", "1.5 - 3" (no substance at all),
# "200 µg/m2" (no substance at all), "≤0.1 total fluorine on
# weight of fabric" (a measurement method, not a named substance). Hand-
# read and hand-mapped row by row instead of parsed. Rows that name no
# specific substance (only a bare concentration, or "total fluorine",
# which is an aggregate organofluorine measurement rather than a named
# substance) are dropped, consistent with excluding the aggregate-only
# A.109/A.113/A.114/A.115 tables.

a17_rows <- tribble(
  ~product, ~substance_name, ~abbreviation, ~source_text,
  "High performance upholstery", "C6 side-chain fluorinated polymer", "C6 SCFP",
    "<0.1% C6 SCFP in final textile product",
  "Outdoor textiles", NA, "FEP",
    "2% of FEP/PFAA in final product",
  "Outdoor textiles", "Perfluoroalkyl acid (unspecified)", "PFAA",
    "2% of FEP/PFAA in final product",
  "Chemical protective suits", NA, "PTFE",
    "PTFE (max 1%), THV (max 1%) or FKM or Fluorosilicone (50-90%)",
  "Chemical protective suits", NA, "THV",
    "PTFE (max 1%), THV (max 1%) or FKM or Fluorosilicone (50-90%)",
  "Chemical protective suits", NA, "FKM",
    "PTFE (max 1%), THV (max 1%) or FKM or Fluorosilicone (50-90%)",
  "Chemical protective suits", "Fluorosilicone", NA,
    "PTFE (max 1%), THV (max 1%) or FKM or Fluorosilicone (50-90%)",
  "Protective and technical textiles where PTFE is used as a membrane material", NA, "PTFE",
    "100% of membrane is PTFE",
  "Medical gowns, drapes and PPE", "C6 side-chain fluorinated polymer (unspecified)", "C6 SCFP",
    "C6 concentration average for all products <0.5%",
  "Within some face masks", "Expanded PTFE", "ePTFE",
    "1.9% of ePTFE in final products (by weight)",
  "Architectural polyester/PVC fabrics as fluoropolymers", "Fluoropolymers (unspecified)", NA,
    "<1% as a protection of polyester PVC fabrics",
  "Membrane", "C6 side-chain fluorinated polymer (unspecified)", "C6 SCFP",
    "C6 PFAS represent approximately less than 1% of total weight of the membrane"
) %>%
  mutate(cas_number = NA_character_, source = "A.17")

a88_rows <- tribble(
  ~product, ~substance_name, ~abbreviation, ~source_text,
  "Outdoor textiles", NA, "FEP",
    "2% of FEP/PFAA in final product",
  "Outdoor textiles", "Perfluoroalkyl acid (unspecified)", "PFAA",
    "2% of FEP/PFAA in final product",
  "Medical textiles", "C6 side-chain fluorinated polymer (unspecified)", "C6 SCFP",
    "C6 concentration average for all products <0.5%; PFAS application level of 1.7% on weight fabrics",
  "Architectural polyester/PVC fabrics as fluoropolymers", "Fluoropolymers (unspecified)", NA,
    "<1% as a protection of polyester PVC fabrics",
  "Membranes for e.g. water treatment", "C6 side-chain fluorinated polymer (unspecified)", "C6 SCFP",
    "C6 PFAS represent approximately less than 1% of total weight of the membrane",
  "Textiles used in engine bays", "C6 side-chain fluorinated polymer (unspecified)", "C6 SCFP",
    "C6 concentration of 0.2-0.5%"
) %>%
  mutate(cas_number = NA_character_, source = "A.88")

# Dropped for naming no specific substance (bare concentration only, or
# "total fluorine", a measurement method not a substance):
#  A.17: "PPE-(non-medical)" (1.5-3%), "Leather straps" (200 ug/m2),
#        "Non-Launderable textiles" (total fluorine)
#  A.88: "Non-Launderable textiles" (total fluorine)

add_table(a17_rows)
add_table(a88_rows)
