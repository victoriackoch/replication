# Master script: runs every table's extraction (00-17) and assembles four
# output tables:
#   1. pfas_product_substance_table.csv -- the main product/substance/CAS
#      table (all in-scope tables except A.100, which is standalone)
#   2. pfas_a100_table.csv -- A.100 standalone (point 1)
#   3. pfas_polymer_composition_table.csv -- "X; a polymer of: Y" /
#      "a copolymer of X and Y" entries broken into name + monomers (point 2)
#   4. pfas_ppa_monomer_listing_table.csv -- rows whose Function/Regulatory
#      Listing text cites the substance as an EU Reg. 10/2011 PPA/monomer
#      (points 10-11)
#
# Main-table columns: product, substance_name, substance_synonym,
# abbreviation, abbreviation_synonym, substance_group, cas_number, source,
# source_text. A row needs a product AND at least one of substance_name/
# abbreviation to be kept -- every extraction function already enforces
# that on its own table, so this step only assembles and deduplicates.

source("R/00_helpers.R")
source("R/00b_name_parsers.R")
source("R/01_long_form_tables.R")
source("R/02_a62_lubricants.R")
source("R/03_a57_energy.R")
source("R/04_a14_fluoroelastomers.R")
source("R/05_name_cas_free_text.R")
source("R/06_clean_tables.R")
source("R/07_a107_metal_plating.R")
source("R/08_a119_a126.R")
source("R/09_a130_a131.R")
source("R/10_a104_fcm_indicative.R")
source("R/11_a105_fcm_full.R")
source("R/12_a21_a25_fcm_packaging.R")
source("R/13_matrix_tables.R")
source("R/14_a17_a88_textiles.R")
source("R/15_concentration_matrices.R")
source("R/16_a100_standalone.R")
source("R/17_a125_medical_polymers.R")

# handful of "Examples of PFAS" cells shred a full IUPAC name with a multi-
# locant stereodescriptor list; split_substance_list()'s locant-merge pass
# recovers most, but a bare orphan locant/stereo-descriptor token
# ("2", "N", "7R"...) can still slip through a broken merge chain -- dropped
# here as a final safety net (no genuine substance abbreviation in this
# domain is ever a bare single letter or digits-plus-locant-letter).
LOCANT_ONLY_RE <- "^[0-9]+[A-Za-z]{0,2}$|^[A-Za-z]$"

assemble <- function(rows) {
  rows %>%
    mutate(across(c(product, substance_name, substance_synonym, abbreviation,
                     abbreviation_synonym, substance_group, source_text), str_squish)) %>%
    filter(!is.na(product), product != "",
           !(is.na(substance_name) & is.na(abbreviation)),
           is.na(substance_name) | !str_detect(substance_name, LOCANT_ONLY_RE)) %>%
    distinct(product, substance_name, abbreviation, cas_number, source, .keep_all = TRUE) %>%
    arrange(source, product, substance_name, abbreviation) %>%
    select(product, substance_name, substance_synonym, abbreviation, abbreviation_synonym,
           substance_group, cas_number, source, source_text)
}

final_table <- assemble(bind_rows(all_tables))
a100_table <- assemble(A100_TABLE)

polymer_table <- bind_rows(POLYMER_ROWS) %>%
  mutate(across(c(full_text, substance_name, cas_number, type, monomer), str_squish)) %>%
  distinct(source, full_text, monomer, .keep_all = TRUE) %>%
  pivot_wider(id_cols = c(source, full_text, substance_name, cas_number, type),
              names_from = monomer_n, values_from = monomer, names_prefix = "monomer_") %>%
  arrange(source, substance_name)
monomer_cols <- names(polymer_table)[str_starts(names(polymer_table), "monomer_")]
monomer_cols <- monomer_cols[order(as.integer(str_remove(monomer_cols, "monomer_")))]
polymer_table <- polymer_table %>% select(source, full_text, substance_name, cas_number, type, all_of(monomer_cols))

ppa_monomer_table <- bind_rows(PPA_MONOMER_ROWS) %>%
  mutate(across(c(substance_name, abbreviation, cas_number, use, regulatory_listing), str_squish)) %>%
  distinct(source, substance_name, use, regulatory_listing, .keep_all = TRUE) %>%
  arrange(source, substance_name)

cat("Tables processed:", length(all_tables), "\n")
cat("Main table rows:", nrow(final_table), "\n")
cat("A.100 table rows:", nrow(a100_table), "\n")
cat("Polymer composition table rows:", nrow(polymer_table), "\n")
cat("PPA/monomer listing table rows:", nrow(ppa_monomer_table), "\n")
cat("Rows per source table (main):\n")
print(final_table %>% count(source, sort = TRUE), n = 50)

dir.create("output", showWarnings = FALSE)
write.csv(final_table, "output/pfas_product_substance_table.csv", row.names = FALSE, na = "")
write.csv(a100_table, "output/pfas_a100_table.csv", row.names = FALSE, na = "")
write.csv(polymer_table, "output/pfas_polymer_composition_table.csv", row.names = FALSE, na = "")
write.csv(ppa_monomer_table, "output/pfas_ppa_monomer_listing_table.csv", row.names = FALSE, na = "")
saveRDS(final_table, "output/pfas_product_substance_table.rds")
cat("\nWritten to output/*.csv\n")
