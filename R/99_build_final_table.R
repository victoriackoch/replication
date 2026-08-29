# Master script: runs every table's extraction (00-15) and assembles the
# final long-format product/substance/CAS table.
#
# Column 1 product        -- named use / application the substance is used in
# Column 2 substance_name  -- full chemical/polymer name, where given
# Column 3 abbreviation    -- abbreviation/acronym, where given
# Column 4 cas_number      -- CAS number, where given
# Column 5 source          -- table code (A.X) the row was drawn from
# Column 6 source_text     -- the original cell text the row was extracted from
#
# A row needs a product AND at least one of substance_name/abbreviation to
# be kept -- every extraction function already enforces that on its own
# table, so this step only assembles and deduplicates.

source("R/00_helpers.R")
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

# A handful of "Examples of PFAS" cells (roughly a dozen rows out of 1400+)
# contain a full IUPAC name with a multi-locant stereodescriptor list (e.g.
# "rel-(3aR,4S,7R,7aS)-3a,4,7,7a-tetrahydro-..."); split_substance_list()'s
# locant-merge pass recovers most of these but occasionally still leaves a
# bare orphan locant/stereo-descriptor token ("2", "N", "7R"...) as its own
# "substance" after a merge chain breaks. These are dropped here as a final
# safety net -- real substance abbreviations in this domain are never a
# bare single letter or digits-plus-locant-letter, so this can't remove a
# genuine entry (spot-checked against the full row set).
LOCANT_ONLY_RE <- "^[0-9]+[A-Za-z]{0,2}$|^[A-Za-z]$"

final_table <- bind_rows(all_tables) %>%
  mutate(across(c(product, substance_name, abbreviation, cas_number, source_text), str_squish)) %>%
  filter(!is.na(product), product != "",
         !(is.na(substance_name) & is.na(abbreviation)),
         is.na(substance_name) | !str_detect(substance_name, LOCANT_ONLY_RE)) %>%
  distinct(product, substance_name, abbreviation, cas_number, source, .keep_all = TRUE) %>%
  arrange(source, product, substance_name, abbreviation)

cat("Tables processed:", length(all_tables), "\n")
cat("Final row count:", nrow(final_table), "\n")
cat("Rows per source table:\n")
print(final_table %>% count(source, sort = TRUE), n = 50)

dir.create("output", showWarnings = FALSE)
write.csv(final_table, "output/pfas_product_substance_table.csv", row.names = FALSE, na = "")
saveRDS(final_table, "output/pfas_product_substance_table.rds")
cat("\nWritten to output/pfas_product_substance_table.csv\n")
