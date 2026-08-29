# A.105 (PFAS identified for food-contact/packaging use, 245 rows). Roughly
# a sixth of the rows are PDF line-wrap artifacts: some carry a genuine
# second "Use" for the same substance but the repeated Substance
# Name/Abbreviation/CAS/Formula got lost to extraction on that row; others
# are pure fragments (no Use at all -- either leftover Function/Name text
# wrapped onto its own row, or a stray repeated header block).
#
# Fix (agreed): forward-fill Substance Name/Abbreviation/CAS/Formula down
# through blank rows, then keep only rows that have a Use value. This
# recovers the genuine multi-use rows; rows with no Use (including the
# fragment/header rows) are dropped regardless of what forward-fill left in
# their other columns. Consequence: ~8 substance names end up truncated
# mid-word where the *name itself* wrapped across two rows (its own
# continuation row necessarily has no Use, so it's dropped rather than
# merged back in) -- CAS/Use stay correct for those rows even though the
# name is cut short.
#
# Use-column override (point 7): where Use is one of three generic
# catch-all categories ("Non-food packaging", "Non-food P&B packaging",
# "Industrial food processing and food transport equipment") AND the
# Function/Regulatory-Listing column gives real descriptive text (not
# blank/"No data"/a PPA-or-monomer regulatory citation, which goes to the
# separate table below instead), that text replaces the generic Use. When
# the Function text is a short two-item list ("X, Y. <trailing note>"), it
# splits into two product rows. This scope is a judgment call bounded by
# the exact examples given -- it is NOT applied to "Food & feed packaging"
# or "Consumer cookware" (each ~20-120 rows), since no example fell in
# those categories; flagged for review in case that's wrong.
#
# PPA/monomer regulatory-listing table (points 10-11): every row (from
# A.105 and, separately, A.106) whose Function text cites the substance as
# an EU Reg. 10/2011 PPA (processing aid) and/or monomer listing is pulled,
# one line per row, into its own table regardless of whether that row
# survived into the main table.

GENERIC_USE_OVERRIDE_RE <- c("Non-food packaging", "Non-food P&B packaging",
                              "Industrial food processing and food transport equipment")

override_use_from_function <- function(use, func) {
  if (is.na(func) || is_no_info(func) || str_detect(func, PPA_MONOMER_RE)) return(use)
  if (!(use %in% GENERIC_USE_OVERRIDE_RE)) return(use)
  # short two-item list "X, Y." followed by an optional trailing sentence
  m <- str_match(func, "^([^,\\.]{3,60}),\\s*([^,\\.]{3,60})\\.(\\s+.*)?$")
  if (!is.na(m[1, 1])) return(c(str_trim(m[1, 2]), str_trim(m[1, 3])))
  str_trim(str_remove(func, "\\.$"))
}

extract_a105 <- function() {
  tab <- load_table("A.105")
  names(tab) <- c("substance_name", "abbreviation", "cas", "formula", "use", "function_reg")

  for (col in c("substance_name", "abbreviation", "cas", "formula")) {
    tab[[col]] <- ffill(tab[[col]])
  }

  # PPA/monomer table: scan every row with a substance name, regardless of Use
  ppa_hits <- which(!is.na(tab$function_reg) & str_detect(tab$function_reg, PPA_MONOMER_RE) & !is.na(tab$substance_name))
  if (length(ppa_hits) > 0) {
    add_ppa_row(tibble(
      source = "A.105", substance_name = tab$substance_name[ppa_hits],
      abbreviation = tab$abbreviation[ppa_hits], cas_number = tab$cas[ppa_hits],
      use = tab$use[ppa_hits], regulatory_listing = tab$function_reg[ppa_hits]
    ))
  }

  keep <- !is.na(tab$use) & str_trim(tab$use) != ""
  tab <- tab[keep, , drop = FALSE]

  abbrev <- ifelse(vapply(tab$abbreviation, is_no_info, logical(1)), NA_character_, tab$abbreviation)
  cas <- ifelse(vapply(tab$cas, is_no_info, logical(1)), NA_character_, tab$cas)

  out <- map(seq_len(nrow(tab)), function(i) {
    triple <- process_substance_triple(tab$substance_name[i], abbrev[i], cas[i], "A.105")
    products <- override_use_from_function(tab$use[i], tab$function_reg[i])
    expand_grid(product = products, row = seq_len(nrow(triple))) %>%
      mutate(
        substance_name = triple$substance_name[row], substance_synonym = triple$substance_synonym[row],
        substance_group = triple$substance_group[row], abbreviation = triple$abbreviation[row],
        abbreviation_synonym = triple$abbreviation_synonym[row], cas_number = triple$cas_number[row]
      ) %>%
      select(-row) %>%
      mutate(source = "A.105",
             source_text = paste0("A.105 row: ", tab$substance_name[i], " -- ", coalesce(tab$function_reg[i], "")))
  })
  bind_rows(out)
}

add_table(extract_a105())
