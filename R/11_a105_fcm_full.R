# A.105 (PFAS identified for food-contact/packaging use) was manually
# cleaned up by the user, adding a "Use - taking into account function"
# column that already folds in the more specific Function/Regulatory-
# Listing detail wherever it adds real information over the six generic
# Use categories (e.g. "Non-food packaging" -> "Coating for polyethylene
# film used e.g. for packaging toys and foodstuff.") and otherwise just
# repeats the generic Use verbatim -- this column is now the product
# directly; no need for the old GENERIC_USE_OVERRIDE_RE heuristic that
# used to reconstruct the same thing from the raw Function text.
#
# The cleanup also fully repeats Substance Name/Abbreviation/CAS/Formula
# on every row for a multi-use substance (rather than leaving them blank
# on continuation rows), so the forward-fill this table used to need is
# now a no-op -- kept anyway since it's harmless and cheap insurance.
#
# PPA/monomer regulatory-listing table (points 10-11): every row (from
# A.105 and, separately, A.106) whose Function text cites the substance as
# an EU Reg. 10/2011 PPA (processing aid) and/or monomer listing is pulled,
# one line per row, into its own table regardless of whether that row
# survived into the main table.

extract_a105 <- function() {
  tab <- load_table("A.105")
  names(tab) <- c("substance_name", "abbreviation", "cas", "formula", "use", "use_with_function", "function_reg")

  for (col in c("substance_name", "abbreviation", "cas", "formula")) {
    tab[[col]] <- ffill(tab[[col]])
  }

  # PPA/monomer table: scan every row with a substance name, regardless of Use
  ppa_hits <- which(!is.na(tab$function_reg) & str_detect(tab$function_reg, PPA_MONOMER_RE) & !is.na(tab$substance_name))
  if (length(ppa_hits) > 0) {
    add_ppa_row(tibble(
      source = "A.105", substance_name = tab$substance_name[ppa_hits],
      abbreviation = tab$abbreviation[ppa_hits], cas_number = tab$cas[ppa_hits],
      use = tab$use_with_function[ppa_hits], regulatory_listing = tab$function_reg[ppa_hits]
    ))
  }

  keep <- !is.na(tab$use_with_function) & str_trim(tab$use_with_function) != ""
  tab <- tab[keep, , drop = FALSE]

  abbrev <- ifelse(vapply(tab$abbreviation, is_no_info, logical(1)), NA_character_, tab$abbreviation)
  cas <- ifelse(vapply(tab$cas, is_no_info, logical(1)), NA_character_, tab$cas)
  # a few rows end in "." and others with the same wording don't (a data-
  # entry inconsistency in the manually-cleaned column, not a meaningful
  # difference) -- stripped so the same product text doesn't fork into two
  # near-duplicate product strings.
  product <- str_remove(str_squish(tab$use_with_function), "\\.$")

  out <- map(seq_len(nrow(tab)), function(i) {
    triple <- process_substance_triple(tab$substance_name[i], abbrev[i], cas[i], "A.105")
    tibble(
      product = product[i],
      substance_name = triple$substance_name, substance_synonym = triple$substance_synonym,
      substance_group = triple$substance_group, abbreviation = triple$abbreviation,
      abbreviation_synonym = triple$abbreviation_synonym, cas_number = triple$cas_number,
      source = "A.105",
      source_text = paste0("A.105 row: ", tab$substance_name[i], " -- ", coalesce(tab$function_reg[i], ""))
    )
  })
  bind_rows(out)
}

add_table(extract_a105())
