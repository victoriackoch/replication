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

extract_a105 <- function() {
  tab <- load_table("A.105")
  names(tab) <- c("substance_name", "abbreviation", "cas", "formula", "use", "function_reg")

  for (col in c("substance_name", "abbreviation", "cas", "formula")) {
    tab[[col]] <- ffill(tab[[col]])
  }

  keep <- !is.na(tab$use) & str_trim(tab$use) != ""
  tab <- tab[keep, , drop = FALSE]

  abbrev <- ifelse(vapply(tab$abbreviation, is_no_info, logical(1)), NA_character_, tab$abbreviation)
  cas <- ifelse(vapply(tab$cas, is_no_info, logical(1)), NA_character_, tab$cas)

  make_row(tab$use, tab$substance_name, abbrev, cas, source = "A.105",
           source_text = paste0("A.105 row: ", tab$substance_name, " -- ", coalesce(tab$function_reg, "")))
}

add_table(extract_a105())
