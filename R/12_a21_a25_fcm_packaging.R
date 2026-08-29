# A.21 (grease-proof paper in FCM flexible packaging): no per-row product
# column -- "Grease-proof paper" is the table's whole subject (title only).
extract_a21 <- function() {
  tab <- load_table("A.21")
  names(tab)[1:2] <- c("chemical_name", "cas")
  tab <- tab[!is.na(tab$chemical_name), , drop = FALSE]
  cleaned <- str_remove(tab$chemical_name, regex("^(Polymeric PFAS|Non-polymeric PFAS|PFAA and PFAA precursors)\\s*:\\s*", ignore_case = TRUE))
  parsed <- map(cleaned, parse_name_abbrev)
  cas <- ifelse(tab$cas %in% c("-", ""), NA_character_, tab$cas)
  make_row(
    product = "Grease-proof paper (FCM flexible packaging)",
    substance_name = ifelse(is.na(map_chr(parsed, "name")), cleaned, map_chr(parsed, "name")),
    abbreviation = map_chr(parsed, "abbrev"),
    cas_number = cas,
    source = "A.21",
    source_text = paste0("A.21 row: ", tab$chemical_name)
  )
}

# A.25 (inks, lacquers, waxes in FCM flexible packaging): "Chemical name
# [application]" bundles the product into the name cell as a bracketed
# tag, e.g. "Poly(tetrafluoroethylene) [lacquer]" -- split into
# product = "Flexible packaging -- <bracket tag>" and the chemical name.
extract_a25 <- function() {
  tab <- load_table("A.25")
  names(tab)[1:2] <- c("chemical_name", "cas")
  tab <- tab[!is.na(tab$chemical_name), , drop = FALSE]
  m <- str_match(tab$chemical_name, "^(.*?)\\s*\\[([^\\]]+)\\]\\s*$")
  name_part <- ifelse(is.na(m[, 2]), tab$chemical_name, m[, 2])
  app_part <- ifelse(is.na(m[, 3]), "Flexible packaging (FCM)", paste0("Flexible packaging (FCM) -- ", m[, 3]))
  parsed <- map(name_part, parse_name_abbrev)
  make_row(
    product = app_part,
    substance_name = ifelse(is.na(map_chr(parsed, "name")), name_part, map_chr(parsed, "name")),
    abbreviation = map_chr(parsed, "abbrev"),
    cas_number = tab$cas,
    source = "A.25",
    source_text = paste0("A.25 row: ", tab$chemical_name)
  )
}

add_table(extract_a21())
add_table(extract_a25())
