# A.119 (fluorinated gases in commercial/HVACR use): product is the
# General use / Sub-use / Specific use hierarchy (General use is only
# printed on the first row of each block, so it's forward-filled; Sub-use
# and Specific use are per-row already).
extract_a119 <- function() {
  tab <- load_table("A.119")
  names(tab) <- c("substance", "code", "cas", "general_use", "sub_use", "specific_use")
  tab$general_use <- ffill(tab$general_use)
  product <- apply(tab[c("general_use", "sub_use", "specific_use")], 1, function(r) {
    r <- r[!is.na(r) & str_trim(r) != ""]
    if (length(r) == 0) NA_character_ else paste(r, collapse = " / ")
  })
  keep <- !is.na(product) & !is.na(tab$substance)
  code <- ifelse(vapply(tab$code, is_no_info, logical(1)), NA_character_, tab$code)
  cas <- ifelse(vapply(tab$cas, is_no_info, logical(1)), NA_character_, tab$cas)
  make_row(product[keep], tab$substance[keep], code[keep], cas[keep],
           source = "A.119", source_text = paste0("A.119 row: ", tab$substance[keep]))
}

# A.126 (other polymeric PFAS, non-PTFE): "Uses" is a comma-separated prose
# list of applications rather than a clean enumeration (e.g. "Laboratory,
# analytical and medical equipment, internal connection parts in vacuum
# pumps, diaphragm pumps, tubes, seals, bushes, cables and valves") -- kept
# as one product string per row rather than split, since the commas here
# are descriptive, not a delimiter between distinct discrete products (same
# rule used for A.104/A.131's non-tagged multi-value cells). No CAS column
# exists in this table.
extract_a126 <- function() {
  tab <- load_table("A.126")
  names(tab) <- c("fluoropolymer", "properties", "uses")
  parsed <- map(tab$fluoropolymer, parse_name_abbrev)
  make_row(
    product = tab$uses,
    substance_name = ifelse(is.na(map_chr(parsed, "name")), tab$fluoropolymer, map_chr(parsed, "name")),
    abbreviation = map_chr(parsed, "abbrev"),
    cas_number = NA_character_,
    source = "A.126",
    source_text = paste0("A.126 row: ", tab$fluoropolymer)
  )
}

add_table(extract_a119())
add_table(extract_a126())
