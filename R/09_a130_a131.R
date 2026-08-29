# A.130 (EU approved PPP active substances): no separate "product" column,
# but "Regulatory Program" states what kind of product the substance is an
# active substance in (e.g. "Active substance in plant protection
# products") -- used as product directly.
extract_a130 <- function() {
  tab <- load_table("A.130")
  names(tab)[c(1, 3, 6)] <- c("substance_name", "cas", "regulatory_program")
  parsed <- map(tab$substance_name, parse_name_abbrev)
  keep <- !is.na(tab$regulatory_program) & str_trim(tab$regulatory_program) != "" &
    !is.na(tab$substance_name)
  make_row(
    product = tab$regulatory_program[keep],
    substance_name = ifelse(is.na(map_chr(parsed, "name"))[keep], tab$substance_name[keep], map_chr(parsed, "name")[keep]),
    abbreviation = map_chr(parsed, "abbrev")[keep],
    cas_number = tab$cas[keep],
    source = "A.130",
    source_text = paste0("A.130 row: ", tab$substance_name[keep])
  )
}

# A.131 (EU approved biocidal active substances): "Product-type" lists one
# or more EU biocidal product-types per substance, e.g. "PT08-Wood
# preservatives, PT18-Insecticides, acaricides and products to control
# other arthropods" -- two product-types here, NOT three, because the
# comma inside "PT18-Insecticides, acaricides..." is part of that single
# type's own descriptive name. A marker is inserted right before each
# "PTnn-" boundary that isn't already at the start of the string, then the
# text is split on that marker -- so splitting happens only at true
# product-type boundaries, not on every comma.
split_product_types <- function(text) {
  marker <- "@@PT@@"
  marked <- str_replace_all(text, "(?<!^)(PT[0-9]+-)", paste0(marker, "\\1"))
  parts <- str_split(marked, fixed(marker))[[1]]
  parts <- str_remove(parts, "^,\\s*")
  parts <- str_remove(parts, ",\\s*$")
  parts <- str_trim(parts)
  parts[parts != ""]
}

extract_a131 <- function() {
  tab <- load_table("A.131")
  names(tab)[c(1, 3, 5)] <- c("substance_name", "cas", "product_type")
  parsed <- map(tab$substance_name, parse_name_abbrev)
  out <- map(seq_len(nrow(tab)), function(i) {
    types <- split_product_types(tab$product_type[i])
    tibble(
      product = types,
      substance_name = if (is.na(parsed[[i]]$name)) tab$substance_name[i] else parsed[[i]]$name,
      abbreviation = parsed[[i]]$abbrev,
      cas_number = tab$cas[i],
      source = "A.131",
      source_text = paste0("A.131 row: ", tab$substance_name[i])
    )
  })
  bind_rows(out)
}

add_table(extract_a130())
add_table(extract_a131())
