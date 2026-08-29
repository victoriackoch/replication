# A.130 (EU approved PPP active substances): no separate "product" column,
# but "Regulatory Program" states what kind of product the substance is an
# active substance in (e.g. "Active substance in plant protection
# products") -- used as product directly.
extract_a130 <- function() {
  tab <- load_table("A.130")
  names(tab)[c(1, 3, 6)] <- c("substance_name", "cas", "regulatory_program")
  keep <- !is.na(tab$regulatory_program) & str_trim(tab$regulatory_program) != "" &
    !is.na(tab$substance_name)
  tab <- tab[keep, , drop = FALSE]
  out <- map(seq_len(nrow(tab)), function(i) {
    triple <- process_substance_triple(tab$substance_name[i], NA_character_, tab$cas[i], "A.130")
    make_row(
      product = tab$regulatory_program[i],
      substance_name = triple$substance_name,
      abbreviation = triple$abbreviation,
      cas_number = triple$cas_number,
      source = "A.130",
      source_text = paste0("A.130 row: ", tab$substance_name[i]),
      substance_synonym = triple$substance_synonym,
      substance_group = triple$substance_group
    )
  })
  bind_rows(out)
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
  out <- map(seq_len(nrow(tab)), function(i) {
    types <- split_product_types(tab$product_type[i])
    triple <- process_substance_triple(tab$substance_name[i], NA_character_, tab$cas[i], "A.131")
    # cross the product-types against the (usually single) substance result
    expand_grid(product = types, row = seq_len(nrow(triple))) %>%
      mutate(
        substance_name = triple$substance_name[row],
        substance_synonym = triple$substance_synonym[row],
        substance_group = triple$substance_group[row],
        cas_number = triple$cas_number[row],
        abbreviation = triple$abbreviation[row]
      ) %>%
      select(-row) %>%
      mutate(source = "A.131", source_text = paste0("A.131 row: ", tab$substance_name[i]))
  })
  bind_rows(out)
}

add_table(extract_a130())
add_table(extract_a131())
