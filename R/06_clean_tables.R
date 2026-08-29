# Tables that already give clean, separate Name/Abbreviation/CAS/Use
# columns per row -- no free-text splitting needed, but the name/abbrev/CAS
# triple is still run through process_substance_triple() (00b) to handle
# "a polymer of:" truncation, "X or Y" synonym/multi-CAS splitting, and
# abbreviation-synonym splitting. `product` can be a column name (ffilled
# if `ffill_product`) or a constant string when the table only names its
# product in the title.

extract_clean <- function(sheet, name_col, abbrev_col = NULL, cas_col = NULL,
                           product_col = NULL, product_const = NULL,
                           ffill_product = FALSE, func_col = NULL) {
  tab <- load_table(sheet)
  name <- tab[[name_col]]
  keep <- !is.na(name) & str_trim(name) != "" & !vapply(name, is_no_info, logical(1))
  tab <- tab[keep, , drop = FALSE]
  name <- tab[[name_col]]

  if (!is.null(func_col)) {
    func <- tab[[func_col]]
    ppa_hits <- which(!is.na(func) & str_detect(func, PPA_MONOMER_RE))
    if (length(ppa_hits) > 0) {
      abbrev_col_vals <- if (!is.null(abbrev_col)) tab[[abbrev_col]][ppa_hits] else NA_character_
      cas_col_vals <- if (!is.null(cas_col)) tab[[cas_col]][ppa_hits] else NA_character_
      add_ppa_row(tibble(
        source = sheet, substance_name = name[ppa_hits], abbreviation = abbrev_col_vals,
        cas_number = cas_col_vals, use = if (!is.null(product_const)) product_const else NA_character_,
        regulatory_listing = func[ppa_hits]
      ))
    }
  }

  if (!is.null(product_const)) {
    product <- rep(product_const, nrow(tab))
  } else {
    product <- tab[[product_col]]
    if (ffill_product) product <- ffill(product)
  }

  abbrev_raw <- if (!is.null(abbrev_col)) tab[[abbrev_col]] else rep(NA_character_, nrow(tab))
  cas_raw <- if (!is.null(cas_col)) tab[[cas_col]] else rep(NA_character_, nrow(tab))

  out <- map(seq_len(nrow(tab)), function(i) {
    triple <- process_substance_triple(name[i], abbrev_raw[i], cas_raw[i], sheet)
    make_row(
      product = product[i],
      substance_name = triple$substance_name,
      abbreviation = triple$abbreviation,
      cas_number = triple$cas_number,
      source = sheet,
      source_text = paste0(sheet, " row: ", name[i]),
      substance_synonym = triple$substance_synonym,
      abbreviation_synonym = triple$abbreviation_synonym,
      substance_group = triple$substance_group
    )
  })
  bind_rows(out)
}

a129_table <- extract_clean("A.129", "Name(s) as specified in the sources consulted", NULL,
                            "CAS no", product_col = "Use in lubricant application")
lowercased <- paste0(tolower(substr(a129_table$product, 1, 1)),
                     substr(a129_table$product, 2, nchar(a129_table$product)))
a129_table$product <- paste("Lubricant", lowercased)
add_table(a129_table)

add_table(extract_clean("A.106", "Substance Name", "Abbreviation", "CAS Number**",
                         product_const = "Consumer cookware",
                         func_col = "Function and Listing in EU Regulation 10/2011"))
