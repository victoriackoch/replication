# Tables that already give clean, separate Name/Abbreviation/CAS/Use
# columns per row -- no free-text splitting needed. `product` can be a
# column name (ffilled if `ffill_product`) or a constant string when the
# table only names its product in the title.

extract_clean <- function(sheet, name_col, abbrev_col = NULL, cas_col = NULL,
                           product_col = NULL, product_const = NULL,
                           ffill_product = FALSE) {
  tab <- load_table(sheet)
  name <- tab[[name_col]]
  keep <- !is.na(name) & str_trim(name) != "" & !vapply(name, is_no_info, logical(1))
  tab <- tab[keep, , drop = FALSE]

  if (!is.null(product_const)) {
    product <- rep(product_const, nrow(tab))
  } else {
    product <- tab[[product_col]]
    if (ffill_product) product <- ffill(product)
  }

  cas <- if (!is.null(cas_col)) {
    x <- tab[[cas_col]]
    ifelse(vapply(x, is_no_info, logical(1)), NA_character_, x)
  } else NA_character_

  if (!is.null(abbrev_col)) {
    abbrev <- tab[[abbrev_col]]
    abbrev <- ifelse(vapply(abbrev, is_no_info, logical(1)), NA_character_, abbrev)
    substance_name <- tab[[name_col]]
    make_row(product, substance_name, abbrev, cas, source = sheet,
             source_text = paste0(sheet, " row: ", tab[[name_col]]))
  } else {
    # no separate abbreviation column -- try to recover one embedded in the
    # name text itself, e.g. "Poly(1,1,2,2-tetrafluoroethylene) (PTFE)"
    parsed <- map(tab[[name_col]], parse_name_abbrev)
    substance_name <- ifelse(is.na(map_chr(parsed, "name")), tab[[name_col]], map_chr(parsed, "name"))
    abbrev <- map_chr(parsed, "abbrev")
    make_row(product, substance_name, abbrev, cas, source = sheet,
             source_text = paste0(sheet, " row: ", tab[[name_col]]))
  }
}

add_table(extract_clean("A.100", "Name", "Abbreviation", "CAS", product_col = "Use"))
add_table(extract_clean("A.129", "Name(s) as specified in the sources consulted", NULL,
                         "CAS no", product_col = "Use in lubricant application"))
add_table(extract_clean("A.125", "Polymer", "Abbreviation", "CAS number",
                         product_const = "Medical devices (incl. medical device production)"))
add_table(extract_clean("A.106", "Substance Name", "Abbreviation", "CAS Number**",
                         product_const = "Consumer cookware"))
