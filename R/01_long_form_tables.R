# "Long-form" tables: a Use/Sub-use column (values only printed on the first
# row of a merged block, forward-filled here) plus a free-text "Examples of
# PFAS(s)" column, split into individual substance rows.
#
# Points 17-19: product is "Use: Sub-use", and when either Use or Sub-use
# itself lists several comma-separated items in one cell, each item is
# split out and the FULL cross-product against every individual substance
# in Examples is taken (e.g. A.51's "Insulation, sheaths, tapes, jackets,
# sleeves, binders" x 11 listed substances = 66 rows for that one source
# row). None of these give a CAS number per substance. Trailing footnote-
# reference numbers ("Machinery93") are stripped from Use/Sub-use text.

strip_footnote_number <- function(x) str_remove(x, "(?<=[A-Za-z\\)])[0-9]{2,3}$")

# Splits Use and Sub-use on commas into atomic items and returns every
# "Use item: Sub-use item" combination (points 17-19).
cross_product_products <- function(use, subuse) {
  use_items <- if (is.na(use)) NA_character_ else str_trim(str_split(use, ",")[[1]])
  subuse_items <- if (is.na(subuse)) NA_character_ else str_trim(str_split(subuse, ",")[[1]])
  combos <- expand_grid(u = use_items, s = subuse_items)
  products <- apply(combos, 1, function(r) {
    parts <- r[!is.na(r) & str_trim(r) != ""]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = ": ")
  })
  products[!is.na(products)]
}

extract_long_form <- function(sheet, use_col, subuse_col, examples_col, ffill_use = TRUE) {
  tab <- load_table(sheet)
  use <- strip_footnote_number(tab[[use_col]])
  if (ffill_use) use <- ffill(use)
  subuse <- if (!is.null(subuse_col)) strip_footnote_number(tab[[subuse_col]]) else rep(NA_character_, nrow(tab))
  examples <- tab[[examples_col]]

  out <- map(seq_len(nrow(tab)), function(i) {
    subs <- split_substance_list(examples[i])
    if (length(subs) == 0) return(NULL)

    products <- cross_product_products(use[i], subuse[i])
    if (length(products) == 0) return(NULL)

    expand_grid(product = products, sub = subs) %>%
      { make_row_raw(.$product, .$sub, source = sheet, source_text = examples[i]) }
  })
  bind_rows(out)
}

add_table(extract_long_form("A.51", "Use category", "Sub-use", "Examples of PFASs"))
add_table(extract_long_form("A.53", "Use category", "Sub-use", "Examples of PFAS"))
add_table(extract_long_form("A.59", "Use category", "Sub-use(s)", "Examples of PFASs"))
add_table(extract_long_form("A.83", "Use", "Sub-use", "Examples of PFASs used"))
# A.62 (lubricants) and A.90 (broader industrial, embedded CAS numbers) need
# bespoke handling -- see 02_a62_lubricants.R and 05_name_cas_free_text.R
