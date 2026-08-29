# "Long-form" tables: a Use/Sub-use column (values only printed on the first
# row of a merged block, forward-filled here) plus a free-text "Examples of
# PFAS(s)" column, split into individual substance rows. None of these give
# a CAS number per substance.

extract_long_form <- function(sheet, use_cols, examples_col, ffill_cols = use_cols, sep = " / ") {
  tab <- load_table(sheet)
  for (col in ffill_cols) tab[[col]] <- ffill(tab[[col]])
  product <- if (length(use_cols) == 1) {
    tab[[use_cols]]
  } else {
    apply(tab[use_cols], 1, function(r) {
      r <- r[!is.na(r) & str_trim(r) != ""]
      if (length(r) == 0) NA_character_ else paste(r, collapse = sep)
    })
  }
  examples <- tab[[examples_col]]
  out <- map2(seq_along(product), product, function(i, p) {
    subs <- split_substance_list(examples[i])
    if (length(subs) == 0 || is.na(p)) return(NULL)
    make_row_raw(p, subs, source = sheet, source_text = examples[i])
  })
  bind_rows(out)
}

add_table(extract_long_form("A.51", "Use category", "Examples of PFASs"))
add_table(extract_long_form("A.53", "Use category", "Examples of PFAS"))
add_table(extract_long_form("A.59", "Use category", "Examples of PFASs"))
add_table(extract_long_form("A.83", "Use", "Examples of PFASs used"))
# A.62 (lubricants) and A.90 (broader industrial, embedded CAS numbers) need
# bespoke handling -- see 02_a62_lubricants.R and 03_name_cas_tables.R
