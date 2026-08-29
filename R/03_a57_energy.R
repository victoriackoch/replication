# A.57 (energy industry uses) was extracted from a PDF table whose header
# cells span two physical columns while the data cells only fill the first
# of each pair, so the real content sits one position to the LEFT of its
# printed header (col1=Use, col3=Sub-use, col5=Properties, col7=Area of
# use/application, col9=Examples of PFAS; even columns 2/4/6/8/10 are
# empty padding throughout). This uses positional indexing rather than the
# (misleading) column names for that reason.

extract_a57 <- function() {
  tab <- load_table("A.57")
  use <- ffill(tab[[1]])
  subuse <- tab[[3]]
  examples <- tab[[9]]

  product <- map2_chr(use, subuse, function(u, s) {
    parts <- c(u, s)
    parts <- parts[!is.na(parts) & str_trim(parts) != ""]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = " / ")
  })

  out <- map2(seq_along(product), product, function(i, p) {
    subs <- split_substance_list(examples[i])
    if (length(subs) == 0 || is.na(p)) return(NULL)
    make_row_raw(p, subs, source = "A.57", source_text = examples[i])
  })
  bind_rows(out)
}

add_table(extract_a57())
