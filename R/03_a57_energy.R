# A.57 (energy industry uses) was extracted from a PDF table whose header
# cells span two physical columns while the data cells only fill the first
# of each pair, so the real content sits one position to the LEFT of its
# printed header (col1=Use, col3=Sub-use, col5=Properties, col7=Area of
# use/application, col9=Examples of PFAS; even columns 2/4/6/8/10 are
# empty padding throughout). This uses positional indexing rather than the
# (misleading) column names for that reason.
#
# Points 17-19: product is "Use: Sub-use", cross-joined against every
# comma-split item in each and against every individual listed substance
# (e.g. "Solar collector: Film/coating").

extract_a57 <- function() {
  tab <- load_table("A.57")
  use <- strip_footnote_number(tab[[1]])
  use <- ffill(use)
  subuse <- strip_footnote_number(tab[[3]])
  examples <- tab[[9]]

  out <- map(seq_len(nrow(tab)), function(i) {
    subs <- split_substance_list(examples[i])
    if (length(subs) == 0) return(NULL)
    products <- cross_product_products(use[i], subuse[i])
    if (length(products) == 0) return(NULL)
    expand_grid(product = products, sub = subs) %>%
      { make_row_raw(.$product, .$sub, source = "A.57", source_text = examples[i]) }
  })
  bind_rows(out)
}

add_table(extract_a57())
