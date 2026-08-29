# A.110, A.111, A.112: measured-concentration matrices (product rows x
# substance columns). Per your call, a product-substance row is kept only
# where the substance was actually detected (a non-blank value, or -- for
# A.111's paired Occurrence/Content columns -- an occurrence count > 0).

strip_unit <- function(x) str_trim(str_remove(x, "\\s*\\[[^\\]]*\\]\\s*$"))

extract_a110 <- function() {
  tab <- load_table("A.110")
  names(tab)[1] <- "product"
  subst_cols <- names(tab)[-(1:2)]
  out <- map(subst_cols, function(col) {
    val <- as.character(tab[[col]])
    hit <- !is.na(val) & str_trim(val) != ""
    if (!any(hit)) return(NULL)
    make_row_raw(tab$product[hit], strip_unit(col), source = "A.110",
                 source_text = paste0(strip_unit(col), " = ", val[hit],
                                       " (sampled ", tab[["Year of product sampling"]][hit], ")"))
  })
  bind_rows(out)
}

extract_a111 <- function() {
  tab <- load_table("A.111")
  names(tab)[1] <- "product_group"
  occ_cols <- grep(" - Occurrence", names(tab), value = TRUE)
  out <- map(occ_cols, function(occ_col) {
    subst <- str_remove(occ_col, "\\s*-\\s*Occurrence.*$")
    content_col <- names(tab)[which(names(tab) == occ_col) + 1]
    occ_n <- as.numeric(str_extract(tab[[occ_col]], "^[0-9]+"))
    hit <- !is.na(occ_n) & occ_n > 0
    if (!any(hit)) return(NULL)
    make_row_raw(tab$product_group[hit], subst, source = "A.111",
                 source_text = paste0(subst, ": ", tab[[occ_col]][hit],
                                       "; content ", tab[[content_col]][hit], " mg/kg"))
  })
  bind_rows(out)
}

extract_a112 <- function() {
  tab <- load_table("A.112")
  names(tab)[1:2] <- c("product_group", "product_number")
  tab$product_group <- ffill(tab$product_group)
  product <- paste0(tab$product_group, " (sample ", tab$product_number, ")")
  subst_cols <- names(tab)[-(1:2)]
  out <- map(subst_cols, function(col) {
    val <- as.character(tab[[col]])
    hit <- !is.na(val) & str_trim(val) != ""
    if (!any(hit)) return(NULL)
    make_row_raw(product[hit], strip_unit(col), source = "A.112",
                 source_text = paste0(strip_unit(col), " = ", val[hit], " mg/kg"))
  })
  bind_rows(out)
}

add_table(extract_a110())
add_table(extract_a111())
add_table(extract_a112())
