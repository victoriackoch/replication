# A.78 and A.123 are checklist matrices: substances as column headers,
# products as rows, a mark (X, (X), or a check mark) where that substance
# is used in that product. Melted into one row per marked cell. Per your
# call, "(X)" is treated the same as "X".

melt_matrix <- function(product, substance_cols, marks, col_to_token, source, source_text) {
  out <- list()
  for (j in seq_along(substance_cols)) {
    col <- substance_cols[j]
    hit <- str_detect(coalesce(as.character(marks[[col]]), ""), regex("^\\(?x\\)?$|✓", ignore_case = TRUE))
    hit <- hit & !is.na(product)
    if (any(hit)) {
      out[[col]] <- make_row_raw(product[hit], col_to_token[[col]], source = source,
                                  source_text = source_text[hit])
    }
  }
  bind_rows(out)
}

extract_a78 <- function() {
  tab <- load_table("A.78")
  names(tab)[1:2] <- c("use", "group_label")
  group <- ffill(tab$group_label)
  product <- map2_chr(group, tab$use, function(g, u) {
    parts <- c(g, u)
    parts <- parts[!is.na(parts) & str_trim(parts) != ""]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = " / ")
  })
  # only rows that actually name a specific use (not the group-header-only rows)
  is_data_row <- !is.na(tab$use)
  product[!is_data_row] <- NA_character_

  substance_cols <- c("PTFE", "ETFE", "PCTFE", "FEP", "PFA", "F- HDPE", "Non-specified FPs")
  col_to_token <- list(
    PTFE = "PTFE", ETFE = "ETFE", PCTFE = "PCTFE", FEP = "FEP", PFA = "PFA",
    `F- HDPE` = "Fluorinated HDPE (F-HDPE)",
    `Non-specified FPs` = "Non-specified fluoropolymers"
  )
  src_text <- paste0("A.78 row (", coalesce(product, ""), "): marks = ",
                      apply(tab[substance_cols], 1, function(r) paste(na.omit(r), collapse = ", ")))
  melt_matrix(product, substance_cols, tab, col_to_token, "A.78", src_text)
}

extract_a123 <- function() {
  tab <- load_table("A.123")
  names(tab)[1:2] <- c("specialty", "devices")
  product <- paste(tab$specialty, tab$devices, sep = " / ")

  # A.123 is a general materials table (Teo et al. 2016) covering many
  # non-fluorinated polymers for comparison -- PE, PA, PDMS, PHA, PET, PP,
  # Silicone, LCP, Parylene, PMMA, PEK, PI, SU8 are not PFAS. Only the PTFE
  # column is in scope here.
  substance_cols <- c("PTFE")
  col_to_token <- list(PTFE = "PTFE")
  src_text <- paste0("A.123 row (", product, "): marks = ",
                      apply(tab[substance_cols], 1, function(r) paste(na.omit(r), collapse = ", ")))
  melt_matrix(product, substance_cols, tab, col_to_token, "A.123", src_text)
}

add_table(extract_a78())
add_table(extract_a123())
