# A.125 (polymers/elastomers used in medical devices): product is a
# constant "Medical devices (incl. medical device production)" from the
# title, except polychlorotrifluoroethylene (PCTFE), which is used
# specifically in pharmaceutical packaging rather than devices themselves
# (point 15). One row ("fluorosilicones fluorosilicone rubber" / "FVQM
# FVQM" / "63148-56-1,64706-30-5") packs two distinct materials into one
# cell with no delimiter between the names or abbreviations, only between
# the CAS numbers -- split into two lines by hand (point 14).

extract_a125 <- function() {
  tab <- load_table("A.125")
  names(tab) <- c("polymer", "abbreviation", "cas")

  fluorosilicone_idx <- which(str_detect(coalesce(tab$polymer, ""), "fluorosilicones fluorosilicone rubber"))
  fixed_rows <- tibble(
    polymer = c("fluorosilicones", "fluorosilicone rubber"),
    abbreviation = c("FVQM", "FVQM"),
    cas = c("63148-56-1", "64706-30-5")
  )
  if (length(fluorosilicone_idx) == 1) {
    tab <- bind_rows(tab[-fluorosilicone_idx, ], fixed_rows)
  }

  tab <- tab[!is.na(tab$polymer) & str_trim(tab$polymer) != "", , drop = FALSE]

  out <- map(seq_len(nrow(tab)), function(i) {
    triple <- process_substance_triple(tab$polymer[i], tab$abbreviation[i], tab$cas[i], "A.125")
    product <- if (str_detect(tab$polymer[i], regex("polychlorotrifluoroethylene", ignore_case = TRUE))) {
      "Pharmaceutical packaging"
    } else {
      "Medical devices (incl. medical device production)"
    }
    make_row(
      product = product, substance_name = triple$substance_name, abbreviation = triple$abbreviation,
      cas_number = triple$cas_number, source = "A.125", source_text = paste0("A.125 row: ", tab$polymer[i]),
      substance_synonym = triple$substance_synonym, abbreviation_synonym = triple$abbreviation_synonym,
      substance_group = triple$substance_group
    )
  })
  bind_rows(out)
}

add_table(extract_a125())
