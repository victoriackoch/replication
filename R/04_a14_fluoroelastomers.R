# A.14: three sector columns (Automotive / Aerospace / Industrial), each
# cell is a specific application; the substance is "Fluoroelastomers"
# (abbreviation FKM) for the whole table -- that's the table's whole
# subject, given only in the title, not a per-row field.

extract_a14 <- function() {
  tab <- load_table("A.14")
  names(tab) <- c("Automotive", "Aerospace", "Industrial")

  # PDF line-wrap: "Industrial roll covers (100%" / "FKM or laminates with other elastomers)"
  wrap_idx <- which(str_trim(coalesce(tab$Industrial, "")) ==
                       "FKM or laminates with other elastomers)")
  if (length(wrap_idx) == 1 && wrap_idx > 1) {
    tab$Industrial[wrap_idx - 1] <- paste0(tab$Industrial[wrap_idx - 1], " ",
                                            tab$Industrial[wrap_idx])
    tab <- tab[-wrap_idx, ]
  }

  # point 16: tag each product with the sector its column came from
  long <- map_dfr(names(tab), function(sector) {
    vals <- tab[[sector]]
    vals <- vals[!is.na(vals) & str_trim(vals) != ""]
    tibble(product = paste0(vals, " (", sector, ")"))
  })

  tibble(
    product = long$product,
    substance_name = "Fluoroelastomers",
    abbreviation = "FKM",
    cas_number = NA_character_,
    source = "A.14",
    source_text = "Table A.14 title: Fluoroelastomers - non-exhaustive overview over specific uses and applications sectors"
  )
}

add_table(extract_a14())
