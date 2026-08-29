# A.107: single "Group/substance" column mixes two section-header rows
# ("Metal plating", "Manufacture of metal products" -- these ARE the
# product/use categories, given nowhere else in the table) with substance
# rows. No CAS column exists in this table. Two rows are PDF line-wraps
# that split a single substance name, and a single EU-market value, across
# two physical rows; both are merged back before extraction.

extract_a107 <- function() {
  tab <- load_table("A.107")
  names(tab)[1:2] <- c("group_substance", "market")

  # name wrap: "Sodium N-(2-carboxylatoethyl)-3-[(2-" / "carboxyethyl)...propanaminium"
  wrap1 <- which(str_detect(coalesce(tab$group_substance, ""), "Sodium N-\\(2-carboxylatoethyl\\)-3-\\[\\(2-$"))
  if (length(wrap1) == 1) {
    tab$group_substance[wrap1] <- paste0(tab$group_substance[wrap1], tab$group_substance[wrap1 + 1])
    tab <- tab[-(wrap1 + 1), ]
  }

  # market-value wrap: "...1-10 (registration dossier," / "volume not limited to the use..."
  wrap2 <- which(str_detect(coalesce(tab$market, ""), "\\(registration dossier,$"))
  if (length(wrap2) == 1) {
    tab$market[wrap2] <- paste(tab$market[wrap2], tab$market[wrap2 + 1])
    tab <- tab[-(wrap2 + 1), ]
  }

  group_labels <- c("Metal plating", "Manufacture of metal products")
  is_header <- tab$group_substance %in% group_labels
  group <- ifelse(is_header, tab$group_substance, NA_character_)
  group <- ffill(group)

  tab <- tab[!is_header, , drop = FALSE]
  group <- group[!is_header]

  parsed <- map(tab$group_substance, parse_name_abbrev)
  make_row(
    product = group,
    substance_name = ifelse(is.na(map_chr(parsed, "name")), tab$group_substance, map_chr(parsed, "name")),
    abbreviation = map_chr(parsed, "abbrev"),
    cas_number = NA_character_,
    source = "A.107",
    source_text = paste0("EU market: ", coalesce(tab$market, "not given"))
  )
}

add_table(extract_a107())
