# A.119 (fluorinated gases in commercial/HVACR use): product is the
# General use / Sub-use / Specific use hierarchy (General use is only
# printed on the first row of each block, so it's forward-filled; Sub-use
# and Specific use are per-row already).
#
# Rows 95-135 of the loaded table (source rows covering "Methoxytridecafluoro-
# heptene isomers" through "1,1,2,3,3,3-Hexafluoropropene, oxidized, polymd.
# (Perfluoropolyether, PFPE)") are corrupted at the source-extraction level:
# Code/CAS/General use/Specific use are shifted across columns (e.g. a Code
# value like "MPHE, SionTM" ends up in the CAS column) and some Specific-use
# text is dropped entirely, with the substance name itself wrapping into the
# Code column for a few rows. This is hand-corrected below using the
# original PDF table images (pp. 441-447) as ground truth, rather than
# algorithmically un-shifting columns that have already lost content.
A119_CORRUPTED_SUBSTANCES <- c(
  "Methoxytridecafluoro-heptene isomers", "Dodecafluoro-2-methyl-3-pentanone",
  "1,1,2,2-Tetrafluoro-1- (2,2,2-trifluoroethoxy) ethane",
  "Methyl perfluoropropyl ether",
  "Methyl nonafluorobutyl ether + Methyl nonafluoroisobutyl ether",
  "1-Ethoxy-nonafluorobutane"
)

a119_fix <- tribble(
  ~substance, ~code, ~cas, ~general_use, ~sub_use, ~specific_use,
  "Methoxytridecafluoro-heptene isomers", "MPHE, SionTM", "No data",
    "Solvents", "Precision & electronics cleaning, commercial & industrial cleaning and carrier solvent & lubricants", NA,
  "Methoxytridecafluoro-heptene isomers", "MPHE, SionTM", "No data",
    "Other", "Debinding agent, 3D printing", NA,
  "Dodecafluoro-2-methyl-3-pentanone", "FK-5-1-12", "756-13-8",
    "Cover gases", "Magnesium casting", NA,
  "Dodecafluoro-2-methyl-3-pentanone", "FK-5-1-12", "756-13-8",
    "Fire Suppressant", "Local streaming agent", NA,
  "1,1,2,2-Tetrafluoro-1-(2,2,2-trifluoroethoxy)ethane", "HFE-347pc-f2", "406-78-0",
    "Solvents", "Precision & electronics cleaning, commercial & industrial cleaning", NA,
  "Methyl perfluoropropyl ether", "HFE-7000", "375-03-1",
    "Solvents", "Carrier solvent & lubricants", NA,
  "Methyl nonafluorobutyl ether", "HFE-449mccc (HFE-7100 blend)", "163702-08-7",
    "Solvents", "Precision & electronics cleaning, commercial & industrial cleaning and carrier solvent & lubricants", NA,
  "Methyl nonafluoroisobutyl ether", "HFE-449s1 (HFE-7100 blend)", "163702-07-6",
    "Solvents", "Precision & electronics cleaning, commercial & industrial cleaning and carrier solvent & lubricants", NA,
  "Methyl nonafluorobutyl ether", "HFE-449mccc (HFE-7100 blend)", "163702-08-7",
    "Other", "Immersion cooling of electronics", NA,
  "Methyl nonafluoroisobutyl ether", "HFE-449s1 (HFE-7100 blend)", "163702-07-6",
    "Other", "Immersion cooling of electronics", NA,
  "Methyl nonafluorobutyl ether", "HFE-449mccc (HFE-7100 blend)", "163702-08-7",
    "Cover gas", "Magnesium casting", NA,
  "Methyl nonafluoroisobutyl ether", "HFE-449s1 (HFE-7100 blend)", "163702-07-6",
    "Cover gas", "Magnesium casting", NA,
  "Methyl nonafluorobutyl ether", "HFE-449mccc (HFE-7100 blend)", "163702-08-7",
    "Solvents", "Cultural heritage paper preservation", NA,
  "Methyl nonafluoroisobutyl ether", "HFE-449s1 (HFE-7100 blend)", "163702-07-6",
    "Solvents", "Cultural heritage paper preservation", NA,
  "1-Ethoxy-nonafluorobutane", "HFE-569mccc/HFE-569sf2 (HFE-7200)", "163702-05-4",
    "Solvents", "Precision & electronics cleaning, commercial & industrial cleaning and carrier solvent & lubricants", NA,
  "1-Ethoxy-nonafluorobutane", "HFE-569mccc/HFE-569sf2 (HFE-7200)", "163702-05-4",
    "Cover gas", "Magnesium casting", NA,
  "3-Methoxyperfluoro(2-methylpentane)", "HFE-7300", "132182-92-4",
    "Solvents", NA, NA,
  "3-Ethoxyperfluoro(2-methylhexane)", "HFE-7500", "297730-93-9",
    "Solvents", "Commercial & industrial cleaning", NA,
  "3-Ethoxyperfluoro(2-methylhexane)", "HFE-7500", "297730-93-9",
    "Refrigerant", "Electronics cooling, military applications", NA,
  "Hexafluoroisopropanol", "HFIP", "920-66-1",
    "Solvents", "3D printing processing liquid", NA,
  "2,3,3,3-tetrafluoro-2-(trifluoromethyl)-propanenitrile", "C4-FN", "42532-60-5",
    "Insulating gas", "Electrical switchgear (high voltage)", NA,
  "1,1,1,3,4,4,4-heptafluoro-3-(trifluoromethyl)-2-butanone", "C5-FK", "756-12-7",
    "Insulating gas", "Electrical switchgear (medium voltage)", NA,
  "(E)-1,1,1,2,3,4,4,4-nonafluoro-4-(trifluoromethyl)-2-pentene", "FA-188", "3709-71-5",
    "Foam-blowing agents", "Polyurethane foam, closed cell", NA,
  "Perfluorohexane (n- and iso-)", "FC-72/PF-5060", "1064697-81-9",
    "Solvents", "Heat transfer agent", NA,
  "Perfluorohexane (n- and iso-)", "FC-72/PF-5060", "1064697-81-9",
    "Solvents", "Cultural heritage paper preservation", NA,
  "Perfluorotripropylamine (perfluamine)", "FC-3283", "338-83-0",
    "Solvents", "Heat transfer agent", NA,
  "Perfluorotributylamine", "FC-40/FC-3284", "311-89-7 (1064698-37-8)",
    "Solvents", "Heat transfer agent", NA,
  "Perfluorotributylamine", "FC-40/FC-3284", "311-89-7 (1064698-37-8)",
    "Other", "Immersion cooling of electronics", NA,
  "Perfluoro-N-propyl-morpholine (mixture of isomers)", "FC-770", "1093615-61-2",
    "Solvents", "Heat transfer agent", NA,
  "Perfluoro-2-methylpentane", "Flutec RC1", "355-04-4",
    "Foam-blowing agents", "Rigid closed-cell PU/PIR insulation foam", NA,
  "1,1,2,3,3,3-Hexafluoropropene, oxidized, polymd. (Perfluoropolyether, PFPE)", "Galden HT-55/HT-70", "69991-67-9",
    "Other", "Immersion cooling of electronics", NA
)

extract_a119 <- function() {
  tab <- load_table("A.119")
  names(tab) <- c("substance", "code", "cas", "general_use", "sub_use", "specific_use")

  # drop the corrupted rows (identified by substance name, since row
  # position also carries the mid-block wrap fragments with substance = NA)
  idx <- seq_len(nrow(tab))
  bad_idx <- which(tab$substance %in% A119_CORRUPTED_SUBSTANCES | (idx >= 95 & idx <= 135))
  tab <- tab[-bad_idx, ]
  tab <- bind_rows(tab, a119_fix)

  # PDF line-wrap: a substance name with an unclosed "(" (e.g. "HFC/HFO
  # Blend (HFC-32/125/134a HFO-") continues on the next row (e.g.
  # "1234yf)"), which otherwise has no general/sub/specific-use of its own
  # -- merge it back rather than keep the truncated name.
  unbalanced <- str_count(coalesce(tab$substance, ""), fixed("(")) -
    str_count(coalesce(tab$substance, ""), fixed(")"))
  wrap_idx <- which(unbalanced > 0)
  if (length(wrap_idx) > 0) {
    for (w in rev(wrap_idx)) {
      if (w < nrow(tab) && is.na(tab$general_use[w + 1]) && is.na(tab$sub_use[w + 1])) {
        tab$substance[w] <- paste0(tab$substance[w], tab$substance[w + 1])
        tab <- tab[-(w + 1), ]
      }
    }
  }

  # PDF line-wrap, general case: every real row in this table has both a
  # Code and a CAS number; a row with BOTH blank (but a non-blank
  # Substance) is pure wrap-continuation text, with fragments of the
  # substance name, general-use, AND specific-use each landing in their
  # own column on this row (e.g. row "HFC Blend" / code=R-507A / gen=
  # "Refrigeration" / specific="Large-scale food storage" continues on
  # the next row as substance="(HFC-125/143a)", gen="and heat pumps",
  # specific="and processing", with no code/cas of its own) -- merge every
  # fragment back into the row above rather than emit it as if it named
  # its own substance.
  no_code_cas <- is.na(tab$code) & is.na(tab$cas) & !is.na(tab$substance)
  frag_idx <- which(no_code_cas)
  if (length(frag_idx) > 0) {
    for (w in rev(frag_idx)) {
      if (w > 1) {
        for (col in c("substance", "general_use", "sub_use", "specific_use")) {
          if (!is.na(tab[[col]][w])) {
            tab[[col]][w - 1] <- paste(coalesce(tab[[col]][w - 1], ""), tab[[col]][w])
          }
        }
        tab <- tab[-w, ]
      }
    }
  }

  tab$general_use <- ffill(tab$general_use)
  product <- apply(tab[c("general_use", "sub_use", "specific_use")], 1, function(r) {
    r <- r[!is.na(r) & str_trim(r) != ""]
    if (length(r) == 0) NA_character_ else paste(r, collapse = " / ")
  })
  keep <- !is.na(product) & !is.na(tab$substance)
  code <- ifelse(vapply(tab$code, is_no_info, logical(1)), NA_character_, tab$code)
  cas <- ifelse(vapply(tab$cas, is_no_info, logical(1)), NA_character_, tab$cas)
  make_row(product[keep], tab$substance[keep], code[keep], cas[keep],
           source = "A.119", source_text = paste0("A.119 row: ", tab$substance[keep]))
}

# A.126 (other polymeric PFAS, non-PTFE): "Uses" is a comma-separated prose
# list of applications rather than a clean enumeration (e.g. "Laboratory,
# analytical and medical equipment, internal connection parts in vacuum
# pumps, diaphragm pumps, tubes, seals, bushes, cables and valves") -- kept
# as one product string per row rather than split, since the commas here
# are descriptive, not a delimiter between distinct discrete products (same
# rule used for A.104/A.131's non-tagged multi-value cells). No CAS column
# exists in this table.
extract_a126 <- function() {
  tab <- load_table("A.126")
  names(tab) <- c("fluoropolymer", "properties", "uses")
  parsed <- map(tab$fluoropolymer, parse_name_abbrev)
  make_row(
    product = tab$uses,
    substance_name = ifelse(is.na(map_chr(parsed, "name")), tab$fluoropolymer, map_chr(parsed, "name")),
    abbreviation = map_chr(parsed, "abbrev"),
    cas_number = NA_character_,
    source = "A.126",
    source_text = paste0("A.126 row: ", tab$fluoropolymer)
  )
}

add_table(extract_a119())
add_table(extract_a126())
