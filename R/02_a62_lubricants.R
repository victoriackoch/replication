# A.62 (lubricant applications) uses a different "Examples of PFAS" style
# than the other long-form tables: names are space-separated (not comma-
# separated), qualifiers like "(oils and greases)" are folded onto the
# preceding acronym, and one row (a table used in Watchmaking/Medical
# devices/Drug delivery) folds in the non-PFAS resins PA66, HDPE, PE, POM
# under a "Resins:" sub-label. There are only 39 distinct "Examples of PFAS"
# strings across the table's 123 data rows (one is a two-row PDF line-wrap:
# "...Fluorocarbon gel (not further" / "specified)"), so rather than a
# general regex splitter -- which would either mis-split "PTFE (micropowder)
# PFA, ETFE, FEP" or wrongly emit PA66/HDPE/PE/POM as PFAS -- each distinct
# string is mapped by hand to its substance list below. Function/physical-
# form-only fragments ("Not specified", "assumed fluoropolymers", "Mixture
# of PTFE and solvent") are dropped; vague-but-named PFAS classes
# ("Fluoropolymer (not further specified)", "Side-chain fluorinated
# polymer(s)", "Fluorine grease (not further specified)") are kept.

a62_lookup <- tribble(
  ~text, ~subs,
  "Polymeric PFAS: PFPE PTFE PCTFE", "PFPE;PTFE;PCTFE",
  "Polymeric PFAS: PFPE PTFE", "PFPE;PTFE",
  "Polymeric PFAS: PFPE", "PFPE",
  "Polymeric PFAS: PTFE (micropowder) PFA, ETFE, FEP", "PTFE;PFA;ETFE;FEP",
  "Polymeric PFAS: PFPE (grease) PTFE (micropowder) ETFE FEP", "PFPE;PTFE;ETFE;FEP",
  "Polymeric PFAS: PTFE (micropowder) PFPE", "PTFE;PFPE",
  "PTFE (wax or spray)", "PTFE",
  "Polymeric PFAS: PFPE PTFE Unspecified PFAS assumed fluoropolymers", "PFPE;PTFE",
  "Polymeric PFAS: PFPE PFPE Unspecified PFAS assumed fluoropolymers Non-polymeric: Side-chain fluorinated polymer (not further specified) Fluorinated gases: Solvents (not further specified)*",
    "PFPE;Side-chain fluorinated polymer (not further specified);Solvents (not further specified) [fluorinated gas]",
  "Polymeric PFAS: PTFE PFPE PCTFE", "PTFE;PFPE;PCTFE",
  "Polymeric PFAS: PFPE (grease)", "PFPE",
  "PCTFE (oil)", "PCTFE",
  "Polymeric PFAS: Fluoropolymer (not further specified)", "Fluoropolymer (not further specified)",
  "Polymeric PFAS: PCTFE", "PCTFE",
  "PCTFE", "PCTFE",
  "Polymeric PFAS: PTFE PFPE", "PTFE;PFPE",
  "Polymeric PFAS: PTFE", "PTFE",
  "Polymeric PFAS: PCTFE (oils and greases)", "PCTFE",
  "Polymeric PFAS: PCTFE (oils)", "PCTFE",
  "Polymeric PFAS: PCTFE (grease)", "PCTFE",
  "PTFE PFPE (grease)", "PTFE;PFPE",
  "Not specified", "",
  "Polymeric PFAS: PTFE Unspecified fluoropolymers assumed PTFE FEP PFPE", "PTFE;FEP;PFPE",
  "Polymeric PFAS: Unspecified assumed fluoropolymers Other types of fluoropolymer (siloxane-based)", "Other types of fluoropolymer (siloxane-based)",
  "Polymeric PFAS: PTFE PFPE Fluorinated gases: Mixture of PTFE and solvent", "PTFE;PFPE",
  "Polymeric PFAS: PFPE (oil)", "PFPE",
  "Polymeric PFAS: PFPE (oil and grease)", "PFPE",
  "Polymeric PFAS: PFPE (oil) PCTFE", "PFPE;PCTFE",
  "Polymeric PFAS: PCTFE (oil)", "PCTFE",
  "Polymeric PFAS: PTFE Unspecified PFAS", "PTFE",
  "assumed fluoropolymers PFA (Perfluoroalkoxy alkane) PVDF ETFE PA66 HDPE Resins: PE POM", "PFA (Perfluoroalkoxy alkane);PVDF;ETFE",
  "Fluorine grease (not further specified)", "Fluorine grease (not further specified)",
  "Polymeric PFAS: PCTFE (grease/wax)", "PCTFE",
  "Polymeric PFAS: PTFE (powder) Other fluoropolymers (Confidential substances) Non-polymeric PFAS: Side-chain fluorinated polymers Fluorinated gases: Solvents used as carrier fluids",
    "PTFE;Other fluoropolymers (Confidential substances);Side-chain fluorinated polymers;Solvents used as carrier fluids [fluorinated gas]",
  "Polymeric PFAS: PTFE PFPE Unspecified PFAS assumed to be fluoropolymers PCTFE (oils and greases)", "PTFE;PFPE;PCTFE",
  "Polymeric PFAS: Unspecified PFAS assumed to be fluoropolymers Fluorocarbon gel (not further specified)", "Fluorocarbon gel (not further specified)",
  "Polymeric PFAS: Fluoropolymer (Not further specified)", "Fluoropolymer (Not further specified)",
  "Polymeric PFAS: PCTFE (oil and grease) PFPE", "PCTFE;PFPE"
)

extract_a62 <- function() {
  tab <- load_table("A.62")
  names(tab)[1:4] <- c("sector_subuse", "application", "tech_function", "examples")

  # fix the one PDF line-wrap: "...Fluorocarbon gel (not further" / "specified)"
  wrap_idx <- which(str_trim(coalesce(tab$examples, "")) == "specified)")
  if (length(wrap_idx) == 1 && wrap_idx > 1) {
    tab$examples[wrap_idx - 1] <- paste0(tab$examples[wrap_idx - 1], " ", tab$examples[wrap_idx])
    tab <- tab[-wrap_idx, ]
  }

  # page-spill: "...oxygen delivery systems and oxygen heating" / "systems"
  # (point 20) -- a bare one-word Application continuation with no
  # sector_subuse of its own
  spill_idx <- which(is.na(tab$sector_subuse) & str_trim(coalesce(tab$application, "")) == "systems")
  if (length(spill_idx) == 1 && spill_idx > 1) {
    tab$application[spill_idx - 1] <- paste(tab$application[spill_idx - 1], tab$application[spill_idx])
    tab <- tab[-spill_idx, ]
  }

  # strip trailing footnote-reference numbers (point 20), e.g. "Machinery93" -> "Machinery"
  tab$sector_subuse <- str_remove(tab$sector_subuse, "(?<=[A-Za-z])[0-9]{2,3}$")

  tab$sector_subuse <- ffill(tab$sector_subuse)
  product <- apply(tab[c("sector_subuse", "application")], 1, function(r) {
    r <- r[!is.na(r) & str_trim(r) != ""]
    if (length(r) == 0) NA_character_ else paste(r, collapse = " / ")
  })

  out <- map2(seq_along(product), product, function(i, p) {
    txt <- tab$examples[i]
    if (is.na(p) || is_no_info(txt)) return(NULL)
    hit <- a62_lookup$subs[a62_lookup$text == str_trim(txt)]
    if (length(hit) == 0) {
      warning("A.62: no manual mapping for text: ", txt)
      subs <- split_substance_list(txt)
    } else {
      subs <- str_split(hit, ";")[[1]]
      subs <- subs[subs != ""]
    }
    if (length(subs) == 0) return(NULL)
    make_row_raw(p, subs, source = "A.62", source_text = txt)
  })
  bind_rows(out)
}

add_table(extract_a62())
