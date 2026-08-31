# Helper functions for extracting PFAS product/substance/CAS rows from the
# ECHA Annex A background-document tables.
#
# Every sheet in the source workbook shares the same layout: row 1 = table
# title, row 2 = source/page reference, row 3 = "Back to TOC" link, row 4 =
# blank, row 5 = column header, row 6+ = data, followed (on many sheets) by a
# "Footnotes / notes as printed in the source:" marker row and then loose
# footnote text. read_excel(..., skip = 4) lands exactly on the header row,
# so col_names = TRUE picks it up automatically.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)

XLSX_PATH <- "data-raw/ECHA_BackgroundDocument_AnnexA_tables.xlsx"

# Read one sheet's data block (header + data rows), dropping the trailing
# footnote block if present.
load_table <- function(sheet) {
  raw <- read_excel(XLSX_PATH, sheet = sheet, skip = 4, col_names = TRUE,
                     .name_repair = "unique_quiet")
  first_col <- raw[[1]]
  marker <- which(str_detect(coalesce(as.character(first_col), ""),
                              regex("Footnotes / notes", ignore_case = TRUE)))
  if (length(marker) > 0) raw <- raw[seq_len(min(marker) - 1), , drop = FALSE]

  # Multi-page source tables occasionally keep a repeated header row mid-
  # table (the cleaning notes say these were dropped, but a few slipped
  # through, e.g. A.105, A.119) -- drop any row whose cells reproduce the
  # column headers themselves.
  header_vals <- str_trim(names(raw))
  is_header_dup <- apply(raw, 1, function(r) {
    r <- str_trim(as.character(r))
    nonblank <- !is.na(r) & r != ""
    if (sum(nonblank) < 2) return(FALSE)
    # compare positionally first; a shifted/merged repeated-header block can
    # land its cells under the wrong columns, so also check whether the
    # cells simply reproduce the *set* of header labels regardless of position
    positional <- mean(r[nonblank] == header_vals[nonblank], na.rm = TRUE) > 0.5
    any_position <- mean(r[nonblank] %in% header_vals, na.rm = TRUE) > 0.5
    positional || any_position
  })
  raw[!is_header_dup, , drop = FALSE]
}

# Forward-fill a vector: repeat the last non-blank value down through
# following blank/NA entries. Used for merged "category" cells that only
# print the category name on their first row.
ffill <- function(x) {
  x <- as.character(x)
  is_blank <- is.na(x) | str_trim(x) == ""
  idx <- which(!is_blank)
  if (length(idx) == 0) return(x)
  out <- rep(NA_character_, length(x))
  for (k in seq_along(idx)) {
    from <- idx[k]
    to <- if (k < length(idx)) idx[k + 1] - 1 else length(x)
    out[from:to] <- x[from]
  }
  out
}

# One row of the final long table, when the source already gives a clean,
# separate substance name and abbreviation (e.g. A.100, A.104, A.105).
make_row <- function(product, substance_name, abbreviation = NA_character_,
                      cas_number = NA_character_, source, source_text,
                      substance_synonym = NA_character_, abbreviation_synonym = NA_character_,
                      substance_group = NA_character_) {
  tibble(
    product = product,
    substance_name = substance_name,
    substance_synonym = substance_synonym,
    abbreviation = abbreviation,
    abbreviation_synonym = abbreviation_synonym,
    substance_group = substance_group,
    cas_number = cas_number,
    source = source,
    source_text = source_text
  )
}

# Many tables (the free-text "Examples of PFAS" long-form tables, matrix
# tables, A.14, A.107, A.126...) give only a single token per substance --
# either a bare short acronym ("PTFE"), a full descriptive name with no
# acronym ("Side-chain fluorinated polymer (not further specified)"), or
# "ACRONYM (Full name)" / "Full name (ACRONYM)". This splits that single
# token into the clean substance_name/abbreviation pair used everywhere
# else, so all 29 source tables end up mapped onto the same two columns.
# A token counts as an "abbreviation" if it's short, has no spaces, and
# carries at least two uppercase letters (PTFE, PFHxA, LiTFSI, GenX) --
# this rules out ordinary lowercase qualifier words like "micropowder" or
# "confidential" that also happen to be short.
is_acronym_token <- function(x) {
  x <- str_trim(x)
  # ordinary abbreviation (PTFE, PFHxA, LiTFSI, GenX)
  plain <- str_detect(x, "^[A-Za-z0-9][A-Za-z0-9\\-]{1,9}$") && str_count(x, "[A-Z]") >= 2
  # chain-length-tagged abbreviation (6:2 FTOH, 8:2 FTS, 6:2-8:2 FTOH)
  chain_tagged <- str_detect(x, "^[0-9]+:[0-9]+(-[0-9]+:[0-9]+)?\\s+[A-Za-z][A-Za-z0-9]{1,9}$")
  plain || chain_tagged
}


# Acronyms of the fluoropolymer/fluoroelastomer/refrigerant family that
# recur throughout this workbook embedded inside an otherwise descriptive
# phrase rather than standing alone or wrapped in its own parentheses (e.g.
# "PTFE powder or wax", "PTFE added to polyamid", "High-MW PTFE") -- when a
# free-text item doesn't cleanly match either "ACRONYM (name)" pattern
# below but does contain one of these as a whole word, that word is still
# the substance's abbreviation, just with a qualifier attached to it.
KNOWN_EMBEDDED_ACRONYMS <- c(
  "PTFE", "PFPE", "FKM", "FEP", "PVDF", "ETFE", "FFKM", "FEPM", "FVMQ",
  "PCTFE", "ECTFE", "FEVE", "PFA", "PMVE"
)
extract_embedded_acronym <- function(name) {
  for (a in KNOWN_EMBEDDED_ACRONYMS) {
    if (str_detect(name, paste0("\\b", a, "\\b"))) return(a)
  }
  NA_character_
}

parse_name_abbrev <- function(token) {
  if (is.na(token)) return(list(name = NA_character_, abbrev = NA_character_))
  token <- str_trim(token)
  # "ACRONYM (longer descriptive text)" -- the parenthesised text may itself
  # contain nested parentheses (e.g. "FFKM (Perfluoro(Methyl Vinyl Ether)-
  # Tetrafluoroethylene Copolymer)"), so this matches from the first "(" to
  # the LAST ")" in the token rather than excluding "()" from the middle.
  m <- str_match(token, "^([A-Za-z0-9][A-Za-z0-9\\-]{1,9})\\s*\\((.{4,})\\)$")
  if (!is.na(m[1, 1]) && is_acronym_token(m[1, 2])) {
    return(list(name = str_trim(m[1, 3]), abbrev = m[1, 2]))
  }
  # "Longer descriptive text (ACRONYM)"
  m <- str_match(token, "^(.{4,}?)\\s*\\(([A-Za-z0-9][A-Za-z0-9\\-]{1,9})\\)$")
  if (!is.na(m[1, 1]) && is_acronym_token(m[1, 3])) {
    return(list(name = str_trim(m[1, 2]), abbrev = m[1, 3]))
  }
  if (is_acronym_token(token)) {
    return(list(name = NA_character_, abbrev = token))
  }
  list(name = token, abbrev = extract_embedded_acronym(token))
}

# Extract "...name... (CAS: 12345-67-8)" pairs from a free-text cell that
# lists several substances this way (A.70, A.90), using each CAS-number
# match itself as the delimiter (far more reliable here than ";"/"," -- the
# source mixes ";", "and", and bare spaces between entries inconsistently,
# and names themselves may contain their own parenthetical abbreviation,
# e.g. "PFHxA (Perfluoro-n-hexanoic acid) (CAS: 307-24-4)"). Any text after
# the last CAS match is kept as one further no-CAS entry if it still names
# something once "claimed as confidential" filler is stripped.
extract_name_cas_pairs <- function(text) {
  empty <- tibble(name = character(0), cas = character(0))
  if (is_no_info(text)) return(empty)
  # fix the occasional source typo of a missing opening paren, e.g.
  # "... methyl ether, CAS. 13171-18-1), methyl ..." (A.90)
  text <- str_replace_all(text, regex("(?<!\\()(CAS\\.?:?\\s*[0-9]{2,7}-[0-9]{2}-[0-9]\\))", ignore_case = TRUE), "(\\1")
  cas_re <- regex("\\(CAS\\.?:?\\s*([0-9]{2,7}-[0-9]{2}-[0-9])\\)", ignore_case = TRUE)
  m <- str_locate_all(text, cas_re)[[1]]
  if (nrow(m) == 0) return(empty)
  cas_vals <- str_match_all(text, cas_re)[[1]][, 2]
  starts <- c(1, m[, "end"] + 1)
  chunks <- map_chr(seq_len(nrow(m)), function(i) str_sub(text, starts[i], m[i, "start"] - 1))
  chunks <- str_trim(chunks)
  chunks <- str_remove(chunks, "^[;,]\\s*")
  chunks <- str_remove(chunks, regex("^and\\s+", ignore_case = TRUE))
  chunks <- str_remove_all(chunks, CONFIDENTIAL_RE)
  chunks <- str_trim(chunks)
  out <- tibble(name = chunks, cas = cas_vals)
  out <- out[out$name != "" & !vapply(out$name, is_no_info, logical(1)), ]

  trailing <- str_sub(text, max(m[, "end"]) + 1)
  trailing_subs <- split_substance_list(trailing)
  if (length(trailing_subs) > 0) {
    out <- bind_rows(out, tibble(name = trailing_subs, cas = NA_character_))
  }
  out
}

# One row of the final long table, from a single free-text substance token
# that still needs to be split into name/abbreviation via parse_name_abbrev().
make_row_raw <- function(product, raw_substance, cas_number = NA_character_,
                          source, source_text) {
  parsed <- map(raw_substance, parse_name_abbrev)
  tibble(
    product = product,
    substance_name = map_chr(parsed, "name"),
    substance_synonym = NA_character_,
    abbreviation = map_chr(parsed, "abbrev"),
    abbreviation_synonym = NA_character_,
    substance_group = NA_character_,
    cas_number = cas_number,
    source = source,
    source_text = source_text
  )
}

# Cell values that mean "nothing named here" -- should never become a row.
NO_INFO_RE <- regex(
  "^(no information( received)?\\.?|no data\\.?|data unavailable\\.?|not applicable\\.?|n/?a\\.?|unknown\\.?|-+)$",
  ignore_case = TRUE
)

is_no_info <- function(x) {
  if (is.na(x)) return(TRUE)
  x <- str_trim(x)
  x == "" || str_detect(x, NO_INFO_RE)
}

CLASS_PREFIX_RE <- regex(
  "(Polymeric PFAS[s]?\\s*:|Non[-\\s]polymeric PFAS[s]?\\s*:|Polymeric\\s*:|Non[-\\s]polymeric\\s*:|Fluorinated gas(es)?\\s*:)",
  ignore_case = TRUE
)

CONFIDENTIAL_RE <- regex(
  "(other )?substances? claimed as confidential\\.?",
  ignore_case = TRUE
)

# Split a free-text "Examples of PFAS(s)" cell into individual substance
# names, e.g. "Polymeric PFAS: PTFE, PFA, ETFE" -> c("PTFE","PFA","ETFE").
# Strips known class prefixes and "claimed as confidential" filler, then
# splits the remainder on ";" and ",". A handful of entries with internal
# commas in a proper chemical name (rare in these free-text list cells,
# which are mostly short acronyms) may split incorrectly -- flagged
# separately for spot-checking rather than guarded against here.
# A handful of "Examples of PFAS" cells run several class labels together
# with no delimiter at all between them ("...PFA, PTFE Copolymers for
# PVDF: VDF, TrFE..." -- no comma before "Copolymers"), which no generic
# splitter can recover. Overridden by exact text match rather than parsed.
SUBSTANCE_LIST_OVERRIDES <- list(
  "Polymeric PFAS: Fluoroelastomer, PVDF, PFA, PTFE Copolymers for PVDF: VDF, TrFE, TFE, CTFE, HFP Non-polymeric: LiTFSI, LiTFS" =
    c("Fluoroelastomer", "PVDF", "PFA", "PTFE", "VDF", "TrFE", "TFE", "CTFE", "HFP", "LiTFSI", "LiTFS"),
  # A.51 "Wires and cables: Connectors" -- a run of bare locant digits
  # ("1,1,2,2,3,3,4-...") genuinely belongs to different neighbouring
  # compound names depending on position (some extend the preceding
  # acronym, some start the next one), which no generic rule can tell
  # apart; hand-reconstructed instead.
  "Polymeric PFAS: PTFE, FVMQ (CAS 63148-56-1/ 68037-87-6), PVDF copolymer, PFPE etc. Non-polymeric PFAS: HFP, 1,1,2,2,3,3,4-hepta fluorocyclopentane, Tetraethylammonium heptadecafluorooctanesulphon ate, Tetraethylazanium nonafluorobutane-1-sulfonate, Propene, 1,3,3,3,-tetrafluoro- ,(E)-, 1,1,1,2- Tetrafluoroethane, 1,1,1,2,2,3,4,5,5,5-decafluoro- 3-methoxy-4- (trifluoromethyl)pentane, 1-ethoxynonafluorobutane etc." =
    # "FVMQ (CAS 63148-56-1/68037-87-6)" is kept as the bare acronym "FVMQ"
    # here rather than "FVMQ" with the CAS pair as its "name" (which is what
    # parse_name_abbrev's "ACRONYM (text)" pattern would otherwise do with
    # it) -- this table's Examples cells don't carry a per-substance CAS
    # through to the final cas_number column at all (see split_substance_list),
    # so leaving the CAS pair attached just produces a CAS-shaped fake name.
    c("PTFE", "FVMQ", "PVDF copolymer", "PFPE", "HFP",
      "1,1,2,2,3,3,4-heptafluorocyclopentane", "Tetraethylammonium heptadecafluorooctanesulphonate",
      "Tetraethylazanium nonafluorobutane-1-sulfonate", "1,3,3,3-tetrafluoropropene, (E)-",
      "1,1,1,2-Tetrafluoroethane", "1,1,1,2,2,3,4,5,5,5-decafluoro-3-methoxy-4-(trifluoromethyl)pentane",
      "1-ethoxynonafluorobutane"),
  # A.59 (construction) reuses an "X-, Y- and Z-based side-chain
  # fluorinated polymers" shared-suffix construction across several rows,
  # each with a different subset/ordering/typo -- same shape as point 9's
  # locant-prefix pattern but for adjectival prefixes, which the generic
  # splitter can't safely separate from an unrelated preceding item
  # ("Fluorosurfactant and acrylate-..." -- "Fluorosurfactant" is its own
  # substance, not one of the "-based" prefixes).
  "Polymeric PFAS: PTFE Non-polymeric PFAS: Fluorosurfactant and acrylate-, urethane-and siloxane-based side-chain fluorinated polymers (non-polymeric Precursors)" =
    c("PTFE", "Fluorosurfactant", "Acrylate-based side-chain fluorinated polymers (non-polymeric Precursors)",
      "Urethane-based side-chain fluorinated polymers (non-polymeric Precursors)",
      "Siloxane-based side-chain fluorinated polymers (non-polymeric Precursors)"),
  "Non-polymeric PFAS: Fluorosurfactants and acrylate-, urethane-and siloxane-based side-chain fluorinated polymers (non-polymeric precursors)" =
    c("Fluorosurfactants", "Acrylate-based side-chain fluorinated polymers (non-polymeric precursors)",
      "Urethane-based side-chain fluorinated polymers (non-polymeric precursors)",
      "Siloxane-based side-chain fluorinated polymers (non-polymeric precursors)"),
  "Polymeric PFAS: PTFE powder or wax. Non-polymeric PFAS: Acrylate-, urethane-and silane/siloxane based side-chain fluorinated polymers (non-polymeric precursors)" =
    c("PTFE powder or wax", "Acrylate-based side-chain fluorinated polymers (non-polymeric precursors)",
      "Urethane-based side-chain fluorinated polymers (non-polymeric precursors)",
      "Silane/siloxane-based side-chain fluorinated polymers (non-polymeric precursors)"),
  "Polymeric PFAS: PTFE Non-polymeric PFAS: Fluorosurfactant and acrylate-and urethane -based side-chain fluorinated polymers" =
    c("PTFE", "Fluorosurfactant", "Acrylate-based side-chain fluorinated polymers",
      "Urethane-based side-chain fluorinated polymers"),
  "Polymeric PFAS: PTFE, PVDF, ECTFE, FEVE, FEP, PFPE Non-polymeric PFAS:Acrylate-and silane/siloxane-based side-chain fluorinated polymers (non-polymeric precursors)" =
    c("PTFE", "PVDF", "ECTFE", "FEVE", "FEP", "PFPE",
      "Acrylate-based side-chain fluorinated polymers (non-polymeric precursors)",
      "Silane/siloxane-based side-chain fluorinated polymers (non-polymeric precursors)"),

  # A.59 "Fluoropolymers and mostly PTFE (90-95%)" -- one substance (PTFE,
  # with a purity note), not two ("Fluoropolymers" as a separate generic
  # class plus "PTFE (90-95%)").
  "Polymeric PFAS: Fluoropolymers and mostly PTFE (90-95%)" = c("PTFE"),

  # A.53 "Plasma [Dry] Etch"/"Chemical vapour deposition chamber"/"DRIE"
  # rows: PDF subscript extraction detached every chemical-formula
  # subscript digit from its element letter and appended them, in order,
  # as bare trailing numbers (sometimes split further across the visual
  # column layout) -- e.g. "PFC-318 (C F), PFC-14 4 8 (CF), C F , C F ,
  # C F . 4 2 6 3 8 5 8" is "PFC-318 (C4F8), PFC-14 (CF4), C2F6, C3F8,
  # C5F8" with all 9 subscript digits stripped out and moved to the end
  # (in the same left-to-right order they belong in). Reassembled here
  # using each formula's own well-known identity (these are all standard
  # semiconductor-fab PFC/HFC process gases) as cross-check.
  "Fluorinated gases: PFC, HFC and HFO gases e.g. HFC-23, HFC-134a, PFC-318 (C F), PFC-14 4 8 (CF), C F , C F , C F . 4 2 6 3 8 5 8" =
    c("HFC-23", "HFC-134a", "PFC-318 (C4F8)", "PFC-14 (CF4)", "C2F6", "C3F8", "C5F8"),
  "Fluorinated gases: PFC, HFC and HFO gases e.g. CF , 4 C F , C F , C F , C F , CHF , CH F 2 6 3 8 4 8 5 8 3 3" =
    c("CF4", "C2F6", "C3F8", "C4F8", "C5F8", "CHF3", "CH3F"),
  "Fluorinated gases: CF , C F , C F 4 3 8 4 8" = c("CF4", "C3F8", "C4F8"),

  # A.53 "Thermal Testing"/"Advanced semiconductor packaging" heat-transfer
  # rows: "PFPMIE and other fully fluorinated liquids (perfluorinated
  # amines and perfluoroalkylmorpholines, Reaction mass of X and Y" names
  # PFPMIE, then introduces a descriptive class ("other fully fluorinated
  # liquids... amines and morpholines") whose one given example is a
  # single reaction-mass mixture of two named amines -- kept as one
  # substance, not shredded on the "and" inside its own name -- plus
  # "Hydrofluoroethers" as a separate closing item (the source's
  # unbalanced "(" is never closed, an extraction artifact, not content).
  "Polymeric PFAS: perfluoropolyethers Non-polymeric PFAS: PFPMIE and other fully fluorinated liquids (perfluorinated amines and perfluoroalkylmorpholines, Reaction mass of 1,1,2,2,3,3,4,4,4-nonafluoro- N,N-bis(nonafluorobutyl)butan-1-amine and 1,1,2,2,3,3,4,4,4-nonafluoro-N-[1,1,2,3,3-hexafluoro-2- (trifluoromethyl)propyl]-N- (1,1,2,2,3,3,4,4,4-nonafluorobutyl)butan-1-amine Fluorinated gases: Hydrofluoroethers" =
    c("Perfluoropolyethers", "PFPMIE",
      "Reaction mass of 1,1,2,2,3,3,4,4,4-nonafluoro-N,N-bis(nonafluorobutyl)butan-1-amine and 1,1,2,2,3,3,4,4,4-nonafluoro-N-[1,1,2,3,3-hexafluoro-2-(trifluoromethyl)propyl]-N-(1,1,2,2,3,3,4,4,4-nonafluorobutyl)butan-1-amine",
      "Hydrofluoroethers"),

  # A.53 Photolithography "Dielectric fluorinated polymers (PBO/PI)" rows:
  # a simple comma list (3 short items) followed by one long IUPAC name
  # built as "A polymer with B and C" containing its own internal commas
  # (a stereodescriptor list) and an "and" that is part of the compound
  # name, not a list separator -- kept as one item.
  "Non-polymeric PFAS: Side-chain fluorinated polymers (Water-insoluble C1 PFAS polymers), Bisphenol AF, fluorinated polyimide, 4,4'-Oxybisbenzoic acid polymer with rel-(3aR,4S,7R,7aS)-3a,4,7,7a-tetrahydro-4,7-methanoisobenzofuran-1,3-dione and 4,4'-[2,2,2-trifluoro-1- (trifluoromethyl)ethylidene]bis[2-aminophenol]" =
    c("Side-chain fluorinated polymers (Water-insoluble C1 PFAS polymers)", "Bisphenol AF", "Fluorinated polyimide",
      "4,4'-Oxybisbenzoic acid polymer with rel-(3aR,4S,7R,7aS)-3a,4,7,7a-tetrahydro-4,7-methanoisobenzofuran-1,3-dione and 4,4'-[2,2,2-trifluoro-1-(trifluoromethyl)ethylidene]bis[2-aminophenol]"),
  # same row repeated later in the sheet, missing the "fluorinated
  # polyimide" item.
  "Non-polymeric PFAS: Side-chain fluorinated polymers (Water-insoluble C1 PFAS polymers), Bisphenol AF, 4,4'-Oxybisbenzoic acid polymer with rel-(3aR,4S,7R,7aS)-3a,4,7,7a-tetrahydro-4,7-methanoisobenzofuran-1,3-dione and 4,4'-[2,2,2-trifluoro-1- (trifluoromethyl)ethylidene]bis[2-aminophenol]" =
    c("Side-chain fluorinated polymers (Water-insoluble C1 PFAS polymers)", "Bisphenol AF",
      "4,4'-Oxybisbenzoic acid polymer with rel-(3aR,4S,7R,7aS)-3a,4,7,7a-tetrahydro-4,7-methanoisobenzofuran-1,3-dione and 4,4'-[2,2,2-trifluoro-1-(trifluoromethyl)ethylidene]bis[2-aminophenol]"),

  # A.53 Advanced Semiconductor Packaging "Flux": "Likely non-polymeric
  # PFAS: Surfactants" -- "Likely" isn't matched by CLASS_PREFIX_RE (it
  # only recognises the class label starting the segment), so the whole
  # phrase would otherwise survive as one ungainly item.
  "Likely non-polymeric PFAS: Surfactants" = c("Non-polymeric surfactants"),

  # A.51 "Liquid crystal displays (LCD) - Inter layer": "Non-polymeric
  # PFASs, PFHxA, fluorinated polyimide." has no colon after "PFASs" (the
  # form CLASS_PREFIX_RE requires), so "Non-polymeric PFASs" itself would
  # otherwise survive as a bogus first list item instead of being read as
  # the (colon-less) class label.
  "Non-polymeric PFASs, PFHxA, fluorinated polyimide." = c("PFHxA", "fluorinated polyimide"),

  # A.51 "Piezzoelectric devices": "PVDF and co-polymers, PFPE" -- the "and"
  # inside "PVDF and co-polymers" is part of that one item's own name, not
  # a list separator, but replace_last_and()'s generic rule (replace the
  # LAST " and " with a comma) would still catch it here since it's the
  # only " and " in the string, splitting it into three items instead of
  # two ("PVDF", "co-polymers", "PFPE").
  "Polymeric PFAS: PVDF and co-polymers, PFPE" = c("PVDF and co-polymers", "PFPE"),

  # A.51 "Plastic additives: Anti-drip agent / flame retardant additive in
  # plastics": one copolymer IUPAC name built as "A, B, polymer with C and
  # D" (own internal commas/locants) followed by two distinct sulfonate
  # substances (the potassium salt and its parent acid) run together with
  # no delimiter between the salt's own unclosed name and "PFBS".
  "Polymeric PFAS: PTFE, 1-Propene, 1,1,2,3,3,3-hexafluoro-, polymer with 1,1-difluoroethene and tetrafluoroethene Non-polymeric PFAS: K-PFBS (Potassium 1,1,2,2,3,3,4,4,4-nonafluorobutane-1-sulphonate, PFBS" =
    c("PTFE", "1-Propene, 1,1,2,3,3,3-hexafluoro-, polymer with 1,1-difluoroethene and tetrafluoroethene",
      "K-PFBS (Potassium 1,1,2,2,3,3,4,4,4-nonafluorobutane-1-sulphonate)", "PFBS"),

  # A.51 "transfer fluid: immersion cooling" -- a long run of full IUPAC
  # names, each individually shot through with its own internal locant
  # commas and mid-word line-wraps ("tetrahyd rofuran", "butan- 1-amine"),
  # immediately followed by a run of "TradeName; CAS No. X" pairs and
  # closed off by one more name with a trailing "(CAS number X)" -- far too
  # irregular (multiple distinct compounds each individually shredded by
  # the same locant-comma ambiguity split_substance_list can't resolve
  # between separate names) for the generic splitter; hand-split instead.
  "Polymeric PFAS: PFPE Fluorinated gases: HFO and HFE. HFO e.g. (Z)-1,1,1,4,4,4-Hexafluoro-2-buten, HFE e.g. Butane, 1-ethoxy-1,1,2,2,3,3,4,4,4-nonafluoro-, 2,3,3,4,4-pentafluoro-5-methoxy-2,5-bis[1,2,2,2-tetrafluoro-1- (trifluoromethyl)ethyl]tetrahyd rofuran, 1,1,1,2,2,4,5,5,5-nonafluoro- 4-(trifluoromethyl)-3-pentanone, 2-(Trifluoromethyl)-3-ethoxydodecafluorohexane, HFE-7100; CAS No. 163702-08-7, HFE-7200; CAS No. 163702-05-4, HFE-7300; CAS No. 132182-92-4, HFE-7500; CAS No. 297730-93-9, HFE- 356mec; CAS No. 382-34-3 Non-polymeric PFAS: Perfluamine, Reaction mass of 1,1,2,2,3,3,4,4,4-nonafluoro- N,N-bis(nonafluorobutyl)butan- 1-amine and 1,1,2,2,3,3,4,4,4-nonafluoro-N-[1,1,2,3,3-hexafluoro-2- (trifluoromethyl)propyl]-N- (1,1,2,2,3,3,4,4,4-nonafluorobutyl)butan-1-amine, nonafluoro-2-trifluoromethyl-3-pentanone (CAS number 756-13-8)" =
    c("PFPE", "(Z)-1,1,1,4,4,4-Hexafluoro-2-butene",
      "Butane, 1-ethoxy-1,1,2,2,3,3,4,4,4-nonafluoro-",
      "2,3,3,4,4-pentafluoro-5-methoxy-2,5-bis[1,2,2,2-tetrafluoro-1-(trifluoromethyl)ethyl]tetrahydrofuran",
      "1,1,1,2,2,4,5,5,5-nonafluoro-4-(trifluoromethyl)-3-pentanone",
      "2-(Trifluoromethyl)-3-ethoxydodecafluorohexane",
      "HFE-7100", "HFE-7200", "HFE-7300", "HFE-7500", "HFE-356mec",
      "Perfluamine",
      "Reaction mass of 1,1,2,2,3,3,4,4,4-nonafluoro-N,N-bis(nonafluorobutyl)butan-1-amine and 1,1,2,2,3,3,4,4,4-nonafluoro-N-[1,1,2,3,3-hexafluoro-2-(trifluoromethyl)propyl]-N-(1,1,2,2,3,3,4,4,4-nonafluorobutyl)butan-1-amine",
      "Nonafluoro-2-trifluoromethyl-3-pentanone")
)

# Replaces the LAST " and " in text with a comma (English list convention:
# "A, B, C and D"), so the final item splits out like the others. Skipped
# if there's no " and " to replace.
replace_last_and <- function(text) {
  positions <- str_locate_all(text, regex("\\band\\b", ignore_case = TRUE))[[1]]
  if (nrow(positions) == 0) return(text)
  last <- positions[nrow(positions), ]
  paste0(str_sub(text, 1, last[1] - 1), ",", str_sub(text, last[2] + 1))
}

split_substance_list <- function(text) {
  if (is_no_info(text)) return(character(0))
  for (key in names(SUBSTANCE_LIST_OVERRIDES)) {
    if (str_trim(text) == key) return(SUBSTANCE_LIST_OVERRIDES[[key]])
  }
  cleaned <- str_remove_all(text, CONFIDENTIAL_RE)
  # a handful of known PDF-extraction typos/artifacts, fixed by literal
  # substring replacement rather than a general rule (too narrow to
  # generalize safely): a stray space splitting "PFPE" in two, and a
  # footnote-reference number ("72") fused onto a real compound code
  # ("HCFO-1233zd72") with no delimiter of its own.
  cleaned <- str_replace_all(cleaned, "\\bP FPE\\b", "PFPE")
  cleaned <- str_replace_all(cleaned, fixed("HCFO-1233zd72"), "HCFO-1233zd")
  # this table's Examples cells never carry a genuine per-substance CAS
  # number through to the final table (see file header), so a CAS clause
  # embedded in the list text -- "Name; CAS No. 123-45-6" or "Name (CAS
  # number 123-45-6)" -- is dropped here rather than left to become a
  # meaningless standalone "substance" of its own once split on the comma/
  # semicolon that (mis)delimits it from its neighbours.
  cleaned <- str_remove_all(cleaned, regex(
    "[;,]?\\s*\\(?CAS\\.?\\s*(No\\.?|number)?\\s*:?\\s*[0-9]{2,7}-[0-9]{2}-[0-9]\\)?",
    ignore_case = TRUE
  ))
  # "<class name> such as X, Y" / "<class name> e.g. X, Y" names specific
  # members of a class just before introducing them -- once the specific
  # members are listed, the generic class name is redundant and is dropped
  # entirely (along with the "such as"/"e.g." connector itself), leaving
  # just the named members to split out on their own, e.g. "Fluoroelastomers
  # such as FKM, FEPM, FFKM and FVMQ" yields FKM/FEPM/FFKM/FVMQ, not also a
  # bare "Fluoroelastomers" line.
  cleaned <- str_replace_all(
    cleaned,
    regex("\\b(\\w[\\w'/-]*(\\s+\\w[\\w'/-]*){0,3})\\s+(such as|e\\.g\\.,?)\\s*", ignore_case = TRUE),
    ""
  )
  # replace (rather than delete) each class prefix with a delimiter, so the
  # boundary between class-tagged segments also becomes a split point
  cleaned <- str_replace_all(cleaned, CLASS_PREFIX_RE, ";")

  # Split on ";" first (top level, parens-respecting) -- this is the ONLY
  # boundary a bare-acronym segment gets re-merged across (see below), so a
  # comma-separated list like "Fluoroelastomer, PVDF, PFA, PTFE" is never
  # affected by that merge (only semicolon-adjacent segments are).
  segments <- split_respecting_parens(cleaned, delims = ";")
  keep <- rep(TRUE, length(segments))
  for (i in seq_along(segments)) {
    # Occasionally a "Full descriptive name; ABBREV" pair for one substance
    # gets split apart by the ";" (e.g. "Lithium Bis(trifluoromethanesulfonyl)
    # imide; LITFSI") -- if this WHOLE segment is nothing but a bare
    # acronym, it's that abbreviation, not a new list item; fold it back
    # into the preceding segment as "Name (ABBR)". But only when the
    # preceding segment is itself a single name (no internal comma) -- a
    # multi-item comma list ("PTFE, ETFE, FEP, PVDF") followed by a lone
    # acronym segment ("PBSF") means the acronym is the next class's own
    # (single-item) list, not an abbreviation of the list's last member.
    if (i > 1 && keep[i - 1] && is_acronym_token(segments[i]) && !str_detect(segments[i - 1], ",")) {
      segments[i - 1] <- paste0(segments[i - 1], " (", segments[i], ")")
      keep[i] <- FALSE
    }
  }
  segments <- segments[keep]

  # within each segment, replace a trailing " and " with a comma, then
  # split on commas (parens-respecting) to get the individual items
  parts <- unlist(lapply(segments, function(seg) split_respecting_parens(replace_last_and(seg), delims = ",")))
  parts <- parts[parts != "" & !vapply(parts, is_no_info, logical(1))]
  # A complex IUPAC name with a stereodescriptor list, e.g.
  # "rel-(3aR,4S,7R,7aS)-3a,4,7,7a-tetrahydro-...", commonly appears among
  # these acronym lists and gets shredded by the "," split above into bare
  # locant fragments ("1", "3a", "4S", "7R", "N"...). Re-merge any fragment
  # that is just a locant/stereo-descriptor token back onto the growing
  # name before it, so these collapse back into one (still imperfectly
  # reconstructed, but no longer emitted as meaningless standalone
  # "substances") rather than the acronym/name list next to them.
  locant_re <- regex("^\\(?[0-9]+[A-Za-z']{0,3}\\)?-?$|^[A-Za-z]$")
  keep2 <- rep(TRUE, length(parts))
  anchor <- 1
  for (i in seq_along(parts)) {
    if (i > 1 && str_detect(parts[i], locant_re)) {
      # keep growing the same anchor across a whole RUN of locant
      # fragments ("1,1,2,2,3,3,4-heptafluoro-..."), not just the first one
      parts[anchor] <- paste0(parts[anchor], ",", parts[i])
      keep2[i] <- FALSE
    } else {
      anchor <- i
    }
  }
  parts <- parts[keep2]
  # the "such as"/"e.g." class-name deletion above can leave a stray
  # sentence-terminator (a period, from "...HFE. HFO e.g. X") stuck to the
  # front of the following item once split; trim it off.
  parts <- str_trim(str_remove(parts, "^[.,;:]+\\s*"))
  parts <- parts[parts != ""]
  unique(parts)
}

# Splits text on delimiter characters (default ",;"), but ignores any
# delimiter that falls inside parentheses -- so "Lithium-ion batteries
# (Prismatic, cylindrical and pouch) (secondary)" splits as one item, not
# three, and CAS field "56357-87-0 (ethene, fluoro, polymer mixture)"
# stays one CAS with a descriptive note, not three fake CAS numbers. Used
# everywhere a comma/semicolon list needs splitting in this workbook,
# since parenthesised commas turn out to be common throughout.
split_respecting_parens <- function(text, delims = ",;") {
  if (is.na(text)) return(character(0))
  chars <- str_split(text, "")[[1]]
  depth <- 0
  out <- character(0)
  buf <- character(0)
  for (ch in chars) {
    if (ch == "(") depth <- depth + 1
    if (ch == ")") depth <- max(0, depth - 1)
    if (depth == 0 && ch %in% str_split(delims, "")[[1]]) {
      out <- c(out, paste(buf, collapse = ""))
      buf <- character(0)
    } else {
      buf <- c(buf, ch)
    }
  }
  out <- c(out, paste(buf, collapse = ""))
  parts <- str_trim(out)
  parts[parts != ""]
}

all_tables <- list()
add_table <- function(df) {
  all_tables[[length(all_tables) + 1]] <<- df
  invisible(df)
}
