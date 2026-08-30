# Advanced substance-name parsers: polymer/copolymer composition,
# "A or B" synonym / multi-CAS splitting, and abbreviation-synonym
# splitting. Requires 00_helpers.R to already be sourced.

POLYMER_ROWS <- list()
add_polymer_row <- function(df) POLYMER_ROWS[[length(POLYMER_ROWS) + 1]] <<- df

# Rows (from A.105 and A.106) whose Function/Regulatory-Listing text cites
# the substance as an EU Reg. 10/2011 PPA (processing aid) and/or monomer
# listing -- pulled into their own table (points 10-11).
PPA_MONOMER_RE <- regex(
  "PPA (in|listed in)|Monomer(s)? (in|listed in)|Monomer and emulsifier \\(PPA\\)",
  ignore_case = TRUE
)
PPA_MONOMER_ROWS <- list()
add_ppa_row <- function(df) PPA_MONOMER_ROWS[[length(PPA_MONOMER_ROWS) + 1]] <<- df

split_monomers <- function(text) {
  text <- str_trim(text)
  text <- str_remove(text, "^:\\s*")
  marked <- str_replace_all(text, regex(",?\\s*\\band-?\\s*", ignore_case = TRUE), "@@M@@")
  marked <- str_replace_all(marked, ",", "@@M@@")
  parts <- str_split(marked, fixed("@@M@@"))[[1]]
  parts <- str_trim(parts)
  parts[parts != ""]
}

clean_cas_field <- function(cas) {
  if (is.na(cas)) return(cas)
  str_trim(str_remove(cas, regex("^e\\.?g\\.?\\s*", ignore_case = TRUE)))
}

# Detect "X; a polymer of: Y[, and Z]" / "X; is a copolymer of: Y[, and Z]"
# (prefix form) or a bare "A copolymer of X and Y ... [long description]"
# (no separate short name given -- the main table keeps the original text
# since there's nothing shorter to use). Returns list(main_name, is_match,
# type, monomers) -- main_name is what the MAIN table should use.
parse_polymer_of <- function(full_text, cas, source_table) {
  if (is.na(full_text)) return(list(main_name = full_text, matched = FALSE))
  cas <- clean_cas_field(cas)

  # prefix form: "SubstanceName; a copolymer of: monomer list"
  m <- str_match(full_text, regex(
    "^(.+?);\\s*(?:is\\s+)?a\\s+(polymer|copolymer)\\s+of:?\\s*(.+)$", ignore_case = TRUE))
  if (!is.na(m[1, 1])) {
    main_name <- str_trim(m[1, 2])
    type <- tolower(m[1, 3])
    monomers <- split_monomers(m[1, 4])
    add_polymer_row(tibble(full_text = full_text, substance_name = main_name,
                            cas_number = cas, type = type,
                            monomer = monomers, monomer_n = seq_along(monomers),
                            source = source_table))
    return(list(main_name = main_name, matched = TRUE))
  }

  # bare form: "A copolymer of X and Y ...(modified with/cured .../etc.)"
  m <- str_match(full_text, regex(
    "^A\\s+(polymer|copolymer)\\s+of\\s+(.+?)(?:,?\\s+(?:cured|modified|followed by curing|manufactured and characteri[sz]ed)\\b.*)?$",
    ignore_case = TRUE))
  if (!is.na(m[1, 1])) {
    type <- tolower(m[1, 2])
    monomers <- split_monomers(m[1, 3])
    add_polymer_row(tibble(full_text = full_text, substance_name = full_text,
                            cas_number = cas, type = type,
                            monomer = monomers, monomer_n = seq_along(monomers),
                            source = source_table))
    return(list(main_name = full_text, matched = TRUE))
  }

  # "produced by (co)polymerizing/terpolymerizing X, Y, and Z ..." (no clean
  # short name available either)
  m <- str_match(full_text, regex(
    "produced by (?:ter)?(?:co)?polymerizing\\s+(.+?)(?:,?\\s+(?:and subsequent curing|followed by curing|cured)\\b.*)?$",
    ignore_case = TRUE))
  if (!is.na(m[1, 1])) {
    type <- if (str_detect(full_text, regex("terpolymeriz", ignore_case = TRUE))) "terpolymer" else "copolymer"
    monomers <- split_monomers(m[1, 2])
    add_polymer_row(tibble(full_text = full_text, substance_name = full_text,
                            cas_number = cas, type = type,
                            monomer = monomers, monomer_n = seq_along(monomers),
                            source = source_table))
    return(list(main_name = full_text, matched = TRUE))
  }

  list(main_name = full_text, matched = FALSE)
}

# "X or Y" / "X; or Y" -- treated as synonyms of ONE substance when exactly
# one CAS is given (first kept as substance_name, second moved to
# substance_synonym), or as TWO SEPARATE substances positionally mapped to
# two CAS numbers when more than one CAS is given (point 8 correction).
# Guards against long descriptive sentences (curing-agent "X or Y" choices)
# via a keyword denylist and a length cap on each side.
# "P1, P2, P3 or Pn SUFFIX" -- a shared descriptive suffix written once,
# attached only to the last of several short locant-style prefixes (e.g.
# "8:2, 10:2, 12:2, 14:2 or 16:2 fluorotelomer alcohol (FTOH) and mono-
# phosphate or di-phosphate", or "6:2-8:2 or 8:2-8:2-di polyfluoroalkyl
# phosphate ester (PAP)"). Splits into one substance per prefix, each with
# the full suffix appended. Returns NULL if the text doesn't match this
# shape (prefixes must be short digit/colon/hyphen locant codes).
LOCANT_TOKEN_RE <- regex("^[0-9]+(:[0-9]+)?(-[0-9]+(:[0-9]+)?)*(-di)?$")

split_shared_suffix_prefix_list <- function(name) {
  if (is.na(name)) return(NULL)
  m <- str_match(name, regex("^(.+?)\\s+or\\s+(.+)$"))
  if (is.na(m[1, 1])) return(NULL)
  left <- m[1, 2]
  right <- m[1, 3]

  left_prefixes <- split_respecting_parens(left)
  if (!all(str_detect(left_prefixes, LOCANT_TOKEN_RE))) return(NULL)

  rm <- str_match(right, regex(paste0("^([0-9]+(?::[0-9]+)?(?:-[0-9]+(?::[0-9]+)?)*(?:-di)?)\\s+(.+)$")))
  if (is.na(rm[1, 1])) return(NULL)
  last_prefix <- rm[1, 2]
  suffix <- rm[1, 3]

  all_prefixes <- c(left_prefixes, last_prefix)
  paste(all_prefixes, suffix)
}

# For a "<chain-prefix> <descriptive suffix (ABBR) ...>" name produced by
# split_shared_suffix_prefix_list(), pull out "<chain-prefix> <ABBR>" as
# the abbreviation (e.g. "10:2 fluorotelomer alcohol (FTOH) and mono-
# phosphate or di-phosphate" -> "10:2 FTOH"), since each chain length is a
# distinct substance and a bare "FTOH" would collide across all of them.
extract_chain_abbrev <- function(name) {
  prefix <- str_extract(name, "^[0-9]+(:[0-9]+)?(-[0-9]+(:[0-9]+)?)*(-di)?")
  abbr <- str_match(name, "\\(([A-Za-z][A-Za-z0-9]{1,9})\\)")[, 2]
  if (is.na(prefix) || is.na(abbr)) return(NA_character_)
  paste(prefix, abbr)
}

OR_SENTENCE_GUARD_RE <- regex(
  "produced by|copolymerizing|terpolymerizing|\\bcured\\b|\\bcuring\\b|subsequent|described in the notification",
  ignore_case = TRUE
)

split_or_synonyms <- function(name, cas) {
  empty_syn <- tibble(substance_name = name, substance_synonym = NA_character_, cas_number = cas)
  if (is.na(name) || str_detect(name, OR_SENTENCE_GUARD_RE)) return(empty_syn)

  m <- str_match(name, regex("^(.{3,220}?);?\\s+or\\s+(.{3,220})$", ignore_case = TRUE))
  if (is.na(m[1, 1])) return(empty_syn)
  x <- str_trim(m[1, 2])
  y <- str_trim(m[1, 3])

  cas_list <- if (is.na(cas)) character(0) else split_respecting_parens(cas)
  cas_list <- cas_list[!vapply(cas_list, is_no_info, logical(1))]

  if (length(cas_list) >= 2) {
    return(tibble(substance_name = c(x, y), substance_synonym = NA_character_,
                  cas_number = c(cas_list[1], cas_list[2])))
  }
  tibble(substance_name = x, substance_synonym = y, cas_number = cas)
}

# Comma-separated abbreviation list ("GenX, HFPO-DA, FRD-903") -> first
# kept as abbreviation, rest joined into abbreviation_synonym.
split_abbrev_synonym <- function(abbrev) {
  if (is.na(abbrev) || is_no_info(abbrev)) return(list(abbrev = NA_character_, synonym = NA_character_))
  abbrev <- str_remove(abbrev, regex("^e\\.?g\\.?\\s*", ignore_case = TRUE))
  m <- str_match(abbrev, "^\\((.+)\\)$")
  if (!is.na(m[1, 1])) abbrev <- m[1, 2]
  parts <- split_respecting_parens(abbrev)
  if (length(parts) <= 1) return(list(abbrev = str_trim(abbrev), synonym = NA_character_))
  list(abbrev = parts[1], synonym = paste(parts[-1], collapse = ", "))
}

# Full per-row pipeline for the "clean" Name/Abbreviation/CAS tables: runs
# polymer-of truncation, then or-synonym/multi-CAS splitting, then
# abbreviation-synonym splitting, on one (name, abbrev, cas) triple. Returns
# a tibble with one row per resulting substance (usually 1, sometimes 2 for
# the multi-CAS "or" case).
# Point 5: one confirmed instance of a "Group name: Example1 Example2
# Example3" cell with parallel "e.g. AB1 AB2 AB3" abbreviations and
# "e.g. CAS1,CAS2,CAS3" CAS lists (A.105/A.106, Perfluoroalkyl vinyl
# ethers). No reliable general delimiter exists in the space-separated
# example text to detect this shape automatically elsewhere in the
# workbook (checked: this is the only occurrence), so it's matched by
# exact text and hand-split rather than parsed.
GROUP_EXAMPLE_OVERRIDES <- list(
  list(
    name_match = "Perfluoroalkyl vinyl ethers: Perfluoromethyl vinyl ether Perfluoroethyl vinyl ether Perfluoropropyl vinyl ether",
    group = "Perfluoroalkyl vinyl ethers",
    examples = c("Perfluoromethyl vinyl ether", "Perfluoroethyl vinyl ether", "Perfluoropropyl vinyl ether"),
    abbrevs = c("PFMVE", "PFEVE", "PFPVE"),
    cas = c("1187-93-5", "10493-43-3", "1623-05-8")
  )
)

process_substance_triple <- function(name, abbrev, cas, source_table) {
  cas <- clean_cas_field(cas)

  for (ov in GROUP_EXAMPLE_OVERRIDES) {
    if (!is.na(name) && str_trim(name) == ov$name_match) {
      return(tibble(substance_name = ov$examples, substance_synonym = NA_character_,
                    substance_group = ov$group, abbreviation = ov$abbrevs,
                    abbreviation_synonym = NA_character_, cas_number = ov$cas))
    }
  }

  poly <- parse_polymer_of(name, cas, source_table)
  # For a matched polymer-of entry, a multi-value CAS field is [substance's
  # own CAS, monomer 1 CAS, monomer 2 CAS, ...] -- the full blob is right
  # for the polymer table (already recorded by parse_polymer_of above), but
  # the MAIN table's substance row should carry only the substance's own
  # (first) CAS, not the monomers' CAS numbers mixed in.
  main_cas <- if (poly$matched && !is.na(cas)) {
    parts <- split_respecting_parens(cas)
    if (length(parts) > 0) parts[1] else cas
  } else cas

  shared_suffix <- split_shared_suffix_prefix_list(poly$main_name)
  if (!is.null(shared_suffix)) {
    result <- tibble(substance_name = shared_suffix, substance_synonym = NA_character_,
                      substance_group = NA_character_, cas_number = main_cas,
                      abbreviation = map_chr(shared_suffix, extract_chain_abbrev))
    result$abbreviation_synonym <- NA_character_
    return(result)
  } else {
    result <- split_or_synonyms(poly$main_name, main_cas)
    result$substance_group <- NA_character_
    # Point 6: a plain class/group name (no "a polymer of", no "or" split)
    # with multiple CAS numbers -- one row per CAS, substance_group set to
    # the shared class name. Skipped for polymer-of matches, where a
    # multi-value CAS field is [substance CAS, monomer CAS, ...], not
    # several alternate CAS for the same substance.
    if (!poly$matched && nrow(result) == 1 && !is.na(cas)) {
      cas_list <- split_respecting_parens(cas)
      cas_list <- cas_list[!vapply(cas_list, is_no_info, logical(1))]
      # only expand if every piece actually looks like a CAS number --
      # guards against e.g. "56357-87-0 (ethene, fluoro, polymer mixture)"
      # (one real CAS with a parenthetical note) being mistaken for several
      if (length(cas_list) >= 2 && all(str_detect(cas_list, "^[0-9]{2,7}-[0-9]{2}-[0-9]$"))) {
        result <- tibble(substance_name = rep(result$substance_name, length(cas_list)),
                          substance_synonym = NA_character_,
                          substance_group = result$substance_name,
                          cas_number = cas_list)
      }
    }
  }
  ab <- split_abbrev_synonym(abbrev)
  result$abbreviation <- ab$abbrev
  result$abbreviation_synonym <- ab$synonym
  result
}
