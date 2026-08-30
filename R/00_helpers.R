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

parse_name_abbrev <- function(token) {
  if (is.na(token)) return(list(name = NA_character_, abbrev = NA_character_))
  token <- str_trim(token)
  # "ACRONYM (longer descriptive text)"
  m <- str_match(token, "^([A-Za-z0-9][A-Za-z0-9\\-]{1,9})\\s*\\(([^()]{4,})\\)$")
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
  list(name = token, abbrev = NA_character_)
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
  "^(no information( received)?\\.?|no data\\.?|data unavailable\\.?|not applicable\\.?|n/?a\\.?|unknown\\.?)$",
  ignore_case = TRUE
)

is_no_info <- function(x) {
  if (is.na(x)) return(TRUE)
  x <- str_trim(x)
  x == "" || str_detect(x, NO_INFO_RE)
}

CLASS_PREFIX_RE <- regex(
  "(Polymeric PFAS[s]?\\s*:|Non-polymeric PFAS[s]?\\s*:|Polymeric\\s*:|Non-polymeric\\s*:|Fluorinated gas(es)?\\s*:)",
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
    c("Fluoroelastomer", "PVDF", "PFA", "PTFE", "VDF", "TrFE", "TFE", "CTFE", "HFP", "LiTFSI", "LiTFS")
)

split_substance_list <- function(text) {
  if (is_no_info(text)) return(character(0))
  for (key in names(SUBSTANCE_LIST_OVERRIDES)) {
    if (str_trim(text) == key) return(SUBSTANCE_LIST_OVERRIDES[[key]])
  }
  # replace (rather than delete) each class prefix with a delimiter, so the
  # boundary between class-tagged segments also becomes a split point
  cleaned <- str_replace_all(text, CLASS_PREFIX_RE, ";")
  cleaned <- str_remove_all(cleaned, CONFIDENTIAL_RE)
  parts <- split_respecting_parens(cleaned)
  parts <- parts[parts != "" & !vapply(parts, is_no_info, logical(1))]
  # Occasionally a "Full descriptive name; ABBREV" pair for one substance
  # gets split apart by the ";" above (e.g. "Lithium Bis
  # (trifluoromethanesulfonyl)imide; LITFSI"). Re-merge a bare acronym
  # token into the preceding chunk so parse_name_abbrev() can recover it as
  # one name+abbreviation pair instead of two fragments -- but only when
  # the preceding chunk is a multi-word full name (contains a space), not
  # a short single-word class label like "Fluoroelastomer" or "PTFE",
  # which is legitimately its own separate list item (e.g. "Fluoroelastomer,
  # PVDF, PFA, PTFE" is four distinct substances, not one name+abbreviation).
  keep <- rep(TRUE, length(parts))
  for (i in seq_along(parts)) {
    if (i > 1 && keep[i - 1] && is_acronym_token(parts[i]) &&
        !is_acronym_token(parts[i - 1]) && str_detect(parts[i - 1], "\\s")) {
      parts[i - 1] <- paste0(parts[i - 1], " (", parts[i], ")")
      keep[i] <- FALSE
    }
  }
  parts <- parts[keep]
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
  for (i in seq_along(parts)) {
    if (i > 1 && keep2[i - 1] && str_detect(parts[i], locant_re)) {
      parts[i - 1] <- paste0(parts[i - 1], ",", parts[i])
      keep2[i] <- FALSE
    }
  }
  parts <- parts[keep2]
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
