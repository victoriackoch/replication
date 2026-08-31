# "Long-form" tables: a Use/Sub-use column (values only printed on the first
# row of a merged block, forward-filled here) plus a free-text "Examples of
# PFAS(s)" column, split into individual substance rows.
#
# Points 17-19: product is "Use: Sub-use", and when either Use or Sub-use
# itself lists several comma-separated items in one cell, each item is
# split out and the FULL cross-product against every individual substance
# in Examples is taken (e.g. A.51's "Insulation, sheaths, tapes, jackets,
# sleeves, binders" x 11 listed substances = 66 rows for that one source
# row). None of these give a CAS number per substance. Trailing footnote-
# reference numbers ("Machinery93") are stripped from Use/Sub-use text.

strip_footnote_number <- function(x) {
  x <- str_remove(x, "(?<=[A-Za-z\\)])[0-9]{2,3}$")
  # a PDF line-wrap split "panels" across two lines as "pan els"
  str_replace_all(x, "pan els", "panels")
}

# Splits Use and Sub-use on commas into atomic items and returns every
# "Use item: Sub-use item" combination (points 17-19).
cross_product_products <- function(use, subuse) {
  use_items <- if (is.na(use)) NA_character_ else split_respecting_parens(use)
  subuse_items <- if (is.na(subuse)) NA_character_ else split_respecting_parens(subuse)
  use_items <- use_items[is.na(use_items) | !vapply(use_items, is_no_info, logical(1))]
  subuse_items <- subuse_items[is.na(subuse_items) | !vapply(subuse_items, is_no_info, logical(1))]
  if (length(use_items) == 0) use_items <- NA_character_
  if (length(subuse_items) == 0) subuse_items <- NA_character_
  combos <- expand_grid(u = use_items, s = subuse_items)
  products <- apply(combos, 1, function(r) {
    parts <- r[!is.na(r) & str_trim(r) != ""]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = ": ")
  })
  products[!is.na(products)]
}

# A cell with an unclosed "(" (e.g. "Photoresist: Image applications
# (surfactant,") continues on the next row (e.g. "additive and PAG)"),
# which then has nothing of its own (no Examples) -- merge it back rather
# than leave the Use/Sub-use text truncated mid-parenthesis.
merge_paren_wraps <- function(tab, col, guard_col) {
  x <- tab[[col]]
  unbalanced <- str_count(coalesce(x, ""), fixed("(")) - str_count(coalesce(x, ""), fixed(")"))
  wrap_idx <- which(unbalanced > 0)
  for (w in rev(wrap_idx)) {
    if (w < nrow(tab) && is.na(tab[[guard_col]][w + 1])) {
      tab[[col]][w] <- paste0(x[w], tab[[col]][w + 1])
      tab <- tab[-(w + 1), ]
      x <- tab[[col]]
    }
  }
  tab
}

# A cell's Examples text is itself sometimes wrapped onto its own physical
# row -- the ONLY populated cell on that row -- when the preceding row's
# Examples cell got cut short. Every genuine data row has a Sub-use of its
# own; a row with no Use, no Sub-use, but a real Examples value is that
# kind of continuation (e.g. "Connectors" row's Examples cut off mid-list,
# continuing on the next row as a bare Examples fragment with nothing
# else). Left unmerged, that fragment doesn't just lose its connection to
# the truncated list it completes -- it stands alone with the block's
# forward-filled Use as its only context and generates spurious rows of
# its own from whatever names happen to be in it.
merge_orphan_examples_rows <- function(tab, use_col, subuse_col, examples_col) {
  is_orphan <- is.na(tab[[use_col]]) & is.na(tab[[subuse_col]]) & !is.na(tab[[examples_col]])
  idx <- which(is_orphan)
  for (i in rev(idx)) {
    if (i > 1) {
      tab[[examples_col]][i - 1] <- paste0(coalesce(tab[[examples_col]][i - 1], ""), " ", tab[[examples_col]][i])
      tab <- tab[-i, ]
    }
  }
  tab
}

# A few rows are footnote text that leaked into the Use column mid-table
# (e.g. "77 Renewable energy systems are assessed in the Energy sector,
# c.f. section A.3.13."), rather than at the end where load_table()'s
# footnote-block cutoff catches them -- identifiable by a leading footnote
# number ("NN "), a shape no real "Use category" value in this workbook
# ever takes. Left alone these would (a) forward-fill into real rows below
# once their own Sub-use is blank, and (b) sometimes carry the *previous*
# real row's Examples value too (a PDF merged-cell artifact), generating a
# spurious duplicate-ish output row of their own. Dropped outright rather
# than blanked, so neither happens.
drop_stray_footnote_rows <- function(tab, use_col, subuse_col = NULL, examples_col = NULL) {
  use <- coalesce(tab[[use_col]], "")
  subuse <- if (!is.null(subuse_col)) coalesce(tab[[subuse_col]], "") else rep("", nrow(tab))
  # a footnote's own start row ("77 Renewable energy systems are
  # assessed..."), or a continuation row of one that wrapped onto further
  # physical rows without repeating the leading number (e.g. "metal
  # products, c.f." / "If the coil-coated article is used in construction,
  # it is assessed in the") -- cross-reference phrasing that never occurs
  # in genuine Use/Sub-use text.
  is_footnote <- str_detect(use, "^[0-9]{1,3}\\s") |
    str_detect(use, regex("assessed in the|\\bc\\.f\\.", ignore_case = TRUE)) |
    str_detect(subuse, regex("assessed in the|\\bc\\.f\\.", ignore_case = TRUE))
  # a final trailing fragment of such a footnote (e.g. "Construction
  # sector.") carries none of the above phrasing itself, but is
  # identifiable as: a Use value, no Sub-use, and an Examples value that's
  # an exact repeat of the row directly above (the PDF's merged-Examples-
  # cell artifact) -- a real section-header row never repeats the
  # previous row's Examples this way.
  if (!is.null(examples_col)) {
    examples <- tab[[examples_col]]
    prev_examples <- c(NA_character_, examples[-length(examples)])
    is_trailing_fragment <- use != "" & subuse == "" & !is.na(examples) &
      !is.na(prev_examples) & examples == prev_examples
    is_footnote <- is_footnote | is_trailing_fragment
  }
  tab[!is_footnote, , drop = FALSE]
}

extract_long_form <- function(sheet, use_col, subuse_col, examples_col, ffill_use = TRUE) {
  tab <- load_table(sheet)
  if (!is.null(subuse_col)) tab <- merge_orphan_examples_rows(tab, use_col, subuse_col, examples_col)
  if (!is.null(subuse_col)) tab <- merge_paren_wraps(tab, subuse_col, examples_col)
  tab <- merge_paren_wraps(tab, use_col, examples_col)
  tab <- drop_stray_footnote_rows(tab, use_col, subuse_col, examples_col)
  # a Sub-use cell built as a bulleted "•"-list rollup (e.g. A.57's PEM
  # fuel cells "Membrane electrode assemblies (MEA)62: • catalyst coated
  # membrane (CCM), • gas diffusion layer (GDL), ...") duplicates -- with
  # a generic, less specific Examples value -- what the rows right below
  # it already give individually, one sub-use per row ("MEA-Catalyst
  # coated membrane...", "MEA-Gas diffusion layer (GDL)", ...). Splitting
  # its own comma/bullet mix would only add lower-quality duplicate rows,
  # so it's dropped rather than parsed.
  if (!is.null(subuse_col)) {
    tab <- tab[!str_detect(coalesce(tab[[subuse_col]], ""), fixed("•")), , drop = FALSE]
  }

  use <- strip_footnote_number(tab[[use_col]])
  if (ffill_use) use <- ffill(use)
  subuse <- if (!is.null(subuse_col)) strip_footnote_number(tab[[subuse_col]]) else rep(NA_character_, nrow(tab))
  examples <- tab[[examples_col]]

  out <- map(seq_len(nrow(tab)), function(i) {
    subs <- split_substance_list(examples[i])
    if (length(subs) == 0) return(NULL)

    products <- cross_product_products(use[i], subuse[i])
    if (length(products) == 0) return(NULL)

    expand_grid(product = products, sub = subs) %>%
      { make_row_raw(.$product, .$sub, source = sheet, source_text = examples[i]) }
  })
  bind_rows(out)
}

add_table(extract_long_form("A.51", "Use category", "Sub-use", "Examples of PFASs"))
add_table(extract_long_form("A.53", "Use category", "Sub-use", "Examples of PFAS"))
add_table(extract_long_form("A.59", "Use category", "Sub-use(s) - short form", "Examples of PFASs"))
add_table(extract_long_form("A.83", "Use", "Sub-use", "Examples of PFASs used"))
# A.62 (lubricants) and A.90 (broader industrial, embedded CAS numbers) need
# bespoke handling -- see 02_a62_lubricants.R and 05_name_cas_free_text.R
