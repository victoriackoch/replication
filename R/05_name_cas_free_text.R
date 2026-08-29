# A.70 (printing) and A.90 (broader industrial uses) give free-text
# "Examples of PFAS" cells that embed a CAS number per substance, e.g.
# "PTFE (CAS: 9002-84-0); PFA (CAS: 26655-00-5)". extract_name_cas_pairs()
# (00_helpers.R) uses each CAS match itself as the delimiter.

extract_a70 <- function() {
  tab <- load_table("A.70")
  category <- ffill(tab[["Category"]])
  application <- tab[["Application"]]
  examples <- tab[["PFASs (non-confidential information)"]]

  product <- map2_chr(category, application, function(c, a) {
    parts <- c(c, a)
    parts <- parts[!is.na(parts) & str_trim(parts) != ""]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = " / ")
  })

  out <- map(seq_along(product), function(i) {
    if (is.na(product[i])) return(NULL)
    pairs <- extract_name_cas_pairs(examples[i])
    if (nrow(pairs) == 0) return(NULL)
    map2_dfr(pairs$name, pairs$cas, function(nm, cas) {
      make_row_raw(product[i], nm, cas_number = cas, source = "A.70",
                    source_text = examples[i])
    })
  })
  bind_rows(out)
}

extract_a90 <- function() {
  tab <- load_table("A.90")
  use <- ffill(tab[["Use"]])
  subuse <- tab[["Sub-Use"]]
  examples <- tab[["Examples of PFAS used"]]

  product <- map2_chr(use, subuse, function(u, s) {
    parts <- c(u, s)
    parts <- parts[!is.na(parts) & str_trim(parts) != ""]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = " / ")
  })

  out <- map(seq_along(product), function(i) {
    if (is.na(product[i])) return(NULL)
    txt <- examples[i]
    pairs <- extract_name_cas_pairs(txt)
    if (nrow(pairs) > 0) {
      return(map2_dfr(pairs$name, pairs$cas, function(nm, cas) {
        make_row_raw(product[i], nm, cas_number = cas, source = "A.90",
                      source_text = txt)
      }))
    }
    # no embedded CAS at all in this cell -- fall back to a plain name list
    subs <- split_substance_list(txt)
    if (length(subs) == 0) return(NULL)
    make_row_raw(product[i], subs, source = "A.90", source_text = txt)
  })
  bind_rows(out)
}

add_table(extract_a70())
add_table(extract_a90())
