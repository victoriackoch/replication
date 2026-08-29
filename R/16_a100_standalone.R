# A.100 is pulled out into its own standalone table (point 1), rather than
# mixed into the main product-substance table. Still routed through
# process_substance_triple() for consistency (polymer-of/or-synonym/
# abbreviation-synonym handling), even though this table's names are
# already short and none of those patterns actually occur in it.

extract_a100_standalone <- function() {
  tab <- load_table("A.100")
  keep <- !is.na(tab$Name) & str_trim(tab$Name) != ""
  tab <- tab[keep, , drop = FALSE]
  out <- map(seq_len(nrow(tab)), function(i) {
    triple <- process_substance_triple(tab$Name[i], tab$Abbreviation[i], tab$CAS[i], "A.100")
    make_row(
      product = tab$Use[i],
      substance_name = triple$substance_name,
      abbreviation = triple$abbreviation,
      cas_number = triple$cas_number,
      source = "A.100",
      source_text = paste0("A.100 row: ", tab$Name[i], " (Process: ", coalesce(tab$Process[i], ""),
                            "; Class: ", coalesce(tab$Class[i], ""), ")"),
      substance_synonym = triple$substance_synonym,
      abbreviation_synonym = triple$abbreviation_synonym,
      substance_group = triple$substance_group
    )
  })
  bind_rows(out)
}

A100_TABLE <- extract_a100_standalone()
