# PFAS structural classifier per the ECHA Annex XVII restriction dossier
# definition, applied to pfas_list_final$smiles_master.
#
#   TRIGGER: any substance with >=1 fully-fluorinated methyl (CF3-) or
#   methylene (-CF2-) carbon (no H/Cl/Br/I on that carbon).
#
#   CARVE-OUT: excluded if the substance contains ONLY:
#     CF3-X       where X  = -OR or -NRR'
#     X-CF2-X'    where X  = -OR or -NRR'
#                   and X' = -CH3, -CH2-, aromatic, -C(O)-, -OR'', -SR'', -NR''R'''
#   with every R/R'/R''/R''' in {-H, -CH3, -CH2-, aromatic, -C(O)-}.
#
# Method: find every CF3/CF2 carbon (the trigger atoms), then check EACH
# ONE individually against the carve-out's allowed local environment. A
# substance is PFAS if at least one trigger atom is not carve-out-exempt
# (a molecule can have both exempt and non-exempt fluorinated carbons, so
# a single "does the exempt pattern exist anywhere" test would be wrong).
#
# This is a straight R/rcdk port of scripts/pfas_smarts_classifier.py,
# which was validated 15/15 against hand-picked cases (PFOA, PFOS, GenX,
# Sevoflurane, aromatic/aliphatic CF3 ethers and amines, hexafluorobenzene,
# ...) using RDKit -- SMARTS syntax is shared between RDKit and CDK, and
# every construct used here (atom brackets, H-count, degree, aromatic
# lowercase atoms, recursive $(...) SMARTS) is standard, non-toolkit-
# specific SMARTS. I could not install rcdk in this sandboxed environment
# (its CRAN mirror is network-blocked here) to re-run the same test suite
# through CDK directly -- run the "self-test" section at the bottom on
# your own machine before trusting this on real data, and tell me if any
# of the 15 cases disagree with the Python/RDKit version's answer.
#
# Requires: rcdk (install.packages("rcdk") -- needs a JDK; see
# https://cran.r-project.org/package=rcdk for setup notes), dplyr

if (!requireNamespace("rcdk", quietly = TRUE)) {
  stop("Install rcdk first: install.packages(\"rcdk\")  (needs a JDK on your machine)")
}
library(rcdk)
suppressMessages(library(dplyr))

# ---- Rule A: trigger atoms -------------------------------------------------
CF3_SMARTS <- "[CX4H0](F)(F)F"
CF2_SMARTS <- "[CX4H0](F)(F)([!F;!Cl;!Br;!I])[!F;!Cl;!Br;!I]"

# ---- Rule B: carve-out allowed neighbourhoods ------------------------------
R_GROUP <- "$([H]),$([CH3]),$([CH2]),$(c),$([CX3]=O)"

CF3_EXEMPT_SMARTS <- sprintf(
  "[CX4H0](F)(F)(F)[$([OX2](%s)),$([NX3](%s)%s)]",
  R_GROUP, R_GROUP, sprintf("[%s]", R_GROUP)
)

X_SIDE <- sprintf("[$([OX2](%s)),$([NX3](%s)[%s])]", R_GROUP, R_GROUP, R_GROUP)
XPRIME_SIDE <- sprintf(
  "[$([CH3]),$([CH2]),$(c),$([CX3]=O),$([OX2][%s]),$([SX2][%s]),$([NX3][%s][%s])]",
  R_GROUP, R_GROUP, R_GROUP, R_GROUP
)
CF2_EXEMPT_SMARTS <- sprintf("%s[CX4H0](F)(F)%s", X_SIDE, XPRIME_SIDE)

# Atom indices (1-based, as rcdk returns them) matched by a SMARTS query
# against a parsed molecule, or integer(0) if none.
.smarts_atom_matches <- function(mol, smarts) {
  hits <- tryCatch(rcdk::matches(smarts, mol, return.matches = TRUE), error = function(e) NULL)
  if (is.null(hits) || length(hits) == 0 || !hits[[1]]$match) return(integer(0))
  unique(unlist(hits[[1]]$mapping))
}

#' Classify one SMILES against the ECHA Annex XVII PFAS definition
#'
#' @param smiles a single SMILES string
#' @return a list(is_pfas = TRUE/FALSE/NA, detail = character(1))
classify_smiles <- function(smiles) {
  smiles_trimmed <- if (is.na(smiles)) "" else trimws(smiles)
  if (!nzchar(smiles_trimmed)) {
    return(list(is_pfas = NA, detail = "empty SMILES"))
  }
  mol <- tryCatch(rcdk::parse.smiles(smiles)[[1]], error = function(e) NULL)
  if (is.null(mol)) return(list(is_pfas = NA, detail = "could not parse SMILES"))

  cf3_atoms <- .smarts_atom_matches(mol, CF3_SMARTS)
  cf2_atoms <- .smarts_atom_matches(mol, CF2_SMARTS)
  trigger_atoms <- union(cf3_atoms, cf2_atoms)

  if (length(trigger_atoms) == 0) {
    return(list(is_pfas = FALSE, detail = "no fully-fluorinated CF3/CF2 carbon found"))
  }

  cf3_exempt <- .smarts_atom_matches(mol, CF3_EXEMPT_SMARTS)
  cf2_exempt_all <- .smarts_atom_matches(mol, CF2_EXEMPT_SMARTS)
  cf2_exempt <- intersect(cf2_exempt_all, cf2_atoms)  # keep only the carbon, not the X/X' side atoms

  exempt_atoms <- union(cf3_exempt, cf2_exempt)
  non_exempt <- setdiff(trigger_atoms, exempt_atoms)

  if (length(non_exempt) > 0) {
    return(list(is_pfas = TRUE, detail = sprintf(
      "%d of %d CF3/CF2 carbon(s) not carve-out-exempt", length(non_exempt), length(trigger_atoms)
    )))
  }
  list(is_pfas = FALSE, detail = sprintf("all %d CF3/CF2 carbon(s) fall under the carve-out", length(trigger_atoms)))
}

#' Classify every row of pfas_list_final$smiles_master
#'
#' Adds is_pfas_echa_definition (logical) and classification_detail
#' (character) columns.
classify_pfas_list <- function(df, smiles_col = "smiles_master") {
  stopifnot(smiles_col %in% names(df))
  results <- lapply(df[[smiles_col]], classify_smiles)
  df$is_pfas_echa_definition <- vapply(results, function(r) r$is_pfas, logical(1))
  df$classification_detail <- vapply(results, function(r) r$detail, character(1))
  df
}

# ---------------------------------------------------------------------------
# Usage on your data:
#
#   pfas_list_final <- classify_pfas_list(pfas_list_final, "smiles_master")
#   table(pfas_list_final$is_pfas_echa_definition, useNA = "ifany")
#   write.csv(pfas_list_final, "pfas_list_final_classified.csv", row.names = FALSE)
# ---------------------------------------------------------------------------

# ---- Self-test: run this block first to confirm the SMARTS behave the
# same way in rcdk/CDK as they did in the validated Python/RDKit version.
if (identical(Sys.getenv("PFAS_CLASSIFIER_SELFTEST"), "1") || interactive()) {
  test_cases <- tibble::tribble(
    ~name, ~smiles, ~expected,
    "PFOA", "OC(=O)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)F", TRUE,
    "PFOS", "OS(=O)(=O)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)F", TRUE,
    "CF3-CF2-CF3", "FC(F)(F)C(F)(F)F", TRUE,
    "Ar-O-CF3", "FC(F)(F)Oc1ccccc1", FALSE,
    "Ar-NH-O-CF3 analogue", "FC(F)(F)Oc1ccc(N)cc1", FALSE,
    "Sevoflurane", "FCOC(C(F)(F)F)C(F)(F)F", TRUE,
    "GenX anion", "OC(=O)C(F)(F)OC(F)(F)C(F)(F)F", TRUE,
    "Trifluoroacetamide", "FC(F)(F)C(N)=O", TRUE,
    "2,2,2-Trifluoroethanol", "FC(F)(F)CO", TRUE,
    "CF3-O-CH3", "FC(F)(F)OC", FALSE,
    "CF3-NH-CF3 (not exempt: R is another CF3)", "FC(F)(F)NC(F)(F)F", TRUE,
    "CH3-O-CF2-O-CH3", "COC(F)(F)OC", FALSE,
    "Perfluorohexane", "FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)F", TRUE,
    "Trifluoromethylbenzene", "FC(F)(F)c1ccccc1", TRUE,
    "Hexafluorobenzene (aromatic-only F)", "Fc1c(F)c(F)c(F)c(F)c1F", FALSE
  )
  test_cases <- classify_pfas_list(test_cases, "smiles")
  test_cases$ok <- test_cases$is_pfas_echa_definition == test_cases$expected
  print(test_cases %>% select(name, expected, is_pfas_echa_definition, ok, classification_detail))
  cat(sprintf("\n%d/%d passed\n", sum(test_cases$ok), nrow(test_cases)))
}
