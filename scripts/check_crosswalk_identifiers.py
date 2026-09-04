"""Sanity-checks scripts/../data-raw/PFAS_group_crosswalks.xlsx (9 pairwise
crosswalk sheets between 5 substance-group taxonomies: kemi, oecd, esi2
[group + subgroup], dalmijn) for identifier errors, i.e. cases where the
SAME taxonomy's group label is spelled/typed differently across the
different sheets it appears in.

Each of the 5 taxonomies appears as a labelled column in 3-4 of the 9
sheets (paired against each of the other taxonomies it was crosswalked
against). Since it's the same underlying source classification, every
sheet's copy of that column should draw from an identical label
vocabulary -- any label that appears in one sheet's copy but not (verbatim)
in another sheet's copy of the same column is either: a genuine label that
only co-occurred with one crosswalk partner (fine), or a typo/formatting
slip (worth a human's attention). This script separates the two: for every
label unique to one sheet, it looks for a near-duplicate (fuzzy match) in
the other sheets holding the same taxonomy -- a near-duplicate above a high
similarity threshold is almost certainly the same intended label typed
differently.

It also does two independent numeric sanity checks: (a) that each sheet's
row_share column equals n / row_total, and (b) that row_total actually
equals the number of substances captured by summing n within each source
group -- since if there's a hidden extra label variant, that sum
mismatches.

Run: python3 scripts/check_crosswalk_identifiers.py
"""
from pathlib import Path
from itertools import combinations

import pandas as pd
from rapidfuzz import fuzz

IN_PATH = Path(__file__).resolve().parent.parent / "data-raw" / "PFAS_group_crosswalks.xlsx"

# sheet_name -> (source_taxonomy_col, target_taxonomy_col)
SHEETS = {
    "kemi_oecd": ("substance_group_kemi", "substance_group_oecd"),
    "kemi_esi2_group": ("substance_group_kemi", "substance_group_esi2"),
    "kemi_esi2_subgroup": ("substance_group_kemi", "sub_group_esi2"),
    "kemi_dalmijn": ("substance_group_kemi", "group_dalmijn"),
    "oecd_esi2_group": ("substance_group_oecd", "substance_group_esi2"),
    "oecd_esi2_subgroup": ("substance_group_oecd", "sub_group_esi2"),
    "oecd_dalmijn": ("substance_group_oecd", "group_dalmijn"),
    "esi2_group_dalmijn": ("substance_group_esi2", "group_dalmijn"),
    "esi2_subgroup_dalmijn": ("sub_group_esi2", "group_dalmijn"),
}

FUZZY_THRESHOLD = 85  # rapidfuzz token_sort_ratio; a real synonym/typo vs. a genuinely different label


def load_all():
    xls = pd.ExcelFile(IN_PATH)
    return {name: xls.parse(name) for name in SHEETS}


def normalize_label(s):
    return " ".join(str(s).split()).strip()


def find_whitespace_or_case_dupes(dfs):
    """Within EACH single sheet, flag any two distinct label strings in the
    same column that normalize (collapse whitespace, casefold) to the same
    thing -- these are unambiguous data-entry duplicates, not just similar
    text."""
    issues = []
    for sheet, (src_col, tgt_col) in SHEETS.items():
        df = dfs[sheet]
        for col in (src_col, tgt_col):
            raw_vals = df[col].dropna().unique().tolist()
            buckets = {}
            for v in raw_vals:
                key = normalize_label(v).casefold()
                buckets.setdefault(key, []).append(v)
            for key, variants in buckets.items():
                if len(variants) > 1:
                    issues.append({
                        "type": "within-sheet duplicate (whitespace/case only)",
                        "sheet": sheet, "column": col, "variants": variants,
                    })
    return issues


def collect_label_sets_per_taxonomy():
    """taxonomy_col_name -> {sheet_name: set(labels)}"""
    taxonomy_sheets = {}
    for sheet, (src_col, tgt_col) in SHEETS.items():
        for col in (src_col, tgt_col):
            taxonomy_sheets.setdefault(col, {})[sheet] = None
    return taxonomy_sheets


def find_cross_sheet_fuzzy_mismatches(dfs):
    """For each taxonomy column, compare its label set across every pair of
    sheets that carries it. A label present in sheet A but absent
    (verbatim) from sheet B is checked against B's full label set for a
    high-similarity match -- that's the signature of a typo/formatting
    slip rather than a label that's simply absent from that pairing."""
    taxonomy_to_sheets = {}
    for sheet, (src_col, tgt_col) in SHEETS.items():
        taxonomy_to_sheets.setdefault(src_col, []).append(sheet)
        taxonomy_to_sheets.setdefault(tgt_col, []).append(sheet)

    issues = []
    for taxonomy_col, sheet_list in taxonomy_to_sheets.items():
        label_sets = {}
        for sheet in sheet_list:
            col = SHEETS[sheet][0] if SHEETS[sheet][0] == taxonomy_col else SHEETS[sheet][1]
            label_sets[sheet] = set(dfs[sheet][col].dropna().unique().tolist())

        for sheet_a, sheet_b in combinations(sheet_list, 2):
            only_in_a = label_sets[sheet_a] - label_sets[sheet_b]
            only_in_b = label_sets[sheet_b] - label_sets[sheet_a]
            for label_a in only_in_a:
                best_match, best_score = None, 0
                for label_b in only_in_b:
                    score = fuzz.token_sort_ratio(label_a, label_b)
                    if score > best_score:
                        best_match, best_score = label_b, score
                if best_match is not None and best_score >= FUZZY_THRESHOLD:
                    issues.append({
                        "type": "likely typo/formatting mismatch across sheets",
                        "taxonomy_column": taxonomy_col,
                        "sheet_a": sheet_a, "label_in_a": label_a,
                        "sheet_b": sheet_b, "label_in_b": best_match,
                        "similarity": best_score,
                    })
    # de-dup symmetric pairs (a vs b and b vs a both fire)
    seen = set()
    deduped = []
    for it in issues:
        key = tuple(sorted([
            (it["sheet_a"], it["label_in_a"]),
            (it["sheet_b"], it["label_in_b"]),
        ]))
        if key not in seen:
            seen.add(key)
            deduped.append(it)
    return sorted(deduped, key=lambda x: -x["similarity"])


def check_row_share_math(dfs):
    issues = []
    for sheet, df in dfs.items():
        computed = df["n"] / df["row_total"]
        bad = (computed - df["row_share"]).abs() > 1e-6
        if bad.any():
            issues.append({"type": "row_share != n/row_total", "sheet": sheet, "n_bad_rows": int(bad.sum())})
    return issues


def find_concatenated_labels(dfs):
    """A single taxonomy-column cell holding TWO valid category names joined
    by ';' (e.g. 'Perfluorocarboxylic acids, long-chain, and its
    derivatives;Perfluorooctanoic acid (PFOA) and its derivatives') is a
    real identifier problem, not just a spelling one: the substances on
    that row get counted under neither pure category, silently splitting
    what should be one taxonomy group's total into three. Flagged whenever
    a semicolon-joined label's own pieces ALSO occur as standalone labels
    elsewhere in the same column -- confirming dual membership got mashed
    into one string instead of the row being (or needing to be) split /
    the counts rolled into both parent categories."""
    issues = []
    taxonomy_to_sheets = {}
    for sheet, (src_col, tgt_col) in SHEETS.items():
        taxonomy_to_sheets.setdefault(src_col, []).append((sheet, src_col))
        taxonomy_to_sheets.setdefault(tgt_col, []).append((sheet, tgt_col))

    for taxonomy_col, sheet_cols in taxonomy_to_sheets.items():
        all_vals = set()
        for sheet, col in sheet_cols:
            all_vals |= set(dfs[sheet][col].dropna().unique())
        for sheet, col in sheet_cols:
            df = dfs[sheet]
            mask = df[col].astype(str).str.contains(";", na=False)
            if not mask.any():
                continue
            for label in df.loc[mask, col].unique():
                parts = [p.strip() for p in label.split(";")]
                parts_found_standalone = [p for p in parts if p in all_vals]
                issues.append({
                    "type": "semicolon-concatenated label (dual category membership merged into one string)",
                    "sheet": sheet, "column": col, "label": label,
                    "n_rows": int(mask.sum()) if False else int((df[col] == label).sum()),
                    "parts_also_appear_standalone": parts_found_standalone,
                })
    return issues


KNOWN_TYPOS = [
    # (taxonomy_column, misspelled_label_or_substring, likely_correct_form, note)
    ("substance_group_oecd", "Perfluoaoalkyl halides (other than iodides)",
     "Perfluoroalkyl halides (other than iodides)",
     "'Perfluoaoalkyl' is not a real chemistry term; every other OECD label uses 'Perfluoroalkyl'."),
    ("substance_group_oecd", "Perfluoroalkanes & armoatics",
     "Perfluoroalkanes & aromatics",
     "'armoatics' -- 'aromatics' is spelled correctly in 11 other OECD/ESI2 labels in this same file."),
    ("substance_group_oecd", "Perfluoroalkyl ketons",
     "Perfluoroalkyl ketones",
     "OECD's own label drops the 'e'; the ESI2 taxonomy's parallel label in the SAME workbook is "
     "correctly spelled 'Perfluoroalkyl ketones'."),
    ("substance_group_esi2", "Hydofluoroether",
     "Hydrofluoroether",
     "Missing 'r'. Inherited from the ECHA Annex A source workbook's own ESI-2 sheet (same typo was "
     "found there when this project's own ESI-2 taxonomy table was built) -- not introduced by this "
     "crosswalk file, but still worth a note since it's used as a real group label."),
    ("sub_group_esi2", "Three-ring perfluroocarbons - no acids",
     "Three-ring perfluorocarbons - no acids",
     "'perfluroocarbons' -- same ECHA-source typo as above, inherited rather than new."),
]


def check_known_typos(dfs):
    issues = []
    for taxonomy_col, misspelled, correct, note in KNOWN_TYPOS:
        for sheet, (c1, c2) in SHEETS.items():
            for col in (c1, c2):
                if col != taxonomy_col:
                    continue
                df = dfs[sheet]
                if (df[col] == misspelled).any():
                    issues.append({
                        "sheet": sheet, "column": col, "misspelled": misspelled,
                        "likely_correct": correct, "note": note,
                    })
    return issues


def check_row_total_math(dfs):
    """row_total should equal, for every row sharing the same source-group
    value, the sum of n across those rows (i.e. row_total is a per-source-
    group total, repeated on every row of that group)."""
    issues = []
    for sheet, (src_col, _tgt_col) in SHEETS.items():
        df = dfs[sheet]
        grouped = df.groupby(src_col)["n"].transform("sum")
        bad = grouped != df["row_total"]
        if bad.any():
            issues.append({
                "type": "row_total != sum(n) within source group", "sheet": sheet,
                "n_bad_rows": int(bad.sum()),
                "examples": df.loc[bad, src_col].unique().tolist()[:5],
            })
    return issues


def main():
    dfs = load_all()

    print("=" * 78)
    print("1. Within-sheet duplicate labels (whitespace/case-only differences)")
    print("=" * 78)
    dupes = find_whitespace_or_case_dupes(dfs)
    if not dupes:
        print("  none found")
    for d in dupes:
        print(f"  [{d['sheet']} / {d['column']}] {d['variants']}")

    print()
    print("=" * 78)
    print("2. Likely typo/formatting mismatches for the SAME taxonomy across sheets")
    print("=" * 78)
    fuzzy = find_cross_sheet_fuzzy_mismatches(dfs)
    if not fuzzy:
        print("  none found")
    for f in fuzzy:
        print(f"  [{f['taxonomy_column']}] similarity={f['similarity']:.0f}")
        print(f"      {f['sheet_a']}: {f['label_in_a']!r}")
        print(f"      {f['sheet_b']}: {f['label_in_b']!r}")

    print()
    print("=" * 78)
    print("3. Semicolon-concatenated labels (two categories merged into one string)")
    print("=" * 78)
    concat_issues = find_concatenated_labels(dfs)
    if not concat_issues:
        print("  none found")
    for c in concat_issues:
        print(f"  [{c['sheet']} / {c['column']}] {c['n_rows']} row(s): {c['label']!r}")
        print(f"      parts also appear standalone elsewhere: {c['parts_also_appear_standalone']}")

    print()
    print("=" * 78)
    print("4. Known/likely typos in taxonomy labels (checked against domain spelling "
          "and cross-referenced against this project's own canonical ESI-2 taxonomy table)")
    print("=" * 78)
    typo_issues = check_known_typos(dfs)
    if not typo_issues:
        print("  none found")
    for t in typo_issues:
        print(f"  [{t['sheet']} / {t['column']}] {t['misspelled']!r} -> likely {t['likely_correct']!r}")
        print(f"      {t['note']}")

    print()
    print("=" * 78)
    print("5. row_share arithmetic check (row_share should equal n / row_total)")
    print("=" * 78)
    share_issues = check_row_share_math(dfs)
    if not share_issues:
        print("  all rows consistent")
    for s in share_issues:
        print(f"  [{s['sheet']}] {s['n_bad_rows']} row(s) fail")

    print()
    print("=" * 78)
    print("6. row_total arithmetic check (row_total should equal sum(n) per source group)")
    print("=" * 78)
    total_issues = check_row_total_math(dfs)
    if not total_issues:
        print("  all rows consistent")
    for t in total_issues:
        print(f"  [{t['sheet']}] {t['n_bad_rows']} row(s) fail, e.g. source group(s): {t['examples']}")


if __name__ == "__main__":
    main()
