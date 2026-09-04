"""Builds print-ready (vector PDF) crosswalk heatmaps and a structural
summary table from data-raw/PFAS_group_crosswalks.xlsx, for dropping into
an Overleaf/LaTeX document.

Each of the 9 sheets is a pairwise crosswalk between two of five substance-
group taxonomies (KEMI, OECD, ESI-2 group, ESI-2 subgroup, Dalmijn): for
every (source group, target group) pair, n = number of substances in the
source group that also fall in that target group, and row_share = n /
row_total (row_total = all substances in that source group with a match
against this particular crosswalk partner).

One heatmap per sheet: rows = source-taxonomy groups, columns = target-
taxonomy groups, cell color/annotation = row_share (each row sums to
100%). Target axes with more than COLLAPSE_THRESHOLD distinct groups are
collapsed to the top TOP_N (by total substances mapped into that group)
plus a single "Other (k groups)" column, so every figure stays legible in
print -- otherwise e.g. the OECD axis (134 groups) would be unreadable.

Colors follow this project's dataviz skill: the sequential-magnitude
context uses the single validated blue ramp (steps 100->700), never a
rainbow; chart ink/gridlines/surface use the same skill's light-mode chart
chrome tokens.

Run: python3 scripts/build_crosswalk_visuals.py
Outputs: output/crosswalk_figures/<sheet>.pdf (9 files)
         output/crosswalk_summary_tables.tex (LaTeX tables, Overleaf-ready)
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
IN_PATH = ROOT / "data-raw" / "PFAS_group_crosswalks.xlsx"
FIG_DIR = ROOT / "output" / "crosswalk_figures"
TEX_PATH = ROOT / "output" / "crosswalk_summary_tables.tex"

# sheet -> (source_col, target_col, source_label, target_label)
SHEETS = {
    "kemi_oecd": ("substance_group_kemi", "substance_group_oecd", "KEMI", "OECD"),
    "kemi_esi2_group": ("substance_group_kemi", "substance_group_esi2", "KEMI", "ESI-2 group"),
    "kemi_esi2_subgroup": ("substance_group_kemi", "sub_group_esi2", "KEMI", "ESI-2 subgroup"),
    "kemi_dalmijn": ("substance_group_kemi", "group_dalmijn", "KEMI", "Dalmijn"),
    "oecd_esi2_group": ("substance_group_oecd", "substance_group_esi2", "OECD", "ESI-2 group"),
    "oecd_esi2_subgroup": ("substance_group_oecd", "sub_group_esi2", "OECD", "ESI-2 subgroup"),
    "oecd_dalmijn": ("substance_group_oecd", "group_dalmijn", "OECD", "Dalmijn"),
    "esi2_group_dalmijn": ("substance_group_esi2", "group_dalmijn", "ESI-2 group", "Dalmijn"),
    "esi2_subgroup_dalmijn": ("sub_group_esi2", "group_dalmijn", "ESI-2 subgroup", "Dalmijn"),
}

COLLAPSE_THRESHOLD = 16
TOP_N = 15

# Cosmetic-only label corrections for the figures (the underlying identifier
# check in scripts/check_crosswalk_identifiers.py reports these against the
# RAW file separately -- fixed here only so the publication figures don't
# carry visible typos).
LABEL_DISPLAY_FIXES = {
    "Perfluoaoalkyl halides (other than iodides)": "Perfluoroalkyl halides (other than iodides)",
    "Perfluoroalkanes & armoatics": "Perfluoroalkanes & aromatics",
    "Perfluoroalkyl ketons": "Perfluoroalkyl ketones",
    "Hydofluoroether": "Hydrofluoroether",
    "Three-ring perfluroocarbons - no acids": "Three-ring perfluorocarbons - no acids",
}

# dataviz skill: sequential blue ramp, steps 100 (near-surface) -> 700 (saturated)
SEQUENTIAL_BLUE_STEPS = [
    "#cde2fb", "#b7d3f6", "#9ec5f4", "#86b6ef", "#6da7ec", "#5598e7",
    "#3987e5", "#2a78d6", "#256abf", "#1c5cab", "#184f95", "#104281", "#0d366b",
]
SEQ_BLUE_CMAP = LinearSegmentedColormap.from_list("dataviz_seq_blue", SEQUENTIAL_BLUE_STEPS)

# dataviz skill: light-mode chart chrome
INK_PRIMARY = "#0b0b0b"
INK_MUTED = "#898781"
GRIDLINE = "#e1e0d9"
SURFACE = "#fcfcfb"

plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["DejaVu Sans", "Arial", "Helvetica"],
    "text.color": INK_PRIMARY,
    "axes.edgecolor": GRIDLINE,
    "axes.labelcolor": INK_PRIMARY,
    "xtick.color": INK_MUTED,
    "ytick.color": INK_MUTED,
    "figure.facecolor": SURFACE,
    "axes.facecolor": SURFACE,
    "savefig.facecolor": SURFACE,
})


def load_all():
    xls = pd.ExcelFile(IN_PATH)
    dfs = {name: xls.parse(name) for name in SHEETS}
    for sheet, (src_col, tgt_col, _sl, _tl) in SHEETS.items():
        df = dfs[sheet]
        df[src_col] = df[src_col].replace(LABEL_DISPLAY_FIXES)
        df[tgt_col] = df[tgt_col].replace(LABEL_DISPLAY_FIXES)
    return dfs


def collapse_axis(df, col, other_col):
    """If col has more than COLLAPSE_THRESHOLD distinct values, keep the
    TOP_N with the largest total n and fold the rest into 'Other (k groups)',
    re-aggregating n so nothing is double counted. Applied independently to
    both the source and target columns so BOTH axes of a heatmap stay
    legible (e.g. OECD appears as the 76-group source axis in two sheets,
    not just as a large target axis)."""
    totals = df.groupby(col)["n"].sum().sort_values(ascending=False)
    if len(totals) <= COLLAPSE_THRESHOLD:
        return df, None
    keep = set(totals.index[:TOP_N])
    n_dropped = len(totals) - TOP_N
    df = df.copy()
    df[col] = df[col].where(df[col].isin(keep), f"Other ({n_dropped} groups)")
    df = df.groupby([col, other_col], as_index=False)["n"].sum()
    return df, n_dropped


def wrap_label(s, width=28):
    import textwrap
    return "\n".join(textwrap.wrap(s, width=width, break_long_words=False))


def build_heatmap(sheet, df, src_col, tgt_col, src_label, tgt_label):
    df, n_dropped_tgt = collapse_axis(df, tgt_col, src_col)
    df, n_dropped_src = collapse_axis(df, src_col, tgt_col)

    src_totals = df.groupby(src_col)["n"].sum().sort_values(ascending=False)
    src_order = [s for s in src_totals.index if not str(s).startswith("Other (")]
    src_order += [s for s in src_totals.index if str(s).startswith("Other (")]

    tgt_totals = df.groupby(tgt_col)["n"].sum().sort_values(ascending=False)
    tgt_order = [t for t in tgt_totals.index if not str(t).startswith("Other (")]
    other_cols = [t for t in tgt_totals.index if str(t).startswith("Other (")]
    tgt_order = tgt_order + other_cols  # "Other" column always trails, if present

    mat = df.pivot_table(index=src_col, columns=tgt_col, values="n", aggfunc="sum", fill_value=0)
    mat = mat.reindex(index=src_order, columns=tgt_order, fill_value=0)
    row_totals = mat.sum(axis=1)
    share = mat.div(row_totals, axis=0).fillna(0)

    n_rows, n_cols = share.shape
    fig_w = max(6.0, 1.1 * n_cols + 2.6)
    fig_h = max(3.5, 0.55 * n_rows + 1.8)
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))

    im = ax.imshow(share.values, cmap=SEQ_BLUE_CMAP, vmin=0, vmax=1, aspect="auto")

    ax.set_xticks(range(n_cols))
    ax.set_xticklabels([wrap_label(str(c), 22) for c in share.columns], fontsize=8, ha="center")
    ax.set_yticks(range(n_rows))
    ax.set_yticklabels([f"{wrap_label(str(r), 34)}  (n={int(row_totals[r])})" for r in share.index], fontsize=8.5)
    ax.tick_params(axis="both", length=0)

    for spine in ax.spines.values():
        spine.set_visible(False)
    ax.set_xticks(np.arange(-0.5, n_cols, 1), minor=True)
    ax.set_yticks(np.arange(-0.5, n_rows, 1), minor=True)
    ax.grid(which="minor", color=SURFACE, linewidth=2)
    ax.tick_params(which="minor", length=0)

    for i in range(n_rows):
        for j in range(n_cols):
            val = share.values[i, j]
            if val < 0.01:
                continue
            text_color = "#ffffff" if val >= 0.55 else INK_PRIMARY
            ax.text(j, i, f"{val * 100:.0f}%", ha="center", va="center", fontsize=7.5, color=text_color)

    ax.set_xlabel(f"{tgt_label} group", fontsize=9.5, labelpad=8)
    ax.set_ylabel(f"{src_label} group", fontsize=9.5, labelpad=8)
    title = f"{src_label} → {tgt_label} crosswalk"
    subtitle = f"cell = share of each {src_label} group's matched substances; row n = total matched substances"
    # pad/subtitle offset given in points via annotate (not axes-fraction),
    # so headroom is constant regardless of how tall a given figure is.
    ax.set_title(title, fontsize=11.5, fontweight="bold", loc="left", pad=32)
    ax.annotate(subtitle, xy=(0, 1), xytext=(0, 20), xycoords="axes fraction",
                textcoords="offset points", fontsize=8, color=INK_MUTED, ha="left", va="bottom")

    cbar = fig.colorbar(im, ax=ax, fraction=0.035, pad=0.02)
    cbar.set_label("row share", fontsize=8, color=INK_MUTED)
    cbar.ax.tick_params(labelsize=7, color=INK_MUTED, labelcolor=INK_MUTED)
    cbar.outline.set_visible(False)

    plt.setp(ax.get_xticklabels(), rotation=30, ha="right", rotation_mode="anchor")
    fig.tight_layout()
    out_path = FIG_DIR / f"{sheet}.pdf"
    fig.savefig(out_path, dpi=300, bbox_inches="tight")
    plt.close(fig)
    return out_path, n_rows, n_cols, n_dropped_src, n_dropped_tgt


def escape_tex(s):
    return (str(s).replace("&", "\\&").replace("%", "\\%").replace("_", "\\_")
            .replace("#", "\\#"))


def build_summary_tables(dfs):
    taxonomy_cols = {
        "KEMI": [("kemi_oecd", "substance_group_kemi"), ("kemi_esi2_group", "substance_group_kemi"),
                 ("kemi_esi2_subgroup", "substance_group_kemi"), ("kemi_dalmijn", "substance_group_kemi")],
        "OECD": [("kemi_oecd", "substance_group_oecd"), ("oecd_esi2_group", "substance_group_oecd"),
                 ("oecd_esi2_subgroup", "substance_group_oecd"), ("oecd_dalmijn", "substance_group_oecd")],
        "ESI-2 group": [("kemi_esi2_group", "substance_group_esi2"), ("oecd_esi2_group", "substance_group_esi2"),
                        ("esi2_group_dalmijn", "substance_group_esi2")],
        "ESI-2 subgroup": [("kemi_esi2_subgroup", "sub_group_esi2"), ("oecd_esi2_subgroup", "sub_group_esi2"),
                           ("esi2_subgroup_dalmijn", "sub_group_esi2")],
        "Dalmijn": [("kemi_dalmijn", "group_dalmijn"), ("oecd_dalmijn", "group_dalmijn"),
                    ("esi2_group_dalmijn", "group_dalmijn"), ("esi2_subgroup_dalmijn", "group_dalmijn")],
    }
    struct_rows = []
    for taxo, sheet_cols in taxonomy_cols.items():
        labels = set()
        for sheet, col in sheet_cols:
            labels |= set(dfs[sheet][col].dropna().unique())
        struct_rows.append((taxo, len(labels)))

    lines = []
    lines.append("% Structural summary: distinct group labels per taxonomy,")
    lines.append("% as used across this crosswalk file.")
    lines.append("\\begin{table}[htbp]")
    lines.append("\\centering")
    lines.append("\\caption{Number of distinct substance-group labels per source taxonomy.}")
    lines.append("\\label{tab:pfas-taxonomy-structure}")
    lines.append("\\begin{tabular}{lr}")
    lines.append("\\toprule")
    lines.append("Taxonomy & Distinct groups \\\\")
    lines.append("\\midrule")
    for taxo, n in struct_rows:
        lines.append(f"{escape_tex(taxo)} & {n} \\\\")
    lines.append("\\bottomrule")
    lines.append("\\end{tabular}")
    lines.append("\\end{table}")
    lines.append("")

    lines.append("% Pairwise crosswalk coverage: how many substances were matched")
    lines.append("% between each pair of taxonomies, and how many distinct groups")
    lines.append("% on each side were involved.")
    lines.append("\\begin{table}[htbp]")
    lines.append("\\centering")
    lines.append("\\caption{Pairwise crosswalk coverage between substance-group taxonomies.}")
    lines.append("\\label{tab:pfas-crosswalk-coverage}")
    lines.append("\\begin{tabular}{llrrr}")
    lines.append("\\toprule")
    lines.append("Source & Target & Matched substances & Source groups & Target groups \\\\")
    lines.append("\\midrule")
    for sheet, (src_col, tgt_col, src_label, tgt_label) in SHEETS.items():
        df = dfs[sheet]
        n_total = int(df["n"].sum())
        n_src = df[src_col].nunique()
        n_tgt = df[tgt_col].nunique()
        lines.append(f"{escape_tex(src_label)} & {escape_tex(tgt_label)} & {n_total} & {n_src} & {n_tgt} \\\\")
    lines.append("\\bottomrule")
    lines.append("\\end{tabular}")
    lines.append("\\end{table}")

    TEX_PATH.write_text("\n".join(lines) + "\n")


def main():
    FIG_DIR.mkdir(parents=True, exist_ok=True)
    dfs = load_all()

    print("Building heatmaps...")
    for sheet, (src_col, tgt_col, src_label, tgt_label) in SHEETS.items():
        out_path, n_rows, n_cols, n_dropped_src, n_dropped_tgt = build_heatmap(
            sheet, dfs[sheet], src_col, tgt_col, src_label, tgt_label
        )
        notes = []
        if n_dropped_src:
            notes.append(f"collapsed {n_dropped_src} small {src_label} groups into 'Other'")
        if n_dropped_tgt:
            notes.append(f"collapsed {n_dropped_tgt} small {tgt_label} groups into 'Other'")
        note = f" ({'; '.join(notes)})" if notes else ""
        print(f"  {out_path.relative_to(ROOT)}  ({n_rows}x{n_cols}){note}")

    print("Building summary tables...")
    build_summary_tables(dfs)
    print(f"  {TEX_PATH.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
