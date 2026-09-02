# PFAS structural classifier per the ECHA Annex XVII restriction dossier
# definition:
#
#   TRIGGER: any substance with >=1 fully-fluorinated methyl (CF3-) or
#   methylene (-CF2-) carbon (no H/Cl/Br/I on that carbon).
#
#   CARVE-OUT: a substance is excluded if it contains ONLY:
#     CF3-X          where X  = -OR or -NRR'
#     X-CF2-X'       where X  = -OR or -NRR'
#                       and X' = -CH3, -CH2-, aromatic, -C(O)-, -OR'', -SR'', -NR''R'''
#   with every R/R'/R''/R''' in {-H, -CH3, -CH2-, aromatic, -C(O)-}.
#
# Method: find every CF3/CF2 carbon (the "trigger" atoms), then check each
# one individually against the carve-out's allowed local environment. If
# ALL trigger atoms found are carve-out-exempt, the substance is NOT a
# PFAS under this definition; if the substance has no trigger atom at all,
# it's trivially not a PFAS either; otherwise it IS a PFAS.
#
# Requires: rdkit (pip install rdkit)

from rdkit import Chem

# ---- Rule A: trigger atoms -------------------------------------------------
# CF3-: sp3 C, no H, exactly 3 F neighbours (4th bond unconstrained).
CF3_SMARTS = Chem.MolFromSmarts("[CX4H0](F)(F)F")
# -CF2-: sp3 C, no H, exactly 2 F neighbours, and its other two neighbours
# are neither halogen (Cl/Br/I) nor H -- i.e. genuinely "fully fluorinated"
# on this carbon with nothing else halogenated hanging off it.
CF2_SMARTS = Chem.MolFromSmarts("[CX4H0](F)(F)([!F;!Cl;!Br;!I])[!F;!Cl;!Br;!I]")

# ---- Rule B: carve-out allowed neighbourhoods ------------------------------
# R / R' / R'' / R''' allowed on the O or N attached to the fluorinated
# carbon: H, methyl, methylene(chain), aromatic ring atom, or a carbonyl
# carbon. Expressed as the allowed atom directly bonded to that O/N.
_R = "$([H]),$([CH3]),$([CH2]),$(c),$([CX3]=O)"

# CF3-X, X = -O-R or -N(R)(R'): the CF3 carbon's 4th (non-F) neighbour is an
# O or N, and every OTHER neighbour of that O/N (besides the CF3 carbon
# itself) is one of the allowed R groups.
CF3_EXEMPT_SMARTS = Chem.MolFromSmarts(
    f"[CX4H0](F)(F)(F)[$([OX2]([{_R}])),$([NX3]([{_R}])[{_R}])]"
)

# X-CF2-X': one side (X) is -O-R or -N(R)(R'); the other side (X') is
# methyl, methylene, aromatic, carbonyl, -O-R'', -S-R'', or -N(R'')(R''').
_X_SIDE = f"[$([OX2]([{_R}])),$([NX3]([{_R}])[{_R}])]"
_XPRIME_SIDE = f"[$([CH3]),$([CH2]),$(c),$([CX3]=O),$([OX2][{_R}]),$([SX2][{_R}]),$([NX3][{_R}][{_R}])]"
CF2_EXEMPT_SMARTS = Chem.MolFromSmarts(f"{_X_SIDE}[CX4H0](F)(F){_XPRIME_SIDE}")


def classify(smiles):
    """Returns (is_pfas: bool|None, detail: str). is_pfas is None if the
    SMILES couldn't be parsed."""
    mol = Chem.MolFromSmiles(smiles)
    if mol is None:
        return None, "could not parse SMILES"

    cf3_atoms = {m[0] for m in mol.GetSubstructMatches(CF3_SMARTS)}
    cf2_atoms = {m[0] for m in mol.GetSubstructMatches(CF2_SMARTS)}
    trigger_atoms = cf3_atoms | cf2_atoms

    if not trigger_atoms:
        return False, "no fully-fluorinated CF3/CF2 carbon found"

    cf3_exempt = {m[0] for m in mol.GetSubstructMatches(CF3_EXEMPT_SMARTS)}
    cf2_exempt = {m[0] for m in mol.GetSubstructMatches(CF2_EXEMPT_SMARTS, useChirality=False)}
    # CF2_EXEMPT_SMARTS's matched atom 0 is the first X-side atom, not the
    # carbon -- re-match to pull out the carbon index specifically.
    cf2_exempt_carbons = set()
    for match in mol.GetSubstructMatches(CF2_EXEMPT_SMARTS):
        for idx in match:
            if idx in cf2_atoms:
                cf2_exempt_carbons.add(idx)

    exempt_atoms = cf3_exempt | cf2_exempt_carbons
    non_exempt = trigger_atoms - exempt_atoms

    if non_exempt:
        return True, f"{len(non_exempt)} of {len(trigger_atoms)} CF3/CF2 carbon(s) not carve-out-exempt"
    return False, f"all {len(trigger_atoms)} CF3/CF2 carbon(s) fall under the carve-out"


def classify_csv(in_path, out_path, smiles_col="SMILES"):
    """Reads a CSV with a SMILES column, appends 'is_pfas_echa_definition'
    and 'classification_detail' columns, writes the result."""
    import csv
    with open(in_path, newline="") as f:
        rows = list(csv.DictReader(f))
    if not rows:
        raise ValueError("input CSV has no rows")
    if smiles_col not in rows[0]:
        raise ValueError(f"column {smiles_col!r} not found; available: {list(rows[0])}")

    for row in rows:
        is_pfas, detail = classify(row[smiles_col])
        row["is_pfas_echa_definition"] = "" if is_pfas is None else str(is_pfas)
        row["classification_detail"] = detail

    fieldnames = list(rows[0].keys())
    with open(out_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)
    n_pfas = sum(1 for r in rows if r["is_pfas_echa_definition"] == "True")
    n_unparsed = sum(1 for r in rows if r["is_pfas_echa_definition"] == "")
    print(f"{len(rows)} rows -> {n_pfas} classified PFAS, "
          f"{len(rows) - n_pfas - n_unparsed} not PFAS, {n_unparsed} unparsed SMILES")


if __name__ == "__main__":
    import sys
    if len(sys.argv) >= 3:
        # usage: python3 pfas_smarts_classifier.py in.csv out.csv [smiles_column_name]
        classify_csv(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else "SMILES")
        raise SystemExit

    tests = [
        # (name, smiles, expected_is_pfas)
        ("PFOA (perfluorooctanoic acid)", "OC(=O)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)F", True),
        ("PFOS (perfluorooctane sulfonic acid)", "OS(=O)(=O)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)F", True),
        ("PTFE repeat unit (as a small fragment, CF3-CF2-CF3)", "FC(F)(F)C(F)(F)F", True),
        ("Trifluoromethyl phenyl ether (Ar-O-CF3)", "FC(F)(F)Oc1ccccc1", False),
        ("Trifluoromethoxy-aniline (Ar-N direct, CF3 on N side check)", "FC(F)(F)Oc1ccc(N)cc1", False),
        ("Sevoflurane (CF3 attached to carbon, not O/N)", "FCOC(C(F)(F)F)C(F)(F)F", True),
        ("HFPO-DA / GenX anion", "OC(=O)C(F)(F)OC(F)(F)C(F)(F)F", True),
        ("Trifluoroacetamide (CF3-C(=O)-NH2, amide carbon not exempt path)", "FC(F)(F)C(N)=O", True),
        ("2,2,2-Trifluoroethanol (CF3-CH2-OH, CF3 attached to carbon not O/N)", "FC(F)(F)CO", True),
        ("Methyl trifluoromethyl ether (CF3-O-CH3)", "FC(F)(F)OC", False),
        # each CF3's N-neighbour is the OTHER CF3 group -- not a valid R
        # (R must be plain -CH3/H/aromatic/carbonyl, not another CF3), so
        # neither CF3 qualifies for the carve-out: correctly a trigger.
        ("Bis-trifluoromethyl amine (CF3-NH-CF3, R on N is another CF3, not a valid R)", "FC(F)(F)NC(F)(F)F", True),
        ("Difluoromethylene ether bridging two methyls (CH3-O-CF2-O-CH3)", "COC(F)(F)OC", False),
        ("Perfluorohexane (no heteroatoms at all, plain PFC)", "FC(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)C(F)(F)F", True),
        ("Trifluoromethylbenzene aromatic-only F (no aliphatic CF3/CF2 beyond the one CF3)", "FC(F)(F)c1ccccc1", True),
        ("Hexafluorobenzene (fully aromatic C-F, no CF3/CF2 at all)", "Fc1c(F)c(F)c(F)c(F)c1F", False),
    ]
    ok = 0
    for name, smi, expected in tests:
        is_pfas, detail = classify(smi)
        status = "OK" if is_pfas == expected else "MISMATCH"
        if status == "OK":
            ok += 1
        print(f"[{status}] {name}: got={is_pfas} expected={expected} ({detail})")
    print(f"\n{ok}/{len(tests)} passed")
