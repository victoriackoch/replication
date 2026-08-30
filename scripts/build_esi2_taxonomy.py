import re
import openpyxl

ESI2_XLSX = "/root/.claude/uploads/9c3a0553-6723-59bf-af99-2ae863779397/7c12dc56-ESI2.xlsm"
OUT_XLSX = "output/pfas_esi2_taxonomy_table.xlsx"

DATA_SHEETS = [
    "PFAAs", "PFPIA-based", "PASF-based", "PACF-based", "Fluorotelomer-based",
    "Cyclic PFAS", "Other Nonpolymers", "Side-chain fluorinated aromatic",
    "Per- and polyfluoroalkyl ether ", "Hydrofluoroether", "Non-polymers_Polymers",
    "Fluoropolymer", "Side-chain fluorinated polymers", "Perfluoropolyether",
]

# sheet name -> the group label used on the "Overview" sheet
GROUP_LABEL = {
    "PFAAs": "Perfluoroalkyl acids (PFAAs)",
    "PFPIA-based": "Perfluoroalkyl phosphinic acids (PFPIA)-based substances",
    "PASF-based": "Perfluoroalkane sulfonyl fluoride (PASF)-based substances",
    "PACF-based": "Perfluoroalkyl carbonyl fluoride (PACF)-based substances",
    "Fluorotelomer-based": "Fluorotelomer-based substances",
    "Cyclic PFAS": "Cyclic PFAS",
    "Other Nonpolymers": "Other Nonpolymers",
    "Side-chain fluorinated aromatic": "Aromatics with fluorinated side-chains",
    "Per- and polyfluoroalkyl ether ": "Per- and polyfluoroalkyl ether",
    "Hydrofluoroether": "Hydofluoroether",
    "Non-polymers_Polymers": "Non-polymers? Polymers?",
    "Fluoropolymer": "Fluoropolymers",
    "Side-chain fluorinated polymers": "Side-chain fluorinated polymers",
    "Perfluoropolyether": "Perfluoropolyether",
}

NAME_PAREN_RE = re.compile(r"^(.*?)\s*\(([^()]+)\)\s*$")

def split_name(name):
    if not name:
        return name, None
    name = str(name).strip()
    m = NAME_PAREN_RE.match(name)
    if m:
        return m.group(1).strip(), m.group(2).strip()
    return name, None

wb_in = openpyxl.load_workbook(ESI2_XLSX, data_only=True)

wb_out = openpyxl.Workbook()
ws_out = wb_out.active
ws_out.title = "ESI-2 Taxonomy"
headers = ["group", "header", "acronym", "name", "abbreviation", "chemical_formula",
           "mono_isotopic_mass", "cas_number", "trade_names", "oecd_inclusion"]
ws_out.append(headers)

total = 0
for sheet in DATA_SHEETS:
    ws = wb_in[sheet]
    group = GROUP_LABEL.get(sheet, sheet)
    current_header = None
    for r in range(2, ws.max_row + 1):
        col_a = ws.cell(row=r, column=1).value
        col_b = ws.cell(row=r, column=2).value  # Name
        if col_b is None or str(col_b).strip() == "":
            # header/description row for the current subgroup block
            if col_a is not None and str(col_a).strip() != "":
                current_header = str(col_a).strip()
            continue
        acronym = ws.cell(row=r, column=1).value
        name_raw = str(col_b).strip()
        name, abbrev = split_name(name_raw)
        formula = ws.cell(row=r, column=5).value
        mass = ws.cell(row=r, column=6).value
        cas = ws.cell(row=r, column=7).value
        trade_names = ws.cell(row=r, column=9).value
        oecd = ws.cell(row=r, column=10).value
        ws_out.append([
            group, current_header,
            str(acronym).strip() if acronym is not None else None,
            name, abbrev, formula, mass,
            str(cas).strip() if cas is not None else None,
            trade_names, oecd,
        ])
        total += 1

widths = [30, 45, 18, 45, 16, 20, 14, 16, 25, 16]
for i, w in enumerate(widths, start=1):
    ws_out.column_dimensions[ws_out.cell(row=1, column=i).column_letter].width = w
ws_out.freeze_panes = "A2"

notes = wb_out.create_sheet("Notes")
notes.append(["Built from ESI2.xlsm."])
notes.append(["'group' = the substance-family sheet the row came from (matches the 'Group' column on the 'Overview' sheet)."])
notes.append(["'header' = the subgroup label/description printed above that row's data block on its source sheet"])
notes.append(["(e.g. 'Perfluoroalkyl carboxylic acids (PFCAs)   CnF2n+1COOH' on the PFAAs sheet)."])
notes.append(["'name'/'abbreviation' = the sheet's 'Name' column split on a single trailing parenthetical, e.g."])
notes.append(["'Perfluorobutanoic acid (PFBA)' -> name 'Perfluorobutanoic acid', abbreviation 'PFBA'."])
notes.append(["Rows with no parenthetical in Name have a blank abbreviation."])
notes.column_dimensions["A"].width = 110

wb_out.save(OUT_XLSX)
print("rows:", total)
print("wrote", OUT_XLSX)
