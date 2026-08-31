# Shared text-matching helper for the taxonomy-match / gap-analysis
# scripts. Two independent weaknesses showed up when spot-checking the
# first pass of matches:
#
# 1. Plain token_set_ratio penalizes a short query against a long,
#    wordy candidate even when every one of the query's real words is a
#    literal substring match ("Fc-600f Light Water Brand Atc/Afff" vs
#    "Aqueous film-forming foams (AFFF)" scored 27, though "Afff" is a
#    literal hit) -- fixed by also trying partial_token_set_ratio, which
#    is far more tolerant of one side carrying extra noise (brand names,
#    batch/part numbers), and taking the best of the two.
# 2. Genuine vocabulary mismatches never score well under ANY token
#    metric -- "Socks [10001348]" and "Non-leather indoor wear
#    (clothing)" share not one word -- fixed by expanding both sides
#    with a small domain synonym dictionary before scoring, so a
#    clothing-item word and a general "clothing/apparel" phrase land in
#    the same canonical vocabulary.
#
# Both fixes are applied together: the query and every candidate are
# expanded with SYNONYM_GROUPS, then scored as
# max(token_set_ratio, partial_token_set_ratio) on the expanded text.

import re
from rapidfuzz import fuzz, process

# Each group: member words/phrases (matched case-insensitively, whole
# word/phrase) that all mean roughly the same thing in this domain -->
# every member gets the group's canonical tag string appended when found,
# so two differently-worded members of the same group share vocabulary
# after expansion. Grounded in the actual weak-match cases found across
# the product/Gaines/Consumer-Goods matchings, plus common PFAS-chemistry
# abbreviation expansions.
SYNONYM_GROUPS = [
    # clothing / apparel
    (["sock", "hosiery", "stocking", "pantyhose", "shirt", "blouse", "polo shirt",
      "t-shirt", "tshirt", "top", "dress", "skirt", "jacket", "blazer", "cardigan",
      "waistcoat", "coat", "sweater", "pullover", "pant", "trouser", "short",
      "brief", "undershort", "overall", "bodysuit", "belt", "brace", "cummerbund",
      "bib", "sleepwear", "costume", "glove", "hat", "apparel", "garment",
      "textile", "clothing", "wear", "fabric", "outdoor wear", "indoor wear",
      "blanket", "throw", "upholstery", "bedding"],
     "clothing apparel wear textile fabric garment"),

    # personal care / cosmetics
    (["antiperspirant", "deodorant", "deo", "dry spray", "hairspray", "hair spray",
      "mousse", "sanitizer", "shampoo", "cosmetic", "perfume", "fragrance",
      "lotion", "personal care", "shave gel", "shaving gel", "shaving cream"],
     "personal care cosmetic deodorant spray"),

    # industrial cleaning / degreasing
    (["degreaser", "degreasing", "cleaner", "cleaning", "duster", "dust off",
      "dust-off", "canned air", "compressed air", "solvent", "detergent"],
     "cleaning solvent degreasing spray"),

    # fire safety / firefighting foam
    (["afff", "aqueous film forming foam", "aqueous film-forming foam",
      "fire extinguisher", "fire-extinguisher", "firefighting", "fire fighting",
      "fire-fighting", "halon", "fire suppression", "extinguishing",
      "extinguisher", "extinguishant", "light water"],
     "fire firefighting extinguishing foam suppression"),

    # pest control
    (["insecticide", "pesticide", "ant bait", "termite", "insect", "pest",
      "pest control"],
     "pesticide insecticide pest control agricultural"),

    # battery
    (["battery", "batteries", "cell", "electrode", "cathode", "rechargeable",
      "non-rechargeable"],
     "battery"),

    # electronics soldering / flux
    (["flux", "defluxer", "flux stripper", "solder", "soldering", "pcb",
      "semiconductor", "circuit board"],
     "flux solder electronics semiconductor"),

    # refrigerant / mechanical lubricant
    (["pag oil", "refrigerant", "lubricant", "lube", "grease", "compressor oil",
      "o-ring conditioner"],
     "refrigerant lubricant oil"),

    # mold release
    (["mold release", "mould release", "release agent", "demolding", "demoulding"],
     "mold release agent"),

    # food contact / paper packaging
    (["food contact", "cellulosic", "paperboard", "paper and board",
      "packaging"],
     "food contact packaging paper board"),

    # foam insulation
    (["styrofoam", "polystyrene", "insulation", "foam board", "xps"],
     "foam insulation board"),

    # PFAS chemistry abbreviation expansions (useful across all three source lists)
    (["ptfe", "polytetrafluoroethylene", "teflon"], "ptfe polytetrafluoroethylene"),
    (["pfoa", "perfluorooctanoic acid", "perfluorooctanoate"], "pfoa perfluorooctanoic acid"),
    (["pfos", "perfluorooctane sulfonate", "perfluorooctanesulfonic acid",
      "perfluorooctane sulfonic acid"], "pfos perfluorooctane sulfonate"),
    (["pvdf", "polyvinylidene fluoride"], "pvdf polyvinylidene fluoride"),
    (["etfe", "ethylene tetrafluoroethylene"], "etfe ethylene tetrafluoroethylene"),
    (["fep", "fluorinated ethylene propylene"], "fep fluorinated ethylene propylene"),
    (["hfc", "hydrofluorocarbon"], "hfc hydrofluorocarbon"),
    (["hfe", "hydrofluoroether"], "hfe hydrofluoroether"),
    (["hfo", "hydrofluoroolefin", "hydrofluoroleofin"], "hfo hydrofluoroolefin"),
    (["pfc", "perfluorocarbon"], "pfc perfluorocarbon"),
    (["stain resistant", "stain resist", "soil resistant", "repellent", "repellant"],
     "stain repellent finish coating"),
]

# pre-compile: (compiled whole-word/phrase regex, canonical tag) pairs.
# "s?" after each member tolerates a plain plural ("Socks", "Dresses",
# "Batteries" needs its own entry since -y -> -ies isn't a plain "s") --
# these lists are a mix of singular category labels and plural product
# names, and matching only the exact form in the dictionary would miss
# half of them.
_COMPILED = [
    (re.compile(r"\b(" + "|".join(re.escape(m) for m in members) + r")(es|s)?\b", re.IGNORECASE), tag)
    for members, tag in SYNONYM_GROUPS
]


# underscores and slashes are common field-separators in these source
# files ("Extinguisher_halon", "Atc/Afff") but count as \w characters, so
# they'd otherwise glue two real words together and hide a \b boundary
# the synonym regexes need to see.
_SEPARATOR_RE = re.compile(r"[_/]")


def expand(text):
    """Append canonical synonym tags for every recognized group found in text."""
    if not text:
        return text
    scan_text = _SEPARATOR_RE.sub(" ", text)
    tags = []
    for rx, tag in _COMPILED:
        if rx.search(scan_text):
            tags.append(tag)
    if not tags:
        return text
    return text + " | " + " ".join(tags)


def top_matches_batch(queries, choices, top_n=3):
    """For each query in `queries`, returns the top_n (original_choice_text,
    score) pairs among `choices`, scored with token_set_ratio on
    synonym-expanded text (vectorized via process.cdist).

    partial_token_set_ratio was tried as a second signal to catch a short
    query buried in a long, noisy candidate string, but rejected: on
    strings this heavy with digits/punctuation (part numbers, batch
    codes) it produces spurious near-100 scores for genuinely unrelated
    pairs (e.g. a battery product against an unrelated firefighting-foam
    taxonomy entry) whenever a short substring happens to align --
    exactly the kind of false positive this matching is meant to avoid.
    token_set_ratio on synonym-expanded text does not have that failure
    mode and already recovers the literal-overlap cases (AFFF, mold
    release, degreaser, ...) that motivated trying partial ratio at all.
    """
    q_expanded = [expand(q) for q in queries]
    c_expanded = [expand(c) for c in choices]
    m = process.cdist(q_expanded, c_expanded, scorer=fuzz.token_set_ratio)

    out = []
    for row in m:
        ranked = sorted(range(len(row)), key=lambda i: row[i], reverse=True)[:top_n]
        out.append([(choices[i], round(float(row[i]), 1)) for i in ranked])
    return out
