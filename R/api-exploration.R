# ---------------------------------------------------------------------------
# estilex — empirical exploration of the Riigi Teataja public API
# ---------------------------------------------------------------------------
#
# A record of how the API was mapped. Not part of the package; excluded via
# .Rbuildignore.
#
# The API replaced the previous technical solution on 1 June 2026. It is
# UNDOCUMENTED: there is no OpenAPI specification on either the production or
# the beta host, and every endpoint below was found by probing. That means any
# of them may change without notice, which is the single largest risk to this
# package. A message was sent to ert@riigiteataja.ee asking which endpoints
# are supported.
#
# Explored: August–September 2026
# Author:   Kristian Vepsäläinen
#
# Base:     https://www.riigiteataja.ee/public-api/api/v1
# Bulk:     https://www.riigiteataja.ee/avaandmed/ERT/
# Terms:    Riigi Teataja Act § 12 — published texts must cite the Riigi
#           Teataja version when reproduced elsewhere.
#
# ---------------------------------------------------------------------------

library(httr2)
library(xml2)

UA   <- "estilex R package (exploration)"
BASE <- "https://www.riigiteataja.ee/public-api/api/v1"

# ---------------------------------------------------------------------------
# 1. Five parsing hazards — read this before writing any code
# ---------------------------------------------------------------------------
#
# The service returns correct data with incorrect metadata in five separate
# ways. Each has to be worked around explicitly.
#
# (a) XML is served as Content-Type: application/octet-stream, not
#     application/xml. httr2 refuses to parse it, so resp_body_xml() needs
#     check_type = FALSE. A consequence of serving files from S3.
#
# (b) The namespace declaration is not an absolute URI:
#       Estonian acts:  xmlns="tyviseadus_1_10.02.2010"
#       translations:   xmlns="Juurakt"
#     libxml2 warns, and the value appears to vary by document and schema
#     version, so no fixed prefix can be written into XPath. Call
#     xml2::xml_ns_strip() immediately after parsing and use plain XPath.
#
# (c) JSON and XML expose DIFFERENT fields for the same document. The link
#     from a translation back to its Estonian original exists in JSON
#     (tolkeSeosId) but not in XML. Never infer one from the other.
#
# (d) Date formats differ between languages for the same field:
#       Estonian:    kehtivus = "2026-03-01+02:00"
#       translation: kehtivus = "2026-03-01"
#     substr(x, 1, 10) before as.Date() handles both.
#
# (e) The `versioon` field in translation metadata contains "12022-12-23",
#     apparently a version number and a date concatenated without a
#     separator. Treat as opaque; do not attempt to parse.

fetch_xml <- function(id) {
  x <- request(paste0(BASE, "/akt/", id, "/xml")) |>
    req_user_agent(UA) |>
    req_perform() |>
    resp_body_xml(check_type = FALSE, encoding = "UTF-8")
  xml_ns_strip(x)
  x
}

fetch_json <- function(id) {
  request(paste0(BASE, "/akt/", id)) |>
    req_user_agent(UA) |>
    req_perform() |>
    resp_body_json()
}

# ---------------------------------------------------------------------------
# 2. Endpoints
# ---------------------------------------------------------------------------
#
# Confirmed working:
#   GET /akt/{id}                  -> JSON metadata
#   GET /akt/{id}/xml              -> XML content and metadata
#   GET /akt/{id}/redaktsioonid    -> list of consolidated versions
#   GET /akt/{grupiId}/redaktsioonid  -> same, by group identifier
#
# Confirmed absent (clean 404):
#   /aktid  /otsing  /search  /avalik/*  /grupp/{id}  /eli/{id}
#   /tolge/{id}  /akt/{id}/tolge  /akt/{id}/versioonid
#
# Broken:
#   /akt/{translationId}/xml       -> HTTP 500, empty body
#     The metadata endpoint works for the same identifier and the document
#     exists in the bulk archive, so this looks like a defect. Translation
#     XML must be read from the bulk archive instead.
#
# Note the error format: unknown sub-paths under a valid /akt/{id} return
# YAML rather than JSON —
#     staatus: "Viga"
#     paring: "/v1/akt/111042025003/json"
#     vead: ["Tundmatu päring"]
# whereas wholly unknown paths return an empty body. The difference is a
# useful signal when probing: a YAML error means the route pattern exists.
#
# NO ENUMERATION OR SEARCH ENDPOINT EXISTS. Identifiers must come from the
# bulk archives (section 5). The website's own Angular front end calls
# /public-api/api/v1/... paths including something like
# `tulemus?tekstNumber=...`, but those were not reachable under the paths
# tried here.

# ---------------------------------------------------------------------------
# 3. Identifiers
# ---------------------------------------------------------------------------
#
# Format: {type:1}{day:2}{month:2}{year:4}{sequence:3}, twelve digits.
#
#   111042025003  ->  1 | 11 | 04 | 2025 | 003   Estonian, 11 Apr 2025, #3
#   523042025002  ->  5 | 23 | 04 | 2025 | 002   translation
#
# The leading digit distinguishes language: 1 = Estonian, 5 = translation.
# All 6,391 files in the English bulk archive begin with 5.
#
# BUT the format is NOT universal. Older consolidated versions use short
# sequential numbers in the same identifier space:
#
#   redaktsioonid for the Constitution (group 151381):
#     111042025003  111042025002  115052015002  127042011002
#     12846827      633949        24304
#
# So identifiers must be handled as CHARACTER, never coerced to a fixed
# width. They do fit within double precision (12 digits < 2^53), so
# format(x, scientific = FALSE) is sufficient when reading JSON — no
# bigint handling is needed — but they are identifiers, not numbers.
#
# Two identifier spaces coexist:
#   kehtivId / act id       — a specific consolidated version
#   grupiId / terviktekstiGrupiID — the act across all its versions
#
# Both work in /akt/{id}/redaktsioonid.
#
# aktViide inside muutmismarge uses the SAME space as act ids (verified:
# /akt/550572 resolves to "Eesti Vabariigi põhiseaduse muutmise seadus...").
# The citation network is therefore traversable without a lookup table.

# ---------------------------------------------------------------------------
# 4. JSON metadata
# ---------------------------------------------------------------------------

j <- fetch_json("111042025003")   # the Constitution

# Top level:
#   kehtivId        current version identifier
#   grupiId         act identifier across versions
#   tekstiliik      "terviktekst" = consolidated text
#   dokumentliik    "seadus" = act
#   aktiStaatus     "KEHTIV" = in force
#   tolkeSeosId     identifier of the translation
#   tolkeSeosTekst  title of the linked document
#   viited          related materials (NOT cross-references — see below)
#   liigitused      systematic classification, e.g. "SÜSTJAOT¤RIIGI¤PÕHI"
#
# aktiParameetrid:
#   pealkiri, lyhend            title and abbreviation ("PS")
#   kehtivuseAlgus / -Lopp      period of validity
#   avaldamiseKuupaev           publication date
#   staatus                     "KEHTIVAD_KEHTETUTETA"
#   aktideAadress, lisaAadress, pildiAadress, htmlLisaAadress
#
# CORRECTION: `viited` is NOT a cross-reference list. For the Constitution it
# holds annotated edition, constitutional assembly materials, Supreme Court
# analyses, and translations into Ukrainian and Russian:
#   KOMVA, PSA, PSK, RKAN, TUK, TVK
# Cross-references between acts live in the XML, as muutmismarge (section 6).

# tolkeSeosId is bidirectional --------------------------------------------
#
# Estonian  111042025003 -> tolkeSeosId 523042025002
# English   523042025002 -> tolkeSeosId 111042025003
#                           tolkeSeosTekst "Eesti Vabariigi põhiseadus (PS)"
#
# An earlier conclusion that the link was one-way was wrong: it was drawn
# from XML metadata, which lacks the field. JSON has it in both directions.
#
# tolkeSeosId is a VERSION identifier, not a group identifier, so it changes
# whenever the translation is updated. Never cache it.

# Translations carry less metadata -----------------------------------------
#   viited      NULL
#   liigitused  empty
#   aktideAadress and the other address fields  NULL
# Related materials and classifications must be read from the Estonian text.

# Whether a translation is current -----------------------------------------
#
# Compare kehtivuseAlgus, not avaldamiseKuupaev. For the Constitution the
# publication dates differ by twelve days (2025-04-10 vs 2025-04-22) while
# kehtivuseAlgus is identical (2026-02-28), i.e. the translation is current.
#
# Translations carry no version information of their own, so this comparison
# is the only way to tell. A translation may lag the Estonian text, and has
# no official legal force.

# ---------------------------------------------------------------------------
# 5. Bulk archives
# ---------------------------------------------------------------------------
#
# https://www.riigiteataja.ee/avaandmed/ERT/ is an open directory listing:
#
#   xml.2026.zip      42.6 GB     xml.2026.en.zip   1.06 GB
#   xml.2025.zip       2.64 GB    xml.2025.en.zip   82 MB
#   xml.2024.zip     344 MB       xml.2024.en.zip   41 MB
#   MD5SUMS, SHA1SUMS, SHA256SUMS, LOEMIND.txt
#
# Filenames are identifiers: 501062023002.xml etc. This is the ONLY route to
# an identifier list, since the API has no enumeration endpoint.
#
# ARCHIVE SEMANTICS — the important finding:
#
#   xml.2024.en.zip     81 files, years 2020–2024
#   xml.2026.en.zip  6,391 files, years 2013–2026
#   intersect(ids2024, ids2026) -> 0
#
# No overlap at all. Since translations receive a NEW identifier on every
# update, and the identifier encodes that version's publication date, each
# archive is a complete snapshot at its own point in time rather than a set
# of that year's documents.
#
# Confirmed via group identifiers: group 100515 (State Fees Act) appears in
# both archives under different act identifiers. So the GROUP persists while
# the act identifier changes.
#
# CONSEQUENCE: download only the most recent archive. Older ones are
# historical snapshots, useful only for studying how translations changed.
#
# The 42.6 GB Estonian archive for 2026 is presumably inflated by the June
# system migration touching every document. Not yet verified.

# ---------------------------------------------------------------------------
# 6. XML structure
# ---------------------------------------------------------------------------

d <- fetch_xml("111042025003")

xml_name(xml_root(d))              # "oigusakt"
xml_name(xml_children(d))
#  metaandmed, aktinimi, muutmismarge (x6), sisu
#
# NOTE the muutmismarge count here is misleading: xml_children() sees only
# the document-level ones. //muutmismarge finds 16 in this document, because
# ten more sit inside individual sections. See section 7.

# metaandmed --------------------------------------------------------------
#   valjaandja, dokumentLiik, tekstiliik, lyhend, marksona, dokumentEtapp,
#   dokumentStaatus, vastuvoetud, avaldamismarge, kehtivus, skeemiNimi,
#   globaalID, metaandmedVersioon, metaandmedVersioonKuupaev,
#   metaandmedVersioonPohjustaja, terviktekstiGrupiID
#
# Translations have a slightly different set: no marksona, but versioon and
# eesmark instead.
#
#   globaalID       = the act identifier, NOT an ELI URI
#   marksona        = keyword ("põhiseadus"); only one on the Constitution,
#                     may be multiple elsewhere — worth checking
#   eesmark         = schema description, identical across documents; not a
#                     document property despite the name ("purpose")
#   avaldamismarge  = "RT19922634924304", the Riigi Teataja publication mark

# ELI ----------------------------------------------------------------------
#
# ELI is NOT in the API. globaalID is the act identifier.
#
# But ELI URIs still resolve on the website:
#   https://www.riigiteataja.ee/eli/111042025003            -> 200
#   https://www.riigiteataja.ee/en/eli/111042025003/consolide -> 200
#   /public-api/api/v1/eli/111042025003                     -> 404
#
# and the page declares prefix="eli: http://data.europa.eu/eli/ontology#",
# so the metadata exists as RDFa in the presentation layer.
#
# For lexverse this means ELI is derivable but not first-class here. estilex
# can expose an `eli` column constructed from the identifier.

# sisu ---------------------------------------------------------------------
#
# Element counts for the Constitution:
#   paragrahv 168, loige 324, alampunkt 102, peatykk 15, sisuTekst 427,
#   tavatekst 427, muutmismarge 10, aktViide 10
#
# Hierarchy:
#   sisu
#   ├── sisuTekst              preamble
#   └── peatykk [id]           chapter
#       ├── peatykkNr
#       ├── peatykkPealkiri
#       └── paragrahv [id]     section
#           ├── paragrahvNr    "1"
#           ├── kuvatavNr      CDATA, formatted: "§ 1."
#           └── loige [id]     subsection
#               └── sisuTekst
#                   └── tavatekst
#
# alampunkt / alampunktNr are sub-points; reavahetus is a line break; sup
# appears in translations only (superscript).
#
# Cleaner than the Danish equivalent: the marker lives in its own element,
# so there is no run-together problem of the kind that required a custom
# renderer in danlex.

# ---------------------------------------------------------------------------
# 7. muutmismarge — amendment history
# ---------------------------------------------------------------------------
#
# Fields (stable across three documents):
#   aktikuupaev                          date of the amending act
#   avaldamismarge/RTosa                 "RT I"
#   avaldamismarge/RTaasta               year
#   avaldamismarge/RTnr                  issue number
#   avaldamismarge/RTartikkel            article number
#   avaldamismarge/aktViide              identifier of the amending act
#   joustumine                           entry into force
#
# Example text content: "2003-02-25+02:00 RT I 2003 29 174 550572 2005-10-17"
#
# TWO LEVELS. Of the sixteen in the Constitution, seven sit at document level
# and nine are attached to specific sections:
#
#   scope <- ancestor::paragrahv/paragrahvNr
#   table(scope): 127(1) 128(1) 156(2) 65(1) 74(1) 78(3) NA(7)
#
# So a references function needs a `scope` column giving the section number,
# or NA for document-level entries. Same ancestor-axis pattern as danlex's
# is_amendment.
#
# Amending acts have NO muutmismarge of their own: /akt/550572 returned none.

# ---------------------------------------------------------------------------
# 8. Version history
# ---------------------------------------------------------------------------

# GET /akt/{grupiId}/redaktsioonid
#   -> list(redaktsioonid = [...], kehtivRedaktsioon = <id>)
#
# Each entry: id, kehtivuseAlgus, tavatekstAlgus, kehtivKehtetus,
#             mitteJoustunud, onHetkelKehtivKuvada, avaldamiseKuupaev
#
# The Constitution (group 151381) has seven versions, 2002 to 2026.
#
# /akt/{id}/redaktsioonid returned an EMPTY list for the act identifier while
# the group identifier returned the full list — so use grupiId.
#
# /akt/{id}/redaktsioonid?kehtivus=koik returns 200, so the endpoint accepts
# parameters, but the vocabulary is unknown. Asked in the message to
# ert@riigiteataja.ee.
#
# This is the feature no other lexverse source offers: point-in-time queries
# over the text of the law.

# ---------------------------------------------------------------------------
# 9. Parallel corpus — the strongest finding
# ---------------------------------------------------------------------------
#
# Structural counts, Constitution, Estonian vs English:
#
#   paragrahv      168  vs  168      exact
#   alampunkt      102  vs  102      exact
#   peatykk         15  vs   15      exact
#   muutmismarge    10  vs   10      exact
#   loige          324  vs  323      one difference
#
# And paragrahvNr vectors are IDENTICAL:
#   identical(nr_et, nr_en) -> TRUE
#
# The single loige difference is in section 168, the last one — presumably a
# closing provision formatted differently. Estonian has 9 reavahetus to the
# translation's 7, and the translation has 3 sup elements the Estonian lacks.
#
# CONSEQUENCE: sections can be aligned across languages using paragrahvNr,
# safely and exactly. Subsection alignment must tolerate differing counts.
#
# No other source in lexverse offers a section-aligned parallel corpus. This
# is worth a function of its own.

# ---------------------------------------------------------------------------
# 10. Decisions
# ---------------------------------------------------------------------------
#
# Prefix:      etx_   (elx_ belongs to eurlex; et is Estonia's language code)
# Package:     estilex
#
# Identifiers: CHARACTER throughout
# Parsing:     resp_body_xml(check_type = FALSE) then xml_ns_strip()
# Dates:       substr(x, 1, 10) before as.Date()
# Metadata:    read JSON and XML separately; neither is a superset
# Enumeration: bulk archive only; download the newest, ignore older ones
# Translations: XML from the archive (API returns 500); metadata from JSON
#
# v1 scope:
#   etx_get_doc()          JSON metadata
#   etx_get_text()         rendered running text
#   etx_get_paragraphs()   one row per subsection
#   etx_get_references()   muutmismarge as an edge table, with scope
#   etx_get_translation()  via tolkeSeosId
#   etx_get_parallel()     section-aligned Estonian and English
#   etx_download_corpus()  bulk archive into the session cache
#   etx_list_documents()   identifiers from the archive
#
# Deferred: etx_get_versions(). The endpoint works without parameters and
# would suffice, but the parameter vocabulary is unknown. Decide at release:
# include it if Riigi Teataja has replied, otherwise ship without and add in
# the next version.
