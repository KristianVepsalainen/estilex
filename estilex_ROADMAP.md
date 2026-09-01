# estilex — roadmap

A living design document. Not part of the build (see `.Rbuildignore`).

`estilex` provides access to Estonian legislation via the Riigi Teataja
public API. Part of **lexverse**, alongside `eurlex`, `finlex`, `swelex` and
`danlex`.

- Function prefix: `etx_` (`elx_` belongs to `eurlex`; `et` is Estonia's
  language code)
- API base: `https://www.riigiteataja.ee/public-api/api/v1`
- Bulk: `https://www.riigiteataja.ee/avaandmed/ERT/`

---

## 1. The largest risk

**The API is undocumented.** It replaced the previous system on 1 June 2026,
and there is no OpenAPI specification on either the production or the beta
host. Every endpoint was found by probing, so any of them may change without
notice.

A message was sent to `ert@riigiteataja.ee` on 2026-09-01 reporting three
defects and asking five questions. Until there is a reply, treat every
endpoint as provisional and keep the parsing layer thin enough to adapt.

---

## 2. Endpoints

| Endpoint | Returns |
|---|---|
| `/akt/{id}` | JSON metadata |
| `/akt/{id}/xml` | XML content and metadata |
| `/akt/{grupiId}/redaktsioonid` | list of consolidated versions |

Absent (clean 404): `/aktid`, `/otsing`, `/search`, `/avalik/*`,
`/grupp/{id}`, `/eli/{id}`, `/tolge/{id}`, `/akt/{id}/tolge`.

Broken: `/akt/{translationId}/xml` returns HTTP 500 with an empty body, while
the metadata endpoint for the same identifier works. Translation XML must
come from the bulk archive.

**There is no enumeration or search endpoint.** Identifiers come from the
bulk archives only.

Error format is a useful probing signal: unknown sub-paths under a valid
`/akt/{id}` return YAML (`staatus: "Viga"`, `vead: ["Tundmatu päring"]`),
whereas wholly unknown paths return an empty body. A YAML error means the
route pattern exists.

---

## 3. Five parsing hazards

The service returns correct data with incorrect metadata in five ways. Each
needs an explicit workaround.

1. **XML is served as `application/octet-stream`.** `resp_body_xml()` needs
   `check_type = FALSE`. A consequence of serving from S3.
2. **The namespace is not an absolute URI** — `tyviseadus_1_10.02.2010` for
   acts, `Juurakt` for translations — and varies by document. Call
   `xml_ns_strip()` immediately after parsing, then use plain XPath.
3. **JSON and XML expose different fields.** The link from a translation back
   to its original exists in JSON but not XML. Never infer one from the other.
4. **Date formats differ by language** for the same field: `2026-03-01+02:00`
   in Estonian documents, `2026-03-01` in translations. Take the first ten
   characters before `as.Date()`.
5. **`versioon` is unparseable** — `"12022-12-23"` concatenates a version
   number and a date without a separator. Treat as opaque.

---

## 4. Identifiers

Format `{type:1}{day:2}{month:2}{year:4}{sequence:3}`:

```
111042025003  ->  1 | 11 | 04 | 2025 | 003   Estonian
523042025002  ->  5 | 23 | 04 | 2025 | 002   translation
```

The leading digit distinguishes language: 1 = Estonian, 5 = translation. All
6,391 files in the English archive begin with 5.

**But the format is not universal.** Older consolidated versions use short
sequential numbers in the same space — the Constitution's versions run
`111042025003`, `115052015002`, `127042011002`, `12846827`, `633949`,
`24304`. Handle identifiers as **character**, never as a fixed width.

They do fit in double precision (12 digits < 2^53), so
`format(x, scientific = FALSE)` suffices when reading JSON. No bigint
handling is needed.

Two identifier spaces coexist and both work in `redaktsioonid`:

- `kehtivId` / act id — one consolidated version
- `grupiId` / `terviktekstiGrupiID` — the act across all versions

`aktViide` inside `muutmismarge` uses the same space as act ids, so the
citation network is traversable without a lookup table.

---

## 5. Document structure

```
sisu
├── sisuTekst                  preamble
└── peatykk [id]               chapter
    ├── peatykkNr
    ├── peatykkPealkiri
    └── paragrahv [id]         section
        ├── paragrahvNr        "1"
        ├── kuvatavNr          CDATA, formatted: "§ 1."
        └── loige [id]         subsection
            └── sisuTekst
                └── tavatekst
```

Also `alampunkt`/`alampunktNr` for sub-points, `reavahetus` for line breaks,
`sup` for superscript (translations only).

Cleaner than the Danish equivalent: the marker is in its own element, so
there is no run-together problem requiring a custom renderer.

---

## 6. Parallel corpus — the strongest finding

Structural counts for the Constitution, Estonian against English:

| Element | Estonian | English |
|---|---|---|
| `paragrahv` | 168 | 168 |
| `alampunkt` | 102 | 102 |
| `peatykk` | 15 | 15 |
| `muutmismarge` | 10 | 10 |
| `loige` | 324 | 323 |

And `paragrahvNr` vectors are **identical**. Sections can therefore be
aligned across languages exactly, using the number rather than position.
Subsection alignment must tolerate differing counts — the one discrepancy is
in section 168, presumably a closing provision formatted differently.

No other lexverse source offers a section-aligned parallel corpus. This
deserves a function of its own, and it is the clearest argument for Estonia
in the wider project: it makes translation fidelity measurable.

---

## 7. Amendment history

`muutmismarge` fields, stable across three documents: `aktikuupaev`,
`avaldamismarge/{RTosa, RTaasta, RTnr, RTartikkel, aktViide}`, `joustumine`.

**Two levels.** Of the sixteen in the Constitution, seven are at document
level and nine attach to specific sections (127, 128, 156×2, 65, 74, 78×3).
A references function needs a `scope` column giving the section number, or
`NA` for document-level entries — the same ancestor-axis pattern as
`danlex`'s `is_amendment`.

Amending acts carry no `muutmismarge` of their own.

Note that `viited` in the JSON is **not** a cross-reference list. It holds
related materials: annotated edition, constitutional assembly papers,
Supreme Court analyses, and translations into Ukrainian and Russian.

---

## 8. Translations

`tolkeSeosId` links in **both** directions, and `tolkeSeosTekst` gives the
linked document's title. It is a version identifier, not a group identifier,
so it changes on every translation update — **never cache it**.

Translations carry less metadata: `viited` is `NULL`, `liigitused` is empty,
address fields are `NULL`. Related materials and classifications must come
from the Estonian text.

**Whether a translation is current:** compare `kehtivuseAlgus`, not
`avaldamiseKuupaev`. For the Constitution the publication dates differ by
twelve days while `kehtivuseAlgus` is identical, i.e. the translation is up
to date. Translations carry no version information of their own, so this
comparison is the only signal available.

This must be prominent in the documentation: a translation may lag the
Estonian text and has no official legal force.

---

## 9. Bulk archives

```
xml.2026.zip     42.6 GB     xml.2026.en.zip   1.06 GB
xml.2025.zip      2.64 GB    xml.2025.en.zip   82 MB
xml.2024.zip    344 MB       xml.2024.en.zip   41 MB
```

Filenames are identifiers. This is the only route to an identifier list.

**Archive semantics.** The 2024 English set has 81 files spanning 2020–2024;
the 2026 set has 6,391 spanning 2013–2026; and they share **no identifiers at
all**. Since a translation receives a new identifier on every update, and the
identifier encodes that version's publication date, each archive is a
complete snapshot at its own point in time rather than that year's documents.

Confirmed via group identifiers: group 100515 (State Fees Act) appears in
both archives under different act identifiers.

**Download only the most recent archive.** Older ones are historical
snapshots, useful only for studying how translations changed over time.

---

## 10. ELI

ELI is **not** in the API — `globaalID` is the act identifier, not an ELI URI,
and `/public-api/api/v1/eli/{id}` returns 404.

But ELI URIs resolve on the website (`/eli/{id}`, `/en/eli/{id}/consolide`)
and the page declares `prefix="eli: http://data.europa.eu/eli/ontology#"`, so
the metadata exists as RDFa in the presentation layer.

For lexverse this means ELI is derivable but not first class. `estilex` can
expose an `eli` column constructed from the identifier. Whether it will be
exposed through the API is question 4 to Riigi Teataja.

This weakens, but does not remove, the original strategic case for doing
Estonia before Lithuania. If ELI never reaches the API, `lexcore` rests on
`eurlex` and `danlex`, and Lithuania becomes a more useful next step than a
fourth ELI implementation would have been.

---

## 11. Function plan

### v1

- [ ] `etx_get_doc()` — JSON metadata
- [ ] `etx_get_text()` — rendered running text
- [ ] `etx_get_paragraphs()` — one row per subsection
- [ ] `etx_get_references()` — `muutmismarge` edge table with `scope`
- [ ] `etx_get_translation()` — via `tolkeSeosId`
- [ ] `etx_get_parallel()` — section-aligned Estonian and English
- [ ] `etx_download_corpus()` — bulk archive into the session cache
- [ ] `etx_list_documents()` — identifiers from the archive
- [ ] `etx_cache_status()`, `etx_clear_cache()`

### Deferred to release-time decision

`etx_get_versions()`. `/akt/{grupiId}/redaktsioonid` works without parameters
and returns the full version list, which would suffice. But
`?kehtivus=koik` is accepted and the parameter vocabulary is unknown.

**Decision rule:** include it if Riigi Teataja has replied by the time the
package is ready; otherwise ship without and add in the next version. Point-
in-time queries are the package's most valuable feature and deserve to be
built once, correctly.

### Cache design

Session-scoped, following `danlex`: a package-local environment, nothing
written to disk, `saveRDS()` documented for users who want persistence. The
1.06 GB English archive makes this heavier than the Danish case, so
`etx_download_corpus()` should stream to `tempfile()` and extract only what
is asked for.

---

## 12. Open questions

1. What does the 42.6 GB Estonian 2026 archive contain? Presumably inflated
   by the June migration touching every document, but unverified.
2. `redaktsioonid` parameter vocabulary.
3. Are there multiple `marksona` keywords on other acts? Only one on the
   Constitution.
4. What are the `liigitused` classification codes
   (`SÜSTJAOT¤RIIGI¤PÕHI`)? A systematic subject index would be material no
   other lexverse source has.
5. Does the website's Angular front end expose a search endpoint? Traffic
   showed `tulemus?tekstNumber=...` but the path was not reachable under
   `/public-api/api/v1/`.
6. Which endpoints are supported and stable — the question that matters most.

---

## 13. CRAN pre-flight

- [ ] `httptest2` mocks or synthetic fixtures for offline tests
- [ ] Pre-computed vignette (`.Rmd.orig` + `data-raw/precompute.R`)
- [ ] `urlchecker::url_check()`
- [ ] `usethis::use_spell_check()` — Estonian vocabulary in `inst/WORDLIST`
- [ ] `devtools::check(cran = TRUE)`, win-builder, R-hub v2
- [ ] `cran-comments.md`
- [ ] Riigi Teataja Act § 12: every result must carry the source URL

Per the lexverse release policy the first submission bar is high. Ship
product-level coverage rather than a thin wrapper.
