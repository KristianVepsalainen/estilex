# estilex — roadmap

A living design document. Not part of the build (see `.Rbuildignore`).

`estilex` provides access to Estonian legislation via the Riigi Teataja
public API and its open data archives. Part of **lexverse**, alongside
`eurlex`, `finlex`, `swelex` and `danlex`.

- Function prefix: `etx_` (`elx_` belongs to `eurlex`; `et` is Estonia's
  language code)
- API base: `https://www.riigiteataja.ee/public-api/api/v1`
- Bulk: `https://www.riigiteataja.ee/avaandmed/ERT/`

---

## 1. The largest risk

**The API is undocumented.** It replaced the previous system on 1 June 2026,
and there is no OpenAPI specification on either the production or the beta
host. Probing for one is misleading: `openapi.json`, `/swagger` and similar
paths all return HTTP 200 with `text/html`, because the site is an Angular
application that serves `index.html` for any route it does not recognise.
Every endpoint below was found by trial.

A message was sent to `ert@riigiteataja.ee` on 2026-09-01 reporting three
defects and asking five questions. No reply as of this writing. Send a
reminder in the same thread around late September, and add the observation
that the translation metadata endpoint has now failed on four separate days,
which makes it a persistent fault rather than the intermittent one described
in the original message.

Until there is a reply, treat every endpoint as provisional.

---

## 2. Endpoints

| Endpoint | Returns |
|---|---|
| `/akt/{id}` | JSON metadata |
| `/akt/{id}/xml` | XML content and metadata |
| `/akt/{grupiId}/redaktsioonid` | list of consolidated versions |

Absent (clean 404): `/aktid`, `/otsing`, `/search`, `/avalik/*`,
`/grupp/{id}`, `/eli/{id}`, `/tolge/{id}`, `/akt/{id}/tolge`.

Broken: `/akt/{translationId}/xml` returns HTTP 500 with an empty body, and
`/akt/{translationId}` (metadata) has failed the same way on four separate
days having worked during the initial mapping. **Translations must come from
the bulk archive**, for metadata as well as content.

**There is no enumeration or search endpoint.** Identifiers come from the
bulk archives only.

Error format is a useful probing signal: unknown sub-paths under a valid
`/akt/{id}` return YAML (`staatus: "Viga"`, `vead: ["Tundmatu päring"]`),
whereas wholly unknown paths return an empty body. A YAML error means the
route pattern exists.

---

## 3. Parsing hazards

The service returns correct data with incorrect metadata in six ways. Each
needs an explicit workaround, and all six are covered by regression tests.

1. **XML is served as `application/octet-stream`.** `resp_body_xml()` needs
   `check_type = FALSE`. A consequence of serving from S3.
2. **The namespace declaration is not an absolute URI** — `tyviseadus_1_10.02.2010`
   for acts, `Juurakt` for translations — and varies per document. Call
   `xml_ns_strip()` immediately after parsing, then use plain XPath.
3. **JSON and XML expose different fields.** The translation link exists in
   JSON but not XML. Never infer one from the other.
4. **Date formats differ by language** for the same field: `2026-03-01+02:00`
   in Estonian documents, `2026-03-01` in translations, and JSON uses
   timestamps (`2026-02-28T22:00:00Z`). Take the first ten characters.
5. **`kehtivus` is a structured element**, with `kehtivuseAlgus` and
   `kehtivuseLopp` children. Reading the element's text concatenates them
   into `"2023-06-012023-06-30+03:00"`, from which taking ten characters
   happens to give the right answer for the wrong reason. Read the child.
6. **`versioon` is unparseable** — `"12022-12-23"` concatenates a version
   number and a date without a separator. Treat as opaque. Likewise `eesmark`
   is a schema description identical across documents, not a document
   property despite the name.

---

## 4. Identifiers

Format `{type:1}{day:2}{month:2}{year:4}{sequence:3}`:

```
111042025003  ->  1 | 11 | 04 | 2025 | 003   Estonian
523042025002  ->  5 | 23 | 04 | 2025 | 002   translation
```

The leading digit distinguishes language: 1 = Estonian, 5 = translation. All
6,391 files in the current English archive begin with 5.

**But the format is not universal.** Older consolidated versions use short
sequential numbers in the same space — the Constitution's versions run
`111042025003`, `115052015002`, `127042011002`, `12846827`, `633949`,
`24304`. Handle identifiers as **character**, never as a fixed width.

They fit in double precision (twelve digits < 2^53), so
`format(x, scientific = FALSE)` suffices when reading JSON. No bigint
handling is needed.

Two identifier spaces coexist and both work in `redaktsioonid`:

- `kehtivId` / act id — one consolidated version
- `grupiId` / `terviktekstiGrupiID` — the act across all versions

`aktViide` inside `muutmismarge` uses the same space as act ids, so the
citation network is traversable without a lookup table.

Group identifiers are **not** shared between languages. A translation's
`terviktekstiGrupiID` (e.g. 100515) belongs to a separate space from the
Estonian act's (e.g. 151381), and `redaktsioonid` on a translation group
returns that translation's own earlier versions. `tolkeSeosId` is the only
bridge.

---

## 5. Document structure

```
oigusakt
├── metaandmed
├── aktinimi
├── muutmismarge (x n, document level)
└── sisu
    ├── sisuTekst                  preamble
    └── peatykk [id]               chapter
        ├── peatykkNr              "I"
        ├── kuvatavNr              "I. peatükk"
        ├── peatykkPealkiri        "ÜLDSÄTTED"
        └── paragrahv [id]         section
            ├── paragrahvNr        "1"
            ├── kuvatavNr          "§ 1."
            ├── muutmismarge       (section level)
            └── loige [id]         subsection
                ├── sisuTekst
                │   └── tavatekst  (may contain reavahetus)
                └── alampunkt [id] sub-point
                    ├── alampunktNr  "1"
                    ├── kuvatavNr    "1)"
                    └── sisuTekst

Also: reavahetus (line break, inside tavatekst), sup (superscript,
translations only).
```

**Three rendering faults were found and fixed**, each now covered by a
regression test:

- `reavahetus` is an empty element *inside* `tavatekst`, so `xml_text()`
  swallows it and runs fragments together (`"riiki,mis on loodud"`). Walk the
  children instead.
- `kuvatavNr` exists at chapter, section **and** sub-point level. Combining
  it with `peatykkNr` produced `"I\n\nI. peatükk"`; combining it with
  `alampunktNr` produced doubled sub-point numbers. Use `kuvatavNr` alone and
  distinguish levels by the parent element.
- Sub-point numbers are kept inline in `text`, unlike `danlex` where the
  equivalent markers are dropped. Estonian cross-references cite sub-points
  ("§ 12 punkt 3"), so removing them costs more than the noise they add.

Cleaner than the Danish equivalent overall: markers live in their own
elements, so there is no run-together problem in the general case.

---

## 6. Amendment history

`muutmismarge` fields, stable across three documents: `aktikuupaev`,
`avaldamismarge/{RTosa, RTaasta, RTnr, RTartikkel, aktViide}`, `joustumine`.

**Two levels.** Of the sixteen in the Constitution, seven are at document
level and nine attach to specific sections (127, 128, 156×2, 65, 74, 78×3).
`scope_nr` and `scope` give the section, or `NA` for document-level notes —
the same ancestor-axis pattern as `danlex`'s `is_amendment`.

Note that `//muutmismarge` finds sixteen while `xml_children(root)` shows
only seven, because the rest are nested inside sections.

Amending acts carry no `muutmismarge` of their own.

`rt_viide` assembles the four publication components into the citation form
Estonian legal sources use in print: `"RT I 2003, 29, 174"`.

`viited` in the JSON is **not** a cross-reference list. It holds related
materials: annotated edition, constitutional assembly papers, Supreme Court
analyses, and translations into Ukrainian and Russian.

---

## 7. Translations and the parallel corpus

This is the package's distinguishing feature and the strongest argument for
Estonia in the wider lexverse project.

**Structure is aligned across languages.** For the Constitution, Estonian and
English both contain 168 `paragrahv`, 102 `alampunkt`, 15 `peatykk` and 10
`muutmismarge`, and the `paragrahvNr` vectors are **identical**. Sections can
therefore be joined on the number rather than on position.

`loige` counts differ by one (324 against 323), in section 168 — presumably a
closing provision formatted differently. **Alignment stops at section level**;
subsections of a section are concatenated. Aligning below section level would
work in most documents and fail silently in others, which is the worst kind
of defect in research use.

**Versions are paired.** Riigi Teataja links each consolidated version to the
translation matching it, not always to the latest:

| Consolidation | Estonian | Translation |
|---|---|---|
| current | 2026-02-28 | 2026-03-01 |
| 2015 | 2015-08-12 | 2015-08-14 |

Two days apart in both cases. Note that the 2015 consolidation's
`tolkeSeosId` is `530122020003`, whose identifier encodes 2020 — the date the
*file* was published, not the content's validity. The comparison is
contemporaneous, not anachronistic. `kehtivuse_algus` and
`kehtivuse_algus_en` record which two versions were compared.

Combined with `redaktsioonid` and archives reaching back to 2013, this makes
**translation fidelity measurable as a time series**, not only at the present
moment. This is exactly the class of research the lexverse project exists to
enable, and no other source in the family supports it.

**Missing translations return Estonian rows with `text_en` missing**, not
zero rows. Most Estonian acts have no English version — the archive holds
around 6,400 against a far larger body of legislation — and in fidelity work
an absent translation is an observation rather than an error.

**Whether a translation is current:** compare `kehtivuse_algus`, not
`avaldamise_kuupaev`. Publication dates differ by days even when the
translation is up to date. Translations carry no version information of their
own, so this comparison is the only signal.

A translation has no official legal force. Section 12 of the Riigi Teataja
Act requires reproduced texts to cite the published version; every function
returns `source_url` for that purpose, and `eli` alongside it.

---

## 8. Bulk archives

```
xml.2026.zip     39.7 GB     xml.2026.en.zip   1.01 GB
xml.2025.zip      2.64 GB    xml.2025.en.zip   82 MB
xml.2024.zip    344 MB       xml.2024.en.zip   39 MB
MD5SUMS, SHA1SUMS, SHA256SUMS, LOEMIND.txt
```

Filenames are identifiers. This is the only route to an identifier list and
the only dependable route to translations.

### Archive semantics — resolved

The 2024 and 2026 English archives share **no identifiers at all**, which
initially looked as though they were disjoint datasets. They are not.

Each archive holds, for each act, the version in force at that archive's
point in time. Since a translation receives a new identifier whenever it is
updated, the same act appears in both archives under different identifiers.
Confirmed by group identifier: of twenty groups sampled from the 2024
archive, ten appear in a 200-document sample of the 2026 archive, and
`terviktekstiGrupiID` 100515 (State Fees Act) is present in both.

The 2026 English archive holds 6,391 files spanning 2013 to 2026, of which
5,729 are dated 2024 or earlier — so it does carry history, not just the
present.

**Download only the most recent archive.** Earlier ones are useful only for
studying how a translation changed.

The Estonian 2026 archive is presumably inflated by the June migration
touching every document. Not verified — nobody has downloaded 40 GB to check.

### Storage and the environment variable

Archives default to a temporary directory, so nothing is written permanently
and no CRAN policy on user directories applies.

Three problems follow from a gigabyte download, and all three are addressed:

1. **Re-downloading every session.** `dest` names a directory to keep the
   archive in; a subsequent call with the same `dest` reuses the file.
2. **Re-registering every session.** A package-local environment does not
   survive restarting R, nor `devtools::load_all()` during development. The
   `ESTILEX_CORPUS_DIR` environment variable is checked automatically, so an
   archive already in that directory is adopted without being asked:

   ```
   ESTILEX_CORPUS_DIR=~/riigiteataja
   ```

   Set it in `~/.Renviron` via `usethis::edit_r_environ()`. The newest year
   present is chosen, matching the download default.
3. **Downloading 40 GB by accident.** A HEAD request reports the size first.
   Above 5 GB, interactive sessions ask for confirmation and non-interactive
   ones abort unless `force = TRUE`.

`confirm_large_download()` takes `interactive()` as an argument rather than
calling it, because testthat runs interactively in RStudio and the guard
would otherwise prompt during the test run.

**`etx_clear_corpus()` deletes archives held in the temporary directory.**
Tests must not call it — a developer's real archive is likely to be there.
The test suite saves and restores the cache instead.

---

## 9. ELI

ELI is **not** in the API. `globaalID` is the act identifier, and
`/public-api/api/v1/eli/{id}` returns 404.

But ELI URIs resolve on the website (`/eli/{id}`, `/en/eli/{id}/consolide`)
and the page declares `prefix="eli: http://data.europa.eu/eli/ontology#"`, so
the metadata exists as RDFa in the presentation layer. `estilex` therefore
constructs the `eli` column from the identifier rather than retrieving it.

This weakens, but does not remove, the original strategic case for doing
Estonia before Lithuania. If ELI never reaches the API, `lexcore` rests on
`eurlex` and `danlex` for its identifier model, and Lithuania becomes a more
useful next step than a fourth ELI implementation would have been.

The parallel corpus is now the stronger argument for Estonia regardless.

---

## 10. Function status

### Implemented

- [x] `etx_get_doc()` — JSON metadata
- [x] `etx_get_text()` — rendered running text
- [x] `etx_get_paragraphs()` — one row per subsection
- [x] `etx_get_references()` — amendment history with scope
- [x] `etx_get_translation()` — via `tolkeSeosId` and the archive
- [x] `etx_get_parallel()` — section-aligned Estonian and English
- [x] `etx_download_corpus()` — with size guard and `dest`
- [x] `etx_list_documents()` — identifiers from the archive
- [x] `etx_list_archives()` — what is published, without downloading
- [x] `etx_corpus_status()`, `etx_clear_corpus()`

184 tests, of which roughly 150 run offline against synthetic documents.

### Deferred to release-time decision

`etx_get_versions()`. `/akt/{grupiId}/redaktsioonid` works without parameters
and returns the full version list, which would suffice. But
`?kehtivus=koik` is accepted and the parameter vocabulary is unknown, and
`/akt/{actId}/redaktsioonid` returns an empty list where the group identifier
returns the full one.

**Decision rule:** include it if Riigi Teataja has replied by the time the
package is ready; otherwise ship without and add in the next version.
Point-in-time queries are the most valuable feature after the parallel
corpus and deserve to be built once, correctly.

### Not planned for v1

- `etx_get_docs()` — batch retrieval, as in `danlex`. Less pressing here
  because the archive already provides bulk access.
- Ukrainian and Russian translations, which appear in `viited` but have not
  been investigated.

---

## 11. Open questions

1. Which endpoints are supported and stable. The question that matters most.
2. `redaktsioonid` parameter vocabulary.
3. What the 39.7 GB Estonian archive contains.
4. Whether acts other than the Constitution carry multiple `marksona`
   keywords. Only one was present there.
5. The `liigitused` classification vocabulary (`SÜSTJAOT¤RIIGI¤PÕHI`). A
   systematic subject index would be material no other lexverse source has.
6. Whether the Angular front end exposes a search endpoint. Network traffic
   showed `tulemus?tekstNumber=...` but the path was not reachable under
   `/public-api/api/v1/`.
7. Whether translation XML will become available from the API. There is a
   test asserting it currently is not; if that test starts failing, the
   defect has been fixed and `etx_get_parallel()` can stop going through the
   archive.

---

## 12. CRAN pre-flight

- [ ] README and pre-computed vignette (`.Rmd.orig` + `data-raw/precompute.R`)
- [ ] The vignette must not download an archive during pre-computation on
      every run — cache it or use the smallest year
- [ ] Document `ESTILEX_CORPUS_DIR` prominently in both. Without it the first
      impression of the package is a gigabyte download.
- [ ] `urlchecker::url_check()`
- [ ] `usethis::use_spell_check()` — Estonian vocabulary in `inst/WORDLIST`
      (*peatükk*, *paragrahv*, *lõige*, *muutmismärge*, *terviktekst*,
      *Riigi Teataja*, *riigiteataja*)
- [ ] `devtools::check(cran = TRUE)`, win-builder, R-hub v2
- [ ] `cran-comments.md` — note that no example or test downloads an archive
- [ ] Version to `0.1.0`, `NEWS.md`
- [ ] Check `git remote -v` is SSH, not HTTPS. `use_github()` sets HTTPS and
      the global `insteadOf` rule does not apply to remotes already set.

Per the lexverse release policy the first submission bar is high: ship
product-level coverage rather than a thin wrapper.

---

## 13. Lexverse notes

Two findings feed into `lexcore`:

- **`lex_documents` needs a language axis, not just a document level.**
  Estonia is the first source in the family where the same provision exists
  in two languages with a reliable join key. A standard modelling documents
  as monolingual would lose that.
- **Version identity is not document identity.** `grupiId` and `kehtivId` are
  distinct, and translations have their own group space again. Any
  cross-jurisdictional structure needs to distinguish "this act" from "this
  version of this act" — `danlex` hinted at it through consolidated texts,
  but Estonia makes it explicit.

Sequence: **Denmark -> Estonia -> `lexcore` -> Lithuania -> (Norway + Iceland
on a corpus adapter)**. Latvia excluded: re-use of `likumi.lv` is a chargeable
service under Cabinet Regulation No. 536, with a 5 req/s automated-access
limit.
