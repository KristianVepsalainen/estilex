# estilex — roadmap

A living design document. Not part of the build (see `.Rbuildignore`).

`estilex` provides access to Estonian legislation via the Riigi Teataja
public API and its open data archives. Part of **lexverse**, alongside
`eurlex`, `finlex`, `swelex` and `danlex`.

- Function prefix: `etx_` (`elx_` belongs to `eurlex`; `et` is Estonia's
  language code)
- API base: `https://www.riigiteataja.ee/public-api/api/v1`
- Bulk: `https://www.riigiteataja.ee/avaandmed/ERT/`
- Contact: Eve Agur, product manager, enforcement and legal information team,
  Centre of Registers and Information Systems (RIK)

---

## 1. Status

Ten exported functions, 203 tests, of which about 150 run offline. The
document-level API is complete: retrieval, text, provisions, amendment
history, translations, section-aligned parallel texts, and corpus
enumeration from the archives.

Remaining before CRAN: README, vignette, spell check, version bump, the
usual check matrix. See section 12.

---

## 2. The standing risk

**The API is undocumented, and will stay that way for now.** It replaced the
previous system on 1 June 2026. Asked in September 2026 whether a
machine-readable specification existed, the Riigi Teataja replied that there
is none and that they will consider the need as usage and integration demands
grow.

Probing for one is misleading: `openapi.json`, `/swagger` and similar paths
all return HTTP 200 with `text/html`, because the site is an Angular
application that serves `index.html` for any route it does not recognise.
Every endpoint below was found by trial and then confirmed by correspondence.

**Some endpoints are explicitly unsupported.** Asked about the parameters
`/akt/{id}/redaktsioonid` accepts, they replied that APIs built for special
functions are meant for the user interface and carry no public support. That
answer settles a design question rather than leaving one open — see section 9.

Treat the supported surface as: `/akt/{id}`, `/akt/{id}/xml`, and their `/en/`
counterparts. Everything else may change without notice.

---

## 3. Endpoints

| Endpoint | Returns |
|---|---|
| `/akt/{id}` | JSON metadata, Estonian text |
| `/akt/{id}/xml` | XML content and metadata, Estonian text |
| `/en/akt/{id}` | JSON metadata, translation |
| `/en/akt/{id}/xml` | XML content and metadata, translation |
| `/akt/{grupiId}/redaktsioonid` | version list — **unsupported**, see section 9 |

Absent (clean 404): `/aktid`, `/otsing`, `/search`, `/avalik/*`,
`/grupp/{id}`, `/eli/{id}`, `/tolge/{id}`, `/akt/{id}/tolge`.

**There is no enumeration or search endpoint.** Identifiers come from the
bulk archives, or from references inside documents already retrieved.

Error format is a useful probing signal: unknown sub-paths under a valid
`/akt/{id}` return YAML (`staatus: "Viga"`, `vead: ["Tundmatu päring"]`),
whereas wholly unknown paths return an empty body. A YAML error means the
route pattern exists.

Unknown identifiers returned HTTP 500 in September 2026 and HTTP 404 in
October. estilex treats both as non-fatal, since a 500 cannot be told apart
from a genuine outage, and the test asserting this accepts either.

---

## 4. Parsing hazards

The service returns correct data with incorrect metadata in several ways.
Each needs an explicit workaround, and all are covered by regression tests.

1. **XML is served as `application/octet-stream`.** `resp_body_xml()` needs
   `check_type = FALSE`. Not a side effect of S3, as first assumed — it was
   changed deliberately for the system's own reasons. The Riigi Teataja
   agrees that separating public from internal APIs would let the public one
   use `application/xml` correctly, but that is a long-term intention.
2. **The namespace declaration is not an absolute URI** — `tyviseadus_1_10.02.2010`
   for acts, `Juurakt` for translations — and varies per document. The XML
   schema dates from 2009 and was built around their own validation and
   transformation needs rather than public standards. Conformance is
   achievable but depends on their client's priorities. Call
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

## 5. Identifiers

Format `{type:1}{day:2}{month:2}{year:4}{sequence:3}`:

```
111042025003  ->  1 | 11 | 04 | 2025 | 003   Estonian
523042025002  ->  5 | 23 | 04 | 2025 | 002   translation
```

A leading 5 marks a translation and routes the request to `/en/`.

**The leading digit only carries meaning in the twelve-digit form.** Older
consolidated versions use short sequential numbers in the same space — the
Constitution's versions run `111042025003`, `115052015002`, `127042011002`,
`12846827`, `633949`, `24304` — and `550572`, an act amending the
Constitution, begins with 5 without being a translation. Routing on the
leading digit alone sent it to `/en/akt/550572`, which does not exist. The
check is now `nchar(id) == 12 && substr(id, 1, 1) == "5"`.

That bug reached the test suite and was caught there. It is the clearest
argument in this package for testing the reference-following path rather than
only the happy one.

Identifiers are handled as **character** throughout. They fit in double
precision (twelve digits < 2^53), so `format(x, scientific = FALSE)` suffices
when reading JSON, but they are identifiers, not numbers.

Two identifier spaces coexist:

- `kehtivId` / act id — one consolidated version
- `grupiId` / `terviktekstiGrupiID` — the act across all versions

Group identifiers are **not** shared between languages. A translation's group
belongs to a separate space from the Estonian act's, and `redaktsioonid` on a
translation group returns that translation's own earlier versions.
`tolkeSeosId` is the only bridge, and it works in both directions.

`aktViide` inside `muutmismarge` uses the same space as act ids, so the
citation network is traversable without a lookup table.

---

## 6. Document structure

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

---

## 7. Amendment history

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

## 8. Translations and the parallel corpus

The package's distinguishing feature, and the strongest argument for Estonia
in the wider lexverse project.

### The /en/ route

Translations were thought broken for a month: `/akt/{translationId}/xml`
returns HTTP 500 with an empty body, and the metadata endpoint failed the
same way on several days. The Riigi Teataja explained the reason in October
2026, and it is not a defect.

Only the Estonian text has legal force in domestic proceedings. Estonian is
the state language; translations are kept current but are not usable as
official texts. They are therefore held apart from the law in force, and
reached by inserting `/en/` into the path:

```
/public-api/api/v1/akt/111042025003/xml        Estonian
/public-api/api/v1/en/akt/523042025002/xml     translation
```

estilex routes automatically on the identifier. **No archive download is
needed for translations** — before this was known, `etx_get_parallel()`
required a gigabyte. It now takes two HTTP requests.

### Structural alignment

For the Constitution, Estonian and English both contain 168 `paragrahv`, 102
`alampunkt`, 15 `peatykk` and 10 `muutmismarge`, and the `paragrahvNr`
vectors are **identical**. Sections join on the number rather than on
position.

`loige` counts differ by one (324 against 323), in section 168 — presumably a
closing provision formatted differently. **Alignment stops at section level**;
subsections of a section are concatenated. Aligning below section level would
work in most documents and fail silently in others, which is the worst kind
of defect in research use.

### Versions are paired

The Riigi Teataja links each consolidated version to the translation matching
it, not always to the latest:

| Consolidation | Estonian | Translation |
|---|---|---|
| current | 2026-02-28 | 2026-03-01 |
| 2015 | 2015-08-12 | 2015-08-14 |

Two days apart in both cases. Note that the 2015 consolidation's
`tolkeSeosId` is `530122020003`, whose identifier encodes 2020 — the date the
*file* was published, not the content's validity. The comparison is
contemporaneous, not anachronistic. `kehtivuse_algus` and
`kehtivuse_algus_en` record which two versions were compared.

This makes **translation fidelity measurable as a time series**, not only at
the present moment. No other source in the lexverse family supports it.

### Missing translations

`etx_get_parallel()` returns Estonian rows with `text_en` missing rather than
zero rows. Most Estonian acts have no English version — the archives hold
around 6,800 against a far larger body of legislation — and in fidelity work
an absent translation is an observation rather than an error.

**Whether a translation is current:** compare `kehtivuse_algus`, not
`avaldamise_kuupaev`. Publication dates differ by days even when the
translation is up to date. Translations carry no version information of their
own, so this comparison is the only signal.

Section 12 of the Riigi Teataja Act requires reproduced texts to cite the
published version; every function returns `source_url` for that purpose.

---

## 9. Version history: deliberately not implemented

`/akt/{grupiId}/redaktsioonid` works, returns the full list of consolidated
versions with their validity dates, and would make point-in-time queries
straightforward. It was planned as `etx_get_versions()`.

**It is not implemented, and should not be.** Asked what parameters it
supports, the Riigi Teataja replied that APIs built for special functions are
meant for the user interface and carry no public support. A CRAN package
resting on an explicitly unsupported endpoint will be archived when it
breaks, and it will break without warning.

The capability is not lost. Older versions have identifiers of their own, and
`etx_get_doc()`, `etx_get_text()` and `etx_get_parallel()` all accept them —
so a point-in-time comparison works whenever the identifier is known. Only
*enumerating* versions is unavailable.

Revisit if the endpoint is ever documented, or if a supported equivalent
appears.

---

## 10. Bulk archives

```
xml.2026.zip     39.7 GB     xml.2026.en.zip   1.01 GB
xml.2025.zip      2.64 GB    xml.2025.en.zip   ~70 MB
xml.2024.zip    344 MB       xml.2024.en.zip   39 MB
MD5SUMS, SHA1SUMS, SHA256SUMS, LOEMIND.txt
```

Filenames are identifiers. Since the API has no search endpoint, this is the
only route to a list of them.

### Archives are complementary, not snapshots

An earlier reading of this was wrong. The 2024 and 2026 English archives
share no identifiers at all, which looked like each being a complete snapshot
superseding the last. The Riigi Teataja corrected it: an annual folder holds
the acts published that year **together with** acts published earlier that
received a new meta version during it — because they were amended, or had an
end of validity recorded. Download only 2026 and acts untouched since 2024
are simply absent.

Measured on the English archives: 81 + 340 + 6,409 = 6,830 documents, with
**no overlap whatsoever**. Taking only the newest loses 421, or 6%.

`year = "all"` is therefore the default. The session cache holds several
archives per language, and `etx_list_documents()` combines them with an
`archive_year` column.

**Coverage begins in 2024.** The archives describe "everything published or
amended since 2024", not the whole body of Estonian legislation. Acts still
in force but unamended since then are absent, and must be retrieved by
identifier through `etx_get_doc()`, which has no such limit.

### Storage

Archives default to a temporary directory, so nothing is written permanently
and no CRAN policy on user directories applies.

Three problems follow from a gigabyte download, and all three are addressed:

1. **Re-downloading every session.** `dest` names a directory to keep them in;
   a later call with the same `dest` reuses what is there.
2. **Re-registering every session.** A package-local environment does not
   survive restarting R, nor `devtools::load_all()` during development. The
   `ESTILEX_CORPUS_DIR` environment variable is checked automatically, and
   every year present is adopted — not just the newest, since the archives
   are complementary.
3. **Downloading 43 GB by accident.** A HEAD request reports the size of
   everything about to be fetched, summed. Above 5 GB, interactive sessions
   ask and non-interactive ones abort unless `force = TRUE`.

`confirm_large_download()` takes `interactive()` as an argument rather than
calling it, because testthat runs interactively in RStudio and the guard
would otherwise prompt during the test run.

**`etx_clear_corpus()` deletes archives held in the temporary directory.**
Tests must not call it — a developer's real archives are likely to be there.
The suite saves and restores the cache instead, and no test ever downloads.

---

## 11. ELI: withdrawn, not missing

ELI identifiers do not exist in the current Riigi Teataja. The previous
scheme did not conform to the standard and was abandoned on 1 June 2026; the
old non-standard URIs now redirect to the plain act address:

```
/en/eli/523042025002/consolide  ->  /en/akt/523042025002
```

Reinstating a conforming implementation has not been a priority.

estilex therefore offers **no `eli` column**. An earlier version constructed
one from the identifier, which produced a working link that was not an ELI
identifier — worse than offering none.

**This removes the original strategic case for doing Estonia before
Lithuania.** The plan was for Estonia to give `lexcore` a third independent
ELI implementation alongside `eurlex` and `danlex`. It does not. `lexcore`
rests on those two for its identifier model, and Lithuania is now a more
useful next step than a fourth ELI implementation would have been.

The parallel corpus is the stronger argument for Estonia regardless, and it
is an argument the ELI plan did not anticipate.

---

## 12. CRAN pre-flight

- [ ] README and pre-computed vignette (`.Rmd.orig` + `data-raw/precompute.R`)
- [ ] Both need rewriting: the drafts assumed translations came from the
      archive, which is no longer true and made the first impression of the
      package a gigabyte download
- [ ] `urlchecker::url_check()`
- [ ] `usethis::use_spell_check()` — Estonian vocabulary in `inst/WORDLIST`
      (*peatükk*, *paragrahv*, *lõige*, *muutmismärge*, *terviktekst*,
      *Riigi Teataja*, *riigiteataja*, *tõlge*)
- [ ] `devtools::check(cran = TRUE)`, win-builder, R-hub v2
- [ ] `cran-comments.md` — note that no example or test downloads an archive
- [ ] Version to `0.1.0`, `NEWS.md`
- [ ] Check `git remote -v` is SSH, not HTTPS. `use_github()` sets HTTPS and
      the global `insteadOf` rule does not apply to remotes already set.

Submit after `danlex` is accepted. The two share an architecture — network
client, `if (FALSE)` examples, pre-computed vignette, `status` columns — so a
reviewer's objection to one would apply to the other, and it is better to
hear it once.

---

## 13. Open questions

Most of the original list was answered in October 2026. What remains:

1. What the 39.7 GB Estonian archive contains. Presumably inflated by the
   June migration giving every document a new meta version, but nobody has
   downloaded 40 GB to check.
2. Whether acts other than the Constitution carry multiple `marksona`
   keywords. Only one was present there.
3. The `liigitused` classification vocabulary (`SÜSTJAOT¤RIIGI¤PÕHI`). A
   systematic subject index would be material no other lexverse source has,
   and it is worth asking about directly.
4. Whether the Angular front end exposes a search endpoint. Network traffic
   showed `tulemus?tekstNumber=...` but the path was not reachable under
   `/public-api/api/v1/`. Likely one of the unsupported interface APIs.
5. Ukrainian and Russian translations, which appear in `viited` but have not
   been investigated. If they follow the same pattern as English, a
   three-language parallel corpus is possible.

---

## 14. Lexverse notes

Two findings feed into `lexcore`:

- **`lex_documents` needs a language axis, not just a document level.**
  Estonia is the first source in the family where the same provision exists
  in two languages with a reliable join key, and where versions are paired
  across languages. A standard modelling documents as monolingual would lose
  that.
- **Version identity is not document identity.** `grupiId` and `kehtivId` are
  distinct, and translations have their own group space again. Any
  cross-jurisdictional structure needs to distinguish "this act" from "this
  version of this act" — `danlex` hinted at it through consolidated texts,
  but Estonia makes it explicit.

A third finding is about process rather than structure. Two of this package's
hardest problems — the `/en/` route and the archive semantics — were resolved
by writing to the publisher, not by more probing. Both had been mapped
incorrectly with considerable effort first. For an undocumented API, the
email is cheaper than the exploration.

Sequence: **Denmark -> Estonia -> `lexcore` -> Lithuania -> (Norway + Iceland
on a corpus adapter)**. Latvia excluded: re-use of `likumi.lv` is a chargeable
service under Cabinet Regulation No. 536, with a 5 req/s automated-access
limit.
