# Tests for the bulk archive functions and for translations.
#
# Two rules shape this file.
#
# First, no test ever downloads an archive. The English ones come to a
# gigabyte together and the Estonian ones to forty. Live archive tests skip
# unless archives happen to be present already, so the suite is usable on a
# laptop and on CI.
#
# Second, no test calls etx_clear_corpus(), which deletes archives stored in
# the temporary directory. A developer running the suite may well have real
# archives there. The cache is saved and restored instead.
#
# Note that translation tests no longer need an archive at all: translations
# are served by the API under the /en/ route.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Save the archive cache and the environment variable, restore on exit
local_clean_cache <- function(frame = parent.frame()) {
  saved_cache <- as.list(.etx_cache, all.names = TRUE)
  saved_var   <- Sys.getenv("ESTILEX_CORPUS_DIR", unset = NA)

  rm(list = ls(.etx_cache, all.names = TRUE), envir = .etx_cache)
  Sys.unsetenv("ESTILEX_CORPUS_DIR")

  withr::defer(
    {
      rm(list = ls(.etx_cache, all.names = TRUE), envir = .etx_cache)
      for (nm in names(saved_cache)) assign(nm, saved_cache[[nm]], envir = .etx_cache)
      if (is.na(saved_var)) {
        Sys.unsetenv("ESTILEX_CORPUS_DIR")
      } else {
        Sys.setenv(ESTILEX_CORPUS_DIR = saved_var)
      }
    },
    envir = frame
  )
}

#' A directory holding empty files named like archives
#'
#' Discovery only inspects file names, so no real zip is needed.
fake_archive_dir <- function(files) {
  dir <- file.path(tempdir(), paste0("etx-fake-", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(dir, recursive = TRUE)
  for (f in files) file.create(file.path(dir, f))
  dir
}

# ---------------------------------------------------------------------------
# Routing: Estonian texts and translations take different paths
# ---------------------------------------------------------------------------

test_that("the leading digit identifies a translation", {
  expect_false(is_translation_id("111042025003"))
  expect_true(is_translation_id("523042025002"))
  expect_true(is_translation_id(523042025002))
  expect_false(is_translation_id(NA))

  # Older consolidated versions use short sequential numbers; none begin with 5
  expect_false(is_translation_id("24304"))
})

test_that("translations are requested under the /en/ route", {
  # Translations are unofficial texts with no legal force in domestic
  # proceedings, so the service holds them apart from the law in force.
  # Requesting one on the Estonian route returns HTTP 500 with an empty body,
  # which is what made them look broken before the split was explained.
  expect_equal(
    etx_api_url("111042025003", "xml"),
    paste0(ETX_BASE, "/akt/111042025003/xml")
  )
  expect_equal(
    etx_api_url("523042025002", "xml"),
    paste0(ETX_BASE, "/en/akt/523042025002/xml")
  )
  expect_equal(
    etx_api_url("523042025002"),
    paste0(ETX_BASE, "/en/akt/523042025002")
  )
})

test_that("source URLs follow the same split, and no ELI is offered", {
  # ELI identifiers were withdrawn on 1 June 2026 and old /eli/ addresses now
  # redirect to the plain /akt/ form, so constructing one would give a working
  # link that is not an ELI identifier.
  et <- etx_urls("111042025003")
  en <- etx_urls("523042025002")

  expect_equal(et$source_url, "https://www.riigiteataja.ee/akt/111042025003")
  expect_equal(en$source_url, "https://www.riigiteataja.ee/en/akt/523042025002")
  expect_null(et$eli)
})

# ---------------------------------------------------------------------------
# Archive naming and discovery
# ---------------------------------------------------------------------------

test_that("archive names follow the published convention", {
  expect_equal(archive_name(2026, "en"), "xml.2026.en.zip")
  expect_equal(archive_name(2026, "et"), "xml.2026.zip")
})

test_that("the directory listing is parsed into files, languages and years", {
  # The page is an nginx index with a Cloudflare script appended, so links are
  # extracted by pattern rather than by parsing HTML.
  html <- paste0(
    "<html><body><h1>Index of /avaandmed/ERT/</h1><pre>",
    "<a href='LOEMIND.txt'>LOEMIND.txt</a>",
    "<a href='xml.2024.zip'>xml.2024.zip</a>",
    "<a href='xml.2024.en.zip'>xml.2024.en.zip</a>",
    "<a href='xml.2026.zip'>xml.2026.zip</a>",
    "<a href='xml.2026.en.zip'>xml.2026.en.zip</a>",
    "</pre><script>cloudflare</script></body></html>"
  )
  out <- parse_archive_listing(html)

  expect_equal(nrow(out), 4L)
  expect_setequal(out$lang, c("en", "et"))
  expect_setequal(out$year, c(2024L, 2026L))
  expect_false("LOEMIND.txt" %in% out$file)
})

test_that("a listing with no archives is an error, not an empty table", {
  expect_error(parse_archive_listing("<html>nothing here</html>"),
               class = "estilex_parse_error")
})

test_that("explicit years are accepted and validated", {
  expect_equal(resolve_archive_years("en", 2024), 2024L)
  expect_equal(resolve_archive_years("en", c(2026, 2024)), c(2024L, 2026L))
  expect_equal(resolve_archive_years("en", c(2024, 2024)), 2024L)

  expect_error(resolve_archive_years("en", "abcd"),
               class = "estilex_bad_argument")
  expect_error(resolve_archive_years("en", character(0)),
               class = "estilex_bad_argument")
})

# ---------------------------------------------------------------------------
# Where archives are stored
# ---------------------------------------------------------------------------

test_that("dest wins, then the environment variable, then a temporary dir", {
  local_clean_cache()

  expect_equal(resolve_archive_dir("/tmp/explicit"),
               normalizePath("/tmp/explicit", mustWork = FALSE))

  Sys.setenv(ESTILEX_CORPUS_DIR = "/tmp/configured")
  expect_equal(resolve_archive_dir(NULL),
               normalizePath("/tmp/configured", mustWork = FALSE))
  expect_equal(resolve_archive_dir("/tmp/explicit"),
               normalizePath("/tmp/explicit", mustWork = FALSE))

  Sys.unsetenv("ESTILEX_CORPUS_DIR")
  expect_equal(resolve_archive_dir(NULL), file.path(tempdir(), "estilex"))
})

test_that("every archive in ESTILEX_CORPUS_DIR is adopted, not just the newest", {
  # Archives are complementary rather than successive: each holds that year's
  # publications plus earlier acts amended during it. Taking only the newest
  # would silently drop coverage.
  local_clean_cache()

  dir <- fake_archive_dir(c("xml.2024.en.zip", "xml.2025.en.zip",
                            "xml.2026.en.zip", "xml.2025.zip"))
  withr::defer(unlink(dir, recursive = TRUE))
  Sys.setenv(ESTILEX_CORPUS_DIR = dir)

  en <- archive_entry("en")
  expect_false(is.null(en))
  expect_equal(sort(names(en$archives)), c("2024", "2025", "2026"))

  et <- archive_entry("et")
  expect_equal(names(et$archives), "2025")
})

test_that("adoption is silent when there is nothing to adopt", {
  local_clean_cache()
  expect_null(archive_entry("en"))

  Sys.setenv(ESTILEX_CORPUS_DIR = file.path(tempdir(), "does-not-exist"))
  expect_null(archive_entry("en"))

  dir <- fake_archive_dir("readme.txt")
  withr::defer(unlink(dir, recursive = TRUE))
  Sys.setenv(ESTILEX_CORPUS_DIR = dir)
  expect_null(archive_entry("en"))
})

test_that("archives whose files have vanished are dropped", {
  local_clean_cache()

  a <- tempfile(fileext = ".zip")
  b <- tempfile(fileext = ".zip")
  file.create(a, b)
  register_archives("en", c(2025L, 2026L), c(a, b))
  expect_length(archive_entry("en")$archives, 2L)

  # One missing: the rest survive
  unlink(a)
  expect_equal(names(archive_entry("en")$archives), "2026")

  # All missing: the entry goes
  unlink(b)
  expect_null(archive_entry("en"))
})

test_that("registered archives are keyed by year, in order", {
  local_clean_cache()

  paths <- replicate(3, tempfile(fileext = ".zip"))
  file.create(paths)
  withr::defer(unlink(paths))

  register_archives("en", c(2026L, 2024L, 2025L), paths)
  expect_equal(names(.etx_cache$en$archives), c("2024", "2025", "2026"))
})

# ---------------------------------------------------------------------------
# Size guard
# ---------------------------------------------------------------------------

test_that("sizes are reported in readable units", {
  expect_equal(format_bytes(512), "512 bytes")
  expect_match(format_bytes(43 * 1024^3), "GB")
  expect_equal(format_bytes(NA), "unknown size")
})

test_that("a very large combined download is refused without confirmation", {
  # The Estonian archives come to about 43 GB together. The guard sums across
  # everything about to be fetched, since no single one trips it on its own in
  # the English case while three Estonian ones certainly do.
  #
  # interactive() is passed as an argument rather than called, because
  # testthat runs interactively in RStudio and the guard would otherwise
  # prompt during the test run.
  big <- 43 * 1024^3

  expect_error(
    confirm_large_download(big, "xml.2024.zip, xml.2025.zip, xml.2026.zip",
                           force = FALSE, interactive = FALSE),
    class = "estilex_large_download"
  )
  expect_true(confirm_large_download(big, "x", force = TRUE, interactive = FALSE))
})

test_that("ordinary downloads and unknown sizes pass without a prompt", {
  expect_true(confirm_large_download(1.1 * 1024^3, "x", FALSE, interactive = FALSE))
  expect_true(confirm_large_download(NA, "x", FALSE, interactive = FALSE))
  expect_true(confirm_large_download(0, "x", FALSE, interactive = FALSE))
})

# ---------------------------------------------------------------------------
# Using the archive without one present
# ---------------------------------------------------------------------------

test_that("archive functions explain themselves when nothing is downloaded", {
  local_clean_cache()

  expect_error(etx_list_documents("en"), class = "estilex_no_corpus")
  expect_error(etx_archive_xml("501062023002", "en"),
               class = "estilex_no_corpus")
})

test_that("status reports both languages whether or not they are present", {
  local_clean_cache()
  st <- etx_corpus_status()

  expect_equal(nrow(st), 2L)
  expect_setequal(st$lang, c("en", "et"))
  expect_false(any(st$available))
  expect_true(all(c("years", "n_archives") %in% names(st)))
})

# ---------------------------------------------------------------------------
# Section collapsing and alignment
# ---------------------------------------------------------------------------

test_that("subsections are collapsed into one row per section", {
  p <- tibble::tibble(
    peatykk_nr       = c("I", "I", "I"),
    peatykk_pealkiri = c("Head", "Head", "Head"),
    paragrahv_nr     = c("1", "1", "2"),
    paragrahv        = c("§ 1.", "§ 1.", "§ 2."),
    text             = c("First.", "Second.", "Third.")
  )
  out <- collapse_sections(p)

  expect_equal(nrow(out), 2L)
  expect_equal(out$paragrahv_nr, c("1", "2"))
  expect_equal(out$text[1], "First.\nSecond.")
  expect_equal(out$peatykk_nr[1], "I")
})

test_that("missing subsection text is dropped rather than pasted as NA", {
  p <- tibble::tibble(
    peatykk_nr = c("I", "I"), peatykk_pealkiri = c("H", "H"),
    paragrahv_nr = c("1", "1"), paragrahv = c("§ 1.", "§ 1."),
    text = c("Only this.", NA_character_)
  )
  expect_equal(collapse_sections(p)$text, "Only this.")
})

test_that("section numbers and text are read from a translation document", {
  x <- xml2::read_xml(paste0(
    "<oigusakt><sisu>",
    "<paragrahv><paragrahvNr>1</paragrahvNr>",
    "<kuvatavNr><![CDATA[§ 1.]]></kuvatavNr>",
    "<loige><sisuTekst><tavatekst>Estonia is a republic.</tavatekst></sisuTekst></loige>",
    "</paragrahv>",
    "<paragrahv><paragrahvNr>2</paragrahvNr>",
    "<kuvatavNr><![CDATA[§ 2.]]></kuvatavNr>",
    "<loige><sisuTekst><tavatekst>Indivisible.</tavatekst></sisuTekst></loige>",
    "</paragrahv>",
    "</sisu></oigusakt>"
  ))
  xml2::xml_ns_strip(x)

  s <- sections_from_xml(x)
  expect_equal(s$paragrahv_nr, c("1", "2"))
  expect_equal(s$text[1], "Estonia is a republic.")
})

test_that("a document with no sections yields NULL", {
  x <- xml2::read_xml("<oigusakt><sisu/></oigusakt>")
  expect_null(sections_from_xml(x))
})

test_that("validity dates are read from the structured kehtivus element", {
  # <kehtivus> has kehtivuseAlgus and kehtivuseLopp children. Reading the
  # element's own text concatenates them into "2023-06-012023-06-30+03:00",
  # from which taking ten characters gives the right answer for the wrong
  # reason — and would break on any document ordering them differently.
  x <- xml2::read_xml(paste0(
    "<oigusakt><metaandmed><kehtivus>",
    "<kehtivuseAlgus>2023-06-01</kehtivuseAlgus>",
    "<kehtivuseLopp>2023-06-30+03:00</kehtivuseLopp>",
    "</kehtivus></metaandmed></oigusakt>"
  ))
  xml2::xml_ns_strip(x)

  expect_equal(parse_date_est(xml_chr(x, "//kehtivus/kehtivuseAlgus")),
               as.Date("2023-06-01"))
})

# ---------------------------------------------------------------------------
# Result shapes
# ---------------------------------------------------------------------------

test_that("the empty parallel table carries the documented columns", {
  e <- etx_empty_parallel()
  expect_equal(nrow(e), 0L)
  expect_true(all(c("text_et", "text_en", "kehtivuse_algus",
                    "kehtivuse_algus_en") %in% names(e)))
  expect_s3_class(e$kehtivuse_algus_en, "Date")
})

test_that("a translation row is built for every outcome", {
  none <- translation_row("111042025003", "no_translation")
  expect_equal(none$status, "no_translation")
  expect_true(is.na(none$translation_id))
  expect_match(none$source_url, "/akt/111042025003$")

  # The English source URL appears once a translation is identified, and
  # carries the /en/ segment.
  found <- translation_row("111042025003", "ok",
                           list(translation_id = "523042025002"))
  expect_match(found$source_url_en, "/en/akt/523042025002$")

  expect_equal(names(none), names(found))
})

# ---------------------------------------------------------------------------
# Live: cheap
# ---------------------------------------------------------------------------

test_that("the published archives can be listed", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  a <- etx_list_archives()

  expect_gt(nrow(a), 0L)
  expect_setequal(unique(a$lang), c("en", "et"))
  expect_gte(max(a$year), 2024L)
})

test_that("year = all resolves to every published year", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  a <- etx_list_archives()
  expect_equal(resolve_archive_years("en", "all"),
               sort(a$year[a$lang == "en"]))
})

# ---------------------------------------------------------------------------
# Live: translations, which need no archive
# ---------------------------------------------------------------------------

test_that("a translation is retrieved and reported as current", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  tr <- etx_get_translation("111042025003")

  expect_equal(tr$status, "ok")
  expect_true(tr$is_current)
  expect_gt(tr$n_char, 10000L)
  expect_match(tr$pealkiri_en, "Constitution")
  expect_match(tr$source_url_en, "/en/akt/")
})

test_that("the Constitution aligns section by section", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  par <- etx_get_parallel("111042025003")

  # 168 sections in both languages, with identical numbering
  expect_equal(nrow(par), 168L)
  expect_true(all(par$has_translation))
  expect_equal(sum(is.na(par$text_en)), 0L)

  # English runs roughly a fifth longer; a wildly different ratio would mean
  # the join has gone wrong.
  ratio <- sum(par$n_char_en) / sum(par$n_char_et)
  expect_gt(ratio, 1.0)
  expect_lt(ratio, 1.6)
})

test_that("alignment needs no archive", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # The point of the /en/ route: before it was known, this required a
  # gigabyte download.
  local_clean_cache()
  expect_null(archive_entry("en"))

  expect_equal(nrow(etx_get_parallel("111042025003")), 168L)
})

test_that("older consolidations compare against a contemporaneous translation", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # Riigi Teataja pairs each version with the translation matching it, so this
  # is a 2015 comparison rather than 2015 text against a current translation.
  old <- etx_get_parallel("115052015002")
  new <- etx_get_parallel("111042025003")

  expect_true(old$kehtivuse_algus[1] < new$kehtivuse_algus[1])
  expect_true(old$kehtivuse_algus_en[1] < new$kehtivuse_algus_en[1])

  # The two versions are close together in time
  gap <- abs(as.integer(old$kehtivuse_algus_en[1] - old$kehtivuse_algus[1]))
  expect_lt(gap, 30L)
})

test_that("an act without a translation keeps its Estonian rows", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # 550572 amends the Constitution and has no English version. An absent
  # translation is an observation, so the structure is returned with text_en
  # missing rather than nothing at all.
  tr <- etx_get_translation("550572")
  skip_if(tr$status == "error", "Metadata endpoint unavailable")

  expect_equal(tr$status, "no_translation")
  expect_true(is.na(tr$text))
})

# ---------------------------------------------------------------------------
# Live: needs archives, never downloads them
# ---------------------------------------------------------------------------

test_that("the archive index decodes identifiers and records its source", {
  skip_on_cran()
  skip_if(is.null(archive_entry("en")), "No English archive available")

  idx <- etx_list_documents("en")

  expect_gt(nrow(idx), 0L)
  expect_true(all(idx$is_translation))
  expect_true(all(substr(idx$id, 1, 1) == "5"))
  expect_false(anyNA(idx$year))
  expect_false(anyNA(idx$archive_year))
})

test_that("identifiers are unique across archives", {
  skip_on_cran()
  skip_if(is.null(archive_entry("en")), "No English archive available")

  # Each archive holds a different version of an act, so the same identifier
  # should never appear twice. Observed exactly: 81 + 340 + 6409 = 6830 with
  # nothing deduplicated.
  idx <- etx_list_documents("en")
  expect_equal(anyDuplicated(idx$id), 0L)
})

test_that("archives contribute separately, so the newest alone is not enough", {
  skip_on_cran()
  skip_if(is.null(archive_entry("en")), "No English archive available")

  idx <- etx_list_documents("en")
  skip_if(length(unique(idx$archive_year)) < 2L, "Only one archive present")

  by_archive <- table(idx$archive_year)
  newest <- by_archive[[as.character(max(idx$archive_year))]]

  # Taking only the newest would lose the rest
  expect_lt(newest, nrow(idx))
})

test_that("a document can be read out of the archives", {
  skip_on_cran()
  skip_if(is.null(archive_entry("en")), "No English archive available")

  idx <- etx_list_documents("en")
  r <- etx_archive_xml(idx$id[1], "en")

  expect_equal(r$status, "ok")
  expect_false(is.na(xml_chr(r$body, "//aktinimi")))
})

test_that("an identifier absent from the archives is reported, not raised", {
  skip_on_cran()
  skip_if(is.null(archive_entry("en")), "No English archive available")

  r <- etx_archive_xml("599999999999", "en")
  expect_equal(r$status, "not_found")
})
