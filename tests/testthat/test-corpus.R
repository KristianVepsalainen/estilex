# Tests for the bulk archive functions and for translations.
#
# Two rules shape this file.
#
# First, no test ever downloads an archive. The English one is a gigabyte and
# the Estonian one is forty. Live tests skip unless an archive happens to be
# available already, so the suite is usable on a laptop and on CI.
#
# Second, no test calls etx_clear_corpus(), which deletes archives stored in
# the temporary directory. A developer running the suite may well have a real
# archive there. The cache is saved and restored instead.

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
  dir <- file.path(tempdir(), paste0("etx-fake-", as.integer(runif(1, 1, 1e9))))
  dir.create(dir, recursive = TRUE)
  for (f in files) file.create(file.path(dir, f))
  dir
}

# ---------------------------------------------------------------------------
# Archive naming and discovery
# ---------------------------------------------------------------------------

test_that("archive names follow the published convention", {
  expect_equal(archive_name("en", 2026), "xml.2026.en.zip")
  expect_equal(archive_name("et", 2026), "xml.2026.zip")
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

  # Newest first within each language
  expect_equal(out$year[out$lang == "en"], c(2026L, 2024L))
})

test_that("a listing with no archives is an error, not an empty table", {
  expect_error(parse_archive_listing("<html>nothing here</html>"),
               class = "estilex_parse_error")
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
  # An explicit dest still overrides the variable
  expect_equal(resolve_archive_dir("/tmp/explicit"),
               normalizePath("/tmp/explicit", mustWork = FALSE))

  Sys.unsetenv("ESTILEX_CORPUS_DIR")
  expect_equal(resolve_archive_dir(NULL), file.path(tempdir(), "estilex"))
})

test_that("an archive in ESTILEX_CORPUS_DIR is adopted without asking", {
  # A package-local cache does not survive restarting R, so without this a
  # user would have to re-register a gigabyte archive every session.
  local_clean_cache()

  dir <- fake_archive_dir(c("xml.2024.en.zip", "xml.2026.en.zip",
                            "xml.2025.zip"))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  Sys.setenv(ESTILEX_CORPUS_DIR = dir)

  en <- archive_entry("en")
  expect_false(is.null(en))
  expect_equal(en$year, 2026L)     # newest wins
  expect_match(en$path, "xml\\.2026\\.en\\.zip$")

  et <- archive_entry("et")
  expect_equal(et$year, 2025L)
})

test_that("adoption is silent when there is nothing to adopt", {
  local_clean_cache()
  expect_null(archive_entry("en"))

  Sys.setenv(ESTILEX_CORPUS_DIR = file.path(tempdir(), "does-not-exist"))
  expect_null(archive_entry("en"))

  dir <- fake_archive_dir("readme.txt")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  Sys.setenv(ESTILEX_CORPUS_DIR = dir)
  expect_null(archive_entry("en"))
})

test_that("a cached entry whose file has vanished is forgotten", {
  local_clean_cache()

  path <- tempfile(fileext = ".zip")
  file.create(path)
  register_archive("en", 2026L, path)
  expect_false(is.null(archive_entry("en")))

  unlink(path)
  expect_null(archive_entry("en"))
})

# ---------------------------------------------------------------------------
# Size guard
# ---------------------------------------------------------------------------

test_that("sizes are reported in readable units", {
  expect_equal(format_bytes(512), "512 bytes")
  expect_match(format_bytes(40 * 1024^3), "GB")
  expect_equal(format_bytes(NA), "unknown size")
})

test_that("a very large download is refused without confirmation", {
  # The Estonian archive is around 40 GB. Starting that by accident is
  # expensive enough to be worth a stop.
  #
  # testthat runs interactively in RStudio, so the interactive() branch would
  # prompt and block. Force the non-interactive path.
  withr::local_options(rlang_interactive = FALSE)
  big <- 40 * 1024^3

  expect_error(
    confirm_large_download(big, "xml.2026.zip", force = FALSE,
                           progress = FALSE, interactive = FALSE),
    class = "estilex_large_download"
  )
  expect_true(confirm_large_download(big, "xml.2026.zip", force = TRUE,
                                     progress = FALSE))
})

test_that("ordinary downloads and unknown sizes pass without a prompt", {
  expect_true(confirm_large_download(1024^3, "xml.2026.en.zip", FALSE, FALSE))
  expect_true(confirm_large_download(NA, "xml.2026.en.zip", FALSE, FALSE))
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
})

# ---------------------------------------------------------------------------
# Section collapsing and alignment
# ---------------------------------------------------------------------------

test_that("subsections are collapsed into one row per section", {
  p <- tibble::tibble(
    peatykk_nr       = c("I", "I", "I"),
    peatykk_pealkiri = c("Head", "Head", "Head"),
    paragrahv_nr     = c("1", "1", "2"),
    paragrahv        = c("\u00a7 1.", "\u00a7 1.", "\u00a7 2."),
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
    paragrahv_nr = c("1", "1"), paragrahv = c("\u00a7 1.", "\u00a7 1."),
    text = c("Only this.", NA_character_)
  )
  expect_equal(collapse_sections(p)$text, "Only this.")
})

test_that("section numbers and text are read from a translation document", {
  x <- xml2::read_xml(paste0(
    "<oigusakt><sisu>",
    "<paragrahv><paragrahvNr>1</paragrahvNr>",
    "<kuvatavNr><![CDATA[\u00a7 1.]]></kuvatavNr>",
    "<loige><sisuTekst><tavatekst>Estonia is a republic.</tavatekst></sisuTekst></loige>",
    "</paragrahv>",
    "<paragrahv><paragrahvNr>2</paragrahvNr>",
    "<kuvatavNr><![CDATA[\u00a7 2.]]></kuvatavNr>",
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

  # The English source URL appears only once a translation is identified
  found <- translation_row("111042025003", "ok",
                           list(translation_id = "523042025002"))
  expect_match(found$source_url_en, "/akt/523042025002$")

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

test_that("the newest archive year is chosen by default", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  a <- etx_list_archives()
  expect_equal(resolve_archive_year("en"),
               max(a$year[a$lang == "en"]))
})

# ---------------------------------------------------------------------------
# Live: needs an archive, never downloads one
# ---------------------------------------------------------------------------

test_that("the archive index decodes identifiers", {
  skip_on_cran()
  skip_if(is.null(archive_entry("en")), "No English archive available")

  idx <- etx_list_documents("en")

  expect_gt(nrow(idx), 0L)
  expect_true(all(idx$is_translation))
  expect_true(all(substr(idx$id, 1, 1) == "5"))
  expect_false(anyNA(idx$year))
})

test_that("a document can be read out of the archive", {
  skip_on_cran()
  skip_if(is.null(archive_entry("en")), "No English archive available")

  idx <- etx_list_documents("en")
  r <- etx_archive_xml(idx$id[1], "en")

  expect_equal(r$status, "ok")
  expect_false(is.na(xml_chr(r$body, "//aktinimi")))
})

test_that("an identifier absent from the archive is reported, not raised", {
  skip_on_cran()
  skip_if(is.null(archive_entry("en")), "No English archive available")

  r <- etx_archive_xml("599999999999", "en")
  expect_equal(r$status, "not_found")
})

test_that("the Constitution aligns section by section", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")
  skip_if(is.null(archive_entry("en")), "No English archive available")

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

test_that("older consolidations compare against a contemporaneous translation", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")
  skip_if(is.null(archive_entry("en")), "No English archive available")

  # Riigi Teataja pairs each version with the translation matching it, so this
  # is a 2015 comparison rather than 2015 text against a 2026 translation.
  old <- etx_get_parallel("115052015002")
  new <- etx_get_parallel("111042025003")

  expect_true(old$kehtivuse_algus[1] < new$kehtivuse_algus[1])
  expect_true(old$kehtivuse_algus_en[1] < new$kehtivuse_algus_en[1])

  # The two versions are close together in time
  gap <- abs(as.integer(old$kehtivuse_algus_en[1] - old$kehtivuse_algus[1]))
  expect_lt(gap, 30L)
})

test_that("a full translation is retrieved and reported as current", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")
  skip_if(is.null(archive_entry("en")), "No English archive available")

  tr <- etx_get_translation("111042025003")

  expect_equal(tr$status, "ok")
  expect_true(tr$is_current)
  expect_gt(tr$n_char, 10000L)
  expect_match(tr$pealkiri_en, "Constitution")
  expect_match(tr$source_url_en, "^https://")
})

test_that("an act without a translation keeps its Estonian rows", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")
  skip_if(is.null(archive_entry("en")), "No English archive available")

  # 550572 amends the Constitution and has no English version. An absent
  # translation is an observation, so the structure is returned with text_en
  # missing rather than nothing at all.
  tr <- etx_get_translation("550572")
  skip_if(tr$status == "error", "Metadata endpoint unavailable")

  expect_true(tr$status %in% c("no_translation", "not_in_archive"))
  expect_true(is.na(tr$text))
})
