# Tests for etx_get_translation(), etx_get_parallel() and their internals.
#
# The parallel corpus is the feature that distinguishes this package, so the
# alignment logic is tested offline against synthetic documents as well as
# against the live service.

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# A translation document with two sections. Kept ASCII on purpose: the
# Estonian characters are exercised by the live tests and by test-get_text.R,
# and a test file reads more easily without them.
fake_translation_xml <- function(valid_from = "2026-03-01",
                                 title = "Constitution of the Republic of Estonia",
                                 sections = c("1", "2")) {

  one <- function(nr) {
    paste0(
      "<paragrahv><paragrahvNr>", nr, "</paragrahvNr>",
      "<kuvatavNr>&#167; ", nr, ".</kuvatavNr>",
      "<loige><sisuTekst><tavatekst>Text of section ", nr,
      ".</tavatekst></sisuTekst></loige>",
      "</paragrahv>"
    )
  }

  xml2::read_xml(paste0(
    "<oigusakt>",
    "<aktinimi>", title, "</aktinimi>",
    "<kehtivus><kehtivuseAlgus>", valid_from, "</kehtivuseAlgus>",
    "<kehtivuseLopp/></kehtivus>",
    "<sisu><peatykk>",
    "<peatykkNr>I</peatykkNr><kuvatavNr>Chapter I</kuvatavNr>",
    "<peatykkPealkiri>GENERAL PROVISIONS</peatykkPealkiri>",
    paste0(vapply(sections, one, character(1)), collapse = ""),
    "</peatykk></sisu>",
    "</oigusakt>"
  ))
}

fake_doc <- function(status = "ok", ...) {
  etx_doc_row("111042025003", status = status, values = list(...))
}

fake_paragraphs <- function(nrs = c("1", "2")) {
  tibble::tibble(
    peatykk_nr       = "I",
    peatykk_pealkiri = "ULDSATTED",
    paragrahv_nr     = nrs,
    paragrahv        = paste0("$ ", nrs, "."),
    text             = paste0("Eesti tekst ", nrs, ".")
  )
}

# ---------------------------------------------------------------------------
# etx_get_translation
# ---------------------------------------------------------------------------

test_that("etx_get_translation returns the documented columns", {

  local_mocked_bindings(
    etx_get_doc = function(id) {
      fake_doc(tolke_seos_id = "523042025002",
               pealkiri = "Eesti Vabariigi pohiseadus",
               kehtivuse_algus = as.Date("2026-02-28"))
    },
    etx_get_xml = function(id) list(status = "ok", body = fake_translation_xml())
  )

  tr <- etx_get_translation("111042025003")

  expect_identical(nrow(tr), 1L)
  expect_identical(
    names(tr),
    c("status", "id", "translation_id", "pealkiri", "pealkiri_en",
      "kehtivuse_algus", "kehtivuse_algus_en", "is_current", "text",
      "n_char", "source_url", "source_url_en", "error_message")
  )

  expect_identical(tr$status, "ok")
  expect_identical(tr$translation_id, "523042025002")
  expect_match(tr$pealkiri_en, "Constitution")
  expect_identical(tr$n_char, nchar(tr$text))
})

test_that("etx_get_translation reports an act with no translation", {

  local_mocked_bindings(
    etx_get_doc = function(id) {
      fake_doc(pealkiri = "Mingi seadus", tolke_seos_id = NA_character_)
    }
  )

  tr <- etx_get_translation("111042025003")

  expect_identical(tr$status, "no_translation")
  expect_true(is.na(tr$translation_id))
  expect_true(is.na(tr$text))

  # Most Estonian acts have none, so this path is the common one and must not
  # look like a failure.
  expect_true(is.na(tr$error_message))
})

test_that("etx_get_translation passes on a failed retrieval", {

  local_mocked_bindings(
    etx_get_doc = function(id) {
      fake_doc(status = "not_found", error_message = "HTTP 404")
    }
  )

  tr <- etx_get_translation("999999999999")
  expect_identical(tr$status, "not_found")
  expect_identical(tr$error_message, "HTTP 404")

  local_mocked_bindings(
    etx_get_doc = function(id) {
      fake_doc(tolke_seos_id = "523042025002")
    },
    etx_get_xml = function(id) list(status = "error", message = "HTTP 500")
  )

  tr <- etx_get_translation("111042025003")
  expect_identical(tr$status, "error")
  expect_identical(tr$error_message, "HTTP 500")
  expect_identical(tr$translation_id, "523042025002")
})

test_that("is_current compares validity dates, not publication dates", {

  # Translations carry no version information of their own, and publication
  # dates differ by days even when the translation is up to date.
  current <- function(et, en) {
    local_mocked_bindings(
      etx_get_doc = function(id) {
        fake_doc(tolke_seos_id = "523042025002",
                 kehtivuse_algus = as.Date(et))
      },
      etx_get_xml = function(id) {
        list(status = "ok", body = fake_translation_xml(valid_from = en))
      }
    )
    etx_get_translation("111042025003")$is_current
  }

  expect_true(current("2026-02-28", "2026-03-01"))
  expect_true(current("2026-02-28", "2026-02-28"))
  expect_false(current("2026-02-28", "2015-08-14"))
})

test_that("the translation URL uses the separate /en/ route", {

  # Translations have no legal force in domestic proceedings, so the Riigi
  # Teataja holds them apart from the law in force.
  local_mocked_bindings(
    etx_get_doc = function(id) fake_doc(tolke_seos_id = "523042025002"),
    etx_get_xml = function(id) list(status = "ok", body = fake_translation_xml())
  )

  tr <- etx_get_translation("111042025003")

  expect_match(tr$source_url_en, "/en/akt/523042025002", fixed = TRUE)
  expect_false(grepl("/en/", tr$source_url, fixed = TRUE))
})

test_that("translation_row types are stable across statuses", {

  ok  <- translation_row("111042025003", "ok")
  err <- translation_row("111042025003", "error",
                         list(error_message = "boom"))

  expect_identical(names(ok), names(err))
  expect_identical(vapply(ok, function(x) class(x)[1], character(1)),
                   vapply(err, function(x) class(x)[1], character(1)))
  expect_s3_class(ok$kehtivuse_algus_en, "Date")
})

# ---------------------------------------------------------------------------
# Alignment internals
# ---------------------------------------------------------------------------

test_that("collapse_sections concatenates subsections of a section", {

  p <- tibble::tibble(
    peatykk_nr       = c("I", "I", "I"),
    peatykk_pealkiri = "ULDSATTED",
    paragrahv_nr     = c("1", "1", "2"),
    paragrahv        = c("$ 1.", "$ 1.", "$ 2."),
    text             = c("Esimene loige.", "Teine loige.", "Teine paragrahv.")
  )

  out <- collapse_sections(p)

  expect_identical(nrow(out), 2L)
  expect_identical(out$paragrahv_nr, c("1", "2"))
  expect_identical(out$text[1], "Esimene loige.\nTeine loige.")
})

test_that("collapse_sections drops missing subsection text", {

  p <- tibble::tibble(
    peatykk_nr       = "I",
    peatykk_pealkiri = "ULDSATTED",
    paragrahv_nr     = c("1", "1"),
    paragrahv        = "$ 1.",
    text             = c("Olemas.", NA_character_)
  )

  expect_identical(collapse_sections(p)$text, "Olemas.")
})

test_that("sections_from_xml reads numbers and text", {

  out <- sections_from_xml(fake_translation_xml(sections = c("1", "2", "3")))

  expect_identical(out$paragrahv_nr, c("1", "2", "3"))
  expect_match(out$text[2], "section 2")
})

test_that("sections_from_xml returns NULL for a document with no sections", {

  empty <- xml2::read_xml("<oigusakt><sisu/></oigusakt>")
  expect_null(sections_from_xml(empty))
})

test_that("etx_empty_parallel has the same shape as a populated table", {

  e <- etx_empty_parallel()

  expect_identical(nrow(e), 0L)
  expect_identical(
    names(e),
    c("id", "translation_id", "has_translation", "peatykk_nr",
      "peatykk_pealkiri", "paragrahv_nr", "paragrahv", "text_et", "text_en",
      "n_char_et", "n_char_en", "kehtivuse_algus", "kehtivuse_algus_en",
      "source_url")
  )
  expect_s3_class(e$kehtivuse_algus, "Date")
})

# ---------------------------------------------------------------------------
# etx_get_parallel
# ---------------------------------------------------------------------------

test_that("etx_get_parallel joins on the section number", {

  local_mocked_bindings(
    etx_get_paragraphs = function(id) fake_paragraphs(c("1", "2")),
    etx_get_doc = function(id) {
      fake_doc(tolke_seos_id = "523042025002",
               kehtivuse_algus = as.Date("2026-02-28"))
    },
    etx_get_xml = function(id) {
      # Deliberately in the opposite order: the join must be on the number
      # rather than on position, or it would drift silently.
      list(status = "ok", body = fake_translation_xml(sections = c("2", "1")))
    }
  )

  pp <- etx_get_parallel("111042025003")

  expect_identical(nrow(pp), 2L)
  expect_true(all(pp$has_translation))
  expect_match(pp$text_en[pp$paragrahv_nr == "1"], "section 1")
  expect_match(pp$text_en[pp$paragrahv_nr == "2"], "section 2")
  expect_identical(pp$n_char_et, nchar(pp$text_et))
})

test_that("etx_get_parallel keeps sections the translation omits", {

  # In work on translation fidelity an absent translation is an observation,
  # not an error, and it is the structure of the Estonian act that makes the
  # gap measurable.
  local_mocked_bindings(
    etx_get_paragraphs = function(id) fake_paragraphs(c("1", "2", "3")),
    etx_get_doc = function(id) fake_doc(tolke_seos_id = "523042025002"),
    etx_get_xml = function(id) {
      list(status = "ok", body = fake_translation_xml(sections = "1"))
    }
  )

  pp <- etx_get_parallel("111042025003")

  expect_identical(nrow(pp), 3L)
  expect_identical(sum(is.na(pp$text_en)), 2L)
  expect_false(anyNA(pp$text_et))
})

test_that("etx_get_parallel returns Estonian rows when there is no translation", {

  local_mocked_bindings(
    etx_get_paragraphs = function(id) fake_paragraphs(),
    etx_get_doc = function(id) fake_doc(tolke_seos_id = NA_character_)
  )

  pp <- etx_get_parallel("111042025003")

  expect_identical(nrow(pp), 2L)
  expect_false(any(pp$has_translation))
  expect_true(all(is.na(pp$text_en)))
  expect_true(all(is.na(pp$n_char_en)))
})

test_that("etx_get_parallel warns when the translation cannot be fetched", {

  local_mocked_bindings(
    etx_get_paragraphs = function(id) fake_paragraphs(),
    etx_get_doc = function(id) fake_doc(tolke_seos_id = "523042025002"),
    etx_get_xml = function(id) list(status = "error", message = "HTTP 500")
  )

  expect_warning(pp <- etx_get_parallel("111042025003"), "523042025002")

  # A failed translation is not the same as an act without one, but the
  # Estonian side is still returned so the caller can proceed.
  expect_identical(nrow(pp), 2L)
  expect_false(any(pp$has_translation))
})

test_that("etx_get_parallel returns an empty table for an unavailable act", {

  local_mocked_bindings(
    etx_get_paragraphs = function(id) etx_empty_paragraphs()
  )

  pp <- etx_get_parallel("999999999999")

  expect_identical(nrow(pp), 0L)
  expect_identical(names(pp), names(etx_empty_parallel()))
})

test_that("etx_get_parallel carries both validity dates on every row", {

  # Constant across the table, but carried per row so the temporal context
  # survives a join or a row-bind across acts.
  local_mocked_bindings(
    etx_get_paragraphs = function(id) fake_paragraphs(c("1", "2", "3")),
    etx_get_doc = function(id) {
      fake_doc(tolke_seos_id = "523042025002",
               kehtivuse_algus = as.Date("2015-08-12"))
    },
    etx_get_xml = function(id) {
      list(status = "ok", body = fake_translation_xml(valid_from = "2015-08-14"))
    }
  )

  pp <- etx_get_parallel("115052015002")

  expect_identical(unique(pp$kehtivuse_algus), as.Date("2015-08-12"))
  expect_identical(unique(pp$kehtivuse_algus_en), as.Date("2015-08-14"))
})

# ---------------------------------------------------------------------------
# Network
# ---------------------------------------------------------------------------

test_that("the Constitution has a current English translation", {
  skip_on_cran()
  skip_if_offline()

  tr <- etx_get_translation("111042025003")

  expect_identical(tr$status, "ok")
  expect_match(tr$pealkiri_en, "Constitution")
  expect_false(is.na(tr$is_current))
  expect_gt(tr$n_char, 10000)
  expect_match(tr$source_url_en, "/en/akt/", fixed = TRUE)
})

test_that("Estonian and English sections align for the Constitution", {
  skip_on_cran()
  skip_if_offline()

  pp <- etx_get_parallel("111042025003")

  expect_gt(nrow(pp), 150)
  expect_identical(anyDuplicated(pp$paragrahv_nr), 0L)
  expect_true(all(pp$has_translation))

  # Section numbering is preserved exactly across languages, so nearly every
  # section should find its counterpart. A sharp drop here means either the
  # numbering changed or the join broke.
  expect_gt(mean(!is.na(pp$text_en)), 0.95)
})

test_that("an older version is paired with a contemporaneous translation", {
  skip_on_cran()
  skip_if_offline()

  # Riigi Teataja links each consolidated version to the translation matching
  # it, not always to the newest. This is what makes translation fidelity
  # measurable as a time series rather than only at the present moment.
  old <- etx_get_parallel("115052015002")

  expect_gt(nrow(old), 150)
  expect_true(all(old$has_translation))

  et <- unique(old$kehtivuse_algus)
  en <- unique(old$kehtivuse_algus_en)

  expect_length(et, 1L)
  expect_length(en, 1L)
  expect_lt(abs(as.numeric(en - et)), 30)
  expect_lt(et, Sys.Date() - 365 * 5)
})
