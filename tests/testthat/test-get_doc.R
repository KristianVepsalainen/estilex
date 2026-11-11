# Tests for etx_get_doc() and its internals.
#
# Most of these run offline. The network tests are grouped at the end and
# skip on CRAN. See test-get_text.R for the rendering tests.

# ---------------------------------------------------------------------------
# Row construction
# ---------------------------------------------------------------------------

test_that("etx_doc_row returns one row with the documented columns", {

  row <- etx_doc_row("111042025003", status = "ok")

  expect_s3_class(row, "tbl_df")
  expect_identical(nrow(row), 1L)

  expect_identical(
    names(row),
    c("status", "id", "kehtiv_id", "grupi_id", "pealkiri", "lyhend",
      "tekstiliik", "dokumentliik", "akti_staatus", "kehtivus_staatus",
      "kehtivuse_algus", "kehtivuse_lopp", "avaldamise_kuupaev",
      "is_translation", "tolke_seos_id", "tolke_seos_tekst", "liigitused",
      "n_viited", "source_url", "error_message")
  )
})

test_that("etx_doc_row types are stable across statuses", {

  # Callers row-bind these across many identifiers, so a failed request must
  # produce exactly the same columns and types as a successful one. This is
  # the invariant that makes batch retrieval safe.
  ok  <- etx_doc_row("111042025003", status = "ok")
  err <- etx_doc_row("999999999999", status = "error",
                     values = list(error_message = "boom"))

  expect_identical(names(ok), names(err))
  expect_identical(vapply(ok, function(x) class(x)[1], character(1)),
                   vapply(err, function(x) class(x)[1], character(1)))

  expect_type(ok$id, "character")
  expect_s3_class(ok$kehtivuse_algus, "Date")
  expect_type(ok$is_translation, "logical")
  expect_type(ok$n_viited, "integer")
})

test_that("etx_doc_row flags translations from the identifier", {

  expect_false(etx_doc_row("111042025003", status = "ok")$is_translation)
  expect_true(etx_doc_row("523042025002", status = "ok")$is_translation)

  # A short legacy identifier beginning with 5 is not a translation. Routing
  # on the leading digit alone sent 550572, an act amending the Constitution,
  # to a /en/ address that does not exist.
  expect_false(etx_doc_row("550572", status = "ok")$is_translation)
})

test_that("etx_doc_row supplies a source URL even when retrieval failed", {

  row <- etx_doc_row("999999999999", status = "not_found")
  expect_match(row$source_url, "999999999999", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# JSON parsing
# ---------------------------------------------------------------------------

minimal_json <- function(...) {
  base <- list(
    kehtivId     = "111042025003",
    grupiId      = "12846827",
    tekstiliik   = "terviktekst",
    dokumentliik = "seadus",
    aktiStaatus  = "KEHTIV",
    tolkeSeosId  = "523042025002",
    tolkeSeosTekst = "Constitution of the Republic of Estonia",
    liigitused   = list("SYSTJAOT", "RIIGI"),
    viited       = list("a", "b", "c"),
    aktiParameetrid = list(
      pealkiri          = "Eesti Vabariigi pohiseadus",
      lyhend            = "PS",
      staatus           = "KEHTIV",
      kehtivuseAlgus    = "2026-02-28T22:00:00Z",
      kehtivuseLopp     = NULL,
      avaldamiseKuupaev = "2025-04-11T21:00:00Z"
    )
  )
  utils::modifyList(base, list(...))
}

test_that("etx_parse_doc reads the documented fields", {

  row <- etx_parse_doc(minimal_json(), "111042025003")

  expect_identical(row$status, "ok")
  expect_identical(row$pealkiri, "Eesti Vabariigi pohiseadus")
  expect_identical(row$lyhend, "PS")
  expect_identical(row$grupi_id, "12846827")
  expect_identical(row$tolke_seos_id, "523042025002")
  expect_identical(row$dokumentliik, "seadus")
})

test_that("etx_parse_doc returns aktiParameetrid status as kehtivus_staatus", {

  # `staatus` inside aktiParameetrid would collide with the package-wide
  # `status` column, which carries the outcome of the request rather than the
  # state of the act.
  row <- etx_parse_doc(minimal_json(), "111042025003")

  expect_identical(row$status, "ok")
  expect_identical(row$kehtivus_staatus, "KEHTIV")
})

test_that("etx_parse_doc handles the three date formats", {

  # JSON uses timestamps, Estonian XML appends an offset, translations use a
  # plain date. All three are truncated to the first ten characters.
  row <- etx_parse_doc(minimal_json(), "111042025003")

  expect_identical(row$kehtivuse_algus, as.Date("2026-02-28"))
  expect_identical(row$avaldamise_kuupaev, as.Date("2025-04-11"))
  expect_true(is.na(row$kehtivuse_lopp))
})

test_that("etx_parse_doc collapses classification codes", {

  row <- etx_parse_doc(minimal_json(), "111042025003")
  expect_identical(row$liigitused, "SYSTJAOT; RIIGI")

  # Assigned rather than passed through minimal_json(): modifyList() recurses
  # into a list-valued element, so an empty list would leave the fixture
  # untouched instead of clearing it.
  empty <- minimal_json()
  empty$liigitused <- list()
  expect_true(is.na(etx_parse_doc(empty, "111042025003")$liigitused))

  absent <- minimal_json()
  absent$liigitused <- NULL
  expect_true(is.na(etx_parse_doc(absent, "111042025003")$liigitused))
})

test_that("etx_parse_doc counts related materials rather than listing them", {

  # `viited` holds annotated editions, court analyses and translations into
  # other languages, not cross-references to other acts.
  expect_identical(etx_parse_doc(minimal_json(), "111042025003")$n_viited, 3L)

  j <- minimal_json()
  j$viited <- NULL
  expect_identical(etx_parse_doc(j, "111042025003")$n_viited, 0L)
})

test_that("etx_parse_doc aborts on a response with no document data", {

  # If the API changes shape, failing loudly is right: a silently empty row
  # would propagate into a batch as a plausible-looking result.
  expect_error(etx_parse_doc(list(), "111042025003"),
               class = "estilex_parse_error")
  expect_error(etx_parse_doc("not a list", "111042025003"),
               class = "estilex_parse_error")
})

# ---------------------------------------------------------------------------
# Failure paths, without a network
# ---------------------------------------------------------------------------

test_that("etx_get_doc reports a failed request instead of raising", {

  local_mocked_bindings(
    etx_get_json = function(id) {
      list(status = "error", message = "HTTP 500 Internal Server Error")
    }
  )

  row <- etx_get_doc("999999999999")

  expect_identical(row$status, "error")
  expect_identical(row$error_message, "HTTP 500 Internal Server Error")
  expect_identical(row$id, "999999999999")
  expect_true(is.na(row$pealkiri))
})

test_that("etx_get_doc distinguishes not_found from error", {

  local_mocked_bindings(
    etx_get_json = function(id) list(status = "not_found", message = NA_character_)
  )

  expect_identical(etx_get_doc("999999999999")$status, "not_found")
})

test_that("etx_get_doc accepts an identifier given as a number", {

  # Identifiers fit in a double, but they are identifiers rather than
  # numbers. Whatever arrives must come back as character, without
  # scientific notation.
  local_mocked_bindings(
    etx_get_json = function(id) list(status = "error", message = "mocked")
  )

  row <- etx_get_doc(111042025003)
  expect_identical(row$id, "111042025003")
})

# ---------------------------------------------------------------------------
# Network
# ---------------------------------------------------------------------------

test_that("etx_get_doc retrieves the Constitution", {
  skip_on_cran()
  skip_if_offline()

  doc <- etx_get_doc("111042025003")

  expect_identical(doc$status, "ok")
  expect_match(doc$pealkiri, "seadus")
  expect_identical(doc$dokumentliik, "seadus")
  expect_identical(doc$tekstiliik, "terviktekst")
  expect_false(doc$is_translation)

  expect_s3_class(doc$kehtivuse_algus, "Date")
  expect_false(is.na(doc$kehtivuse_algus))
  expect_false(is.na(doc$grupi_id))
  expect_false(is.na(doc$kehtiv_id))
  expect_true(is.na(doc$error_message))
})

test_that("the translation link works in both directions", {
  skip_on_cran()
  skip_if_offline()

  et <- etx_get_doc("111042025003")
  expect_false(is.na(et$tolke_seos_id))

  en <- etx_get_doc(et$tolke_seos_id)

  expect_identical(en$status, "ok")
  expect_true(en$is_translation)
  expect_match(en$pealkiri, "Constitution")

  # Back again. Group identifiers are not shared between languages, so
  # tolke_seos_id is the only bridge.
  expect_identical(en$tolke_seos_id, et$id)
})

test_that("a legacy short identifier is retrievable", {
  skip_on_cran()
  skip_if_offline()

  # 550572 amends the Constitution and begins with 5 without being a
  # translation. This is the regression that reached the test suite once.
  doc <- etx_get_doc("550572")

  expect_identical(doc$status, "ok")
  expect_false(doc$is_translation)
})

test_that("an unknown identifier is reported, not thrown", {
  skip_on_cran()
  skip_if_offline()

  # The service answered 500 for unknown identifiers in September 2026 and
  # 404 in October. Either is acceptable; a condition is not.
  row <- etx_get_doc("999999999999")

  expect_true(row$status %in% c("not_found", "error"))
  expect_identical(nrow(row), 1L)
  expect_true(is.na(row$pealkiri))

  # A clean 404 needs no explanation, the status says it. An error does,
  # because a 500 cannot be told apart from a genuine outage.
  if (identical(row$status, "error")) {
    expect_false(is.na(row$error_message))
  }
})
