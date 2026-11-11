# Tests for etx_get_references().
#
# The structural point that needs guarding: amendment notes appear both at
# document level and nested inside sections, and the Ref_* style fields sit
# under an avaldamismarge wrapper. A synthetic document covers both cases.

synthetic_amended_act <- function() {
  x <- xml2::read_xml(paste0(
    "<oigusakt>",
    "<metaandmed><globaalID>111042025003</globaalID></metaandmed>",

    # Document-level note
    "<muutmismarge>",
    "<aktikuupaev>2003-02-25+02:00</aktikuupaev>",
    "<avaldamismarge>",
    "<RTosa>RT I</RTosa><RTaasta>2003</RTaasta>",
    "<RTnr>29</RTnr><RTartikkel>174</RTartikkel>",
    "<aktViide>550572</aktViide>",
    "</avaldamismarge>",
    "<joustumine>2005-10-17</joustumine>",
    "</muutmismarge>",

    "<sisu>",
    "<paragrahv id='p1'>",
    "<paragrahvNr>127</paragrahvNr>",
    "<kuvatavNr><![CDATA[\u00a7 127.]]></kuvatavNr>",
    # Section-level note
    "<muutmismarge>",
    "<aktikuupaev>2011-04-27</aktikuupaev>",
    "<avaldamismarge>",
    "<RTosa>RT I</RTosa><RTaasta>2011</RTaasta>",
    "<RTnr>1</RTnr><RTartikkel>2</RTartikkel>",
    "<aktViide>127042011002</aktViide>",
    "</avaldamismarge>",
    "<joustumine>2011-07-22</joustumine>",
    "</muutmismarge>",
    "<loige id='l1'><sisuTekst><tavatekst>Text.</tavatekst></sisuTekst></loige>",
    "</paragrahv>",
    "</sisu>",
    "</oigusakt>"
  ))
  xml2::xml_ns_strip(x)
  x
}

# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

test_that("both document-level and section-level notes are found", {
  notes <- xml2::xml_find_all(synthetic_amended_act(), "//muutmismarge")
  expect_length(notes, 2L)

  rows <- do.call(rbind, lapply(seq_along(notes), function(i) {
    reference_row(notes[[i]], i, "111042025003", "url")
  }))

  expect_equal(nrow(rows), 2L)
  expect_equal(rows$ref_index, 1:2)
})

test_that("scope comes from the ancestor axis", {
  # A document-level note has no enclosing section; a section-level one does.
  # Position cannot distinguish them.
  notes <- xml2::xml_find_all(synthetic_amended_act(), "//muutmismarge")

  doc_level <- reference_row(notes[[1]], 1L, "111042025003", "url")
  expect_true(is.na(doc_level$scope_nr))
  expect_true(is.na(doc_level$scope))

  in_section <- reference_row(notes[[2]], 2L, "111042025003", "url")
  expect_equal(in_section$scope_nr, "127")
  expect_equal(in_section$scope, "\u00a7 127.")
})

test_that("the amending act identifier is read from inside avaldamismarge", {
  # aktViide is nested under the publication wrapper, not a direct child of
  # the note.
  notes <- xml2::xml_find_all(synthetic_amended_act(), "//muutmismarge")
  r <- reference_row(notes[[1]], 1L, "111042025003", "url")

  expect_equal(r$akt_viide, "550572")
  expect_type(r$akt_viide, "character")
})

test_that("dates parse in both formats the service emits", {
  notes <- xml2::xml_find_all(synthetic_amended_act(), "//muutmismarge")
  r <- reference_row(notes[[1]], 1L, "111042025003", "url")

  # aktikuupaev carries a timezone offset here, joustumine does not
  expect_equal(r$akti_kuupaev, as.Date("2003-02-25"))
  expect_equal(r$joustumine, as.Date("2005-10-17"))
})

# ---------------------------------------------------------------------------
# Publication reference
# ---------------------------------------------------------------------------

test_that("the publication reference is assembled in citation form", {
  expect_equal(
    format_rt_reference("RT I", "2003", "29", "174"),
    "RT I 2003, 29, 174"
  )
})

test_that("missing components are tolerated", {
  expect_equal(format_rt_reference("RT I", "2003", NA, NA), "RT I 2003")
  expect_equal(format_rt_reference(NA, "2003", "29", "174"), "2003, 29, 174")
  expect_equal(format_rt_reference("RT I", NA, NA, NA), "RT I")
  expect_true(is.na(format_rt_reference(NA, NA, NA, NA)))
})

# ---------------------------------------------------------------------------
# Empty results
# ---------------------------------------------------------------------------

test_that("an unamended act yields zero rows without warning", {
  x <- xml2::read_xml("<oigusakt><sisu/></oigusakt>")
  expect_length(xml2::xml_find_all(x, "//muutmismarge"), 0L)

  e <- etx_empty_references()
  expect_equal(nrow(e), 0L)
  expect_s3_class(e$joustumine, "Date")
  expect_type(e$ref_index, "integer")
})

test_that("empty and populated results have the same shape", {
  notes <- xml2::xml_find_all(synthetic_amended_act(), "//muutmismarge")
  r <- reference_row(notes[[1]], 1L, "111042025003", "url")
  expect_equal(names(r), names(etx_empty_references()))
})

# ---------------------------------------------------------------------------
# Live
# ---------------------------------------------------------------------------

test_that("the amendment history of the Constitution is returned", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  refs <- etx_get_references("111042025003")

  # Sixteen notes were counted during API mapping: seven at document level
  # and nine attached to sections.
  expect_gt(nrow(refs), 10L)
  expect_gt(sum(is.na(refs$scope_nr)), 0L)
  expect_gt(sum(!is.na(refs$scope_nr)), 0L)

  expect_false(anyNA(refs$akt_viide))
  expect_true(all(grepl("^[0-9]+$", refs$akt_viide)))
  expect_true(all(refs$rt_osa == "RT I", na.rm = TRUE))
})

test_that("references can be followed to the amending act", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # aktViide shares the identifier space used by etx_get_doc(), so no lookup
  # table is needed to traverse the network.
  refs <- etx_get_references("111042025003")
  d <- etx_get_doc(refs$akt_viide[1])

  expect_equal(d$status, "ok")
  expect_false(is.na(d$pealkiri))
})

test_that("an amending act has no amendment history of its own", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # 550572 is an act amending the Constitution.
  expect_equal(nrow(etx_get_references("550572")), 0L)
})

test_that("section scopes correspond to real sections", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  refs <- etx_get_references("111042025003")
  p    <- etx_get_paragraphs("111042025003")

  scoped <- stats::na.omit(refs$scope_nr)
  expect_true(all(scoped %in% p$paragrahv_nr))
})

test_that("amendment dates precede or match entry into force", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  refs <- etx_get_references("111042025003")
  both <- !is.na(refs$akti_kuupaev) & !is.na(refs$joustumine)
  expect_true(all(refs$akti_kuupaev[both] <= refs$joustumine[both]))
})
