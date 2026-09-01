# Tests for etx_get_doc(), etx_get_text() and etx_get_paragraphs().
#
# The offline block uses a synthetic document reproducing the structural
# features that actually caused trouble during development: kuvatavNr
# appearing at three different levels, reavahetus nested inside a passage of
# text, sections outside any chapter, and sub-points carrying their own
# numbering. Each of those produced a wrong rendering at some point, so each
# has a test.
#
# Live tests are guarded by skip_on_cran() and skip_if_offline().

# ---------------------------------------------------------------------------
# Fixture
# ---------------------------------------------------------------------------

synthetic_act <- function() {
  x <- xml2::read_xml(paste0(
    "<oigusakt>",
    "<metaandmed>",
    "<globaalID>111042025003</globaalID>",
    "<terviktekstiGrupiID>151381</terviktekstiGrupiID>",
    "<kehtivus>2026-03-01+02:00</kehtivus>",
    "</metaandmed>",
    "<aktinimi>Test Act</aktinimi>",
    "<sisu>",

    # Preamble with an explicit line break inside one passage
    "<sisuTekst><tavatekst>First line,<reavahetus/>second line.</tavatekst></sisuTekst>",

    # A chapter, carrying its own kuvatavNr alongside number and heading
    "<peatykk id='c1'>",
    "<peatykkNr>I</peatykkNr>",
    "<kuvatavNr><![CDATA[I. peat\u00fckk]]></kuvatavNr>",
    "<peatykkPealkiri>\u00dcLDS\u00c4TTED</peatykkPealkiri>",
    "<paragrahv id='p1'>",
    "<paragrahvNr>1</paragrahvNr>",
    "<kuvatavNr><![CDATA[\u00a7 1.]]></kuvatavNr>",
    "<loige id='l1'><sisuTekst><tavatekst>First subsection.</tavatekst></sisuTekst></loige>",
    "<loige id='l2'>",
    "<sisuTekst><tavatekst>Second subsection:</tavatekst></sisuTekst>",
    "<alampunkt id='a1'>",
    "<alampunktNr>1</alampunktNr>",
    "<kuvatavNr><![CDATA[1)]]></kuvatavNr>",
    "<sisuTekst><tavatekst>first point;</tavatekst></sisuTekst>",
    "</alampunkt>",
    "<alampunkt id='a2'>",
    "<alampunktNr>2</alampunktNr>",
    "<kuvatavNr><![CDATA[2)]]></kuvatavNr>",
    "<sisuTekst><tavatekst>second point.</tavatekst></sisuTekst>",
    "</alampunkt>",
    "</loige>",
    "</paragrahv>",
    "</peatykk>",

    # A section outside any chapter, and with no subsections
    "<paragrahv id='p2'>",
    "<paragrahvNr>2</paragrahvNr>",
    "<kuvatavNr><![CDATA[\u00a7 2.]]></kuvatavNr>",
    "<sisuTekst><tavatekst>A section with no subsections.</tavatekst></sisuTekst>",
    "</paragrahv>",

    "</sisu>",
    "</oigusakt>"
  ))
  xml2::xml_ns_strip(x)
  x
}

# ---------------------------------------------------------------------------
# Identifiers
# ---------------------------------------------------------------------------

test_that("identifiers survive as strings", {
  # They arrive from JSON as doubles. Scientific notation or numeric
  # comparison would corrupt them.
  expect_equal(as_id(111042025003), "111042025003")
  expect_equal(as_id("111042025003"), "111042025003")
  expect_true(is.na(as_id(NULL)))

  # Older consolidated versions use short sequential numbers in the same
  # space, so no fixed width can be assumed.
  expect_equal(as_id(24304), "24304")
})

test_that("the twelve-digit format is decoded, and shorter ids are tolerated", {
  p <- etx_parse_id("111042025003")
  expect_false(p$is_translation)
  expect_equal(p$day, 11L)
  expect_equal(p$month, 4L)
  expect_equal(p$year, 2025L)
  expect_equal(p$sequence, 3L)

  # A leading 5 marks a translation
  expect_true(etx_parse_id("523042025002")$is_translation)

  # Short identifiers yield missing components rather than nonsense
  expect_true(is.na(etx_parse_id("24304")$year))
})

test_that("invalid identifiers are rejected", {
  expect_error(check_id(NULL), class = "estilex_bad_identifier")
  expect_error(check_id(c("1", "2")), class = "estilex_bad_identifier")
  expect_error(check_id("abc"), class = "estilex_bad_identifier")
  expect_error(check_id(""), class = "estilex_bad_identifier")
})

test_that("canonical URLs are constructed", {
  u <- etx_urls("111042025003")
  # ELI resolves on the website but is absent from the API, so it is built
  # from the identifier rather than retrieved.
  expect_equal(u$eli, "https://www.riigiteataja.ee/eli/111042025003")
  expect_match(u$source_url, "/akt/111042025003$")
})

# ---------------------------------------------------------------------------
# Dates
# ---------------------------------------------------------------------------

test_that("all three date formats the service emits are parsed", {
  # Estonian documents carry a timezone offset, translations do not, and JSON
  # uses full timestamps.
  expect_equal(parse_date_est("2026-03-01+02:00"), as.Date("2026-03-01"))
  expect_equal(parse_date_est("2026-03-01"), as.Date("2026-03-01"))
  expect_equal(parse_date_est("2026-02-28T22:00:00Z"), as.Date("2026-02-28"))
  expect_true(is.na(parse_date_est(NULL)))
  expect_silent(parse_date_est("rubbish"))
})

# ---------------------------------------------------------------------------
# Text rendering
# ---------------------------------------------------------------------------

test_that("reavahetus becomes a line break", {
  # It is an empty element inside a passage of text. xml_text() ignores it
  # and runs the fragments together, producing "riiki,mis on loodud".
  node <- xml2::xml_find_first(synthetic_act(), "//tavatekst")
  expect_equal(node_text(node), "First line,\nsecond line.")
  expect_false(grepl("line,second", node_text(node), fixed = TRUE))
})

test_that("kuvatavNr is not emitted twice for chapters", {
  txt <- render_content(xml2::xml_find_first(synthetic_act(), "//sisu"))

  # kuvatavNr exists on chapters, sections AND sub-points. Combining it with
  # peatykkNr produced "I\n\nI. peatükk"; combining it with alampunktNr
  # produced doubled sub-point numbers.
  expect_equal(lengths(regmatches(txt, gregexpr("peat\u00fckk", txt)))[1], 1L)
  expect_match(txt, "I\\. peat\u00fckk \u00dcLDS\u00c4TTED")
})

test_that("chapters and sections open new blocks", {
  txt <- render_content(xml2::xml_find_first(synthetic_act(), "//sisu"))
  expect_match(txt, "\n\nI\\. peat\u00fckk")
  expect_match(txt, "\n\n\u00a7 1\\. First subsection\\.")
})

test_that("sub-points open a new line, not a new block", {
  txt <- render_content(xml2::xml_find_first(synthetic_act(), "//sisu"))
  expect_match(txt, "Second subsection:\n1\\) first point;")
  expect_false(grepl("Second subsection:\n\n1\\)", txt))
})

test_that("sub-point numbers appear once, in display form", {
  # alampunktNr holds "1" and kuvatavNr holds "1)". Emitting both gave
  # "1 1) first point".
  prov <- render_provision(xml2::xml_find_first(synthetic_act(), "//loige[@id='l2']"))
  expect_match(prov, "^Second subsection:\n1\\) first point;\n2\\) second point\\.$")
})

test_that("empty content yields NA rather than an empty string", {
  x <- xml2::read_xml("<oigusakt><sisu/></oigusakt>")
  expect_true(is.na(render_content(xml2::xml_find_first(x, "//sisu"))))
})

# ---------------------------------------------------------------------------
# Provisions
# ---------------------------------------------------------------------------

test_that("chapter context comes from the ancestor axis", {
  # Not every section sits inside a chapter — fourteen do not in the
  # Constitution — so position cannot be used.
  doc <- synthetic_act()
  s <- xml2::xml_find_all(doc, "//paragrahv")

  in_chapter <- section_rows(s[[1]], 1L, "111042025003", FALSE, "url")
  expect_equal(unique(in_chapter$peatykk_nr), "I")
  expect_equal(unique(in_chapter$peatykk), "I. peat\u00fckk")
  expect_equal(unique(in_chapter$peatykk_pealkiri), "\u00dcLDS\u00c4TTED")

  outside <- section_rows(s[[2]], 2L, "111042025003", FALSE, "url")
  expect_true(is.na(outside$peatykk_nr))
  expect_true(is.na(outside$peatykk_pealkiri))
})

test_that("a section with no subsections still yields a row", {
  doc <- synthetic_act()
  s <- xml2::xml_find_all(doc, "//paragrahv")[[2]]
  r <- section_rows(s, 2L, "111042025003", FALSE, "url")

  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$loige_index))
  expect_equal(r$text, "A section with no subsections.")
})

test_that("one row is produced per subsection", {
  doc <- synthetic_act()
  s <- xml2::xml_find_all(doc, "//paragrahv")[[1]]
  r <- section_rows(s, 1L, "111042025003", FALSE, "url")

  expect_equal(nrow(r), 2L)
  expect_equal(r$loige_index, 1:2)
  expect_equal(r$n_alampunkt, c(0L, 2L))
  expect_equal(r$paragrahv_nr, c("1", "1"))
  expect_equal(r$paragrahv, c("\u00a7 1.", "\u00a7 1."))
})

test_that("empty results keep the column shape", {
  e <- etx_empty_paragraphs()
  expect_equal(nrow(e), 0L)
  expect_type(e$paragrahv_index, "integer")
  expect_type(e$is_translation, "logical")

  doc <- synthetic_act()
  s <- xml2::xml_find_all(doc, "//paragrahv")[[1]]
  r <- section_rows(s, 1L, "111042025003", FALSE, "url")
  expect_equal(names(r), names(e))
})

# ---------------------------------------------------------------------------
# Live: metadata
# ---------------------------------------------------------------------------

test_that("an act is retrieved with its metadata", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  d <- etx_get_doc("111042025003")   # the Constitution

  expect_s3_class(d, "tbl_df")
  expect_equal(nrow(d), 1L)
  expect_equal(d$status, "ok")
  expect_equal(d$pealkiri, "Eesti Vabariigi p\u00f5hiseadus")
  expect_equal(d$lyhend, "PS")
  expect_equal(d$grupi_id, "151381")
  expect_equal(d$tekstiliik, "terviktekst")
  expect_equal(d$dokumentliik, "seadus")
  expect_equal(d$akti_staatus, "KEHTIV")
  expect_false(d$is_translation)
})

test_that("the translation link works in both directions", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # An earlier reading of the XML metadata suggested the link was one-way.
  # JSON carries it both ways; the two representations are not supersets of
  # each other.
  et <- etx_get_doc("111042025003")
  en <- etx_get_doc(et$tolke_seos_id)

  expect_true(en$is_translation)
  expect_equal(en$tolke_seos_id, "111042025003")
  expect_match(en$pealkiri, "Constitution")
})

test_that("a translation is current when its validity date matches", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # Compare kehtivuse_algus, not avaldamise_kuupaev: publication dates differ
  # by days even when the translation is up to date.
  et <- etx_get_doc("111042025003")
  en <- etx_get_doc(et$tolke_seos_id)
  expect_equal(et$kehtivuse_algus, en$kehtivuse_algus)
})

test_that("an unknown identifier is reported rather than raised", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # The service returns 500 for identifiers that do not exist, and
  # occasionally for valid ones, so it cannot be treated as fatal.
  d <- etx_get_doc("999999999999")
  expect_equal(d$status, "error")
  expect_false(is.na(d$error_message))
  expect_true(is.na(d$pealkiri))
})

# ---------------------------------------------------------------------------
# Live: text and provisions
# ---------------------------------------------------------------------------

test_that("full text is retrieved and rendered", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  t <- etx_get_text("111042025003")

  expect_equal(t$status, "ok")
  expect_gt(t$n_char, 40000L)

  # Regression tests for the three rendering faults found in development
  expect_false(grepl("riiki,mis", t$text, fixed = TRUE))     # reavahetus
  expect_false(grepl("I\n\nI. peat\u00fckk", t$text))        # doubled chapter
  expect_match(t$text, "\u00a7 1\\. Eesti on iseseisev")     # marker spacing
})

test_that("provisions match the document structure", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  p <- etx_get_paragraphs("111042025003")

  # 168 sections, 324 subsections, 15 chapters — counted during API mapping
  expect_equal(nrow(p), 324L)
  expect_equal(length(unique(p$paragrahv_nr)), 168L)
  expect_equal(length(unique(stats::na.omit(p$peatykk_nr))), 15L)

  # Fourteen subsections belong to sections outside any chapter
  expect_gt(sum(is.na(p$peatykk_nr)), 0L)

  expect_false(anyNA(p$text))
  expect_false(is.unsorted(p$paragrahv_index))
})

test_that("sub-points are rendered inline and counted", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  p <- etx_get_paragraphs("111042025003")
  i <- which(p$n_alampunkt > 0)[1]

  expect_match(p$text[i], "\n1\\) ")
  expect_gt(p$n_alampunkt[i], 1L)
})

test_that("translation XML is unavailable through the API", {
  skip_on_cran()
  skip_if_offline("riigiteataja.ee")

  # Reported to Riigi Teataja on 2026-09-01: /akt/{translationId}/xml returns
  # 500 while the metadata endpoint for the same identifier works. If this
  # test ever fails, the defect has been fixed and etx_get_parallel() can
  # stop going through the bulk archive.
  expect_warning(
    p <- etx_get_paragraphs("523042025002"),
    "500"
  )
  expect_equal(nrow(p), 0L)
})
