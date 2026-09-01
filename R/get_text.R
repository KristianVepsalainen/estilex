#' Retrieve the text of an Estonian legal act
#'
#' Returns the full running text of an act as a single string, with chapter
#' headings and section numbers in place.
#'
#' Use [etx_get_paragraphs()] instead when the structure matters: it returns
#' one row per subsection, which is usually more useful than a single block
#' of text.
#'
#' @section Rendering:
#'
#' The source marks up chapters (`peatykk`), sections (`paragrahv`),
#' subsections (`loige`) and sub-points (`alampunkt`) separately from their
#' numbering, so the text is reassembled rather than taken verbatim. Chapter
#' and section numbers open a new block; sub-point numbers open a new line.
#'
#' One known limitation: `reavahetus`, the source's explicit line-break
#' element, is not honoured inside a passage of text, so a small number of
#' intentional breaks are rendered as spaces.
#'
#' @param id A Riigi Teataja act identifier. See [etx_get_doc()].
#'
#' @return A one-row [tibble][tibble::tibble] with `status`, `id`, `text`,
#'   `n_char`, `source_url`, `eli` and `error_message`. `text` is `NA` unless
#'   `status` is `"ok"`.
#'
#' @seealso [etx_get_paragraphs()] for structured output,
#'   [etx_get_doc()] for metadata.
#'
#' @source Riigi Teataja, <https://www.riigiteataja.ee>. Section 12 of the
#'   Riigi Teataja Act requires reproduced texts to cite the published
#'   version; `source_url` is provided for that purpose.
#'
#' @examples
#' if (FALSE) {
#'   txt <- etx_get_text("111042025003")
#'   cat(substr(txt$text, 1, 500))
#' }
#'
#' @export
etx_get_text <- function(id) {

  id  <- check_id(id)
  res <- etx_get_xml(id)
  urls <- etx_urls(id)

  if (!identical(res$status, "ok")) {
    return(tibble::tibble(
      status        = res$status,
      id            = as_id(id),
      text          = NA_character_,
      n_char        = NA_integer_,
      source_url    = urls$source_url,
      eli           = urls$eli,
      error_message = res$message
    ))
  }

  sisu <- xml2::xml_find_first(res$body, "//sisu")
  if (inherits(sisu, "xml_missing")) {
    etx_abort(
      "The document contained no <sisu> element.",
      paste0("Identifier: ", id, ". The API may have changed."),
      class = "estilex_parse_error"
    )
  }

  txt <- render_content(sisu)

  tibble::tibble(
    status        = "ok",
    id            = as_id(id),
    text          = txt,
    n_char        = if (is.na(txt)) NA_integer_ else nchar(txt),
    source_url    = urls$source_url,
    eli           = urls$eli,
    error_message = NA_character_
  )
}

#' Retrieve the provisions of an Estonian legal act
#'
#' Returns one row per subsection (`loige`), with the enclosing section and
#' chapter as columns, so provisions can be handled as data rather than
#' parsed out of running text.
#'
#' @section Structure:
#'
#' Estonian drafting nests chapters, sections, subsections and sub-points,
#' and the Riigi Teataja preserves that hierarchy in its XML. Not every
#' section sits inside a chapter — in the Constitution, seven do not — so the
#' chapter columns are missing for those rows rather than invented.
#'
#' A section with no subsections yields a single row whose `loige_index` is
#' missing and whose `text` is taken from the section itself.
#'
#' Sub-point numbers appear inline in `text`; `n_alampunkt` counts them.
#'
#' @section Aligning translations:
#'
#' `paragrahv_nr` is stable across languages. For the Constitution, the
#' Estonian text and its English translation contain the same 168 sections
#' with identical numbering, so the two can be joined on that column.
#' Subsection counts may differ slightly, so alignment is reliable at section
#' level but not below it. [etx_get_parallel()] does this join.
#'
#' @param id A Riigi Teataja act identifier. See [etx_get_doc()].
#'
#' @return A [tibble][tibble::tibble] with one row per subsection:
#'   \describe{
#'     \item{`id`}{The identifier requested.}
#'     \item{`is_translation`}{Whether this is a translation.}
#'     \item{`peatykk_nr`, `peatykk_pealkiri`}{Chapter number and heading, or
#'       missing when the section is not inside a chapter.}
#'     \item{`paragrahv_index`}{Position of the section in the document.}
#'     \item{`paragrahv_nr`}{Section number, e.g. `"1"`. Stable across
#'       languages.}
#'     \item{`paragrahv`}{Section number as displayed, e.g. `"\u00a7 1."`}
#'     \item{`loige_index`}{Position of the subsection within its section.}
#'     \item{`n_alampunkt`}{Number of sub-points.}
#'     \item{`text`}{The text of the subsection.}
#'     \item{`source_url`}{Canonical link, per section 12 of the Riigi
#'       Teataja Act.}
#'   }
#'
#'   Zero rows, with a warning, if the document cannot be retrieved.
#'
#' @seealso [etx_get_text()], [etx_get_parallel()]
#'
#' @examples
#' if (FALSE) {
#'   p <- etx_get_paragraphs("111042025003")
#'   p[1:5, c("peatykk_nr", "paragrahv", "text")]
#'
#'   # Sections per chapter
#'   table(p$peatykk_nr, useNA = "ifany")
#' }
#'
#' @export
etx_get_paragraphs <- function(id) {

  id  <- check_id(id)
  res <- etx_get_xml(id)

  if (!identical(res$status, "ok")) {
    warning("Could not retrieve ", id, ": ",
            res$message %||% res$status, call. = FALSE)
    return(etx_empty_paragraphs())
  }

  sisu <- xml2::xml_find_first(res$body, "//sisu")
  if (inherits(sisu, "xml_missing")) {
    warning("No <sisu> element in ", id, ".", call. = FALSE)
    return(etx_empty_paragraphs())
  }

  sections <- xml2::xml_find_all(sisu, ".//paragrahv")
  if (length(sections) == 0L) {
    warning("No sections found in ", id, ".", call. = FALSE)
    return(etx_empty_paragraphs())
  }

  parsed <- etx_parse_id(id)
  urls   <- etx_urls(id)

  rows <- lapply(seq_along(sections), function(i) {
    section_rows(sections[[i]], i, id, parsed$is_translation, urls$source_url)
  })

  out <- do.call(rbind, rows)
  if (is.null(out)) etx_empty_paragraphs() else out
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Rows for one section
#'
#' Chapter context is found on the ancestor axis rather than by position,
#' because sections may sit directly under <sisu> without an enclosing
#' chapter.
#'
#' @noRd
section_rows <- function(section, index, id, is_translation, source_url) {

  chapter <- xml2::xml_find_first(section, "ancestor::peatykk")
  peatykk_nr <- if (inherits(chapter, "xml_missing")) {
    NA_character_
  } else {
    xml_chr(chapter, "./peatykkNr")
  }
  peatykk_pealkiri <- if (inherits(chapter, "xml_missing")) {
    NA_character_
  } else {
    xml_chr(chapter, "./peatykkPealkiri")
  }
  peatykk <- if (inherits(chapter, "xml_missing")) {
    NA_character_
  } else {
    xml_chr(chapter, "./kuvatavNr")
  }
  nr       <- xml_chr(section, "./paragrahvNr")
  kuvatav  <- xml_chr(section, "./kuvatavNr")
  loiked   <- xml2::xml_find_all(section, "./loige")

  base <- function(loige_index, n_alampunkt, text) {
    tibble::tibble(
      id               = as_id(id),
      is_translation   = is_translation,
      peatykk_nr       = peatykk_nr,
      peatykk          = peatykk,
      peatykk_pealkiri = peatykk_pealkiri,
      paragrahv_index  = as.integer(index),
      paragrahv_nr     = nr,
      paragrahv        = kuvatav,
      loige_index      = loige_index,
      n_alampunkt      = as.integer(n_alampunkt),
      text             = text,
      source_url       = source_url
    )
  }

  # A section with no subsections still deserves a row: its text lives
  # directly on the section.
  if (length(loiked) == 0L) {
    return(base(
      NA_integer_,
      length(xml2::xml_find_all(section, ".//alampunkt")),
      render_provision(section)
    ))
  }

  rows <- lapply(seq_along(loiked), function(k) {
    base(
      as.integer(k),
      length(xml2::xml_find_all(loiked[[k]], ".//alampunkt")),
      render_provision(loiked[[k]])
    )
  })

  do.call(rbind, rows)
}

#' Render the text of a single provision
#'
#' Sub-point numbers are kept inline: they carry meaning in cross-references
#' ("§ 12 clause 3"), so dropping them would make the text harder to use than
#' the small amount of noise they add.
#'
#' @noRd
render_provision <- function(node) {

  parts <- xml2::xml_find_all(node, ".//alampunkt/kuvatavNr | .//tavatekst")
  if (length(parts) == 0L) return(NA_character_)

  names_ <- xml2::xml_name(parts)
  texts <- vapply(parts, node_text, character(1))

  sep <- rep(" ", length(parts))
  sep[names_ == "kuvatavNr"] <- "\n"
  # Consecutive passages of text are separate paragraphs.
  is_text <- names_ == "tavatekst"
  sep[is_text & c(FALSE, utils::head(is_text, -1))] <- "\n"
  sep[1] <- ""

  tidy_text(paste0(sep, texts, collapse = ""))
}

#' Render a whole document
#'
#' Walks the structural markers and the text in document order, opening a new
#' block at each chapter and section. A plain xml_text() would run numbering
#' into the text that follows it.
#'
#' @noRd
render_content <- function(sisu) {

  parts <- xml2::xml_find_all(
    sisu,
    ".//kuvatavNr | .//peatykkPealkiri | .//tavatekst"
  )

  names_ <- xml2::xml_name(parts)
  texts  <- vapply(parts, node_text, character(1))

  # kuvatavNr appears on chapters, sections and sub-points alike. A sub-point
  # opens a new line within its subsection; the other two open a block.
  is_sub <- names_ == "kuvatavNr" &
    vapply(parts, function(p) {
      xml2::xml_name(xml2::xml_parent(p)) == "alampunkt"
    }, logical(1))

  prev <- c("", utils::head(names_, -1))

  sep <- vapply(seq_along(names_), function(i) {
    switch(
      names_[i],
      kuvatavNr       = if (is_sub[i]) "\n" else "\n\n",
      peatykkPealkiri = " ",
      tavatekst       = if (identical(prev[i], "tavatekst")) "\n" else " ",
      " "
    )
  }, character(1))
  sep[1] <- ""

  tidy_text(paste0(sep, texts, collapse = ""))
}

#' Text of a node, honouring explicit line breaks
#'
#' `reavahetus` is an empty element inside a passage of text. xml_text()
#' ignores it and runs the surrounding fragments together, so the children
#' are walked instead.
#' @noRd
node_text <- function(node) {
  kids <- xml2::xml_contents(node)
  if (length(kids) == 0L) return(trimws(xml2::xml_text(node)))
  out <- vapply(kids, function(k) {
    if (xml2::xml_name(k) == "reavahetus") "\n" else xml2::xml_text(k)
  }, character(1))
  trimws(paste0(out, collapse = ""))
}

#' Collapse stray whitespace without disturbing block structure
#' @noRd
tidy_text <- function(x) {
  x <- gsub("[ \t]+", " ", x)
  x <- gsub(" *\n *", "\n", x)
  x <- gsub("\n{3,}", "\n\n", x)
  x <- trimws(x)
  if (!nzchar(x)) NA_character_ else x
}

#' Zero-row provision table with the correct columns and types
#' @noRd
etx_empty_paragraphs <- function() {
  tibble::tibble(
    id               = character(0),
    is_translation   = logical(0),
    peatykk_nr       = character(0),
    peatykk          = character(0),
    peatykk_pealkiri = character(0),
    paragrahv_index  = integer(0),
    paragrahv_nr     = character(0),
    paragrahv        = character(0),
    loige_index      = integer(0),
    n_alampunkt      = integer(0),
    text             = character(0),
    source_url       = character(0)
  )
}
