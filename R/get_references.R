#' Retrieve the amendment history of an Estonian legal act
#'
#' Returns the acts that have amended a given act, as an edge table suitable
#' for citation network analysis. The Riigi Teataja records these as
#' `muutmismarge` elements, each naming the amending instrument, where it was
#' published, and when the change entered into force.
#'
#' @section Two levels of amendment note:
#'
#' Amendment notes appear both at document level and attached to individual
#' sections. In the Constitution, seven of sixteen are document-level and the
#' remaining nine belong to specific sections. The `scope_nr` and `scope`
#' columns give the section a note belongs to, and are missing for
#' document-level notes.
#'
#' Amending instruments carry no amendment history of their own, so calling
#' this on one returns zero rows.
#'
#' @section Following the network:
#'
#' `akt_viide` holds the identifier of the amending act in the same space as
#' the identifiers accepted by [etx_get_doc()], so the network can be
#' traversed directly:
#'
#' ```
#' refs <- etx_get_references("111042025003")
#' etx_get_doc(refs$akt_viide[1])
#' ```
#'
#' @section Publication reference:
#'
#' `rt_osa`, `rt_aasta`, `rt_nr` and `rt_artikkel` together form the
#' traditional Riigi Teataja citation — for example RT I 2003, 29, 174 — which
#' is how Estonian legal sources refer to an amendment in print.
#' `rt_viide` assembles them into that form.
#'
#' @param id A Riigi Teataja act identifier. See [etx_get_doc()].
#'
#' @return A [tibble][tibble::tibble] with one row per amendment note:
#'   \describe{
#'     \item{`id`}{The identifier requested.}
#'     \item{`ref_index`}{Position of the note in the document.}
#'     \item{`scope_nr`}{Number of the section the note belongs to, or
#'       missing for document-level notes.}
#'     \item{`scope`}{The same as displayed, e.g. `"\u00a7 127."}
#'     \item{`akt_viide`}{Identifier of the amending act. Accepted by
#'       [etx_get_doc()].}
#'     \item{`akti_kuupaev`}{Date of the amending act.}
#'     \item{`joustumine`}{Date the amendment entered into force.}
#'     \item{`rt_osa`, `rt_aasta`, `rt_nr`, `rt_artikkel`}{Components of the
#'       Riigi Teataja publication reference.}
#'     \item{`rt_viide`}{The publication reference in citation form.}
#'     \item{`source_url`}{Canonical link, per section 12 of the Riigi
#'       Teataja Act.}
#'   }
#'
#'   Zero rows if the act has never been amended, with a warning if it could
#'   not be retrieved at all.
#'
#' @seealso [etx_get_doc()], [etx_get_paragraphs()]
#'
#' @source Riigi Teataja, <https://www.riigiteataja.ee>.
#'
#' @examples
#' if (FALSE) {
#'   refs <- etx_get_references("111042025003")
#'   refs[, c("scope", "akt_viide", "joustumine", "rt_viide")]
#'
#'   # Which sections have been amended most often
#'   sort(table(refs$scope_nr), decreasing = TRUE)
#'
#'   # Follow the first amendment
#'   etx_get_doc(refs$akt_viide[1])$pealkiri
#' }
#'
#' @export
etx_get_references <- function(id) {

  id  <- check_id(id)
  res <- etx_get_xml(id)

  if (!identical(res$status, "ok")) {
    warning("Could not retrieve ", id, ": ",
            res$message %||% res$status, call. = FALSE)
    return(etx_empty_references())
  }

  notes <- xml2::xml_find_all(res$body, "//muutmismarge")
  if (length(notes) == 0L) return(etx_empty_references())

  urls <- etx_urls(id)

  rows <- lapply(seq_along(notes), function(i) {
    reference_row(notes[[i]], i, id, urls$source_url)
  })

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Build one row from an amendment note
#'
#' The section a note belongs to is found on the ancestor axis rather than by
#' position, because notes occur both at document level and inside sections.
#'
#' @noRd
reference_row <- function(note, index, id, source_url) {

  section <- xml2::xml_find_first(note, "ancestor::paragrahv")
  has_section <- !inherits(section, "xml_missing")

  rt_osa      <- xml_chr(note, ".//RTosa")
  rt_aasta    <- xml_chr(note, ".//RTaasta")
  rt_nr       <- xml_chr(note, ".//RTnr")
  rt_artikkel <- xml_chr(note, ".//RTartikkel")

  tibble::tibble(
    id           = as_id(id),
    ref_index    = as.integer(index),
    scope_nr     = if (has_section) xml_chr(section, "./paragrahvNr") else NA_character_,
    scope        = if (has_section) xml_chr(section, "./kuvatavNr")   else NA_character_,
    akt_viide    = as_id(xml_chr(note, ".//aktViide")),
    akti_kuupaev = parse_date_est(xml_chr(note, "./aktikuupaev")),
    joustumine   = parse_date_est(xml_chr(note, "./joustumine")),
    rt_osa       = rt_osa,
    rt_aasta     = rt_aasta,
    rt_nr        = rt_nr,
    rt_artikkel  = rt_artikkel,
    rt_viide     = format_rt_reference(rt_osa, rt_aasta, rt_nr, rt_artikkel),
    source_url   = source_url
  )
}

#' Assemble a Riigi Teataja publication reference
#'
#' Estonian legal sources cite amendments as "RT I 2003, 29, 174" — part,
#' year, issue, article. Any component may be absent, so the parts present
#' are joined and the result is missing only when nothing is available.
#'
#' @noRd
format_rt_reference <- function(osa, aasta, nr, artikkel) {
  numbers <- c(aasta, nr, artikkel)
  numbers <- numbers[!is.na(numbers)]

  if (is.na(osa) && length(numbers) == 0L) return(NA_character_)
  if (length(numbers) == 0L) return(osa)

  joined <- paste(numbers, collapse = ", ")
  if (is.na(osa)) joined else paste0(osa, " ", joined)
}

#' Zero-row reference table with the correct columns and types
#' @noRd
etx_empty_references <- function() {
  tibble::tibble(
    id           = character(0),
    ref_index    = integer(0),
    scope_nr     = character(0),
    scope        = character(0),
    akt_viide    = character(0),
    akti_kuupaev = as.Date(character(0)),
    joustumine   = as.Date(character(0)),
    rt_osa       = character(0),
    rt_aasta     = character(0),
    rt_nr        = character(0),
    rt_artikkel  = character(0),
    rt_viide     = character(0),
    source_url   = character(0)
  )
}
