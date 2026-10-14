#' Retrieve an Estonian legal act
#'
#' Fetches metadata for a single act from the Riigi Teataja and returns it as
#' a one-row tibble. Works for both Estonian texts and their English
#' translations, which share one identifier space.
#'
#' @section Identifiers:
#'
#' Riigi Teataja identifiers are twelve digits, encoding the language, the
#' publication date and a sequence number:
#'
#' ```
#' 111042025003  ->  1 | 11 | 04 | 2025 | 003   Estonian
#' 523042025002  ->  5 | 23 | 04 | 2025 | 002   translation
#' ```
#'
#' A leading 1 marks an Estonian text and a 5 marks a translation, which
#' `is_translation` reports. Older consolidated versions use short sequential
#' numbers in the same space, such as `24304`, so identifiers are always
#' handled as strings and no fixed width is assumed.
#'
#' Two identifier spaces coexist. `id` and `kehtiv_id` refer to one
#' consolidated version; `grupi_id` refers to the act across all its versions
#' and is what `etx_get_versions()` expects.
#'
#' @section Missing documents:
#'
#' The service returns HTTP 500 for identifiers that do not exist, and
#' occasionally for valid ones. estilex therefore reports `status = "error"`
#' with an explanation in `error_message` rather than raising a condition:
#' a 500 cannot be distinguished from a genuine outage, and treating it as
#' fatal would make batch retrieval unusable. Requests are retried before
#' giving up. A clean 404 is reported as `"not_found"`.
#'
#' @section Attribution:
#'
#'
#' @param id A Riigi Teataja act identifier.
#'
#' @return A one-row [tibble][tibble::tibble]:
#'   \describe{
#'     \item{`status`}{`"ok"`, `"not_found"` or `"error"`.}
#'     \item{`id`}{The identifier requested.}
#'     \item{`kehtiv_id`}{Identifier of the version currently in force.}
#'     \item{`grupi_id`}{Identifier of the act across all versions.}
#'     \item{`pealkiri`, `lyhend`}{Title and abbreviation.}
#'     \item{`tekstiliik`}{Text type, e.g. `"terviktekst"` (consolidated).}
#'     \item{`dokumentliik`}{Instrument type, e.g. `"seadus"` (act).}
#'     \item{`akti_staatus`}{e.g. `"KEHTIV"` (in force).}
#'     \item{`kehtivuse_algus`, `kehtivuse_lopp`}{Period of validity.}
#'     \item{`avaldamise_kuupaev`}{Publication date.}
#'     \item{`is_translation`}{Whether this is a translation.}
#'     \item{`tolke_seos_id`}{Identifier of the linked translation, or of the
#'       Estonian original when this row is a translation. The link works in
#'       both directions.}
#'     \item{`tolke_seos_tekst`}{Title of the linked document.}
#'     \item{`liigitused`}{Systematic classification codes, semicolon
#'       separated.}
#'     \item{`n_viited`}{Number of related materials. These are annotated
#'       editions, court analyses and translations into other languages —
#'       not cross-references to other acts, which
#'       [etx_get_references()] returns.}
#'     \item{`error_message`}{Explanation when `status` is `"error"`.}
#'   }
#'
#' @source Riigi Teataja, <https://www.riigiteataja.ee>.
#'
#' @examples
#' if (FALSE) {
#'   # The Constitution of the Republic of Estonia
#'   doc <- etx_get_doc("111042025003")
#'   doc$pealkiri
#'
#'   # Its English translation, reached through the link
#'   etx_get_doc(doc$tolke_seos_id)$pealkiri
#' }
#'
#' @export
etx_get_doc <- function(id) {

  id  <- check_id(id)
  res <- etx_get_json(id)

  if (!identical(res$status, "ok")) {
    return(etx_doc_row(id, status = res$status,
                       values = list(error_message = res$message)))
  }

  etx_parse_doc(res$body, id)
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Construct a document row
#'
#' Every path runs through this so that the columns and their types are
#' identical whether the request succeeded, found nothing, or failed. Callers
#' row-bind these across many identifiers.
#'
#' @noRd
etx_doc_row <- function(id, status, values = list()) {

  parsed <- etx_parse_id(id)
  urls   <- etx_urls(id)

  template <- list(
    status             = status,
    id                 = as_id(id),
    kehtiv_id          = NA_character_,
    grupi_id           = NA_character_,
    pealkiri           = NA_character_,
    lyhend             = NA_character_,
    tekstiliik         = NA_character_,
    dokumentliik       = NA_character_,
    akti_staatus       = NA_character_,
    kehtivus_staatus   = NA_character_,
    kehtivuse_algus    = as.Date(NA),
    kehtivuse_lopp     = as.Date(NA),
    avaldamise_kuupaev = as.Date(NA),
    is_translation     = parsed$is_translation,
    tolke_seos_id      = NA_character_,
    tolke_seos_tekst   = NA_character_,
    liigitused         = NA_character_,
    n_viited           = NA_integer_,
    source_url         = urls$source_url,
    error_message      = NA_character_
  )

  for (nm in names(values)) template[[nm]] <- values[[nm]]

  tibble::as_tibble(template)
}

#' Parse the JSON metadata of an act
#'
#' Note that JSON and XML expose different fields for the same document — the
#' translation link exists only in JSON — so neither can be derived from the
#' other.
#'
#' @noRd
etx_parse_doc <- function(j, id) {

  if (!is.list(j) || length(j) == 0L) {
    etx_abort("The response contained no document data.",
              paste0("Identifier: ", id, ". The API may have changed."),
              class = "estilex_parse_error")
  }

  p <- j[["aktiParameetrid"]]

  # `staatus` inside aktiParameetrid would collide with the package-wide
  # `status` column, so it is returned as `kehtivus_staatus`.
  liik <- j[["liigitused"]]
  liik <- if (is.null(liik) || length(liik) == 0L) {
    NA_character_
  } else {
    paste(unlist(liik), collapse = "; ")
  }

  etx_doc_row(
    id,
    status = "ok",
    values = list(
      kehtiv_id          = as_id(j[["kehtivId"]]),
      grupi_id           = as_id(j[["grupiId"]]),
      pealkiri           = json_chr(p, "pealkiri"),
      lyhend             = json_chr(p, "lyhend"),
      tekstiliik         = json_chr(j, "tekstiliik"),
      dokumentliik       = json_chr(j, "dokumentliik"),
      akti_staatus       = json_chr(j, "aktiStaatus"),
      kehtivus_staatus   = json_chr(p, "staatus"),
      kehtivuse_algus    = parse_date_est(p[["kehtivuseAlgus"]]),
      kehtivuse_lopp     = parse_date_est(p[["kehtivuseLopp"]]),
      avaldamise_kuupaev = parse_date_est(p[["avaldamiseKuupaev"]]),
      tolke_seos_id      = as_id(j[["tolkeSeosId"]]),
      tolke_seos_tekst   = json_chr(j, "tolkeSeosTekst"),
      liigitused         = liik,
      n_viited           = length(j[["viited"]] %||% list())
    )
  )
}
