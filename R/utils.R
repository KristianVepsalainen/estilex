# Internal helpers shared across estilex.
#
# The HTTP layer is heavier here than in the sibling packages, because the
# Riigi Teataja API returns correct data with incorrect metadata in several
# ways. Each workaround is commented where it appears; the full list is in
# data-raw/api-exploration.R, section 1.

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

ETX_BASE <- "https://www.riigiteataja.ee/public-api/api/v1"
ETX_SITE <- "https://www.riigiteataja.ee"
ETX_BULK <- "https://www.riigiteataja.ee/avaandmed/ERT"

# The leading digit of a twelve-digit identifier encodes the language.
# Verified across the whole English bulk archive: all 6,391 files begin with 5.
ETX_ID_ESTONIAN    <- "1"
ETX_ID_TRANSLATION <- "5"

estilex_user_agent <- function() {
  paste0(
    "estilex/", utils::packageVersion("estilex"),
    " (https://github.com/KristianVepsalainen/estilex)"
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------------
# Identifiers
# ---------------------------------------------------------------------------

#' Coerce an identifier to character without losing digits
#'
#' Identifiers arrive from JSON as doubles. They fit within double precision
#' (twelve digits is well under 2^53), so no big-integer handling is needed,
#' but they must never be printed in scientific notation or compared as
#' numbers. Older consolidated versions use short sequential numbers in the
#' same space — 24304 and 111042025003 are both valid — so no fixed width can
#' be assumed either.
#'
#' @noRd
as_id <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  if (is.character(x)) return(trimws(x))
  out <- format(x, scientific = FALSE, trim = TRUE)
  out[is.na(x)] <- NA_character_
  out
}

#' Validate a single identifier
#' @noRd
check_id <- function(id, arg = "id") {
  id <- as_id(id)
  if (length(id) != 1L || is.na(id) || !nzchar(id)) {
    etx_abort(paste0("`", arg, "` must be a single non-missing identifier."),
              class = "estilex_bad_identifier")
  }
  if (!grepl("^[0-9]+$", id)) {
    etx_abort(
      paste0("`", arg, "` must consist of digits only."),
      paste0("Got: ", id),
      class = "estilex_bad_identifier"
    )
  }
  id
}

#' Split a twelve-digit identifier into its components
#'
#' Format is {type:1}{day:2}{month:2}{year:4}{sequence:3}. Older consolidated
#' versions use short sequential numbers instead and yield all-missing
#' components.
#'
#' @noRd
etx_parse_id <- function(id) {
  id <- as_id(id)
  if (is.na(id) || nchar(id) != 12L) {
    return(list(
      is_translation = NA, day = NA_integer_, month = NA_integer_,
      year = NA_integer_, sequence = NA_integer_
    ))
  }
  list(
    is_translation = substr(id, 1, 1) == ETX_ID_TRANSLATION,
    day            = as.integer(substr(id, 2, 3)),
    month          = as.integer(substr(id, 4, 5)),
    year           = as.integer(substr(id, 6, 9)),
    sequence       = as.integer(substr(id, 10, 12))
  )
}

#' Canonical URLs for a document
#'
#' `source_url` exists to satisfy section 12 of the Riigi Teataja Act, which
#' requires texts reproduced elsewhere to cite the version published in the
#' Riigi Teataja. Every function that returns document content returns this
#' alongside it.
#'
#' The ELI URI resolves on the website but is not exposed through the API, so
#' it is constructed rather than retrieved.
#'
#' @noRd
etx_urls <- function(id) {
  list(
    source_url = paste0(ETX_SITE, "/akt/", id),
    eli        = paste0(ETX_SITE, "/eli/", id)
  )
}

# ---------------------------------------------------------------------------
# HTTP
# ---------------------------------------------------------------------------

#' Build a request with the shared settings
#'
#' 500 is treated as transient and retried. The API returns 500 both for
#' genuinely unknown identifiers and intermittently for valid ones — a
#' translation identifier that returned 500 one day returned 200 a few days
#' later — so a single 500 is not conclusive.
#'
#' @noRd
etx_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(estilex_user_agent()) |>
    httr2::req_retry(
      max_tries = 3,
      # A short, fixed backoff. The 500s this service produces come from a
      # missing identifier or an intermittent fault, neither of which is
      # helped by waiting longer, and the default exponential backoff makes
      # the test suite slow for no benefit.
      backoff = function(i) min(0.5 * i, 2),
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
      }
    ) |>
    # Nothing is raised automatically: 404 and 500 both carry meaning here and
    # are classified by the callers below.
    httr2::req_error(is_error = function(resp) FALSE)
}

#' Perform a request and classify the outcome
#'
#' @return A list with `status` (`"ok"`, `"not_found"` or `"error"`),
#'   `response` and `message`.
#' @noRd
etx_perform <- function(url) {

  resp <- tryCatch(
    httr2::req_perform(etx_request(url)),
    error = function(e) {
      etx_abort(
        "Could not connect to Riigi Teataja.",
        conditionMessage(e),
        class = "estilex_connection_error"
      )
    }
  )

  status <- httr2::resp_status(resp)

  if (status == 200L) {
    return(list(status = "ok", response = resp, message = NA_character_))
  }
  if (status == 404L) {
    return(list(status = "not_found", response = NULL, message = NA_character_))
  }
  if (status == 500L) {
    # Deliberately not an error. Riigi Teataja returns 500 for unknown
    # identifiers as well as for genuine failures, so aborting would report a
    # missing document as a system fault and would make batch retrieval
    # unusable.
    return(list(
      status  = "error",
      response = NULL,
      message = paste0(
        "Server returned HTTP 500. The identifier may not exist, or the ",
        "service may be temporarily unavailable."
      )
    ))
  }

  list(
    status   = "error",
    response = NULL,
    message  = paste0("Unexpected HTTP status ", status, ".")
  )
}

#' Retrieve JSON metadata for a document
#' @noRd
etx_get_json <- function(id) {
  res <- etx_perform(paste0(ETX_BASE, "/akt/", id))
  if (!identical(res$status, "ok")) return(res)

  body <- tryCatch(
    httr2::resp_body_json(res$response),
    error = function(e) {
      etx_abort("Could not parse the JSON response.",
                conditionMessage(e), class = "estilex_parse_error")
    }
  )
  list(status = "ok", body = body, message = NA_character_)
}

#' Retrieve and parse document XML
#'
#' Two workarounds are mandatory here. The service sets
#' `Content-Type: application/octet-stream` on XML, a consequence of serving
#' from S3, so the content-type check must be disabled. And the namespace
#' declaration is not an absolute URI — `tyviseadus_1_10.02.2010` for acts,
#' `Juurakt` for translations — and varies between documents, so it is
#' stripped and all XPath is written without prefixes.
#'
#' @noRd
etx_get_xml <- function(id) {
  res <- etx_perform(paste0(ETX_BASE, "/akt/", id, "/xml"))
  if (!identical(res$status, "ok")) return(res)

  doc <- tryCatch(
    suppressWarnings(
      httr2::resp_body_xml(res$response, check_type = FALSE, encoding = "UTF-8")
    ),
    error = function(e) {
      etx_abort("Could not parse the XML response.",
                conditionMessage(e), class = "estilex_parse_error")
    }
  )

  xml2::xml_ns_strip(doc)
  list(status = "ok", body = doc, message = NA_character_)
}

#' Abort with an estilex condition class
#' @noRd
etx_abort <- function(message, info = NULL, class = "estilex_error") {
  msg <- c(message)
  if (!is.null(info)) msg <- c(msg, i = info)
  rlang::abort(msg, class = c(class, "estilex_error"), call = NULL)
}

# ---------------------------------------------------------------------------
# Field access
# ---------------------------------------------------------------------------

#' Pull a scalar from a nested JSON list, returning NA when absent or empty
#' @noRd
json_chr <- function(x, ...) {
  v <- purrr_pluck(x, ...)
  if (is.null(v) || length(v) == 0L) return(NA_character_)
  v <- trimws(as.character(v[[1]] %||% v))
  if (!nzchar(v)) NA_character_ else v
}

#' @noRd
json_lgl <- function(x, ...) {
  v <- purrr_pluck(x, ...)
  if (is.null(v) || length(v) == 0L) return(NA)
  as.logical(v)
}

#' Minimal pluck, to avoid depending on purrr for one operation
#' @noRd
purrr_pluck <- function(x, ...) {
  keys <- c(...)
  for (k in keys) {
    if (is.null(x) || !is.list(x) || !k %in% names(x)) return(NULL)
    x <- x[[k]]
  }
  x
}

#' Text of the first matching element, or NA
#'
#' Document XML carries no namespace once stripped, so plain XPath applies.
#' @noRd
xml_chr <- function(x, xpath) {
  node <- xml2::xml_find_first(x, xpath)
  if (inherits(node, "xml_missing")) return(NA_character_)
  v <- trimws(xml2::xml_text(node))
  if (is.na(v) || !nzchar(v)) NA_character_ else v
}

#' Parse a date, tolerating both formats the service emits
#'
#' The same field carries a timezone offset in Estonian documents
#' (`2026-03-01+02:00`) but not in translations (`2026-03-01`), and JSON uses
#' full timestamps (`2026-02-28T22:00:00Z`). Taking the first ten characters
#' handles all three without warning.
#'
#' Note that the JSON timestamps are UTC, so a date late in the evening
#' corresponds to the following day in Estonia. Values are returned as
#' recorded rather than shifted, since the service's own display follows
#' Estonian time and shifting would disagree with it.
#'
#' @noRd
parse_date_est <- function(x) {
  if (is.null(x) || length(x) == 0L) return(as.Date(NA))
  suppressWarnings(as.Date(substr(as.character(x), 1, 10), format = "%Y-%m-%d"))
}
