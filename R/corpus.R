#' Download the Riigi Teataja bulk archive
#'
#' The API has no search or enumeration endpoint, so the only way to obtain a
#' list of identifiers — and the only reliable route to English translations —
#' is the bulk archive of XML documents.
#'
#' @section Which archive to download:
#'
#' Archives are published per year, in Estonian and in English. They are
#' **complete snapshots**, not that year's documents: the 2024 and 2026
#' English archives share no identifiers at all, because a translation
#' receives a new identifier whenever it is updated. Download the most recent
#' archive; earlier ones are only useful for studying how documents changed.
#'
#' Sizes differ enormously. The English archive is around 1 GB; the Estonian
#' one for 2026 is over 40 GB, apparently because the June 2026 system
#' migration touched every document. Downloading it is guarded: in an
#' interactive session you are asked to confirm, and otherwise `force = TRUE`
#' is required.
#'
#' @section Where it is stored:
#'
#' By default the archive goes to a temporary directory and is kept for the
#' rest of the session, so nothing is written to your file system
#' permanently. Since re-downloading a gigabyte in every session is
#' unpleasant, `dest` lets you keep it somewhere of your choosing:
#'
#' ```
#' etx_download_corpus(dest = "~/riigiteataja")
#' ```
#'
#' A path given in `dest` is used again on subsequent calls without
#' re-downloading, unless `refresh = TRUE`.
#'
#' Better still, set the `ESTILEX_CORPUS_DIR` environment variable once, in
#' `~/.Renviron`:
#'
#' ```
#' ESTILEX_CORPUS_DIR=~/riigiteataja
#' ```
#'
#' estilex then picks up an archive already in that directory without being
#' asked, so [etx_list_documents()] and [etx_get_parallel()] work immediately
#' in a new session. `usethis::edit_r_environ()` opens the file.
#'
#' @param lang `"en"` for English translations or `"et"` for Estonian texts.
#' @param year Archive year. Defaults to the most recent available.
#' @param dest Directory to store the archive in. Defaults to a temporary
#'   directory that is discarded when the session ends.
#' @param refresh Download again even if the archive is already present.
#' @param force Proceed with a very large download without confirmation.
#' @param progress Show download progress. Defaults to [interactive()].
#'
#' @return Invisibly, the path to the archive file.
#'
#' @seealso [etx_list_documents()] for the identifiers it contains,
#'   [etx_corpus_status()], [etx_clear_corpus()], [etx_list_archives()]
#'
#' @source Riigi Teataja open data,
#'   <https://www.riigiteataja.ee/avaandmed/ERT/>.
#'
#' @examples
#' if (FALSE) {
#'   # English translations, most recent archive (~1 GB)
#'   etx_download_corpus("en")
#'
#'   # Keep it between sessions
#'   etx_download_corpus("en", dest = "~/riigiteataja")
#' }
#'
#' @export
etx_download_corpus <- function(lang = c("en", "et"), year = NULL,
                                dest = NULL, refresh = FALSE, force = FALSE,
                                progress = interactive()) {

  lang <- match.arg(lang)
  year <- resolve_archive_year(lang, year)
  name <- archive_name(lang, year)
  url  <- paste0(ETX_BULK, "/", name)

  dir <- resolve_archive_dir(dest)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, name)

  if (file.exists(path) && !refresh) {
    if (isTRUE(progress)) {
      message("Using the archive already at ", path, ".")
    }
    register_archive(lang, year, path)
    return(invisible(path))
  }

  size <- archive_size(url)
  confirm_large_download(size, name, force, progress)

  if (isTRUE(progress)) {
    message("Downloading ", name,
            if (!is.na(size)) paste0(" (", format_bytes(size), ")") else "",
            " to ", dir, ".")
  }

  req <- etx_request(url)
  if (isTRUE(progress)) req <- httr2::req_progress(req)

  resp <- tryCatch(
    httr2::req_perform(req, path = path),
    error = function(e) {
      etx_abort("Could not download the archive.",
                conditionMessage(e), class = "estilex_connection_error")
    }
  )

  if (httr2::resp_status(resp) != 200L) {
    unlink(path)
    etx_abort(
      paste0("Could not download ", name, "."),
      paste0("Server returned HTTP ", httr2::resp_status(resp), "."),
      class = "estilex_http_error"
    )
  }

  register_archive(lang, year, path)
  invisible(path)
}

#' List the documents in a downloaded archive
#'
#' Returns every identifier the archive contains, with its components decoded
#' from the identifier itself. The archive must have been downloaded first
#' with [etx_download_corpus()].
#'
#' @section What the identifiers tell you:
#'
#' Identifiers encode the language, the publication date of that particular
#' version, and a sequence number, so a useful amount can be learned without
#' opening a single document. Note that `year` is the year that *version* was
#' published, which for a translation is the year it was last revised.
#'
#' @param lang `"en"` or `"et"`.
#'
#' @return A [tibble][tibble::tibble] with one row per document:
#'   `id`, `is_translation`, `day`, `month`, `year`, `sequence`, `file`.
#'
#' @seealso [etx_download_corpus()]
#'
#' @examples
#' if (FALSE) {
#'   etx_download_corpus("en")
#'   idx <- etx_list_documents("en")
#'
#'   nrow(idx)
#'   table(idx$year)
#' }
#'
#' @export
etx_list_documents <- function(lang = c("en", "et")) {

  lang <- match.arg(lang)
  entry <- archive_entry(lang)

  if (is.null(entry)) {
    etx_abort(
      paste0("No ", lang, " archive has been downloaded in this session."),
      paste0('Run etx_download_corpus("', lang, '") first.'),
      class = "estilex_no_corpus"
    )
  }

  if (!is.null(entry$index)) return(entry$index)

  files <- tryCatch(
    utils::unzip(entry$path, list = TRUE),
    error = function(e) {
      etx_abort("Could not read the archive.",
                conditionMessage(e), class = "estilex_parse_error")
    }
  )

  ids    <- sub("\\.xml$", "", basename(files$Name))
  parsed <- lapply(ids, etx_parse_id)

  index <- tibble::tibble(
    id             = ids,
    is_translation = vapply(parsed, function(p) p$is_translation, logical(1)),
    day            = vapply(parsed, function(p) p$day, integer(1)),
    month          = vapply(parsed, function(p) p$month, integer(1)),
    year           = vapply(parsed, function(p) p$year, integer(1)),
    sequence       = vapply(parsed, function(p) p$sequence, integer(1)),
    file           = files$Name
  )

  entry$index <- index
  assign(lang, entry, envir = .etx_cache)

  index
}

#' Report on the archives held in this session
#'
#' @return A [tibble][tibble::tibble] with one row per language:
#'   `lang`, `available`, `year`, `path`, `size_mb`, `n_documents`,
#'   `is_temporary`.
#'
#' @seealso [etx_download_corpus()]
#'
#' @examples
#' etx_corpus_status()
#'
#' @export
etx_corpus_status <- function() {
  rows <- lapply(c("en", "et"), function(lang) {
    e <- archive_entry(lang)
    if (is.null(e)) {
      return(tibble::tibble(
        lang = lang, available = FALSE, year = NA_integer_,
        path = NA_character_, size_mb = NA_real_,
        n_documents = NA_integer_, is_temporary = NA
      ))
    }
    tibble::tibble(
      lang         = lang,
      available    = TRUE,
      year         = e$year,
      path         = e$path,
      size_mb      = round(file.size(e$path) / 1024^2, 1),
      n_documents  = if (is.null(e$index)) NA_integer_ else nrow(e$index),
      is_temporary = startsWith(e$path, tempdir())
    )
  })
  do.call(rbind, rows)
}

#' Forget the archives held in this session
#'
#' Removes archives from the session and deletes those stored in the
#' temporary directory. Archives you placed elsewhere with `dest` are left on
#' disk and merely forgotten, so they can be picked up again by calling
#' [etx_download_corpus()] with the same `dest`.
#'
#' @return Invisibly, the number of archives forgotten.
#'
#' @examples
#' etx_clear_corpus()
#'
#' @export
etx_clear_corpus <- function() {
  n <- 0L
  for (lang in c("en", "et")) {
    e <- archive_entry(lang)
    if (is.null(e)) next
    if (startsWith(e$path, tempdir())) unlink(e$path)
    rm(list = lang, envir = .etx_cache)
    n <- n + 1L
  }
  invisible(n)
}

#' List the archives available for download
#'
#' Reads the open data directory and reports which archives exist, without
#' downloading anything.
#'
#' @return A [tibble][tibble::tibble] with `file`, `lang` and `year`.
#'
#' @seealso [etx_download_corpus()]
#'
#' @examples
#' if (FALSE) {
#'   etx_list_archives()
#' }
#'
#' @export
etx_list_archives <- function() {

  res <- etx_perform(paste0(ETX_BULK, "/"))
  if (!identical(res$status, "ok")) {
    etx_abort("Could not read the open data directory.",
              paste0("Tried ", ETX_BULK, "/"),
              class = "estilex_http_error")
  }

  html <- httr2::resp_body_string(res$response)
  parse_archive_listing(html)
}

# ---------------------------------------------------------------------------
# Session cache
# ---------------------------------------------------------------------------

# Session-scoped. Archives default to tempdir(), so nothing persists unless
# the user asks for it with `dest` — no CRAN policy on user directories
# applies and no consent prompt is needed for the default path.
.etx_cache <- new.env(parent = emptyenv())

#' Find the archive for a language
#'
#' Looks in the session cache first, then at the directory named by the
#' ESTILEX_CORPUS_DIR environment variable. The second path matters because a
#' package-local environment does not survive restarting R, and re-registering
#' a gigabyte archive by hand at the start of every session is exactly the
#' friction that variable removes.
#'
#' @noRd
archive_entry <- function(lang) {

  if (exists(lang, envir = .etx_cache, inherits = FALSE)) {
    e <- get(lang, envir = .etx_cache)
    # The file may have gone away between calls, e.g. a tempdir sweep.
    if (file.exists(e$path)) return(e)
    rm(list = lang, envir = .etx_cache)
  }

  adopt_configured_archive(lang)
}

#' Adopt an archive from ESTILEX_CORPUS_DIR, if one is there
#'
#' The most recent year present is chosen, matching the default behaviour of
#' etx_download_corpus(). Returns NULL when the variable is unset or the
#' directory holds no archive for this language.
#'
#' @noRd
adopt_configured_archive <- function(lang) {

  dir <- Sys.getenv("ESTILEX_CORPUS_DIR", unset = "")
  if (!nzchar(dir)) return(NULL)

  dir <- path.expand(dir)
  if (!dir.exists(dir)) return(NULL)

  pattern <- if (identical(lang, "en")) {
    "^xml[.][0-9]{4}[.]en[.]zip$"
  } else {
    "^xml[.][0-9]{4}[.]zip$"
  }

  found <- list.files(dir, pattern = pattern)
  if (length(found) == 0L) return(NULL)

  years <- as.integer(sub("^xml[.]([0-9]{4}).*$", "\\1", found))
  pick  <- which.max(years)

  register_archive(lang, years[pick], file.path(dir, found[pick]))
  get(lang, envir = .etx_cache)
}

#' @noRd
register_archive <- function(lang, year, path) {
  assign(lang, list(year = year, path = path, index = NULL), envir = .etx_cache)
}

#' Decide where an archive should live
#'
#' An explicit `dest` wins; otherwise ESTILEX_CORPUS_DIR if it is set; and
#' failing that a temporary directory, which is discarded with the session and
#' therefore needs no consent to write to.
#'
#' @noRd
resolve_archive_dir <- function(dest = NULL) {
  if (!is.null(dest)) {
    return(normalizePath(path.expand(dest), mustWork = FALSE))
  }
  configured <- Sys.getenv("ESTILEX_CORPUS_DIR", unset = "")
  if (nzchar(configured)) {
    return(normalizePath(path.expand(configured), mustWork = FALSE))
  }
  file.path(tempdir(), "estilex")
}

# ---------------------------------------------------------------------------
# Archive naming and discovery
# ---------------------------------------------------------------------------

#' @noRd
archive_name <- function(lang, year) {
  if (identical(lang, "en")) {
    paste0("xml.", year, ".en.zip")
  } else {
    paste0("xml.", year, ".zip")
  }
}

#' Parse the open directory listing
#'
#' The page is a plain nginx index with a Cloudflare script appended, so the
#' links are extracted with a pattern rather than an HTML parser.
#'
#' @noRd
parse_archive_listing <- function(html) {
  m <- regmatches(html, gregexpr("xml\\.[0-9]{4}(\\.en)?\\.zip", html))[[1]]
  m <- unique(m)

  if (length(m) == 0L) {
    etx_abort(
      "No archives found in the open data directory.",
      "The layout may have changed.",
      class = "estilex_parse_error"
    )
  }

  out <- tibble::tibble(
    file = m,
    lang = ifelse(grepl("\\.en\\.zip$", m), "en", "et"),
    year = as.integer(sub("^xml\\.([0-9]{4}).*$", "\\1", m))
  )
  out[order(out$lang, -out$year), , drop = FALSE]
}

#' Choose the archive year, defaulting to the most recent published
#' @noRd
resolve_archive_year <- function(lang, year = NULL) {

  if (!is.null(year)) {
    year <- suppressWarnings(as.integer(year))
    if (length(year) != 1L || is.na(year)) {
      etx_abort("`year` must be a single four-digit year.",
                class = "estilex_bad_argument")
    }
    return(year)
  }

  available <- etx_list_archives()
  available <- available[available$lang == lang, , drop = FALSE]

  if (nrow(available) == 0L) {
    etx_abort(paste0("No ", lang, " archives are published."),
              class = "estilex_no_corpus")
  }

  max(available$year)
}

# ---------------------------------------------------------------------------
# Size handling
# ---------------------------------------------------------------------------

#' Content length of an archive, without downloading it
#' @noRd
archive_size <- function(url) {
  resp <- tryCatch(
    httr2::req_perform(httr2::req_method(etx_request(url), "HEAD")),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NA_real_)
  len <- httr2::resp_header(resp, "content-length")
  if (is.null(len)) NA_real_ else as.numeric(len)
}

# Above this, downloading by accident is costly enough to be worth a stop.
# The Estonian 2026 archive is around 40 GB.
ETX_LARGE_DOWNLOAD <- 5 * 1024^3

#' @noRd
confirm_large_download <- function(size, name, force, progress,
                                   interactive = base::interactive()) {

  if (isTRUE(force) || is.na(size) || size < ETX_LARGE_DOWNLOAD) {
    return(invisible(TRUE))
  }

  msg <- paste0(name, " is ", format_bytes(size),
                ". Downloading it will take a long time and use that much ",
                "disk space.")

  if (!isTRUE(interactive)) {
    etx_abort(
      msg,
      "Pass force = TRUE to proceed in a non-interactive session.",
      class = "estilex_large_download"
    )
  }

  message(msg, "\n")
  answer <- readline("Continue? [y/N] ")

  if (!tolower(trimws(answer)) %in% c("y", "yes")) {
    etx_abort("Download cancelled.", class = "estilex_large_download")
  }
  invisible(TRUE)
}

#' @noRd
format_bytes <- function(x) {
  if (is.na(x)) return("unknown size")
  units <- c("bytes", "KB", "MB", "GB")
  i <- max(1L, min(length(units), floor(log(max(x, 1), 1024)) + 1L))
  paste0(round(x / 1024^(i - 1), 1), " ", units[i])
}

# ---------------------------------------------------------------------------
# Reading documents out of an archive
# ---------------------------------------------------------------------------

#' Extract and parse one document from a downloaded archive
#'
#' Used by the translation functions, since translation XML is not reliably
#' available from the API: `/akt/{id}/xml` returns HTTP 500 for translation
#' identifiers, and the metadata endpoint has failed intermittently too.
#'
#' @return A list with `status` and `body`, matching etx_get_xml().
#' @noRd
etx_archive_xml <- function(id, lang) {

  id    <- as_id(id)
  entry <- archive_entry(lang)

  if (is.null(entry)) {
    etx_abort(
      paste0("No ", lang, " archive has been downloaded in this session."),
      paste0('Run etx_download_corpus("', lang, '") first.'),
      class = "estilex_no_corpus"
    )
  }

  wanted <- paste0(id, ".xml")
  index  <- etx_list_documents(lang)
  match  <- index$file[basename(index$file) == wanted]

  if (length(match) == 0L) {
    return(list(status = "not_found", body = NULL,
                message = paste0(id, " is not in the ", lang, " archive.")))
  }

  dir <- file.path(tempdir(), "estilex-extract")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  utils::unzip(entry$path, files = match[1], exdir = dir, overwrite = TRUE)
  path <- file.path(dir, match[1])

  doc <- tryCatch(
    suppressWarnings(xml2::read_xml(path, encoding = "UTF-8")),
    error = function(e) {
      etx_abort("Could not parse the archived document.",
                conditionMessage(e), class = "estilex_parse_error")
    }
  )

  # As with the API, the namespace declaration is not an absolute URI and
  # differs between documents, so it is stripped and XPath is written plain.
  xml2::xml_ns_strip(doc)

  list(status = "ok", body = doc, message = NA_character_)
}
