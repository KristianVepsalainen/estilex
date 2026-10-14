#' Download the Riigi Teataja bulk archives
#'
#' The API has no search or enumeration endpoint, so the only way to obtain a
#' list of identifiers is the open data archive of XML documents.
#'
#' @section Archives are complementary, not redundant:
#'
#' Each annual archive holds the acts published in that year **together with**
#' acts published earlier that received a new meta version during it — because
#' they were amended, or had an end of validity recorded. An act published in
#' 2023 and untouched since will therefore appear in the 2023 archive and in
#' no later one.
#'
#' Downloading only the newest archive leaves gaps. `year = "all"`, the
#' default, fetches every published archive and is the only way to get
#' complete coverage of the period they span.
#'
#' Note what that period is: archives begin in 2024, so the corpus they
#' describe is "everything published or amended since 2024", not the whole
#' body of Estonian legislation. Older acts still in force but unamended since
#' then are absent. For those, retrieve documents by identifier through
#' [etx_get_doc()], which has no such limit.
#'
#' @section Sizes:
#'
#' The English archives total around a gigabyte; the Estonian ones exceed
#' forty, since the June 2026 system migration gave every document a new meta
#' version. The combined size is checked before anything is fetched: above
#' 5 GB an interactive session asks for confirmation, and a non-interactive
#' one stops unless `force = TRUE`.
#'
#' @section Where archives are stored:
#'
#' By default they go to a temporary directory and last only for the session,
#' so nothing is written to your file system permanently. Since re-downloading
#' is unpleasant, `dest` keeps them somewhere of your choosing, and a later
#' call with the same `dest` reuses what is already there.
#'
#' Better still, set `ESTILEX_CORPUS_DIR` once in `~/.Renviron`
#' (`usethis::edit_r_environ()` opens the file):
#'
#' ```
#' ESTILEX_CORPUS_DIR=~/riigiteataja
#' ```
#'
#' estilex then picks up archives already in that directory without being
#' asked, so [etx_list_documents()] works immediately in a new session.
#'
#' @param lang `"en"` for English translations or `"et"` for Estonian texts.
#' @param year `"all"` for every published archive, or one or more years.
#' @param dest Directory to store archives in. Defaults to
#'   `ESTILEX_CORPUS_DIR` if set, otherwise a temporary directory.
#' @param refresh Download again even if an archive is already present.
#' @param force Proceed with a very large download without confirmation.
#' @param progress Show progress. Defaults to [interactive()].
#'
#' @return Invisibly, a character vector of archive paths.
#'
#' @seealso [etx_list_documents()], [etx_corpus_status()],
#'   [etx_clear_corpus()], [etx_list_archives()]
#'
#' @source Riigi Teataja open data,
#'   <https://www.riigiteataja.ee/avaandmed/ERT/>.
#'
#' @examples
#' if (FALSE) {
#'   # Every English archive, for complete coverage (~1 GB)
#'   etx_download_corpus("en")
#'
#'   # A single year, if you only want to try things out
#'   etx_download_corpus("en", year = 2024)
#'
#'   # Keep them between sessions
#'   etx_download_corpus("en", dest = "~/riigiteataja")
#' }
#'
#' @export
etx_download_corpus <- function(lang = c("en", "et"), year = "all",
                                dest = NULL, refresh = FALSE, force = FALSE,
                                progress = interactive()) {

  lang  <- match.arg(lang)
  years <- resolve_archive_years(lang, year)

  dir <- resolve_archive_dir(dest)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  names <- vapply(years, archive_name, character(1), lang = lang)
  paths <- file.path(dir, names)
  urls  <- paste0(ETX_BULK, "/", names)

  needed <- refresh | !file.exists(paths)

  if (any(needed)) {
    # Size is checked across everything about to be fetched, not file by file:
    # three Estonian archives are individually large and collectively enormous.
    total <- sum(vapply(urls[needed], archive_size, numeric(1)), na.rm = TRUE)
    confirm_large_download(total, paste(names[needed], collapse = ", "),
                           force = force)
  }

  for (i in seq_along(years)) {
    if (!needed[i]) {
      if (isTRUE(progress)) message("Using the archive already at ", paths[i], ".")
      next
    }
    download_one_archive(urls[i], paths[i], names[i], progress)
  }

  register_archives(lang, years, paths)
  invisible(paths)
}

#' List the documents in the downloaded archives
#'
#' Returns every identifier the archives contain, with its components decoded
#' from the identifier itself. The archives must have been downloaded first
#' with [etx_download_corpus()].
#'
#' @section What the identifiers tell you:
#'
#' Identifiers encode the language, the publication date of that particular
#' version, and a sequence number, so a useful amount can be learned without
#' opening a single document. `year` is the year that *version* was published,
#' which for a translation is the year it was last revised, and is not the
#' same as `archive_year`, the archive it was found in.
#'
#' An act amended several times appears several times, once per version, under
#' different identifiers.
#'
#' @param lang `"en"` or `"et"`.
#'
#' @return A [tibble][tibble::tibble] with one row per document:
#'   `id`, `is_translation`, `day`, `month`, `year`, `sequence`,
#'   `archive_year`, `file`.
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
#'   table(idx$archive_year)
#' }
#'
#' @export
etx_list_documents <- function(lang = c("en", "et")) {

  lang  <- match.arg(lang)
  entry <- archive_entry(lang)

  if (is.null(entry)) {
    etx_abort(
      paste0("No ", lang, " archive has been downloaded in this session."),
      paste0('Run etx_download_corpus("', lang, '") first.'),
      class = "estilex_no_corpus"
    )
  }

  if (!is.null(entry$index)) return(entry$index)

  parts <- lapply(names(entry$archives), function(y) {
    index_one_archive(entry$archives[[y]], as.integer(y))
  })

  index <- do.call(rbind, parts)

  # The same identifier should not occur twice, since each archive holds a
  # different version of an act. Guard anyway, keeping the newest sighting.
  index <- index[order(index$id, -index$archive_year), , drop = FALSE]
  index <- index[!duplicated(index$id), , drop = FALSE]
  index <- index[order(index$id), , drop = FALSE]

  entry$index <- index
  assign(lang, entry, envir = .etx_cache)

  index
}

#' Report on the archives held in this session
#'
#' @return A [tibble][tibble::tibble] with one row per language:
#'   `lang`, `available`, `years`, `n_archives`, `size_mb`, `n_documents`,
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
        lang = lang, available = FALSE, years = NA_character_,
        n_archives = NA_integer_, size_mb = NA_real_,
        n_documents = NA_integer_, is_temporary = NA
      ))
    }
    paths <- unlist(e$archives, use.names = FALSE)
    tibble::tibble(
      lang         = lang,
      available    = TRUE,
      years        = paste(sort(names(e$archives)), collapse = ", "),
      n_archives   = length(paths),
      size_mb      = round(sum(file.size(paths)) / 1024^2, 1),
      n_documents  = if (is.null(e$index)) NA_integer_ else nrow(e$index),
      is_temporary = all(startsWith(paths, tempdir()))
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
    paths <- unlist(e$archives, use.names = FALSE)
    for (p in paths) if (startsWith(p, tempdir())) unlink(p)
    rm(list = lang, envir = .etx_cache)
    n <- n + length(paths)
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

  parse_archive_listing(httr2::resp_body_string(res$response))
}

# ---------------------------------------------------------------------------
# Session cache
# ---------------------------------------------------------------------------

# Session-scoped. Archives default to tempdir(), so nothing persists unless
# the user asks for it with `dest` or ESTILEX_CORPUS_DIR — no CRAN policy on
# user directories applies to the default path.
#
# Each language holds a list of archives keyed by year, plus a lazily built
# index across all of them.
.etx_cache <- new.env(parent = emptyenv())

#' Find the archives for a language
#'
#' Looks in the session cache first, then at ESTILEX_CORPUS_DIR. The second
#' path matters because a package-local environment does not survive
#' restarting R, and re-registering archives by hand at the start of every
#' session is exactly the friction that variable removes.
#'
#' @noRd
archive_entry <- function(lang) {

  if (exists(lang, envir = .etx_cache, inherits = FALSE)) {
    e <- get(lang, envir = .etx_cache)
    # Files may have gone away between calls, e.g. a tempdir sweep.
    keep <- vapply(e$archives, file.exists, logical(1))
    if (all(keep)) return(e)

    rm(list = lang, envir = .etx_cache)
    if (any(keep)) {
      e$archives <- e$archives[keep]
      e$index    <- NULL
      assign(lang, e, envir = .etx_cache)
      return(e)
    }
  }

  adopt_configured_archives(lang)
}

#' Adopt every archive in ESTILEX_CORPUS_DIR
#'
#' All years present are taken, not just the newest, because the archives are
#' complementary rather than successive.
#'
#' @noRd
adopt_configured_archives <- function(lang) {

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
  register_archives(lang, years, file.path(dir, found))
  get(lang, envir = .etx_cache)
}

#' @noRd
register_archives <- function(lang, years, paths) {
  archives <- stats::setNames(as.list(paths), as.character(years))
  archives <- archives[order(names(archives))]
  assign(lang, list(archives = archives, index = NULL), envir = .etx_cache)
}

# ---------------------------------------------------------------------------
# Archive naming, discovery and retrieval
# ---------------------------------------------------------------------------

#' @noRd
archive_name <- function(year, lang) {
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
  m <- unique(regmatches(html, gregexpr("xml\\.[0-9]{4}(\\.en)?\\.zip", html))[[1]])

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

#' Decide which years to fetch
#' @noRd
resolve_archive_years <- function(lang, year = "all") {

  if (identical(year, "all")) {
    available <- etx_list_archives()
    available <- available[available$lang == lang, , drop = FALSE]
    if (nrow(available) == 0L) {
      etx_abort(paste0("No ", lang, " archives are published."),
                class = "estilex_no_corpus")
    }
    return(sort(available$year))
  }

  year <- suppressWarnings(as.integer(year))
  if (length(year) == 0L || anyNA(year)) {
    etx_abort('`year` must be "all" or one or more four-digit years.',
              class = "estilex_bad_argument")
  }
  sort(unique(year))
}

#' Download one archive to a path
#' @noRd
download_one_archive <- function(url, path, name, progress) {

  if (isTRUE(progress)) message("Downloading ", name, " to ", dirname(path), ".")

  req <- etx_request(url)
  if (isTRUE(progress)) req <- httr2::req_progress(req)

  resp <- tryCatch(
    httr2::req_perform(req, path = path),
    error = function(e) {
      unlink(path)
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

  invisible(path)
}

#' Build the index for one archive
#' @noRd
index_one_archive <- function(path, archive_year) {

  files <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) {
      etx_abort("Could not read the archive.",
                conditionMessage(e), class = "estilex_parse_error")
    }
  )

  ids    <- sub("\\.xml$", "", basename(files$Name))
  parsed <- lapply(ids, etx_parse_id)

  tibble::tibble(
    id             = ids,
    is_translation = vapply(parsed, function(p) p$is_translation, logical(1)),
    day            = vapply(parsed, function(p) p$day, integer(1)),
    month          = vapply(parsed, function(p) p$month, integer(1)),
    year           = vapply(parsed, function(p) p$year, integer(1)),
    sequence       = vapply(parsed, function(p) p$sequence, integer(1)),
    archive_year   = as.integer(archive_year),
    file           = files$Name
  )
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
# The Estonian archives come to around 43 GB together.
ETX_LARGE_DOWNLOAD <- 5 * 1024^3

#' @noRd
confirm_large_download <- function(size, name, force,
                                   interactive = base::interactive()) {

  if (isTRUE(force) || is.na(size) || size == 0 || size < ETX_LARGE_DOWNLOAD) {
    return(invisible(TRUE))
  }

  msg <- paste0(name, " comes to ", format_bytes(size),
                ". Downloading will take a long time and use that much disk ",
                "space.")

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
# Storage location
# ---------------------------------------------------------------------------

#' Decide where archives should live
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
# Reading documents out of an archive
# ---------------------------------------------------------------------------

#' Extract and parse one document from the downloaded archives
#'
#' Retained for documents the API cannot serve, and for working offline. The
#' translation functions no longer need it: translations are available from
#' the API under the /en/ route.
#'
#' @return A list with `status`, `body` and `message`, matching etx_get_xml().
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

  index <- etx_list_documents(lang)
  row   <- index[index$id == id, , drop = FALSE]

  if (nrow(row) == 0L) {
    return(list(status = "not_found", body = NULL,
                message = paste0(id, " is not in the ", lang, " archives.")))
  }

  path <- entry$archives[[as.character(row$archive_year[1])]]
  dir  <- file.path(tempdir(), "estilex-extract")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  utils::unzip(path, files = row$file[1], exdir = dir, overwrite = TRUE)

  doc <- tryCatch(
    suppressWarnings(xml2::read_xml(file.path(dir, row$file[1]),
                                    encoding = "UTF-8")),
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
