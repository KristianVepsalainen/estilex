#' Retrieve the English translation of an Estonian act
#'
#' Follows the translation link recorded in the act's metadata and returns the
#' translation.
#'
#' @section Unofficial translations:
#'
#' Only the Estonian text has legal force in domestic proceedings. English
#' translations are kept up to date but are not official texts, so the Riigi
#' Teataja holds them apart from the law in force, on a separate `/en/` route.
#' estilex follows that route automatically — identifiers beginning with 5 are
#' translations — so no archive download is needed.
#'
#' @section Whether a translation is current:
#'
#' Translations carry no version information of their own, so the only way to
#' tell whether one has kept up with the Estonian text is to compare validity
#' dates. `is_current` does that, comparing `kehtivuse_algus` on both sides.
#' Publication dates are not a useful signal: they commonly differ by days
#' even when the translation is up to date.
#'
#' Section 12 of the Riigi Teataja Act requires reproduced texts to cite the
#' published version, and `source_url` is provided for that purpose.
#'
#' @param id Identifier of an Estonian act. See [etx_get_doc()].
#'
#' @return A one-row [tibble][tibble::tibble] with `status`, `id`,
#'   `translation_id`, `pealkiri`, `pealkiri_en`, `kehtivuse_algus`,
#'   `kehtivuse_algus_en`, `is_current`, `text`, `n_char`, `source_url`,
#'   `source_url_en` and `error_message`.
#'
#'   `status` is `"ok"`, `"no_translation"` when the act has none, or
#'   `"not_found"` or `"error"` when retrieval failed.
#'
#' @seealso [etx_get_parallel()] for section-by-section alignment
#'
#' @source Riigi Teataja, <https://www.riigiteataja.ee>.
#'
#' @examples
#' if (FALSE) {
#'   tr <- etx_get_translation("111042025003")
#'   tr$is_current
#'   cat(substr(tr$text, 1, 400))
#' }
#'
#' @export
etx_get_translation <- function(id) {

  id  <- check_id(id)
  doc <- etx_get_doc(id)

  if (!identical(doc$status, "ok")) {
    return(translation_row(id, doc$status,
                           list(error_message = doc$error_message)))
  }

  translation_id <- doc$tolke_seos_id
  if (is.na(translation_id)) {
    return(translation_row(id, "no_translation", list(pealkiri = doc$pealkiri)))
  }

  res <- etx_get_xml(translation_id)

  if (!identical(res$status, "ok")) {
    return(translation_row(
      id, res$status,
      list(
        pealkiri       = doc$pealkiri,
        translation_id = translation_id,
        error_message  = res$message
      )
    ))
  }

  sisu <- xml2::xml_find_first(res$body, "//sisu")
  txt  <- if (inherits(sisu, "xml_missing")) NA_character_ else render_content(sisu)

  # <kehtivus> has kehtivuseAlgus and kehtivuseLopp children. Reading the
  # element's own text would concatenate them into something like
  # "2023-06-012023-06-30+03:00", from which taking the first ten characters
  # happens to give the right answer for the wrong reason.
  kehtivus_en <- parse_date_est(xml_chr(res$body, "//kehtivus/kehtivuseAlgus"))

  translation_row(
    id, "ok",
    list(
      translation_id     = translation_id,
      pealkiri           = doc$pealkiri,
      pealkiri_en        = xml_chr(res$body, "//aktinimi"),
      kehtivuse_algus    = doc$kehtivuse_algus,
      kehtivuse_algus_en = kehtivus_en,
      is_current         = !is.na(kehtivus_en) && !is.na(doc$kehtivuse_algus) &&
        kehtivus_en >= doc$kehtivuse_algus,
      text               = txt,
      n_char             = if (is.na(txt)) NA_integer_ else nchar(txt)
    )
  )
}

#' Align an Estonian act with its English translation
#'
#' Returns one row per section, with the Estonian and English texts side by
#' side, so that the two versions can be compared directly.
#'
#' @section Why alignment is reliable:
#'
#' Section numbering is preserved exactly across languages. For the
#' Constitution, the Estonian text and its translation contain the same 168
#' sections with identical `paragrahvNr` values, so the join is on the number
#' rather than on position and cannot silently drift.
#'
#' Subsection counts may differ slightly — the Constitution differs by one, in
#' its closing section — so the texts are joined at section level, with all
#' subsections of a section concatenated. Alignment below section level is not
#' safe and is not attempted.
#'
#' @section Versions are paired:
#'
#' Riigi Teataja links each consolidated version to the translation that
#' corresponds to it, rather than always to the latest. A consolidation from
#' 2015 links to a translation dated 2020, not to the current one, so
#' comparing an older version of an act against its translation gives a
#' contemporaneous result rather than an anachronistic one.
#'
#' `kehtivuse_algus` and `kehtivuse_algus_en` record which two versions were
#' compared, which makes translation fidelity measurable over time rather than
#' only at the present moment.
#'
#' @section Missing translations:
#'
#' Most Estonian acts have no English translation. When one is absent, the
#' Estonian rows are still returned with `text_en` missing, and
#' `has_translation` is `FALSE`. The same applies to individual sections that
#' appear in one language and not the other.
#'
#' This is deliberate. In work on translation fidelity, an absent translation
#' is an observation rather than an error, and the structure of the Estonian
#' act is what makes the gap measurable.
#'
#' @param id Identifier of an Estonian act. See [etx_get_doc()].
#'
#' @return A [tibble][tibble::tibble] with one row per section:
#'   \describe{
#'     \item{`id`, `translation_id`}{Identifiers of the two versions.}
#'     \item{`has_translation`}{Whether a translation was found at all.}
#'     \item{`peatykk_nr`, `peatykk_pealkiri`}{Chapter context, from the
#'       Estonian text.}
#'     \item{`paragrahv_nr`, `paragrahv`}{Section number and display form.}
#'     \item{`text_et`, `text_en`}{The two texts, subsections concatenated.}
#'     \item{`n_char_et`, `n_char_en`}{Their lengths, a crude first measure
#'       of divergence.}
#'     \item{`kehtivuse_algus`, `kehtivuse_algus_en`}{Validity dates of the
#'       two versions. Constant across the table, but carried on every row so
#'       the temporal context survives a join.}
#'     \item{`source_url`}{Canonical link, per section 12 of the Riigi
#'       Teataja Act.}
#'   }
#'
#'   Zero rows if the Estonian act cannot be retrieved.
#'
#' @seealso [etx_get_translation()], [etx_get_paragraphs()]
#'
#' @examples
#' if (FALSE) {
#'   par <- etx_get_parallel("111042025003")
#'
#'   # Sections whose translation is much shorter than the original
#'   subset(par, !is.na(text_en) & n_char_en < 0.6 * n_char_et)
#'
#'   # Sections with no translation at all
#'   sum(is.na(par$text_en))
#' }
#'
#' @export
etx_get_parallel <- function(id) {

  id <- check_id(id)

  et <- etx_get_paragraphs(id)
  if (nrow(et) == 0L) return(etx_empty_parallel())

  urls <- etx_urls(id)
  doc  <- etx_get_doc(id)
  translation_id <- if (identical(doc$status, "ok")) doc$tolke_seos_id else NA_character_

  en <- NULL
  kehtivus_en <- as.Date(NA)
  if (!is.na(translation_id)) {
    res <- etx_get_xml(translation_id)
    if (identical(res$status, "ok")) {
      en <- sections_from_xml(res$body)
      kehtivus_en <- parse_date_est(xml_chr(res$body, "//kehtivus/kehtivuseAlgus"))
    } else {
      warning("Could not retrieve translation ", translation_id, ": ",
              res$message %||% res$status, call. = FALSE)
      translation_id <- NA_character_
    }
  }

  # One row per section, subsections concatenated. Subsection counts can
  # differ between languages, so the join is at section level only.
  et_sections <- collapse_sections(et)

  text_en <- if (is.null(en)) {
    rep(NA_character_, nrow(et_sections))
  } else {
    en$text[match(et_sections$paragrahv_nr, en$paragrahv_nr)]
  }

  kehtivus_et <- if (identical(doc$status, "ok")) doc$kehtivuse_algus else as.Date(NA)

  tibble::tibble(
    id                 = as_id(id),
    translation_id     = translation_id,
    has_translation    = !is.na(translation_id),
    peatykk_nr         = et_sections$peatykk_nr,
    peatykk_pealkiri   = et_sections$peatykk_pealkiri,
    paragrahv_nr       = et_sections$paragrahv_nr,
    paragrahv          = et_sections$paragrahv,
    text_et            = et_sections$text,
    text_en            = text_en,
    n_char_et          = nchar(et_sections$text),
    n_char_en          = nchar(text_en),
    kehtivuse_algus    = kehtivus_et,
    kehtivuse_algus_en = kehtivus_en,
    source_url         = urls$source_url
  )
}

# ---------------------------------------------------------------------------
# Internals
# ---------------------------------------------------------------------------

#' Collapse a provision table to one row per section
#' @noRd
collapse_sections <- function(p) {
  keys <- unique(p$paragrahv_nr)
  rows <- lapply(keys, function(k) {
    sub <- p[p$paragrahv_nr == k, , drop = FALSE]
    tibble::tibble(
      peatykk_nr       = sub$peatykk_nr[1],
      peatykk_pealkiri = sub$peatykk_pealkiri[1],
      paragrahv_nr     = k,
      paragrahv        = sub$paragrahv[1],
      text             = paste(stats::na.omit(sub$text), collapse = "\n")
    )
  })
  do.call(rbind, rows)
}

#' Section-level text from a parsed document
#'
#' Used for the translation side, where the full provision machinery is not
#' needed — only the number and the text.
#'
#' @noRd
sections_from_xml <- function(doc) {
  sections <- xml2::xml_find_all(doc, "//paragrahv")
  if (length(sections) == 0L) return(NULL)

  tibble::tibble(
    paragrahv_nr = vapply(sections, function(s) xml_chr(s, "./paragrahvNr"),
                          character(1)),
    text = vapply(sections, function(s) {
      t <- render_provision(s)
      if (is.na(t)) NA_character_ else t
    }, character(1))
  )
}

#' Construct a translation row
#' @noRd
translation_row <- function(id, status, values = list()) {

  urls <- etx_urls(id)

  template <- list(
    status             = status,
    id                 = as_id(id),
    translation_id     = NA_character_,
    pealkiri           = NA_character_,
    pealkiri_en        = NA_character_,
    kehtivuse_algus    = as.Date(NA),
    kehtivuse_algus_en = as.Date(NA),
    is_current         = NA,
    text               = NA_character_,
    n_char             = NA_integer_,
    source_url         = urls$source_url,
    source_url_en      = NA_character_,
    error_message      = NA_character_
  )

  for (nm in names(values)) template[[nm]] <- values[[nm]]

  if (!is.na(template$translation_id)) {
    template$source_url_en <- etx_urls(template$translation_id)$source_url
  }

  tibble::as_tibble(template)
}

#' Zero-row parallel table with the correct columns and types
#' @noRd
etx_empty_parallel <- function() {
  tibble::tibble(
    id                 = character(0),
    translation_id     = character(0),
    has_translation    = logical(0),
    peatykk_nr         = character(0),
    peatykk_pealkiri   = character(0),
    paragrahv_nr       = character(0),
    paragrahv          = character(0),
    text_et            = character(0),
    text_en            = character(0),
    n_char_et          = integer(0),
    n_char_en          = integer(0),
    kehtivuse_algus    = as.Date(character(0)),
    kehtivuse_algus_en = as.Date(character(0)),
    source_url         = character(0)
  )
}
