# Map Norwegian large-carnivore management regions (forvaltningsregioner for
# rovvilt, 1--8) to Naturindeks five geographic regions (Øst, Sør, Vest, Midt,
# Nord). NI aggregation in ecRxiv uses single-letter part codes (E, S, W, C, N)
# as in NO_PTAR_001.
#
# Rovvilt region definitions: Stortinget / Miljødirektoratet (eight regions).
# NI region codes: "1" = Øst, "2" = Sør, "3" = Vest, "4" = Midt, "5" = Nord.

#' Lookup table: rovvilt regions 1--8 to Naturindeks geographic regions
#' @return A tibble with columns `rovvilt_region_id`, `rovvilt_region_name`,
#'   `ni_region_code`, `ni_part`, `ni_region_name`.
#' @export
rovvilt_ni_mapping <- function() {
  data.frame(
    rovvilt_region_id = 1:8,
    rovvilt_region_name = c(
      "Vest-Norge",
      "Sør-Norge",
      "Oppland",
      "Oslo/Akershus/Østfold",
      "Hedmark",
      "Midt-Norge",
      "Nordland",
      "Troms/Finnmark"
    ),
    ni_region_code = c("3", "2", "1", "1", "1", "4", "5", "5"),
    ni_part = c("W", "S", "E", "E", "E", "C", "N", "N"),
    ni_region_name = c(
      "Vest", "Sør", "Øst", "Øst", "Øst", "Midt", "Nord", "Nord"
    ),
    stringsAsFactors = FALSE
  )
}

#' Naturindeks region code (1--5) to display name
#' @param ni_region_code Character or numeric 1--5.
#' @return Character vector of region names (Øst, Sør, Vest, Midt, Nord).
#' @export
assign_ni_region_name <- function(ni_region_code) {
  code_chr <- as.character(ni_region_code)
  dplyr::case_when(
    code_chr == "1" ~ "Øst",
    code_chr == "2" ~ "Sør",
    code_chr == "3" ~ "Vest",
    code_chr == "4" ~ "Midt",
    code_chr == "5" ~ "Nord",
    TRUE ~ NA_character_
  )
}

#' Naturindeks region code (1--5) to single-letter part code used in aggregation
#' @param ni_region_code Character or numeric 1--5.
#' @return Character vector E, S, W, C, or N.
#' @export
assign_ni_part_from_code <- function(ni_region_code) {
  lut <- rovvilt_ni_mapping() |>
    dplyr::distinct(ni_region_code, ni_part)
  code_chr <- as.character(ni_region_code)
  lut$ni_part[match(code_chr, lut$ni_region_code)]
}

#' Parse rovvilt region input to integer id 1--8
#' @param x Character or numeric region label (e.g. `5`, `"Region 5"`, `"Hedmark"`).
#' @return Integer vector 1--8; `NA` if not matched.
#' @keywords internal
.parse_rovvilt_region_id <- function(x) {
  lut <- rovvilt_ni_mapping()
  out <- rep(NA_integer_, length(x))

  num <- suppressWarnings(as.integer(x))
  ok <- !is.na(num) & num >= 1L & num <= 8L
  out[ok] <- num[ok]

  still_na <- is.na(out)
  if (any(still_na)) {
    extracted <- stringr::str_extract(as.character(x[still_na]), "[1-8]")
    extracted_num <- suppressWarnings(as.integer(extracted))
    idx <- which(still_na)
    out[idx[!is.na(extracted_num)]] <- extracted_num[!is.na(extracted_num)]
  }

  still_na <- is.na(out)
  if (any(still_na)) {
    norm <- stringi::stri_trans_general(
      stringr::str_squish(tolower(as.character(x[still_na]))),
      "Latin-ASCII"
    )
    name_key <- stringi::stri_trans_general(
      stringr::str_squish(tolower(lut$rovvilt_region_name)),
      "Latin-ASCII"
    )
    name_key <- stringr::str_replace_all(name_key, "/", " ")
    for (i in seq_along(norm)) {
      hit <- lut$rovvilt_region_id[
        stringr::str_detect(name_key, stringr::fixed(norm[i])) |
          stringr::str_detect(norm[i], stringr::fixed(name_key))
      ]
      if (length(hit) == 1L) {
        out[which(still_na)[i]] <- hit
      }
    }
  }

  aliases <- list(
    `1` = c("vest-norge", "region 1", "region1"),
    `2` = c("sor-norge", "sør-norge", "region 2", "region2"),
    `3` = c("oppland", "region 3", "region3"),
    `4` = c("oslo", "akershus", "ostfold", "region 4", "region4"),
    `5` = c("hedmark", "region 5", "region5"),
    `6` = c("midt-norge", "trondelag", "more og romsdal", "region 6", "region6"),
    `7` = c("nordland", "region 7", "region7"),
    `8` = c("troms", "finnmark", "troms finnmark", "region 8", "region8")
  )

  still_na <- is.na(out)
  if (any(still_na)) {
    norm <- stringi::stri_trans_general(
      stringr::str_squish(tolower(as.character(x[still_na]))),
      "Latin-ASCII"
    )
    for (i in seq_along(norm)) {
      for (rid in names(aliases)) {
        if (any(stringr::str_detect(norm[i], aliases[[rid]]))) {
          out[which(still_na)[i]] <- as.integer(rid)
          break
        }
      }
    }
  }

  out
}

#' Map rovvilt management region(s) to Naturindeks geographic region
#'
#' @param rovvilt_region Character or numeric vector: region id 1--8, name, or
#'   labels such as `"Region 5"` / `"Hedmark"` (as in NI `ICunitName` fields).
#' @param output One of `"part"` (E/S/W/C/N), `"code"` (1--5), `"name"`
#'   (Øst/…/Nord), or `"all"` (returns full mapping columns joined).
#' @return Character vector, or tibble if `output = "all"`.
#' @export
assign_rovvilt_to_ni <- function(
  rovvilt_region,
  output = c("part", "code", "name", "all")
) {
  output <- match.arg(output)
  lut <- rovvilt_ni_mapping()
  rid <- .parse_rovvilt_region_id(rovvilt_region)

  if (any(is.na(rid))) {
    bad <- unique(as.character(rovvilt_region)[is.na(rid)])
    stop(
      "Could not parse rovvilt region(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  joined <- lut[match(rid, lut$rovvilt_region_id), , drop = FALSE]

  if (output == "all") {
    return(joined)
  }

  switch(
    output,
    part = joined$ni_part,
    code = joined$ni_region_code,
    name = joined$ni_region_name
  )
}

#' Attach Naturindeks region columns to a data frame with rovvilt region ids
#'
#' @param data A data frame.
#' @param rovvilt_col Column name containing rovvilt region id or label.
#' @return Data frame with `rovvilt_region_id`, `ni_region_code`, `ni_part`,
#'   and `ni_region_name` added.
#' @export
join_rovvilt_ni_regions <- function(data, rovvilt_col) {
  rid <- .parse_rovvilt_region_id(data[[rovvilt_col]])
  lut <- rovvilt_ni_mapping()
  mapped <- lut[match(rid, lut$rovvilt_region_id), , drop = FALSE]
  dplyr::bind_cols(data, mapped)
}
