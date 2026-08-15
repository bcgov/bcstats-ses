# Pure, unit-testable transformations (wayfinder ADR-0008 Tier 1 / ADR-0009).
#
# Sourced by src/ scripts and by tests/testthat/ (test-parse_speed.R,
# test-compute_combined_max.R, test-remove_geographies.R,
# test-chsada-weights.R). These functions perform NO I/O.

# ---- relocated verbatim from src/14_connectivity.R ----

#' Parse CITZ speed threshold strings into numeric values
#' @param speed_str Character vector of speed labels (e.g., "50_10", "<5_1")
#' @return Data frame with down, up, is_lt5_1 columns
parse_speed <- function(speed_str) {
  is_lt <- !is.na(speed_str) & stringr::str_detect(speed_str, "^<")
  ok <- !is.na(speed_str) & stringr::str_detect(speed_str, "^[0-9]+_[0-9]+$")

  data.frame(
    down = suppressWarnings(as.numeric(ifelse(
      ok,
      stringr::str_extract(speed_str, "^[0-9]+(?=_)"),
      ifelse(is_lt, 0, NA)
    ))),
    up = suppressWarnings(as.numeric(ifelse(
      ok,
      stringr::str_extract(speed_str, "(?<=_)[0-9]+$"),
      ifelse(is_lt, 0, NA)
    ))),
    is_lt5_1 = is_lt
  )
}

#' Return the higher of two connectivity speed tiers
#'
#' @param wired_max Character: wired max threshold
#' @param wireless_max Character: wireless max threshold
#' @return Character: the higher of the two (NA if both are NA)
compute_combined_max <- function(wired_max, wireless_max) {
  # Define tier hierarchy (higher index = higher speed)
  tier_order <- c("", "<5_1", "5_1", "10_2", "25_5", "50_10")

  # Helper to get tier rank
  get_rank <- function(x) {
    if (is.na(x) || x == "") {
      return(0)
    }
    which(tier_order == x)
  }

  wired_rank <- get_rank(wired_max)
  wireless_rank <- get_rank(wireless_max)

  # If both NA, return NA
  if (wired_rank == 0 && wireless_rank == 0) {
    return(NA_character_)
  }

  # Return the higher tier
  if (wired_rank >= wireless_rank) {
    return(wired_max)
  } else {
    return(wireless_max)
  }
}

# ---- relocated verbatim from src/15_remove_geo_suppression_ids.R ----

#' Remove specified geographies from a SEI data frame
#'
#' @param sei_data Data frame containing a geography code column
#' @param geo_col Column name holding the geography codes
#' @param geo_codes_to_remove Character vector of codes to drop
#' @param geo_type Label used in the progress message (e.g., "CHSA", "CSD")
#' @return Filtered data frame with the geographies removed
remove_geographies <- function(
  sei_data,
  geo_col,
  geo_codes_to_remove,
  geo_type
) {
  original_count <- nrow(sei_data)

  # Convert to character for consistent matching
  sei_data <- sei_data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(geo_col), as.character))

  # Filter out specified geographies
  sei_filtered <- sei_data |>
    dplyr::filter(!.data[[geo_col]] %in% geo_codes_to_remove)

  removed_count <- original_count - nrow(sei_filtered)

  cat(sprintf(
    "  %s: Removed %d records (%.2f%%)\n",
    geo_type,
    removed_count,
    (removed_count / original_count) * 100
  ))

  return(sei_filtered)
}

# ---- extracted from src/12_output_CHSA_DA_lookup.R (lines ~495-548) ----

#' Compute CHSADA (CHSA x DA) population weights from DB-level populations
#'
#' Given dissemination-block-level rows with YEAR, CHSA, DAUID and POPULATION,
#' computes the nested population totals and the two allocation weights:
#'   - chsada_to_chsa_pop_ratio : weight to aggregate DA values UP to CHSA
#'   - chsada_to_da_pop_ratio   : weight to prorate CHSA values DOWN to DA
#'
#' Behaviour-identical to the inline pipe chain it replaces (wayfinder #7):
#' mutate-based nesting, then count(..., name = "cnt_db") to collapse to
#' CHSADA rows. CHSADA ids are CHSA+DAUID pasted (year not included,
#' matching the original).
#'
#' @param db_pop Data frame with columns YEAR, CHSA, DAUID, POPULATION
#' @return CHSADA-level data frame with population totals and weights.
compute_chsada_weights <- function(db_pop) {
  db_pop |>
    dplyr::group_by(YEAR, CHSA, DAUID) |>
    dplyr::mutate(
      chsada_pop = sum(POPULATION, na.rm = TRUE),
      cnt_db_in_chsada = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, DAUID) |>
    dplyr::mutate(
      da_pop = sum(POPULATION, na.rm = TRUE),
      chsada_to_da_pop_ratio = chsada_pop / da_pop,
      cnt_db_in_da = dplyr::n() # the same within da
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(YEAR, CHSA) |>
    dplyr::mutate(
      chsa_pop = sum(POPULATION, na.rm = TRUE),
      chsada_to_chsa_pop_ratio = chsada_pop / chsa_pop,
      cnt_db_in_chsa = dplyr::n() # the same within chsa
    ) |>
    dplyr::ungroup() |>
    # start to get the weights. now the table is still in DB level since we only implement mutate operation not summarise operation.
    # the weights for our purpose (aggregate da value to chsa value) will be chsada_pop/chsa_pop for da value.
    # the weights for disaggregate chsa value to da value) will be chsada_pop/da_pop for da value, which is the weight/prorate in Jonathan's original code.
    dplyr::count(
      YEAR,
      chsada_pop,
      cnt_db_in_chsada,
      DAUID,
      da_pop,
      chsada_to_da_pop_ratio,
      cnt_db_in_da, # all the same within da
      CHSA,
      chsa_pop,
      chsada_to_chsa_pop_ratio,
      cnt_db_in_chsa, # all the same within chsa
      sort = TRUE,
      name = "cnt_db" # should be the same as cnt_db_in_chsada, remove it later
    ) |>
    dplyr::mutate(
      chsada_id = stringr::str_c(CHSA, DAUID), # Not work if we have year
      CHSA = as.character(CHSA)
    )
}
