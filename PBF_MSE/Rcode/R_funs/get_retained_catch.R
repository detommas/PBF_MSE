#' Get retained catch from the timeseries Report.sso table
#'
#' @template timeseries
#' @param units_of_catch From datalist, the catch units. A named list where the
#' names are the fleets (to provide an extra check)
#' @importFrom tidyr gather separate
#' @return a data frame with retained catch by Yr, Era, Seas, Fleet, and
#'  units (long format)
get_retained_catch <- function(timeseries, units_of_catch) {
  # input checks
  assertive.types::assert_is_data.frame(timeseries)
  nfleets <- length(grep("^F:_\\d+$", colnames(timeseries)))
  assertive.properties::assert_is_of_length(units_of_catch, nfleets)
  fleet_names <- strsplit(grep("^F:_\\d+$", colnames(timeseries), value = TRUE),
                          "_",
                          fixed = TRUE
  )
  fleet_names <- unlist(lapply(fleet_names, function(x) x[2]))
  if (!is.null(names(units_of_catch))) {
    assertive.base::assert_all_are_true(fleet_names == names(units_of_catch))
  }
  
  # calc retained catch
  units_catch_string <- ifelse(units_of_catch == 1, "B", "N")
  retain_catch_colnames <- paste0(
    "retain(", units_catch_string, "):_",
    fleet_names
  )
  # some may not be included in output if no catch.
  retain_catch_colnames <- retain_catch_colnames[
    retain_catch_colnames %in% colnames(timeseries)
  ]
  
  # switch from wide to long format.
  retain_catch_df <- timeseries[, c("Yr", "Era", "Seas", retain_catch_colnames)]
  retain_catch_df <- tidyr::gather(retain_catch_df,
                                   key = "tmp_units_fleet",
                                   value = "retained_catch",
                                   grep(
                                     "^retain\\([BN]\\):_\\d+$",
                                     colnames(retain_catch_df)
                                   )
  )
  # make the fleet column just the numerical values. could maybe to with
  # strsplit instead?
  retain_catch_df <- tidyr::separate(retain_catch_df,
                                     col = "tmp_units_fleet",
                                     into = c("Units", "Fleet"),
                                     sep = ":_", convert = TRUE
  )
  
  retain_catch_df <- retain_catch_df %>%
    dplyr::group_by(.data[["Yr"]], .data[["Era"]], .data[["Seas"]], .data[["Units"]], .data[["Fleet"]]) %>%
    dplyr::summarise(retained_catch = sum(.data[["retained_catch"]])) %>%
    dplyr::select(.data[["Yr"]], .data[["Era"]], .data[["Seas"]], .data[["Units"]], .data[["Fleet"]], .data[["retained_catch"]])
  retain_catch_df <- as.data.frame(retain_catch_df) # want as df and not tibble
  # units are not as concise as they could be, but leave for now.
  retain_catch_df
}