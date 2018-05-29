#' Fix ICO2 code
#'
#' Some ISO2 codes in the EUROCONTROL data are wrong.
#' For example Serbia is "RS" since 2006, not anymore "CS" linke in our data.
#'
#' @param flows a data frame with origin/destination flows
#'
#' @return a data frame with corrected ISO2 codes
#' @export
#'
#' @examples
#' \dontrun{
#' flows %>% fix_iso_countrycode()
#' }
#'
fix_iso_countrycode <- function(flows) {
  flows %>%
    # Serbia ('CS' before 2006, then 'RS' -> fix globally as 'RS')
    dplyr::mutate(
      DEP_ISO_COUNTRY_CODE = ifelse(.data$DEP_ISO_COUNTRY_CODE == "CS", "RS", .data$DEP_ISO_COUNTRY_CODE),
      DES_ISO_COUNTRY_CODE = ifelse(.data$DES_ISO_COUNTRY_CODE == "CS", "RS", .data$DES_ISO_COUNTRY_CODE)) %>%
    # Martinique/Guadaloupe (`XF` but `MQ` will result in a proper UN region)
    dplyr::mutate(
      DEP_ISO_COUNTRY_CODE = ifelse(.data$DEP_ISO_COUNTRY_CODE == "XF", "MQ", .data$DEP_ISO_COUNTRY_CODE),
      DES_ISO_COUNTRY_CODE = ifelse(.data$DES_ISO_COUNTRY_CODE == "XF", "MQ", .data$DES_ISO_COUNTRY_CODE))
}


#' Fix "AN" region value to "Caribbean"
#'
#' @param flows a data frame with origin/destination flows
#'
#' @return a data frame with corrected region
#' @export
#'
#' @examples
#' \dontrun{
#' flows %>% fix_region_AN()
#' }
fix_region_AN <- function(flows) {
  flows %>%
  dplyr::mutate(origin_region_name = ifelse(.data$origin_iso == "AN",
                                    "Caribbean",
                                    .data$origin_region_name)) %>%
  dplyr::mutate(destination_region_name = ifelse(.data$destination_iso == "AN",
                                           "Caribbean",
                                           .data$destination_region_name))
}

#' Fix "TW" region value to "Eastern Asia"
#'
#' @param flows a data frame with origin/destination flows
#'
#' @return a data frame with corrected region
#' @export
#'
#' @examples
#' \dontrun{
#' flows %>% fix_region_TW()
#' }
fix_region_TW <- function(flows) {
  flows %>%
    dplyr::mutate(origin_region_name = ifelse(.data$origin_iso == "TW",
                                      "Eastern Asia",
                                      .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_iso == "TW",
                                           "Eastern Asia",
                                           .data$destination_region_name))
}

#' Fix "IO" region value to "Eastern Africa"
#'
#' @param flows a data frame with origin/destination flows
#'
#' @return a data frame with corrected region
#' @export
#'
#' @examples
#' \dontrun{
#' flows %>% fix_region_IO()
#' }
fix_region_IO <- function(flows) {
  flows %>%
    dplyr::mutate(origin_region_name = ifelse(.data$origin_iso == "IO",
                                      "Eastern Africa",
                                      .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_iso == "IO",
                                           "Eastern Africa",
                                           .data$destination_region_name))
}


#' Fix region value for various countries according to EUROCONTROL classification.
#'
#' @param flows a data frame with origin/destination flows
#'
#' @return a data frame with corrected region
#' @export
#'
#' @examples
#' \dontrun{
#' flows %>% fix_eurocontrol_region()
#' }
fix_eurocontrol_region <- function(flows) {
  flows %>%
    dplyr::mutate(origin_region_name = ifelse(.data$origin_iso == "AN",
                                      "Southern America",
                                      .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_iso == "AN",
                                           "Southern America",
                                           .data$destination_region_name)) %>%
    dplyr::mutate(origin_name = ifelse(.data$origin_iso == "AN",
                                "Antilles",
                                .data$origin_name)) %>%
    dplyr::mutate(destination_name = ifelse(.data$destination_iso == "AN",
                                     "Antilles",
                                     .data$destination_name)) %>%
    dplyr::mutate(origin_region_name = ifelse(.data$origin_iso == "TW",
                                      "Eastern Asia",
                                      .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_iso == "TW",
                                           "Eastern Asia",
                                           .data$destination_region_name)) %>%
    dplyr::mutate(origin_region_name = ifelse(.data$origin_iso == "IO",
                                      "Eastern Africa",
                                      .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_iso == "IO",
                                           "Eastern Africa",
                                           .data$destination_region_name)) %>%
    dplyr::mutate(origin_region_name = ifelse(.data$origin_iso == "SN",
                                      "Western Africa",
                                      .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_iso == "SN",
                                           "Western Africa",
                                           .data$destination_region_name))
}


#' Fix region value for various countries with respect to extra flows.
#'
#' @param flows a data frame with origin/destination flows
#'
#' @return a data frame with corrected region
#' @export
#'
#' @examples
#' \dontrun{
#' flows %>% fix_extra_flows_region()
#' }
fix_extra_flows_region <- function(flows) {
  flows %>%
    dplyr::mutate(origin_region_name = ifelse(.data$origin_region_name == "Russia",
                                       "Asia",
                                       .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_region_name == "Russia",
                                            "Asia",
                                            .data$destination_region_name)) %>%
  # China ->  Asia
    dplyr::mutate(origin_region_name = ifelse(.data$origin_region_name == "China",
                                       "Asia",
                                       .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_region_name == "China",
                                            "Asia",
                                            .data$destination_region_name)) %>%
  # Southern America ->  America
    dplyr::mutate(origin_region_name = ifelse(.data$origin_region_name == "Southern America",
                                       "America",
                                       .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_region_name == "Southern America",
                                            "America",
                                            .data$destination_region_name)) %>%
    # Northern America -> N Ame
    dplyr::mutate(origin_region_name = ifelse(.data$origin_region_name == "Northern America",
                                       "America",
                                       .data$origin_region_name)) %>%
    dplyr::mutate(destination_region_name = ifelse(.data$destination_region_name == "Northern America",
                                            "America",
                                            .data$destination_region_name))
}

