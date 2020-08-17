#' @title Extract daily flight country flows
#' @description Extract country pair flows of flights for the specified date interval.
#' @param wef Start date of the interval of interest (format "YYYY-MM-DD")
#' @param til End date of the interval of interest (NOTE: non-inclusive; format "YYYY-MM-DD")
#' @return A tibble
#' @pretty_print TRUE
#' @details You need access to PRISME's PRU_DEV schema to retrieve the dataset.
#'          ISO country codes need scrutiny because they are not currently well
#'          maintained in PRISME, i.e. "CS" for Serbia.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname retrieve_daily_airport_flows
retrieve_daily_airport_flows <- function(wef, til) {
  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  wef <- parsedate::parse_date(wef)
  til <- parsedate::parse_date(til)
  wef <- format(wef, "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, "%Y-%m-%dT%H:%M:%SZ")

  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- withr::local_db_connection(
    DBI::dbConnect(
      DBI::dbDriver("Oracle"),
      usr, pwd,
      dbname = dbn,
      timezone = "UTC")
  )


  sqlq <- "WITH
      ARGS
      AS
          (SELECT TO_DATE (?WEF,
                           'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                      LOBT_WEF,
                  TO_DATE (?TIL,
                           'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                      LOBT_TIL
             FROM DUAL),
      AIRP_FLIGHT
      AS
          (  SELECT TRUNC (flt_a_asp_prof_time_entry)
                        AS entry_day,
                    flt_dep_ad
                        AS adep,
                    flt_ctfm_ades
                        AS ades,
                    flt_dep_ad || '>' || flt_ctfm_ades
                        AS adep_ades_pair_code,
                       LEAST (flt_dep_ad, flt_ctfm_ades)
                    || '<->'
                    || GREATEST (flt_dep_ad, flt_ctfm_ades)
                        AS AIRPORT_PAIR_Code,
                    COUNT (flt_uid)
                        AS flight
               FROM aru_flt.flt@BO_USER_DWH_OP3 a
              WHERE      (    A.flt_lobt >= (SELECT LOBT_WEF FROM ARGS) - 1
                          AND A.flt_lobt <  (SELECT LOBT_TIL FROM ARGS)
                          AND A.flt_a_asp_prof_time_entry >= (SELECT LOBT_WEF FROM ARGS)
                          AND A.flt_a_asp_prof_time_entry < (SELECT LOBT_TIL FROM ARGS))
                    AND A.flt_state IN ('TE', 'TA', 'AA')
           GROUP BY TRUNC (flt_a_asp_prof_time_entry),
                    flt_dep_ad,
                    flt_ctfm_ades),
      REL_AP_CTRY1
      AS
          (SELECT cfmu_ap_code,
                  cfmu_ap_name,
                  CASE
                      WHEN SUBSTR (cfmu_ap_code, 2) = 'WI' THEN 'INDONESIA'
                      WHEN SUBSTR (cfmu_ap_code, 2) = 'WR' THEN 'INDONESIA'
                      WHEN cfmu_ap_code = 'XSPR' THEN 'CUBA'
                      ELSE iso_ct_name
                  END
                      iso_ct_name,
                  iso_ct_code,
                  latitude,
                  longitude,
                  valid_to,
                  MAX (valid_to) OVER (PARTITION BY cfmu_ap_code)
                      max_date
             FROM swh_fct.DIM_AIRPORT
            WHERE cfmu_ap_code IS NOT NULL),
      REL_AP_CTRY
      AS
          (SELECT DISTINCT cfmu_ap_code,
                           SUBSTR (cfmu_ap_code, 1, 2) AS icao_ctry_code,
                           cfmu_ap_name,
                           iso_ct_name,
                           iso_ct_code,
                           latitude,
                           longitude
             FROM REL_AP_CTRY1
            WHERE valid_to = max_date)
  SELECT a.*,
         c1.cfmu_ap_name AS adep_name,
         c2.cfmu_ap_name AS ades_name,
         c1.iso_ct_name  AS adep_country_iso_name,
         c1.iso_ct_code  AS adep_country_iso_code,
         c2.iso_ct_name  AS ades_country_iso_name,
         c2.iso_ct_code  AS ades_country_iso_code,
         c1.latitude     AS adep_latitude,
         c1.longitude    AS adep_longitude,
         c2.latitude     AS ades_latitude,
         c2.longitude    AS ades_longitude
    FROM AIRP_FLIGHT  a
         LEFT JOIN rel_ap_ctry c1 ON a.adep = c1.cfmu_ap_code
         LEFT JOIN rel_ap_ctry c2 ON a.ades = c2.cfmu_ap_code"

  query <- DBI::sqlInterpolate(con, sqlq, WEF = wef, TIL = til)
  flt   <- DBI::dbSendQuery(con, query)
  data  <- DBI::fetch(flt, n = -1)
  data
}
