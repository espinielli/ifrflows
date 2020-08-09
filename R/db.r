
## ---- retrieve_country_flows ----

#' retrieve the yearly number of flights from countries for a period of years.
#'
#' @param wef the beginning year of the period (included).
#' @param til the end year of the period (not included).
#'
#' @return a dataframe with year, # of flights, departing country code and
#' arrival country code.
#' @export
#'
#' @examples
#' \dontrun{
#' retrieve_country_flows('2001', '2002')
#' # A tibble: 4,743 x 4
#' YEAR  NB_OF_FLIGHT DEP_ISO_COUNTRY_CODE DES_ISO_COUNTRY_CODE
#' <chr>        <dbl> <chr>                <chr>
#'   1 2001          269. CN                   IT
#' 2 2001         1499. BR                   FR
#' 3 2001         1211. JP                   NL
#' 4 2001          484. JO                   TR
#' 5 2001        44710. ES                   FR
#' 6 2001         5684. AE                   GB
#' 7 2001          940. IL                   BE
#' 8 2001         5863. EG                   DE
#' 9 2001         3350. BE                   PT
#' 10 2001         1750. RU                   AT
#' # ... with 4,733 more rows
#' }
#'
retrieve_country_flows <- function(wef, til) {
  if (!requireNamespace("ROracle", quietly = TRUE)) {
    stop("Package \"ROracle\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # DB params
  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  # interval of interest
  wef <- parsedate::parse_iso_8601(wef)
  til <- parsedate::parse_iso_8601(til)
  wef <- format(wef, format = "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, format = "%Y-%m-%dT%H:%M:%SZ")


  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  tz <- "UTC"
  Sys.setenv("TZ" = tz)
  Sys.setenv("ORA_SDTZ" = tz)


  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, usr, pwd, dbname = dbn)

  sqlq_flows <- "SELECT TO_CHAR(F.LOBT, 'YYYY') AS YEAR,
      COUNT(FLT_UID) NB_OF_FLIGHT,
      A1.EC_ISO_CT_CODE DEP_ISO_COUNTRY_CODE,
      A2.EC_ISO_CT_CODE DES_ISO_COUNTRY_CODE
    FROM SWH_FCT.FAC_FLIGHT F,
      SWH_FCT.DIM_AIRPORT A1,
      SWH_FCT.DIM_AIRPORT A2,
      SWH_FCT.DIM_AIRCRAFT_TYPE AC
    WHERE
      A1.SK_AP_ID = F.SK_ADEP_ID
      AND A2.SK_AP_ID   = F.SK_ADES_ID
      AND F.LOBT       >= TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
      AND F.LOBT        < TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
      AND (A1.EC_ISO_CT_CODE != '##' AND A2.EC_ISO_CT_CODE != '##')
      AND F.AIRCRAFT_TYPE_ICAO_ID = AC.ICAO_TYPE_CODE
      AND (F.ICAO_FLT_TYPE != 'M')
      AND SUBSTR(AC.ICAO_DESC,1,1) != 'H'
    GROUP BY A1.EC_ISO_CT_CODE,
      A1.ISO_CT_CODE,
      A2.EC_ISO_CT_CODE,
      TO_CHAR(F.LOBT,'YYYY')"

  query_flows <- DBI::sqlInterpolate(con, sqlq_flows, WEF = wef, TIL = til)

  fltq <- ROracle::dbSendQuery(con, query_flows)
  flows <- ROracle::fetch(fltq, n = -1)
  flows <- tibble::as_tibble(flows)

  ROracle::dbDisconnect(con)
  Sys.unsetenv("TZ")
  Sys.unsetenv("ORA_SDTZ")

  return(flows)
}


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




## ---- retrieve_airport_flows ----

#' retrieve the yearly number of flights between airports for a period of years.
#'
#' @param wef the beginning year of the period (included).
#' @param til the end year of the period (not included).
#'
#' @return a dataframe with year, # of flights, departing country code and arrival country code.
#' @export
#'
#' @examples
#' \dontrun{
#' retrieve_airport_flows('2001', '2002')
#' # A tibble: 8,093,679 x 6
#' YEAR  NB_OF_FLIGHT ADEP  DEP_ISO_COUNTRY_CODE ADES  DES_ISO_COUNTRY_CODE
#' <chr>        <dbl> <chr> <chr>                <chr> <chr>
#'   1 2001           14. EGKB  GB                   LFMD  FR
#' 2 2001           12. LFMD  FR                   LGKR  GR
#' 3 2001            9. EGKB  GB                   LFMD  FR
#' 4 2001            9. LEMD  ES                   LEBL  ES
#' 5 2001            7. EDDR  DE                   ELLX  LU
#' 6 2001            6. LEBL  ES                   LEMD  ES
#' 7 2001            6. LGAT  GR                   LGTS  GR
#' 8 2001            5. EHBK  NL                   EHBK  NL
#' 9 2001            5. GMMX  MA                   LFPB  FR
#' 10 2001            5. EGGW  GB                   EGSS  GB
#' # ... with 8,093,669 more rows
#' }
#'
retrieve_airport_flows <- function(wef, til) {
  if (!requireNamespace("ROracle", quietly = TRUE)) {
    stop("Package \"ROracle\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # DB params
  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  # interval of interest
  wef <- parsedate::parse_iso_8601(wef)
  til <- parsedate::parse_iso_8601(til)
  wef <- format(wef, format = "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, format = "%Y-%m-%dT%H:%M:%SZ")


  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  tz <- "UTC"
  Sys.setenv("TZ" = tz)
  Sys.setenv("ORA_SDTZ" = tz)


  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, usr, pwd, dbname = dbn)

  sqlq_flows <- "SELECT
  TO_CHAR(F.LOBT,'YYYY') YEAR,
  COUNT(FLT_UID) NB_OF_FLIGHT,
  A1.ICAO_AP_CODE ADEP,
  A1.EC_ISO_CT_CODE DEP_ISO_COUNTRY_CODE,
  A2.ICAO_AP_CODE ADES,
  A2.EC_ISO_CT_CODE DES_ISO_COUNTRY_CODE
FROM
  SWH_FCT.FAC_FLIGHT F,
  SWH_FCT.DIM_AIRPORT A1,
  SWH_FCT.DIM_AIRPORT A2,
  SWH_FCT.DIM_AIRCRAFT_TYPE AC
WHERE
  A1.SK_AP_ID = F.SK_ADEP_ID
  AND A2.SK_AP_ID   = F.SK_ADES_ID
  AND F.LOBT       >= TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
  AND F.LOBT       <  TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
  AND (F.ICAO_FLT_TYPE != 'M')
  AND SUBSTR(AC.ICAO_DESC,1,1) != 'H'
  AND F.AIRCRAFT_TYPE_ICAO_ID = AC.ICAO_TYPE_CODE
GROUP BY
  F.LOBT, 'YYYY',
  A1.ICAO_AP_CODE,
  A1.EC_ISO_CT_CODE,
  A2.ICAO_AP_CODE,
  A2.EC_ISO_CT_CODE
ORDER BY
  NB_OF_FLIGHT DESC"

  query_flows <- DBI::sqlInterpolate(con, sqlq_flows, WEF = wef, TIL = til)

  fltq <- ROracle::dbSendQuery(con, query_flows)
  flows <- ROracle::fetch(fltq, n = -1)
  flows <- tibble::as_tibble(flows) %>%
    dplyr::mutate(NB_OF_FLIGHT = as.integer(.data$NB_OF_FLIGHT))

  ROracle::dbDisconnect(con)
  Sys.unsetenv("TZ")
  Sys.unsetenv("ORA_SDTZ")

  return(flows)
}

#' Retrieve airport details
#'
#' @param apts a The ICAO airport IDs to retrieve information about (TODO)
#' @param wef the beginning date of the period (included).
#' @param til the end date of the period (not included).
#'
#' @return A dataframe with
#' \itemize{
#'   \item icao: ICAO ID
#'   \item longitude: Airport Reference Point (APR) longitude
#'   \item latitude: Airport Reference Point (APR) latitude
#'   \item name
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' retrieve_airports("EBBR", "2002-11-24", "2003-01-30")
#' }
#'
retrieve_airports <- function(apts, wef, til) {
  if (!requireNamespace("ROracle", quietly = TRUE)) {
    stop("Package \"ROracle\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # DB params
  usr <- Sys.getenv("PRU_DEV_USR")
  pwd <- Sys.getenv("PRU_DEV_PWD")
  dbn <- Sys.getenv("PRU_DEV_DBNAME")

  # interval of interest
  wef <- parsedate::parse_iso_8601(wef)
  til <- parsedate::parse_iso_8601(til)
  wef <- format(wef, format = "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, format = "%Y-%m-%dT%H:%M:%SZ")


  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  tz <- "UTC"
  Sys.setenv("TZ" = tz)
  Sys.setenv("ORA_SDTZ" = tz)


  drv <- DBI::dbDriver("Oracle")
  con <- ROracle::dbConnect(drv, usr, pwd, dbname = dbn)

  sqlq <- "SELECT
  ICAO_AP_CODE ICAO,
  ICAO_AP_NAME NAME,
  IATA_AP_CODE IATA_CODE,
  IATA_AP_NAME IATA_NAME,
  LONGITUDE,
  LATITUDE,
  ISO_CT_CODE ISO_CODE,
  ISO_CT_NAME,
  VALID_FROM,
  VALID_TO
FROM
  SWH_FCT.DIM_AIRPORT
WHERE
  VALID_FROM <=  TO_DATE(?WEF, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
  AND VALID_TO >  TO_DATE(?TIL, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
ORDER BY
  ICAO_AP_CODE"

  query <- DBI::sqlInterpolate(con, sqlq, WEF = wef, TIL = til)

  aptq <- ROracle::dbSendQuery(con, query)
  apts <- ROracle::fetch(aptq, n = -1)
  apts <- tibble::as_tibble(apts)

  # TODO: retain only the ones passed as input argument

  ROracle::dbDisconnect(con)
  Sys.unsetenv("TZ")
  Sys.unsetenv("ORA_SDTZ")

  return(apts)
}



