#' King County ballot drop box locations
#'
#' A data frame of ballot drop box locations in King County, Washington.
#'
#' @format A data frame with columns including `lat` and `long`.
#' @source King County Elections
"dbox"

#' King County voter sample with geocoded addresses
#'
#' A sample of geocoded voter records from King County, Washington,
#' including latitude and longitude of residential addresses.
#'
#' @format A data frame with columns including `Residence_Addresses_Latitude`
#'   and `Residence_Addresses_Longitude`.
#' @source King County voter file (anonymized sample)
"king_geo"

#' Mecklenburg County early voting locations
#'
#' A data frame of early voting locations in Mecklenburg County,
#' North Carolina.
#'
#' @format A data frame with columns including `lat` and `long`.
#' @source Mecklenburg County Board of Elections
"early_meck"

#' Mecklenburg County voter sample with geocoded addresses
#'
#' A sample of geocoded voter records from Mecklenburg County,
#' North Carolina, including latitude and longitude.
#'
#' @format A data frame with columns including `lat` and `long`.
#' @source Mecklenburg County voter file (anonymized sample)
"voter_meck"

#' Georgia ballot drop box locations (2020)
#'
#' A data frame of 307 ballot drop box locations across 144 counties in
#' Georgia that were active during the 2020 general election. Useful for
#' testing state-wide distance calculations and the Geofabrik/file-based
#' routing workflow.
#'
#' @format A data frame with 307 rows and 5 columns:
#' \describe{
#'   \item{county}{County name}
#'   \item{location_name}{Name of the drop box location}
#'   \item{address}{Street address of the drop box}
#'   \item{lat}{Latitude (WGS-84)}
#'   \item{long}{Longitude (WGS-84)}
#' }
#' @source Georgia Secretary of State
"ga_dbox"

#' Georgia voter sample with geocoded addresses
#'
#' A sample of 400 geocoded voter records from four Georgia counties
#' (Chatham, Clarke, Fulton, Muscogee), 100 voters per county. These
#' counties span urban and mid-sized areas across the state.
#'
#' @format A data frame with 400 rows and 3 columns:
#' \describe{
#'   \item{county}{County name}
#'   \item{lat}{Latitude of residential address (WGS-84)}
#'   \item{long}{Longitude of residential address (WGS-84)}
#' }
#' @source Georgia voter file (anonymized sample)
"ga_voter"
