#' Find nearest polling locations for each voter
#'
#' Calculates the distance between each voter and a set of polling/drop box
#' locations using the Haversine formula or road-network routing. Can return
#' the single nearest location, the k nearest, or all locations within a
#' distance threshold.
#'
#' @param voters A data frame, matrix, or `sf` POINT object containing voter
#'   locations. If a data frame or matrix, must contain lat/lon columns
#'   specified by `voter_coords`.
#' @param locations A data frame, matrix, or `sf` POINT object containing
#'   polling/drop box locations. If a data frame or matrix, must contain
#'   lat/lon columns specified by `location_coords`.
#' @param voter_coords Character vector of length 2: `c("lat_col", "lon_col")`
#'   identifying the latitude and longitude columns in `voters`. Ignored if
#'   `voters` is an `sf` object.
#' @param location_coords Character vector of length 2:
#'   `c("lat_col", "lon_col")` identifying the latitude and longitude columns
#'   in `locations`. Ignored if `locations` is an `sf` object.
#' @param k Integer. Number of nearest locations to return per voter.
#'   Default `1`.
#' @param max_dist Numeric or `NULL`. If not `NULL`, return all locations
#'   within this distance of each voter. Units controlled by `units` (for
#'   Haversine and road-network distance) or interpreted as minutes (for
#'   road-network time). Overrides `k`.
#' @param units Character. One of `"km"`, `"miles"`, or `"meters"`.
#'   Default `"km"`.
#' @param append_data Logical. If `TRUE` (default), include voter and matched
#'   location columns in the output. When `k = 1`, also appends the matched
#'   location row.
#' @param progress Logical. If `TRUE`, print progress for large computations.
#'   Default `FALSE`.
#' @param method Character. One of `"haversine"` (default), `"driving"`,
#'   `"walking"`, or `"cycling"`. Non-haversine methods use road-network
#'   routing via \pkg{dodgr} (required) and optionally \pkg{cppRouting} for
#'   faster computation.
#' @param measure Character. One of `"distance"` (default) or `"time"`.
#'   `"time"` returns travel time (seconds, minutes, hours) and requires a
#'   road-network method.
#' @param network An optional pre-built network object from
#'   [prepare_network()]. Avoids re-downloading the street network on
#'   repeated calls.
#'
#' @return A data frame. When `measure = "distance"`: distance columns
#'   (`distance_m`, `distance_km`, `distance_miles`). When
#'   `measure = "time"`: time columns (`time_seconds`, `time_minutes`,
#'   `time_hours`). If `k = 1` and `max_dist` is `NULL`: one row per voter.
#'   If `k > 1` or `max_dist` is not `NULL`: one row per voter-location pair
#'   with a `rank` column.
#'
#' @export
#' @examples
#' data(meck_ev)
#'
#' # Nearest single location for each voter
#' result <- nearest_location(voter_meck, early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long")
#' )
#' head(result)
#'
#' # 3 nearest locations per voter
#' result_k3 <- nearest_location(voter_meck, early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   k = 3
#' )
#' head(result_k3)
#'
#' # All locations within 10 km
#' result_10km <- nearest_location(voter_meck, early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   max_dist = 10, units = "km"
#' )
#' head(result_10km)
#'
#' \dontrun{
#' # Road-network driving distance (requires dodgr package)
#' result_drive <- nearest_location(voter_meck[1:10, ], early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   method = "driving"
#' )
#'
#' # Walking travel time
#' result_time <- nearest_location(voter_meck[1:10, ], early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   method = "walking", measure = "time"
#' )
#'
#' # Pre-built network for repeated calls
#' net <- prepare_network(
#'   voters = voter_meck, locations = early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   mode = "driving"
#' )
#' result <- nearest_location(voter_meck[1:10, ], early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   method = "driving", network = net
#' )
#' }
nearest_location <- function(voters,
                             locations,
                             voter_coords = NULL,
                             location_coords = NULL,
                             k = 1L,
                             max_dist = NULL,
                             units = c("km", "miles", "meters"),
                             append_data = TRUE,
                             progress = FALSE,
                             method = c("haversine", "driving", "walking",
                                        "cycling"),
                             measure = c("distance", "time"),
                             network = NULL) {
  units <- match.arg(units)
  method <- match.arg(method)
  measure <- match.arg(measure)

  # --- Validate measure/method combination ---
  if (measure == "time" && method == "haversine") {
    stop("measure = 'time' requires a road-network method ",
      "('driving', 'walking', or 'cycling').",
      call. = FALSE
    )
  }

  # --- Extract coordinates ---
  v_coords <- .extract_coords(voters, voter_coords, "voters")
  l_coords <- .extract_coords(locations, location_coords, "locations")

  voter_lat <- v_coords[, 1]
  voter_lon <- v_coords[, 2]
  loc_lat <- l_coords[, 1]
  loc_lon <- l_coords[, 2]

  # --- Input validation ---
  .validate_coords(voter_lat, voter_lon, "voters")
  .validate_coords(loc_lat, loc_lon, "locations")

  k <- as.integer(k)
  if (k < 1L) stop("'k' must be >= 1", call. = FALSE)

  # --- Road-network routing ---
  if (method != "haversine") {
    .check_dodgr()
    profile <- .method_to_profile(method)

    # Build or validate network
    if (!is.null(network)) {
      graph <- .validate_network(network, profile)
    } else {
      bbox <- .compute_bbox(v_coords, l_coords, pad = 0.05)
      graph <- .build_dodgr_graph(bbox, profile, quiet = !progress)
    }

    # Compute full distance/time matrix
    mat <- .dodgr_route_matrix(v_coords, l_coords, graph, measure, progress)

    # Dispatch to k-nearest or threshold mode
    if (!is.null(max_dist)) {
      if (measure == "time") {
        # max_dist interpreted as minutes for time measure
        max_val <- max_dist * 60.0 # convert minutes to seconds
      } else {
        max_val <- .to_meters(max_dist, units)
      }
      raw <- .matrix_to_threshold(mat, max_val)
      if (measure == "time") {
        result <- .format_threshold_result_time(
          raw, voters, locations, append_data
        )
      } else {
        result <- .format_threshold_result(
          raw, voters, locations, units, append_data
        )
      }
    } else {
      raw <- .matrix_to_knearest(mat, k)
      if (measure == "time") {
        result <- .format_knearest_result_time(
          raw, k, voters, locations, append_data
        )
      } else {
        result <- .format_knearest_result(
          raw, k, voters, locations, units, append_data
        )
      }
    }

    return(result)
  }

  # --- Haversine path (unchanged) ---
  if (!is.null(max_dist)) {
    # Threshold mode
    max_dist_m <- .to_meters(max_dist, units)
    raw <- cpp_within_threshold(
      voter_lat, voter_lon, loc_lat, loc_lon,
      max_dist_m, progress
    )
    result <- .format_threshold_result(
      raw, voters, locations, units,
      append_data
    )
  } else {
    # k-nearest mode
    raw <- cpp_k_nearest(voter_lat, voter_lon, loc_lat, loc_lon, k, progress)
    result <- .format_knearest_result(
      raw, k, voters, locations, units,
      append_data
    )
  }

  result
}
