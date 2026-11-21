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
#' @param prefilter Integer or `NULL`. When set, uses the fast C++ haversine
#'   to identify the top `prefilter` closest locations per voter, then
#'   computes expensive road-network distances only for those candidates.
#'   This dramatically reduces computation for large voter/location sets
#'   (e.g., from 7M x 307 = 2.1B routes down to 7M x 10 = 70M routes).
#'   Only used when `method` is `"driving"`, `"walking"`, or `"cycling"`;
#'   ignored for `method = "haversine"`. Must be >= `k`. Default `NULL`
#'   (no pre-filter; full distance matrix is computed).
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
#'
#' # Haversine pre-filter for large datasets (screens to top 10 candidates
#' # per voter by straight-line distance before computing road distances)
#' result_fast <- nearest_location(voter_meck[1:10, ], early_meck,
#'   voter_coords = c("lat", "long"),
#'   location_coords = c("lat", "long"),
#'   method = "driving", network = net,
#'   prefilter = 10
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
                             network = NULL,
                             prefilter = NULL) {
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

  # --- Validate prefilter ---
  if (!is.null(prefilter)) {
    prefilter <- as.integer(prefilter)
    if (method == "haversine") {
      warning("'prefilter' is ignored for method = 'haversine'.",
        call. = FALSE
      )
      prefilter <- NULL
    } else {
      if (prefilter < 1L) {
        stop("'prefilter' must be >= 1.", call. = FALSE)
      }
      if (is.null(max_dist) && prefilter < k) {
        stop("'prefilter' must be >= 'k' (", k, ").", call. = FALSE)
      }
      n_locs <- nrow(l_coords)
      if (prefilter >= n_locs) {
        message(
          "prefilter (", prefilter, ") >= number of locations (", n_locs,
          "). Using full matrix instead."
        )
        prefilter <- NULL
      }
    }
  }

  # --- Road-network routing ---
  if (method != "haversine") {
    .check_dodgr()
    profile <- .method_to_profile(method)

    # Build or validate network
    cpp_cache <- NULL
    if (!is.null(network)) {
      net_obj <- .validate_network(network, profile)
      graph <- net_obj$graph
      cpp_cache <- net_obj$cpp_cache
    } else {
      message(
        "Downloading and building street network (this may be slow).\n",
        "Use prepare_network() to build once and reuse across calls."
      )
      bbox <- .compute_bbox(v_coords, l_coords, pad = 0.05)
      graph <- .build_dodgr_graph(bbox, profile, quiet = !progress)
    }

    # --- Pre-filter branch: haversine screening + sparse routing ---
    if (!is.null(prefilter)) {
      if (progress) message("Haversine pre-filter: selecting top ", prefilter,
                            " candidates per voter...")
      haversine_raw <- cpp_k_nearest(
        voter_lat, voter_lon, loc_lat, loc_lon,
        prefilter, progress
      )

      if (progress) message("Computing road-network distances for ",
                            nrow(v_coords), " x ", prefilter, " pairs...")
      prefiltered <- .route_distances_prefiltered(
        v_coords, l_coords, haversine_raw$indices,
        graph, measure, cpp_cache = cpp_cache
      )

      # Extract k-nearest or threshold from pre-filtered results
      if (!is.null(max_dist)) {
        if (measure == "time") {
          max_val <- max_dist * 60.0
        } else {
          max_val <- .to_meters(max_dist, units)
        }
        raw <- .prefiltered_to_threshold(prefiltered, max_val)
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
        raw <- .prefiltered_to_knearest(prefiltered, k)
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

    # Compute full distance/time matrix
    mat <- .dodgr_route_matrix(
      v_coords, l_coords, graph, measure, progress,
      cpp_cache = cpp_cache
    )

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
