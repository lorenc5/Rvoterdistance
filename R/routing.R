#' Download an OpenStreetMap extract for a region
#'
#' Downloads a pre-built OSM extract (PBF file) from Geofabrik or BBBike for
#' use with [prepare_network()]. This is the recommended approach for
#' state-sized or larger areas where the Overpass API would be too slow or
#' hit rate limits.
#'
#' Requires the \pkg{osmextract} package.
#'
#' @param place Character string matching a region name recognised by the
#'   provider (e.g., `"Georgia"`, `"North Carolina"`, `"Germany"`).
#' @param provider Character. Which OSM extract provider to use. One of
#'   `"geofabrik"` (default) or `"bbbike"`.
#' @param download_dir Character. Directory to save the downloaded file. If
#'   `NULL` (default), uses the \pkg{osmextract} cache directory.
#' @param quiet Logical. If `TRUE`, suppress download messages.
#'   Default `FALSE`.
#'
#' @return Character string: the file path to the downloaded `.osm.pbf` file.
#'   Pass this path to `prepare_network(file = ...)`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Download Georgia extract from Geofabrik
#' osm_file <- download_osm("Georgia")
#'
#' # Build a driving network from the downloaded file
#' net <- prepare_network(file = osm_file, mode = "driving")
#'
#' # Use with nearest_location()
#' result <- nearest_location(georgia_voters, dropboxes,
#'   voter_coords = c("lat", "lon"),
#'   location_coords = c("lat", "lon"),
#'   method = "driving", network = net
#' )
#' }
download_osm <- function(place,
                         provider = c("geofabrik", "bbbike"),
                         download_dir = NULL,
                         quiet = FALSE) {
  .check_osmextract()
  provider <- match.arg(provider)

  if (!quiet) message("Matching '", place, "' to ", provider, " catalogue...")
  match_result <- osmextract::oe_match(place, provider = provider, quiet = quiet)

  download_args <- list(
    file_url = match_result$url,
    quiet = quiet,
    force_download = FALSE
  )
  if (!is.null(download_dir)) {
    download_args$download_directory <- download_dir
  }

  if (!quiet) message("Downloading OSM extract...")
  file_path <- do.call(osmextract::oe_download, download_args)

  if (!quiet) message("Download complete: ", file_path)
  file_path
}


#' Prepare a road network for routing
#'
#' Downloads and weights an OpenStreetMap street network for use with
#' road-network routing in [nearest_location()]. The resulting object can be
#' reused across multiple calls and saved with [saveRDS()] to avoid
#' re-downloading.
#'
#' Requires the \pkg{dodgr} package. If \pkg{cppRouting} is also installed,
#' a contraction hierarchy is pre-built for faster matrix computation.
#'
#' @param bbox Numeric vector of length 4: `c(xmin, ymin, xmax, ymax)` in
#'   WGS-84 lon/lat. If `NULL`, computed from `voters` and `locations` with
#'   padding.
#' @param voters A data frame, matrix, or `sf` POINT object of voter locations.
#'   Used to compute bounding box when `bbox` is `NULL`.
#' @param locations A data frame, matrix, or `sf` POINT object of polling
#'   locations. Used to compute bounding box when `bbox` is `NULL`.
#' @param voter_coords Character vector of length 2: `c("lat_col", "lon_col")`
#'   for `voters`. Ignored if `voters` is an `sf` object.
#' @param location_coords Character vector of length 2:
#'   `c("lat_col", "lon_col")` for `locations`. Ignored if `locations` is an
#'   `sf` object.
#' @param mode Character. One of `"driving"`, `"walking"`, or `"cycling"`.
#'   Default `"driving"`.
#' @param pad Numeric. Fraction of the bounding box extent to add as padding.
#'   Default `0.05` (5%).
#' @param quiet Logical. If `TRUE`, suppress download messages. Default `FALSE`.
#' @param file Character. Path to a local `.osm.pbf` or `.gpkg` file (e.g.,
#'   from [download_osm()]). When provided, the street network is loaded from
#'   this file instead of downloading via the Overpass API. This is recommended
#'   for state-sized or larger areas. Requires the \pkg{osmextract} package.
#'
#' @return A list of class `"rvd_network"` containing the weighted graph and
#'   metadata. Pass this to `nearest_location(network = ...)`.
#'
#' @export
#' @examples
#' \dontrun{
#' data(meck_ev)
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
#' # Large-area workflow using a Geofabrik extract:
#' osm_file <- download_osm("Georgia")
#' net_ga <- prepare_network(file = osm_file, mode = "driving")
#' }
prepare_network <- function(bbox = NULL,
                            voters = NULL,
                            locations = NULL,
                            voter_coords = NULL,
                            location_coords = NULL,
                            mode = c("driving", "walking", "cycling"),
                            pad = 0.05,
                            quiet = FALSE,
                            file = NULL) {
  .check_dodgr()
  mode <- match.arg(mode)
  profile <- .method_to_profile(mode)

  if (!is.null(file)) {
    # --- Local file path: load via osmextract ---
    net <- .load_osm_file(file, quiet = quiet)
    if (!quiet) message("Weighting street network...")
    graph <- dodgr::weight_streetnet(net, wt_profile = profile)
    attr(graph, "rvd_profile") <- profile
  } else if (!is.null(bbox) || (!is.null(voters) && !is.null(locations))) {
    # --- Overpass API path (existing behaviour) ---
    if (is.null(bbox)) {
      v_coords <- .extract_coords(voters, voter_coords, "voters")
      l_coords <- .extract_coords(locations, location_coords, "locations")
      bbox <- .compute_bbox(v_coords, l_coords, pad)
    }
    if (!quiet) message("Downloading street network from OpenStreetMap...")
    graph <- .build_dodgr_graph(bbox, profile, quiet)
  } else {
    stop(
      "Provide one of: 'file', 'bbox', or both 'voters' and 'locations'.",
      call. = FALSE
    )
  }

  # Pre-build cppRouting structures if available
  cpp_cache <- .build_cpprouting_cache(graph, quiet)

  structure(
    list(
      graph = graph, mode = mode, profile = profile,
      bbox = bbox, cpp_cache = cpp_cache
    ),
    class = "rvd_network"
  )
}


# --- Internal helpers --------------------------------------------------------

#' Check that dodgr is installed
#' @noRd
.check_dodgr <- function() {
  if (!requireNamespace("dodgr", quietly = TRUE)) {
    stop(
      "Package 'dodgr' is required for road-network routing.\n",
      "Install it with: install.packages('dodgr')",
      call. = FALSE
    )
  }
}


#' Map method name to dodgr weighting profile
#' @noRd
.method_to_profile <- function(method) {
  switch(method,
    driving = "motorcar",
    walking = "foot",
    cycling = "bicycle",
    stop("Unknown method: ", method, call. = FALSE)
  )
}


#' Compute bounding box from voter and location coordinate matrices
#' @param v_coords Two-column matrix (lat, lon) of voter coordinates.
#' @param l_coords Two-column matrix (lat, lon) of location coordinates.
#' @param pad Fraction of extent to add as padding.
#' @return Named numeric vector c(xmin, ymin, xmax, ymax) in lon/lat.
#' @noRd
.compute_bbox <- function(v_coords, l_coords, pad) {
  all_lat <- c(v_coords[, 1], l_coords[, 1])
  all_lon <- c(v_coords[, 2], l_coords[, 2])

  lat_range <- range(all_lat)
  lon_range <- range(all_lon)

  lat_pad <- (lat_range[2] - lat_range[1]) * pad
  lon_pad <- (lon_range[2] - lon_range[1]) * pad

  c(
    xmin = lon_range[1] - lon_pad,
    ymin = lat_range[1] - lat_pad,
    xmax = lon_range[2] + lon_pad,
    ymax = lat_range[2] + lat_pad
  )
}


#' Download and weight an OSM street network with retry and server fallback
#' @noRd
.build_dodgr_graph <- function(bbox, profile, quiet) {
  overpass_urls <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://maps.mail.ru/osm/tools/overpass/api/interpreter"
  )

  net <- NULL
  for (url in overpass_urls) {
    for (attempt in 1:3) {
      net <- tryCatch(
        {
          osmdata::set_overpass_url(url)
          dodgr::dodgr_streetnet(bbox = bbox, quiet = quiet)
        },
        error = function(e) {
          if (!quiet) {
            message(
              "Overpass API error (attempt ", attempt, "/3, server: ",
              url, "): ", conditionMessage(e)
            )
          }
          NULL
        }
      )
      if (!is.null(net)) break
      if (attempt < 3) Sys.sleep(5 * attempt) # 5s, 10s backoff
    }
    if (!is.null(net)) break
    if (!quiet) message("Trying alternative Overpass server...")
  }

  if (is.null(net)) {
    stop(
      "Failed to download street network after multiple attempts.\n",
      "The Overpass API may be temporarily unavailable. Try again later.",
      call. = FALSE
    )
  }

  graph <- dodgr::weight_streetnet(net, wt_profile = profile)
  attr(graph, "rvd_profile") <- profile
  graph
}


#' Pre-build cppRouting contraction hierarchies for both distance and time
#' @return List with contracted_dist, contracted_time, verts, or NULL if
#'   cppRouting is not available.
#' @noRd
.build_cpprouting_cache <- function(graph, quiet = FALSE) {
  if (!requireNamespace("cppRouting", quietly = TRUE)) {
    return(NULL)
  }

  if (!quiet) message("Building contraction hierarchies (cppRouting)...")

  verts <- dodgr::dodgr_vertices(graph)

  # Distance contraction hierarchy
  edges_dist <- data.frame(
    from = graph$from_id,
    to = graph$to_id,
    weight = graph$d_weighted,
    stringsAsFactors = FALSE
  )
  valid_dist <- is.finite(edges_dist$weight) & edges_dist$weight >= 0
  edges_dist <- edges_dist[valid_dist, ]
  cpp_graph_dist <- cppRouting::makegraph(edges_dist, directed = TRUE)
  contracted_dist <- cppRouting::cpp_contract(cpp_graph_dist)

  # Time contraction hierarchy
  contracted_time <- NULL
  if ("time_weighted" %in% names(graph)) {
    edges_time <- data.frame(
      from = graph$from_id,
      to = graph$to_id,
      weight = graph$time_weighted,
      stringsAsFactors = FALSE
    )
    valid_time <- is.finite(edges_time$weight) & edges_time$weight >= 0
    edges_time <- edges_time[valid_time, ]
    cpp_graph_time <- cppRouting::makegraph(edges_time, directed = TRUE)
    contracted_time <- cppRouting::cpp_contract(cpp_graph_time)
  }

  list(
    contracted_dist = contracted_dist,
    contracted_time = contracted_time,
    verts = verts
  )
}


#' Check that osmextract is installed
#' @noRd
.check_osmextract <- function() {
  if (!requireNamespace("osmextract", quietly = TRUE)) {
    stop(
      "Package 'osmextract' is required for Geofabrik/BBBike downloads.\n",
      "Install it with: install.packages('osmextract')",
      call. = FALSE
    )
  }
}


#' Load an OSM file (PBF or GPKG) via osmextract
#' @param file Path to a `.osm.pbf` or `.gpkg` file.
#' @param quiet Logical. Suppress messages.
#' @return An sf object with highway lines suitable for `weight_streetnet()`.
#' @noRd
.load_osm_file <- function(file, quiet = FALSE) {
  .check_osmextract()
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }
  if (!quiet) message("Reading OSM data from local file...")
  osmextract::oe_read(
    file,
    layer = "lines",
    extra_tags = c("maxspeed", "oneway", "access", "foot", "bicycle",
                   "motorcar", "motor_vehicle"),
    quiet = quiet
  )
}


#' Validate a user-supplied network object
#' @noRd
.validate_network <- function(network, profile) {
  if (!inherits(network, "rvd_network")) {
    stop("'network' must be an object returned by prepare_network().",
      call. = FALSE
    )
  }
  if (network$profile != profile) {
    warning(
      "Network was built for '", network$mode, "' but method requests '",
      profile, "' profile. Results may be inaccurate.",
      call. = FALSE
    )
  }
  network
}


#' Compute a distance or time matrix using dodgr (+ cppRouting if available)
#'
#' @param v_coords Two-column matrix (lat, lon) of voter coordinates.
#' @param l_coords Two-column matrix (lat, lon) of location coordinates.
#' @param graph A weighted dodgr graph.
#' @param measure `"distance"` or `"time"`.
#' @param progress Logical.
#' @param cpp_cache Optional pre-built cppRouting cache from prepare_network().
#' @return Numeric matrix: rows = voters, columns = locations. Values are
#'   meters (distance) or seconds (time).
#' @noRd
.dodgr_route_matrix <- function(v_coords, l_coords, graph, measure,
                                progress, cpp_cache = NULL) {
  # dodgr expects lon/lat column order
  from_ll <- cbind(lon = v_coords[, 2], lat = v_coords[, 1])
  to_ll <- cbind(lon = l_coords[, 2], lat = l_coords[, 1])

  # Use cached cppRouting structures if available
  if (!is.null(cpp_cache)) {
    mat <- .route_matrix_cpprouting_cached(
      from_ll, to_ll, graph, measure, cpp_cache
    )
    return(mat)
  }

  # Build cppRouting on the fly (no cache)
  if (requireNamespace("cppRouting", quietly = TRUE)) {
    mat <- .route_matrix_cpprouting(from_ll, to_ll, graph, measure)
    return(mat)
  }

  # Fallback to dodgr only
  .route_matrix_dodgr(from_ll, to_ll, graph, measure)
}


#' Compute route matrix using pre-built cppRouting cache (fastest path)
#' @noRd
.route_matrix_cpprouting_cached <- function(from_ll, to_ll, graph, measure,
                                            cpp_cache) {
  verts <- cpp_cache$verts

  if (measure == "time") {
    contracted <- cpp_cache$contracted_time
    if (is.null(contracted)) {
      # Time hierarchy not available, fall back
      return(.route_matrix_dodgr(from_ll, to_ll, graph, measure))
    }
  } else {
    contracted <- cpp_cache$contracted_dist
  }

  from_idx <- dodgr::match_pts_to_verts(verts, from_ll)
  to_idx <- dodgr::match_pts_to_verts(verts, to_ll)

  from_ids <- verts$id[from_idx]
  to_ids <- verts$id[to_idx]

  mat <- cppRouting::get_distance_matrix(
    contracted,
    from = from_ids,
    to = to_ids,
    algorithm = "phast"
  )

  mat[!is.finite(mat)] <- NA_real_
  mat
}


#' Compute route matrix using cppRouting (no cache -- builds on the fly)
#' @noRd
.route_matrix_cpprouting <- function(from_ll, to_ll, graph, measure) {
  edges <- data.frame(
    from = graph$from_id,
    to = graph$to_id,
    stringsAsFactors = FALSE
  )

  if (measure == "time") {
    edges$weight <- graph$time_weighted
  } else {
    edges$weight <- graph$d_weighted
  }

  valid <- is.finite(edges$weight) & edges$weight >= 0
  edges <- edges[valid, ]

  cpp_graph <- cppRouting::makegraph(edges, directed = TRUE)
  contracted <- cppRouting::cpp_contract(cpp_graph)

  verts <- dodgr::dodgr_vertices(graph)

  from_idx <- dodgr::match_pts_to_verts(verts, from_ll)
  to_idx <- dodgr::match_pts_to_verts(verts, to_ll)

  from_ids <- verts$id[from_idx]
  to_ids <- verts$id[to_idx]

  mat <- cppRouting::get_distance_matrix(
    contracted,
    from = from_ids,
    to = to_ids,
    algorithm = "phast"
  )

  mat[!is.finite(mat)] <- NA_real_
  mat
}


#' Compute route matrix using dodgr only (fallback path)
#' @noRd
.route_matrix_dodgr <- function(from_ll, to_ll, graph, measure) {
  if (measure == "time") {
    mat <- dodgr::dodgr_times(graph, from = from_ll, to = to_ll)
  } else {
    mat <- dodgr::dodgr_dists(graph, from = from_ll, to = to_ll)
  }

  mat
}


#' Extract k-nearest from a full distance/time matrix
#'
#' @param mat Numeric matrix (n_voters x n_locations).
#' @param k Integer number of nearest to extract.
#' @return List with `distances` (n_voters x k matrix) and `indices`
#'   (n_voters x k matrix, 1-based).
#' @noRd
.matrix_to_knearest <- function(mat, k) {
  n_voters <- nrow(mat)
  n_locs <- ncol(mat)
  k <- min(k, n_locs)

  dist_mat <- matrix(NA_real_, nrow = n_voters, ncol = k)
  idx_mat <- matrix(NA_integer_, nrow = n_voters, ncol = k)

  for (i in seq_len(n_voters)) {
    row_vals <- mat[i, ]
    ord <- order(row_vals, na.last = TRUE)
    top_k <- ord[seq_len(k)]
    dist_mat[i, ] <- row_vals[top_k]
    idx_mat[i, ] <- top_k
  }

  list(distances = dist_mat, indices = idx_mat)
}


#' Extract within-threshold entries from a full distance/time matrix
#'
#' @param mat Numeric matrix (n_voters x n_locations).
#' @param max_val Numeric threshold.
#' @return List of lists (one per voter), each with `distances` and `indices`
#'   (1-based), matching the structure of `cpp_within_threshold()`.
#' @noRd
.matrix_to_threshold <- function(mat, max_val) {
  n_voters <- nrow(mat)
  result <- vector("list", n_voters)

  for (i in seq_len(n_voters)) {
    row_vals <- mat[i, ]
    within <- which(!is.na(row_vals) & row_vals <= max_val)
    if (length(within) == 0L) {
      result[[i]] <- list(distances = numeric(0), indices = integer(0))
    } else {
      ord <- order(row_vals[within])
      result[[i]] <- list(
        distances = row_vals[within[ord]],
        indices = within[ord]
      )
    }
  }

  result
}
