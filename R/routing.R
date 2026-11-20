#' Prepare a road network for routing
#'
#' Downloads and weights an OpenStreetMap street network for use with
#' road-network routing in [nearest_location()]. The resulting object can be
#' reused across multiple calls and saved with [saveRDS()] to avoid
#' re-downloading.
#'
#' Requires the \pkg{dodgr} package. If \pkg{cppRouting} is also installed,
#' a contraction hierarchy is built for faster matrix computation.
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
#' }
prepare_network <- function(bbox = NULL,
                            voters = NULL,
                            locations = NULL,
                            voter_coords = NULL,
                            location_coords = NULL,
                            mode = c("driving", "walking", "cycling"),
                            pad = 0.05,
                            quiet = FALSE) {
  .check_dodgr()
  mode <- match.arg(mode)
  profile <- .method_to_profile(mode)

  if (is.null(bbox)) {
    if (is.null(voters) || is.null(locations)) {
      stop("Either 'bbox' or both 'voters' and 'locations' must be provided.",
        call. = FALSE
      )
    }
    v_coords <- .extract_coords(voters, voter_coords, "voters")
    l_coords <- .extract_coords(locations, location_coords, "locations")
    bbox <- .compute_bbox(v_coords, l_coords, pad)
  }

  graph <- .build_dodgr_graph(bbox, profile, quiet)

  structure(
    list(graph = graph, mode = mode, profile = profile, bbox = bbox),
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


#' Download and weight an OSM street network
#' @noRd
.build_dodgr_graph <- function(bbox, profile, quiet) {
  net <- dodgr::dodgr_streetnet(bbox = bbox, quiet = quiet)
  graph <- dodgr::weight_streetnet(net, wt_profile = profile)
  attr(graph, "rvd_profile") <- profile
  graph
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
  network$graph
}


#' Compute a distance or time matrix using dodgr (+ cppRouting if available)
#'
#' @param v_coords Two-column matrix (lat, lon) of voter coordinates.
#' @param l_coords Two-column matrix (lat, lon) of location coordinates.
#' @param graph A weighted dodgr graph.
#' @param measure `"distance"` or `"time"`.
#' @param progress Logical.
#' @return Numeric matrix: rows = voters, columns = locations. Values are
#'   meters (distance) or seconds (time).
#' @noRd
.dodgr_route_matrix <- function(v_coords, l_coords, graph, measure,
                                progress) {
  # dodgr expects lon/lat column order
  from_ll <- cbind(lon = v_coords[, 2], lat = v_coords[, 1])
  to_ll <- cbind(lon = l_coords[, 2], lat = l_coords[, 1])

  use_cpprouting <- requireNamespace("cppRouting", quietly = TRUE)

  if (use_cpprouting) {
    mat <- .route_matrix_cpprouting(from_ll, to_ll, graph, measure)
  } else {
    mat <- .route_matrix_dodgr(from_ll, to_ll, graph, measure)
  }

  mat
}


#' Compute route matrix using cppRouting (fast path)
#' @noRd
.route_matrix_cpprouting <- function(from_ll, to_ll, graph, measure) {
  # Extract edge list from dodgr graph
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

  # Remove edges with NA or non-finite weights
  valid <- is.finite(edges$weight) & edges$weight >= 0
  edges <- edges[valid, ]

  # Build cppRouting graph and contraction hierarchy
  cpp_graph <- cppRouting::makegraph(edges, directed = TRUE)
  contracted <- cppRouting::cpp_contract(cpp_graph)

  # Snap coordinates to nearest graph vertices using dodgr
  verts <- dodgr::dodgr_vertices(graph)
  vert_coords <- cbind(x = verts$x, y = verts$y)

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

  # cppRouting returns Inf for unreachable; convert to NA
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
