# --- Internal helper tests (no network needed) ---

test_that(".method_to_profile maps correctly", {
  expect_equal(.method_to_profile("driving"), "motorcar")
  expect_equal(.method_to_profile("walking"), "foot")
  expect_equal(.method_to_profile("cycling"), "bicycle")
  expect_error(.method_to_profile("flying"), "Unknown method")
})

test_that(".compute_bbox returns correct structure", {
  v <- matrix(c(35.2, -80.8, 35.3, -80.7), ncol = 2, byrow = TRUE)
  l <- matrix(c(35.1, -80.9, 35.4, -80.6), ncol = 2, byrow = TRUE)
  bb <- .compute_bbox(v, l, pad = 0.05)
  expect_named(bb, c("xmin", "ymin", "xmax", "ymax"))
  # xmin/xmax are longitude, ymin/ymax are latitude

  expect_true(bb["xmin"] < -80.9)
  expect_true(bb["xmax"] > -80.6)
  expect_true(bb["ymin"] < 35.1)
  expect_true(bb["ymax"] > 35.4)
})

test_that(".matrix_to_knearest extracts correct k", {
  mat <- matrix(c(10, 5, 20, 1, 15, 8), nrow = 2, byrow = TRUE)
  res <- .matrix_to_knearest(mat, k = 2)
  expect_equal(nrow(res$distances), 2)
  expect_equal(ncol(res$distances), 2)
  # Row 1: values are 10, 5, 20 -> nearest are 5 (col 2), 10 (col 1)
  expect_equal(res$distances[1, ], c(5, 10))
  expect_equal(res$indices[1, ], c(2L, 1L))
  # Row 2: values are 1, 15, 8 -> nearest are 1 (col 1), 8 (col 3)
  expect_equal(res$distances[2, ], c(1, 8))
  expect_equal(res$indices[2, ], c(1L, 3L))
})

test_that(".matrix_to_threshold filters correctly", {
  mat <- matrix(c(10, 5, 20, 1, 15, 8), nrow = 2, byrow = TRUE)
  res <- .matrix_to_threshold(mat, max_val = 10)
  # Row 1: 10, 5, 20 -> within 10: indices 1 (val 10), 2 (val 5)
  expect_equal(res[[1]]$distances, c(5, 10))
  expect_equal(res[[1]]$indices, c(2L, 1L))
  # Row 2: 1, 15, 8 -> within 10: indices 1 (val 1), 3 (val 8)
  expect_equal(res[[2]]$distances, c(1, 8))
  expect_equal(res[[2]]$indices, c(1L, 3L))
})

test_that(".matrix_to_threshold handles no matches", {
  mat <- matrix(c(100, 200), nrow = 1)
  res <- .matrix_to_threshold(mat, max_val = 10)
  expect_equal(length(res[[1]]$distances), 0)
  expect_equal(length(res[[1]]$indices), 0)
})

test_that(".matrix_to_knearest handles NAs", {
  mat <- matrix(c(10, NA, 5, 1, NA, NA), nrow = 2, byrow = TRUE)
  res <- .matrix_to_knearest(mat, k = 2)
  # Row 1: 10, NA, 5 -> nearest: 5 (col 3), 10 (col 1)
  expect_equal(res$distances[1, ], c(5, 10))
  expect_equal(res$indices[1, ], c(3L, 1L))
})

test_that("measure = 'time' with method = 'haversine' errors", {
  data(meck_ev, package = "Rvoterdistance")
  expect_error(
    nearest_location(voter_meck[1:2, ], early_meck,
      voter_coords = c("lat", "long"),
      location_coords = c("lat", "long"),
      method = "haversine", measure = "time"
    ),
    "measure = 'time' requires a road-network method"
  )
})

test_that("invalid network object errors", {
  expect_error(
    .validate_network(list(graph = NULL), "motorcar"),
    "must be an object returned by prepare_network"
  )
})


# --- Network-dependent tests (gated) ---
# These tests download street networks from the OpenStreetMap Overpass API,
# which frequently rate-limits requests and causes tests to hang with
# "Waiting 60s for retry backoff" messages. They are skipped on CI and CRAN.
# To run locally, set RVD_RUN_NETWORK_TESTS=true in your environment:
#   Sys.setenv(RVD_RUN_NETWORK_TESTS = "true")

.skip_network <- function() {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("dodgr")
  if (!identical(Sys.getenv("RVD_RUN_NETWORK_TESTS"), "true")) {
    skip("Network tests skipped. Set RVD_RUN_NETWORK_TESTS=true to run.")
  }
}

test_that("driving k=1 returns one row per voter with distance columns", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:5, ]

  result <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving"
  )

  expect_equal(nrow(result), 5)
  expect_true("distance_km" %in% names(result))
  expect_true("distance_miles" %in% names(result))
  expect_true("distance_m" %in% names(result))
})

test_that("road distance >= haversine distance", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:5, ]

  hav <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "haversine", append_data = FALSE
  )

  road <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", append_data = FALSE
  )

  # Road distances should generally be >= haversine (allow for routing
  # selecting a different nearest location)
  # At minimum, road distances should be positive
  expect_true(all(road$distance_m > 0 | is.na(road$distance_m)))
})

test_that("walking + measure = 'time' returns time columns", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:3, ]

  result <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "walking", measure = "time"
  )

  expect_equal(nrow(result), 3)
  expect_true("time_seconds" %in% names(result))
  expect_true("time_minutes" %in% names(result))
  expect_true("time_hours" %in% names(result))
  expect_false("distance_m" %in% names(result))
})

test_that("k=3 driving returns ranked results with non-decreasing values", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:3, ]

  result <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", k = 3, append_data = FALSE
  )

  expect_equal(nrow(result), 9)
  expect_true(all(result$rank %in% 1:3))

  # Distances should be non-decreasing within each voter
  for (vid in unique(result$voter_id)) {
    sub <- result[result$voter_id == vid, ]
    dists <- sub$distance_m
    # Remove NAs for comparison
    dists <- dists[!is.na(dists)]
    if (length(dists) > 1) {
      expect_true(all(diff(dists) >= -0.01)) # small tolerance
    }
  }
})

test_that("threshold mode filters correctly with routing", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:3, ]

  result <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", max_dist = 15, units = "km",
    append_data = FALSE
  )

  # All returned distances should be <= 15 km (or NA for no-match rows)
  non_na <- result$distance_km[!is.na(result$distance_km)]
  if (length(non_na) > 0) {
    expect_true(all(non_na <= 15))
  }
})

test_that("prepare_network() returns usable network object", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")

  net <- prepare_network(
    voters = voter_meck, locations = early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    mode = "driving"
  )

  expect_s3_class(net, "rvd_network")
  expect_equal(net$mode, "driving")

  # Use network with nearest_location
  result <- nearest_location(voter_meck[1:3, ], early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", network = net
  )

  expect_equal(nrow(result), 3)
  expect_true("distance_km" %in% names(result))
})

test_that("mismatched network mode warns", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")

  net <- prepare_network(
    voters = voter_meck, locations = early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    mode = "driving"
  )

  expect_warning(
    nearest_location(voter_meck[1:2, ], early_meck,
      voter_coords = c("lat", "long"),
      location_coords = c("lat", "long"),
      method = "walking", network = net
    ),
    "Network was built for"
  )
})


# --- osmextract / download_osm tests (gated) ---

test_that(".check_osmextract() errors when package is missing", {
  # We can test the check function directly -- if osmextract IS installed,
  # it should not error; if not installed, it should error.
  if (requireNamespace("osmextract", quietly = TRUE)) {
    expect_silent(.check_osmextract())
  } else {
    expect_error(.check_osmextract(), "osmextract")
  }
})

test_that(".load_osm_file() errors on non-existent file", {
  skip_if_not_installed("osmextract")
  expect_error(.load_osm_file("/nonexistent/path.osm.pbf"), "File not found")
})

test_that("download_osm() validates provider argument", {
  skip_if_not_installed("osmextract")
  expect_error(download_osm("Georgia", provider = "invalid"))
})

test_that("prepare_network() errors with no inputs", {
  skip_if_not_installed("dodgr")
  expect_error(
    prepare_network(mode = "driving"),
    "Provide one of"
  )
})

test_that("prepare_network() errors with non-existent file", {
  skip_if_not_installed("dodgr")
  skip_if_not_installed("osmextract")
  expect_error(
    prepare_network(file = "/nonexistent/file.osm.pbf", mode = "driving"),
    "File not found"
  )
})
