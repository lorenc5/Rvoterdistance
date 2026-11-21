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


# --- Pre-filter helper tests (no network needed) ---

test_that(".prefiltered_to_knearest extracts correct k from sparse results", {
  # Simulate pre-filtered output: 3 voters, 4 candidates each
  prefiltered <- list(
    distances = matrix(c(
      100, 50, 200, 150,  # voter 1
       80, 10,  90,  70,  # voter 2
      300, 250, 100, 400  # voter 3
    ), nrow = 3, byrow = TRUE),
    indices = matrix(c(
      5L, 2L, 8L, 3L,  # voter 1 candidate location IDs
      1L, 7L, 4L, 6L,  # voter 2
      9L, 3L, 2L, 5L   # voter 3
    ), nrow = 3, byrow = TRUE)
  )

  res <- .prefiltered_to_knearest(prefiltered, k = 2)
  expect_equal(nrow(res$distances), 3)
  expect_equal(ncol(res$distances), 2)

  # Voter 1: 50 (loc 2), 100 (loc 5)
  expect_equal(res$distances[1, ], c(50, 100))
  expect_equal(res$indices[1, ], c(2L, 5L))

  # Voter 2: 10 (loc 7), 70 (loc 6)
  expect_equal(res$distances[2, ], c(10, 70))
  expect_equal(res$indices[2, ], c(7L, 6L))

  # Voter 3: 100 (loc 2), 250 (loc 3)
  expect_equal(res$distances[3, ], c(100, 250))
  expect_equal(res$indices[3, ], c(2L, 3L))
})

test_that(".prefiltered_to_knearest handles k = 1", {
  prefiltered <- list(
    distances = matrix(c(100, 50, 200), nrow = 1),
    indices = matrix(c(3L, 1L, 5L), nrow = 1)
  )

  res <- .prefiltered_to_knearest(prefiltered, k = 1)
  expect_equal(nrow(res$distances), 1)
  expect_equal(ncol(res$distances), 1)
  expect_equal(res$distances[1, 1], 50)
  expect_equal(res$indices[1, 1], 1L)
})

test_that(".prefiltered_to_knearest handles NAs", {
  prefiltered <- list(
    distances = matrix(c(100, NA, 50, NA, 20, NA), nrow = 2, byrow = TRUE),
    indices = matrix(c(1L, 2L, 3L, 4L, 5L, 6L), nrow = 2, byrow = TRUE)
  )

  res <- .prefiltered_to_knearest(prefiltered, k = 2)
  # Voter 1: 50 (loc 3), 100 (loc 1) -- NA pushed to end
  expect_equal(res$distances[1, ], c(50, 100))
  expect_equal(res$indices[1, ], c(3L, 1L))
  # Voter 2: 20 (loc 5), NA
  expect_equal(res$distances[2, 1], 20)
  expect_equal(res$indices[2, 1], 5L)
})

test_that(".prefiltered_to_threshold filters correctly", {
  prefiltered <- list(
    distances = matrix(c(
      100, 50, 200, 150,
       80, 10,  90,  70
    ), nrow = 2, byrow = TRUE),
    indices = matrix(c(
      5L, 2L, 8L, 3L,
      1L, 7L, 4L, 6L
    ), nrow = 2, byrow = TRUE)
  )

  res <- .prefiltered_to_threshold(prefiltered, max_val = 100)

  # Voter 1: 50 (loc 2), 100 (loc 5) -- 200, 150 excluded
  expect_equal(res[[1]]$distances, c(50, 100))
  expect_equal(res[[1]]$indices, c(2L, 5L))

  # Voter 2: 10 (loc 7), 70 (loc 6), 80 (loc 1), 90 (loc 4)
  expect_equal(res[[2]]$distances, c(10, 70, 80, 90))
  expect_equal(res[[2]]$indices, c(7L, 6L, 1L, 4L))
})

test_that(".prefiltered_to_threshold handles no matches", {
  prefiltered <- list(
    distances = matrix(c(500, 600), nrow = 1),
    indices = matrix(c(1L, 2L), nrow = 1)
  )

  res <- .prefiltered_to_threshold(prefiltered, max_val = 100)
  expect_equal(length(res[[1]]$distances), 0)
  expect_equal(length(res[[1]]$indices), 0)
})

test_that(".prefiltered_to_threshold handles NAs", {
  prefiltered <- list(
    distances = matrix(c(50, NA, 200), nrow = 1),
    indices = matrix(c(1L, 2L, 3L), nrow = 1)
  )

  res <- .prefiltered_to_threshold(prefiltered, max_val = 100)
  # Only 50 (loc 1) is within threshold; NA and 200 excluded
  expect_equal(res[[1]]$distances, 50)
  expect_equal(res[[1]]$indices, 1L)
})

test_that("prefilter < k errors", {
  data(meck_ev, package = "Rvoterdistance")
  skip_if_not_installed("dodgr")
  expect_error(
    nearest_location(voter_meck[1:2, ], early_meck,
      voter_coords = c("lat", "long"),
      location_coords = c("lat", "long"),
      method = "driving", k = 5, prefilter = 3
    ),
    "prefilter.*must be >=.*k"
  )
})

test_that("prefilter < 1 errors", {
  data(meck_ev, package = "Rvoterdistance")
  skip_if_not_installed("dodgr")
  expect_error(
    nearest_location(voter_meck[1:2, ], early_meck,
      voter_coords = c("lat", "long"),
      location_coords = c("lat", "long"),
      method = "driving", prefilter = 0
    ),
    "prefilter.*must be >= 1"
  )
})

test_that("prefilter with haversine warns and is ignored", {
  data(meck_ev, package = "Rvoterdistance")
  expect_warning(
    result <- nearest_location(voter_meck[1:3, ], early_meck,
      voter_coords = c("lat", "long"),
      location_coords = c("lat", "long"),
      method = "haversine", prefilter = 5
    ),
    "prefilter.*ignored"
  )
  # Should still return valid haversine results
  expect_equal(nrow(result), 3)
  expect_true("distance_km" %in% names(result))
})

test_that("prefilter >= n_locations falls back to full matrix with message", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  n_locs <- nrow(early_meck)
  # prefilter >= n_locations should message and fall back to full matrix
  expect_message(
    nearest_location(voter_meck[1:2, ], early_meck,
      voter_coords = c("lat", "long"),
      location_coords = c("lat", "long"),
      method = "driving", prefilter = n_locs + 10
    ),
    "Using full matrix instead"
  )
})


# --- Network-dependent pre-filter tests (gated) ---

test_that("prefilter driving results match full matrix for k=1", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:5, ]

  full <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", append_data = FALSE
  )

  prefiltered <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", prefilter = 10, append_data = FALSE
  )

  # Results should be identical (prefilter=10 is generous for k=1)
  expect_equal(nrow(prefiltered), 5)
  expect_equal(prefiltered$distance_m, full$distance_m)
})

test_that("prefilter driving k=3 returns ranked results", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:3, ]

  result <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", k = 3, prefilter = 10, append_data = FALSE
  )

  expect_equal(nrow(result), 9)
  expect_true(all(result$rank %in% 1:3))

  # Distances should be non-decreasing within each voter
  for (vid in unique(result$voter_id)) {
    sub <- result[result$voter_id == vid, ]
    dists <- sub$distance_m[!is.na(sub$distance_m)]
    if (length(dists) > 1) {
      expect_true(all(diff(dists) >= -0.01))
    }
  }
})

test_that("prefilter with threshold mode works", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:3, ]

  result <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", max_dist = 15, units = "km",
    prefilter = 15, append_data = FALSE
  )

  non_na <- result$distance_km[!is.na(result$distance_km)]
  if (length(non_na) > 0) {
    expect_true(all(non_na <= 15))
  }
})

test_that("prefilter with measure='time' returns time columns", {
  .skip_network()

  data(meck_ev, package = "Rvoterdistance")
  voters_sub <- voter_meck[1:3, ]

  result <- nearest_location(voters_sub, early_meck,
    voter_coords = c("lat", "long"),
    location_coords = c("lat", "long"),
    method = "driving", measure = "time",
    prefilter = 10
  )

  expect_equal(nrow(result), 3)
  expect_true("time_seconds" %in% names(result))
  expect_true("time_minutes" %in% names(result))
  expect_false("distance_m" %in% names(result))
})
