simulate_life_process <- function(
    centers, # Matrix of points representing centers of mass
    raster_grid, # Raster grid
    period, # Start and end of the life process as a Date vector
    time_steps = seq.Date(period[[1]], period[[2]], by = "month"), # Vector of time steps (e.g., seq.Date())
    n_samples = 200, # Number of samples to draw
    bandwidth = 50, # Bandwidth for raster smoothing (kernel size)
    prob_threshold = 0.5 # Probability threshold to create polygons
    ) {
  # Validate inputs
  if (!inherits(raster_grid, "SpatRaster")) stop("raster_grid must be a SpatRaster")
  if (!is.matrix(centers)) stop("centers must be a matrix of coordinates")

  # Create sf points for centers of mass
  centers_sf <- st_as_sf(as.data.frame(centers), coords = c("V1", "V2"), crs = st_crs(raster_grid))

  # Create a list to hold outputs
  results <- list(
    pts = list(),
    ras = list()
  )

  # Create smoothing kernel
  kernel <- focalMat(raster_grid, d = bandwidth, type = "circle")

  # Loop through time steps
  for (time in seq_len(length(time_steps))) {
    # Simulate samples around centers of mass
    pts <- do.call(rbind, lapply(1:nrow(centers), function(i) {
      # Generate random offsets for sampling around each center
      offsets <- data.frame(
        x = rnorm(n_samples, mean = st_coordinates(centers_sf[i, ])[1], sd = bandwidth),
        y = rnorm(n_samples, mean = st_coordinates(centers_sf[i, ])[2], sd = bandwidth)
      )
      st_as_sf(offsets, coords = c("x", "y"), crs = st_crs(raster_grid))
    }))

    # Rasterize points into the grid to create a density surface
    ras <- pts |>
      dplyr::mutate(weight = 1) |>
      vect() |>
      rasterize(raster_grid, field = "weight", fun = sum, background = 0) |>
      focal(w = kernel, fun = mean, na.rm = TRUE) # Smooth the raster using focal operation

    # Save outputs for the current time step
    results$pts[[as.character(time_steps[time])]] <- pts
    results$ras[[as.character(time_steps[time])]] <- ras
  }

  # Combine rasters into a stack
  raster_stack <- rast(results$ras)

  # Probabilities
  raster_stack <- raster_stack / max(global(raster_stack, max, na.rm = TRUE)$max)

  # Crop
  raster_stack <- raster_stack * raster_grid$aoi

  # Add to results
  results$ras <- raster_stack

  # Transform raster to data.frame
  results$ras_df <- raster_stack |>
    as.data.frame(xy = TRUE) |>
    tidyr::pivot_longer(-c(x, y), names_to = "date", values_to = "presence")

  return(results)
}
