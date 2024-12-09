# Function to make raster from data table
make_raster <- function(grd, tab, value_col) {
  tab <- dplyr::arrange(tab, by = "cell_id")
  grd$newcol <- NA
  grd$newcol[tab[["cell_id"]]] <- tab[[value_col]]
  grd <- grd$newcol
  names(grd) <- value_col
  return(grd)
}
