# install.packages(c("imager","dplyr","readr","stringr","ggplot2"))
library(imager)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# Get image dimensions (w = width, h = height)
img_dims <- function(img_path) {
  im <- load.image(img_path)    # cimg: x,y,cc,frame
  list(w = dim(im)[1], h = dim(im)[2])
}

# Create a grid of cells for a single image
# nx, ny = number of cells horizontally and vertically
# zone_id = optional zone label for this image
make_grid_for_image <- function(img_path, nx=40, ny=40, zone_id=1,
                                cell_prefix=NULL) {
  dims <- img_dims(img_path)
  w <- dims$w; h <- dims$h
  # cell boundaries in pixels
  bx <- floor(seq(0, w, length.out = nx+1))
  by <- floor(seq(0, h, length.out = ny+1))
  
  # default cell ID prefix = file name without extension
  if (is.null(cell_prefix)) {
    cell_prefix <- tools::file_path_sans_ext(basename(img_path))
  }
  
  # assemble a data frame of all cells
  cells <- lapply(seq_len(ny), function(iy) {
    lapply(seq_len(nx), function(ix) {
      x0 <- bx[ix]   + 1
      x1 <- bx[ix+1]
      y0 <- by[iy]   + 1
      y1 <- by[iy+1]
      data.frame(
        img_path = img_path,
        zone_id  = zone_id,
        row      = iy,
        col      = ix,
        cell_id  = sprintf("%s_r%02d_c%02d", cell_prefix, iy, ix),
        x0 = x0, x1 = x1, y0 = y0, y1 = y1,
        include = 1L,           # podrazumevano uključi; ručno možeš staviti 0
        stratum = NA_integer_   # to be filled manually
      )
    }) |> bind_rows()
  }) |> bind_rows()
  
  cells
}

# Draw the grid overlay on top of the image
plot_grid_overlay <- function(img_path, grid_df, save_to = NULL) {
  im <- load.image(img_path)
  dims <- list(w = dim(im)[1], h = dim(im)[2])
  
  # convert imager object to data frame for ggplot raster plotting
  im_df <- as.data.frame(im, wide = "c") |>
    rename(r = c.1, g = c.2, b = c.3)
  
  p <- ggplot() +
    geom_raster(data = im_df, aes(x = x, y = y, fill = rgb(r,g,b))) +
    scale_fill_identity() +
    coord_fixed(expand = FALSE, xlim = c(1, dims$w), ylim = c(1, dims$h)) +
    theme_void()
  
  # draw the grid lines
  p <- p + geom_rect(
    data = grid_df,
    aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
    fill = NA, color = "red", linewidth = 0.05
  )
  
  if (!is.null(save_to)) {
    ggsave(save_to, plot = p, width = dims$w/200, height = dims$h/200, dpi = 200, limitsize = FALSE)
  }
  invisible(p)
}

# Generate a grid for each image, save PNG overlays  and 
#create a combined strata_map.csv with all cell information
make_grids_and_exports <- function(image_paths, nx=40, ny=40, zone_id=1,
                                   out_dir = "grid_output") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  all_cells <- lapply(image_paths, function(pth) {
    df <- make_grid_for_image(pth, nx=nx, ny=ny, zone_id=zone_id)
    # save overlay PNG with grid
    overlay_png <- file.path(
      out_dir,
      paste0(tools::file_path_sans_ext(basename(pth)), "_grid.png")
    )
    plot_grid_overlay(pth, df, save_to = overlay_png)
    df
  }) |> bind_rows()
  
  # write a master CSV skeleton for later manual stratification
  strata_map <- all_cells |>
    transmute(
      cell_id,
      zone_id,
      stratum = stratum,     # TODO:napisati sta je koji startum
      include = include,     # 1 = include, set to 0 to exclude cells
      x0, x1, y0, y1,        #  pixel coordinates (useful for automation later)
      img_path
    )
  
  out_csv <- file.path(out_dir, "strata_map.csv")
  write_csv(strata_map, out_csv)
  message("Saved: ", out_csv)
  invisible(list(strata_map = strata_map, out_csv = out_csv, out_dir = out_dir))
}

imgs <- c("./images/slavija-centar.jpeg", "./images/prote-mateje.jpeg",
          './images/nemanjina.jpeg', './images/makenzijeva.jpeg',
          './images/kralja-milana.jpeg', './images/bulevar-oslobodjenja.jpeg',
          './images/beogradska.jpeg')  # stavi tvoje putanje

# Generate a 40x40 grid for each image, PNG overlays, and one strata_map.csv
out <- make_grids_and_exports(
  image_paths = imgs,
  nx = 40, ny = 40,
  zone_id = 1,            
  out_dir = "grid_output",
)
