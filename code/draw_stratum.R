# install.packages(c("imager","dplyr","readr","ggplot2"))
library(imager)
library(dplyr)
library(readr)
library(ggplot2)

# Background: load image as raster for ggplot
img_to_df <- function(img_path) {
  im <- load.image(img_path)          # cimg: x,y,cc,frame (x=col, y=row)
  as.data.frame(im, wide = "c") |>
    dplyr::rename(r = c.1, g = c.2, b = c.3) |>
    dplyr::mutate(rgb = rgb(r, g, b))
}

# Plot one image + overlay strata cells with colors
plot_strata_for_image <- function(csv_df, img_path,
                                  alpha_fill = 0.35,
                                  alpha_excl = 0.35) {
  sub <- csv_df |>
    dplyr::filter(img_path == !!img_path)
  
  if (nrow(sub) == 0) {
    stop("No rows in CSV for: ", img_path)
  }
  
  # Image dimensions (for x/y limits)
  im <- load.image(img_path)
  w <- dim(im)[1]; h <- dim(im)[2]
  im_df <- img_to_df(img_path)
  
  # Color palette per stratum
  pal <- c(`1` = "#E41A1C",   # red = densest
           `2` = "#FFCC00",   # yellow
           `3` = "#4DAF4A")   # green
  
  # Base layers
  p <- ggplot() +
    geom_raster(data = im_df, aes(x = x, y = y, fill = rgb)) +
    scale_fill_identity() +
    coord_fixed(expand = FALSE, xlim = c(1, w), ylim = c(1, h)) +
    theme_void()
  
  # include==0 (grey out excluded cells)
  excl <- sub |> dplyr::filter(include == 0L)
  if (nrow(excl) > 0) {
    p <- p + geom_rect(
      data = excl,
      aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
      fill = "grey30", color = NA, alpha = alpha_excl
    )
  }
  
  # include==1 (color by stratum)
  inc <- sub |> dplyr::filter(include == 1L)
  if (nrow(inc) > 0) {
    p <- p + geom_rect(
      data = inc,
      aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = factor(stratum)),
      color = NA, alpha = alpha_fill, inherit.aes = FALSE
    ) +
      scale_fill_manual(
        name = "Stratum",
        values = pal,
        drop = FALSE,
        na.value = "grey70"
      )
  }
  
  p
}

# Path to CSV (from Python script)
csv_path <- "grid_output/strata_map_auto.csv"
strata <- readr::read_csv(csv_path, show_col_types = FALSE)

# Check minimal set of required columns
need <- c("cell_id","zone_id","include","stratum","x0","x1","y0","y1","img_path")
stopifnot(all(need %in% names(strata)))

# Output folder for visualizations
out_dir <- "grid_output/strata_viz"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Generate PNG for each image
imgs <- unique(strata$img_path)
for (p in imgs) {
  message("Render: ", p)
  g <- plot_strata_for_image(strata, p, alpha_fill = 0.35, alpha_excl = 0.35)
  out_png <- file.path(out_dir, paste0(tools::file_path_sans_ext(basename(p)), "_strata.png"))
  # Automatic size: ~dpi 200, resolution based on image dimensions
  im <- load.image(p)
  w <- dim(im)[1]; h <- dim(im)[2]
  ggsave(out_png, plot = g,
         width = w/200, height = h/200, dpi = 200, limitsize = FALSE)
}

# Aggregate table per image: number of cells by stratum
summary_tbl <- strata |>
  dplyr::filter(include == 1L) |>
  dplyr::count(img_path, stratum, name = "n_cells") |>
  dplyr::arrange(img_path, stratum)

print(summary_tbl)
