# PILOT + NEYMAN

library(dplyr)
library(readr)
library(tidyr)
library(purrr)

csv_path <- "./grid_output/strata_map_auto.csv"
out_dir  <- "./outputs/sampling_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(42)
pilot_n_per_stratum <- 15 
target_n_per_image  <- 180
n_min_per_stratum   <- 10

frame <- read_csv(csv_path, show_col_types = FALSE) %>%
  filter(include == 1, is.finite(stratum))

# N_h by picture and by stratum
Nh_tbl <- frame %>% count(img_path, stratum, name = "Nh")

pilot_df <- frame %>%
  group_by(img_path, stratum) %>%
  slice_sample(n = pilot_n_per_stratum) %>%
  ungroup() %>%
  select(img_path, stratum, cell_id) %>%
  mutate(y = NA_integer_) 

pilot_csv <- file.path(out_dir, "pilot_to_count.csv")
write_csv(pilot_df, pilot_csv)

# Pilot sampling cells visualization
#----------------------------- 
library(dplyr)
library(ggplot2)
library(magick)
library(stringr)

# helper: flip y-intervals for a given image height H
flip_y <- function(df, H) {
  df %>%
    mutate(
      y0_f = H - y1,   # note the swap!
      y1_f = H - y0
    )
}
plot_pilot_overlay_flipped_bitmap <- function(
    im_path,
    frame,
    pilot_with_coords,
    save      = FALSE,
    out_dir   = ".",
    subfolder = "plot_overlays_image",
    prefix    = "pilot_overlay_",
    dpi       = 200
) {
  df_all   <- frame %>% dplyr::filter(img_path == !!im_path)
  df_pilot <- pilot_with_coords %>% dplyr::filter(img_path == !!im_path)
  
  # flip image over x-axis
  img  <- magick::image_read(im_path)
  img  <- magick::image_flip(img)
  info <- magick::image_info(img)
  W <- info$width; H <- info$height
  rast <- as.raster(img)
  
  g <- ggplot() +
    # flipped bitmap
    annotation_raster(rast, xmin = 0, xmax = W, ymin = 0, ymax = H, interpolate = TRUE) +
    # grid (original coords)
    geom_rect(data = df_all,
              aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
              fill = NA, color = scales::alpha("white", 0.5), linewidth = 0.15) +
    # pilot cells (original coords)
    geom_rect(data = df_pilot,
              aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
              fill = NA, color = "green", linewidth = 0.8) +
    # keep your inverted Y (since coords are still top-down)
    coord_fixed(xlim = c(0, W), ylim = c(H, 0), expand = FALSE) +
    theme_void() +
    ggtitle(paste("Pilot overlay (flipped bitmap) —", basename(im_path)))
  
  if (isTRUE(save)) {
    dir.create(file.path(out_dir, subfolder), recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(
      out_dir, subfolder,
      paste0(prefix, tools::file_path_sans_ext(basename(im_path)), ".png")
    )
    ggplot2::ggsave(out_file, g, width = W/dpi, height = H/dpi, dpi = dpi, limitsize = FALSE)
    message("Saved overlay: ", out_file)
  }
  
  return(g)
}


# Save overlay PNG for every image
for (im in unique(frame$img_path)) {
  plot_pilot_overlay_flipped_bitmap(
    im,
    frame,
    pilot_with_coords,
    save      = TRUE,
    out_dir   = out_dir,              
    subfolder = "plot_overlays_image",
    prefix    = "pilot_overlay_",
    dpi       = 200
  )
}

#-----------------------------
# Path to CSV (from Python script)
pilot_csv <- file.path(out_dir, "pilot_to_count_filled.csv")
pilot_filled <- read_csv(pilot_csv, show_col_types = FALSE)

sh_tbl <- pilot_filled %>%
  filter(is.finite(y)) %>%
  group_by(img_path, stratum) %>%
  summarise(sh = sd(y), .groups="drop") %>%
  right_join(Nh_tbl, by=c("img_path","stratum")) %>%
  mutate(sh = ifelse(is.na(sh), 0, sh))

# -------------- NEYMAN ALLOCATION --------------------

# Neyman allocation vector for one image
neyman_alloc_vec <- function(Nh, sh, n_total, n_min=10) {
  Nh <- as.numeric(Nh); sh <- as.numeric(sh)
  w <- Nh * sh
  if (sum(w) > 0) nh <- round(n_total * w / sum(w)) else nh <- round(n_total * Nh / sum(Nh))
  nh <- pmax(nh, pmin(n_min, Nh))
  nh <- pmin(nh, Nh)
  nh
}

# Neyman allocation (per image)
alloc_tbl <- sh_tbl %>%
  group_by(img_path) %>%
  summarise(
    nh = list(neyman_alloc_vec(Nh, sh, n_total = target_n_per_image,
                               n_min = n_min_per_stratum)),
    stratum = list(stratum),
    .groups="drop"
  ) %>%
  mutate(alloc = map2(stratum, nh, ~tibble(stratum=.x, nh=.y))) %>%
  select(img_path, alloc) %>%
  unnest(alloc)

# ---- Report: per image & stratum
alloc_per_stratum <- alloc_tbl %>%
  left_join(Nh_tbl, by = c("img_path","stratum")) %>%   
  arrange(img_path, stratum)

cat("\n=== Neyman allocation: per image & stratum ===\n")
print(alloc_per_stratum)
# columns: img_path, stratum, nh (allocated), Nh (frame size in stratum)

# ---- Report: totals vs. target
alloc_totals <- alloc_per_stratum %>%
  group_by(img_path) %>%
  summarise(
    total_nh = sum(nh, na.rm = TRUE),
    total_Nh = sum(Nh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(target = target_n_per_image) %>%
  select(img_path, total_nh, target, total_Nh) %>%
  arrange(img_path)

cat("\n=== Totals vs. target ===\n")
print(alloc_totals)

# Fix: nudge stratum 1 where sum(nh) != targe
# If diff > 0: add +1 to stratum 1 (if nh < Nh)
# If diff < 0: subtract -1 from stratum 1 (respecting min_allowed = min(n_min_per_stratum, Nh))

# Add Nh and compute per-image diff from target
alloc_aug <- alloc_tbl %>%
  left_join(Nh_tbl, by = c("img_path","stratum"))

diff_tbl <- alloc_aug %>%
  group_by(img_path) %>%
  summarise(diff = target_n_per_image - sum(nh), .groups = "drop")

# Apply correction only to stratum 1
alloc_fixed <- alloc_aug %>%
  left_join(diff_tbl, by = "img_path") %>%
  group_by(img_path, stratum) %>%
  mutate(min_allowed = pmin(n_min_per_stratum, Nh)) %>%
  ungroup() %>%
  group_by(img_path) %>%
  mutate(
    nh = case_when(
      # dodaj u stratumu 1
      diff > 0 & stratum == 1 & nh < Nh ~ nh + pmin(diff, Nh - nh),
      # oduzmi u stratumu 1
      diff < 0 & stratum == 1 & nh > min_allowed ~ nh - pmin(-diff, nh - min_allowed),
      TRUE ~ nh
    )
  ) %>%
  ungroup() %>%
  select(img_path, stratum, nh)

# ---- Report after fix
alloc_totals_fixed <- alloc_fixed %>%
  group_by(img_path) %>%
  summarise(total_nh = sum(nh), .groups = "drop") %>%
  mutate(target = target_n_per_image)

cat("\n=== After fix (totals) ===\n")
print(alloc_totals_fixed)



# -------- MAIN SAMPLING --------
main_df <- frame %>%
  inner_join(alloc_fixed, by = c("img_path","stratum")) %>%
  group_by(img_path, stratum) %>%
  group_modify(~ {
    nh_g <- unique(.x$nh)[1]
    dplyr::slice_sample(.x, n = pmin(nh_g, nrow(.x)))
  }) %>%        
  ungroup() %>%
  select(img_path, stratum, cell_id) %>%
  mutate(y = NA_integer_)

main_csv <- file.path(out_dir, "main_to_count.csv")
write_csv(main_df, main_csv)

# Main sampling cells visualization
#-------------------------------------------
main_with_coords <- main_df %>%
  dplyr::left_join(
    frame %>% dplyr::select(cell_id, img_path, x0, x1, y0, y1),
    by = c("cell_id", "img_path")
  )

# Render & save MAIN overlays
for (im in unique(main_with_coords$img_path)) {
  plot_pilot_overlay_flipped_bitmap(
    im,
    frame,
    main_with_coords,
    save      = TRUE,
    out_dir   = out_dir,
    subfolder = "main_overlays_image",
    prefix    = "main_overlay_",
    dpi       = 200
  )
}
#---------------------------
#Autofill with Python script
