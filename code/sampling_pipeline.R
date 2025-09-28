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

# todo: popuniti y u pilot_to_count.csv

pilot_filled <- read_csv(pilot_csv, show_col_types = FALSE)

sh_tbl <- pilot_filled %>%
  filter(is.finite(y)) %>%
  group_by(img_path, stratum) %>%
  summarise(sh = sd(y), .groups="drop") %>%
  right_join(Nh_tbl, by=c("img_path","stratum")) %>%
  mutate(sh = ifelse(is.na(sh), 0, sh))

# Neyman

neyman_alloc_vec <- function(Nh, sh, n_total, n_min=10) {
  Nh <- as.numeric(Nh); sh <- as.numeric(sh)
  w <- Nh * sh
  if (sum(w) > 0) nh <- round(n_total * w / sum(w)) else nh <- round(n_total * Nh / sum(Nh))
  nh <- pmax(nh, pmin(n_min, Nh))
  nh <- pmin(nh, Nh)
  nh
}

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


# todo: find what causes error
main_df <- frame %>%
  inner_join(alloc_tbl, by = c("img_path","stratum")) %>%
  group_by(img_path, stratum) %>%
  slice_sample(n = pmin(first(nh), n())) %>%              
  ungroup() %>%
  select(img_path, stratum, cell_id) %>%
  mutate(y = NA_integer_)

main_csv <- file.path(out_dir, "main_to_count.csv")
write_csv(main_df, main_csv)
