library(dplyr)
library(readr)
library(tidyr)
library(purrr)

csv_path <- "./grid_output/strata_map_auto.csv"
out_dir  <- "./outputs/sampling_outputs"

# Path to CSV (from Python script)
main_csv <- file.path(out_dir, "main_to_count_filled.csv") 
main_filled <- read_csv(main_csv, show_col_types = FALSE) 

frame <- read_csv(csv_path, show_col_types = FALSE) %>%
  filter(include == 1, is.finite(stratum))

Nh_tbl <- frame %>%
  count(img_path, stratum, name = "Nh")

alpha <- 0.05
z <- qnorm(1 - alpha/2)  # 95% CI

# Total estimate for one image
estimate_stratified_one <- function(main_filled_one, Nh_tbl_one) {
  # main_filled_one: rows for ONE image: columns at least stratum, y
  # Nh_tbl_one     : data frame with columns stratum, Nh for the SAME image
  Hs <- sort(unique(Nh_tbl_one$stratum))
  
  comp <- lapply(Hs, function(h) {
    Nh <- Nh_tbl_one$Nh[Nh_tbl_one$stratum==h]
    yh <- main_filled_one$y[main_filled_one$stratum==h]
    nh <- length(yh)
    
    if (nh==0) return(list(Th=0, Vh=0, nh=0, ybar=NA, s2=NA))
    
    ybar <- mean(yh)
    s2   <- if (nh>1) var(yh) else 0
    Th   <- Nh * ybar
    Vh   <- (Nh^2) * (1 - nh/Nh) * s2 / max(nh,1)
    
    list(Th=Th, Vh=Vh, nh=nh, ybar=ybar, s2=s2)
  })
  T_hat <- sum(sapply(comp, `[[`, "Th"))
  Var_T <- sum(sapply(comp, `[[`, "Vh"))
  SE    <- sqrt(Var_T)
  tibble(T_hat=T_hat, SE=SE, CI_low=T_hat-z*SE, CI_high=T_hat+z*SE)
}

# Result per image
results_by_image <- main_filled %>%
  group_by(img_path) %>%
  group_modify(~{
    Nh_one <- Nh_tbl %>%
      filter(img_path == .y$img_path[[1]]) %>%
      select(stratum, Nh)
    estimate_stratified_one(.x, Nh_one)
  }) %>%
  ungroup()

# Total across disjoint images
T_total <- sum(results_by_image$T_hat)
Var_total <- sum(results_by_image$SE^2)
SE_total <- sqrt(Var_total)

total_res <- tibble(
  T_hat = T_total,
  SE    = SE_total,
  CI_low  = T_total - z*SE_total,
  CI_high = T_total + z*SE_total
)

print(results_by_image)
print(total_res)