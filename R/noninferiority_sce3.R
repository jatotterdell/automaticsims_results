library(parallel)
library(automaticsims)
library(tidyverse)

st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, rep(1, 13), 0.1, 
      kappa_hi_0 = 0.9, kappa_hi_1 = 0.9,
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce3_null.rds")


st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, rep(1, 11)), 0.1, 
      kappa_hi_0 = 0.9, kappa_hi_1 = 0.9, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce3_one_sup.rds")


st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, 1.5, 1.5, rep(1, 9)), 0.1, 
      kappa_hi_0 = 0.9, kappa_hi_1 = 0.9, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce3_three_sup.rds")


st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, rep(1.5, 3), rep(1.25, 3), rep(1, 6)), 0.1, 
      kappa_hi_0 = 0.9, kappa_hi_1 = 0.9, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce3_two_msg_benefit.rds")


st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, seq(1,1.5, length.out = 13), 0.1, kappa_hi_0 = 0.9, kappa_hi_1 = 0.9, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce3_increase_seq.rds")


st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1.25, rep(1, 12)), 0.1, kappa_hi_0 = 0.9, kappa_hi_1 = 0.9,
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
rsaveRDS(resall, "out/noninferior/sce3_ctrl_best.rds")

