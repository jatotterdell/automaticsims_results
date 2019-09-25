# As Scenario 1, but set delta = 0.2 
# instead of delta = 0.1
#-----------------------------------

library(parallel)
library(automaticsims)
library(tidyverse)


res <- lapply(
  1:1000, 
  function(j) run_a_noninf_trial(
    j, rep(1, 13), 0.2, 
    return_all = F, allocate_inactive = F, brar = T))

resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce2_null.rds")

st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, rep(1, 11)), 0.2, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce2_one_sup.rds")

st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, 1.5, 1.5, rep(1, 9)), 0.2, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce2_three_sup.rds")


st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, c(1, rep(1.5, 3), rep(1.25, 3), rep(1, 6)), 0.2, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce2_two_msg_benefit.rds")


st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, seq(1,1.5, length.out = 13), 0.2, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce2_increase_seq.rds")

st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, c(1.25, rep(1, 12)), 0.2, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce2_ctrl_best.rds")
