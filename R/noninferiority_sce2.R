# As Scenario 1, but set delta = 0.2 
# instead of delta = 0.1
#-----------------------------------

library(parallel)
library(automaticsims)
library(tidyverse)


res0 <- lapply(
  1:10000, 
  function(j) run_a_noninf_trial(
    j, rep(1, 13), 0.2, 
    return_all = F, allocate_inactive = F, brar = T))

resall0 <- as_tibble(do.call(rbind, map(res0, simplify)))
saveRDS(resall0, "out/noninferior/sce2_null.rds")

st <- system.time(
  res1 <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, rep(1, 11)), 0.2, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall1 <- as_tibble(do.call(rbind, map(res1, simplify)))
saveRDS(resall1, "out/noninferior/sce2_one_sup.rds")

st <- system.time(
  res3 <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, 1.5, 1.5, rep(1, 9)), 0.2, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall3 <- as_tibble(do.call(rbind, map(res3, simplify)))
saveRDS(resall3, "out/noninferior/sce2_three_sup.rds")


st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, rep(1.5, 3), rep(1.25, 3), rep(1, 6)), 0.2, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce2_two_msg_benefit.rds")
