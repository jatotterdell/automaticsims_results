# As Scenario 1, but set delta 0.1
#-----------------------------------

library(parallel)
library(automaticsims)
library(tidyverse)

delta <- 0.2
out <- paste0("out/firstpastpost/")


res0 <- lapply(
  1:10000, 
  function(j) run_a_firstpastpost_trial(
    j, rep(1, 13), delta, 
    kappa_lo_0 = 0.05, kappa_lo_1 = 0.05,
    kappa_hi_0 = 0.995, kappa_hi_1 = 0.99,
    return_all = F, allocate_inactive = F, brar = T))

resall0 <- as_tibble(do.call(rbind, map(res0, simplify)))
saveRDS(resall0, paste0(out, "sce2_null.rds"))

res1 <- lapply(
  1:10000, 
  function(j) run_a_firstpastpost_trial(
    j, c(1, 1.5, rep(1,11)), delta, 
    kappa_lo_0 = 0.05, kappa_lo_1 = 0.05,
    kappa_hi_0 = 0.995, kappa_hi_1 = 0.99,
    return_all = F, allocate_inactive = F, brar = T))

resall1 <- as_tibble(do.call(rbind, map(res1, simplify)))
saveRDS(resall0, paste0(out, "sce2_one.rds"))

res3 <- lapply(
  1:10000, 
  function(j) run_a_firstpastpost_trial(
    j, c(1, 1.5, 1.5, 1.5, rep(1,9)), delta, 
    kappa_lo_0 = 0.05, kappa_lo_1 = 0.05,
    kappa_hi_0 = 0.995, kappa_hi_1 = 0.99,
    return_all = F, allocate_inactive = F, brar = T))

resall3 <- as_tibble(do.call(rbind, map(res3, simplify)))
saveRDS(resall0, paste0(out, "sce2_three.rds"))
