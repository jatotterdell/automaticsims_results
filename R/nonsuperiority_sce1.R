# As Scenario 1, but set delta = 0.2 
# instead of delta = 0.1
#-----------------------------------

library(parallel)
library(automaticsims)
library(tidyverse)

out <- paste0("out/nonsuperiority/")


res0 <- lapply(
  1:10000, 
  function(j) run_a_nonsup_trial(
    j, rep(1, 13), 0.1, 
    return_all = F, allocate_inactive = F, brar = T))

resall0 <- as_tibble(do.call(rbind, map(res0, simplify)))
saveRDS(resall0, paste0(out, "sce1_null.rds"))

res1 <- lapply(
  1:10000, 
  function(j) run_a_nonsup_trial(
    j, c(1, 1.5, rep(1, 11)), 0.1, 
    return_all = F, allocate_inactive = F, brar = T))

resall1 <- as_tibble(do.call(rbind, map(res1, simplify)))
saveRDS(resall1, paste0(out, "sce1_one.rds"))

st <- system.time(
  res3 <- lapply(
    1:10000, 
    function(j) run_a_nonsup_trial(
      j, c(1, 1.5, 1.5, 1.5, rep(1, 9)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall3 <- as_tibble(do.call(rbind, map(res3, simplify)))
saveRDS(resall3, paste0(out, "sce1_three_sup.rds"))


st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_nonsup_trial(
      j, c(1, rep(1.5, 3), rep(1.25, 3), rep(1, 6)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, paste0(out, "sce1_two_msg_benefit.rds"))


####


resall3 %>% select("interim", "superior", "nonsuperior", starts_with("n.")) %>% summarise_all(mean)
resall3 %>% 
  select(starts_with("p_sup.")) %>% 
  summarise_all(function(x) mean(x > 0.8))
resall3 %>% 
  select(starts_with("p_sup_pair")) %>% 
  summarise_all(mean) %>% 
  matrix(13,13,byrow=T)
