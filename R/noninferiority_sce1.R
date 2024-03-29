library(parallel)
library(automaticsims)
library(tidyverse)

cl <- makeCluster(4)
clusterEvalQ(cl, library(automaticsims))
st <- system.time(res <- parLapply(cl, 1:100,     
  function(j) run_a_noninf_trial(
    j, rep(1, 13), 0.1, 
    kappa_lo_0 = 0.01, kappa_lo_1 = 0.01,
    kappa_hi_0 = 0.8, kappa_hi_1 = 0.8,
    return_all = F, allocate_inactive = F, brar = T)))
stopCluster(cl)
resall <- as_tibble(do.call(rbind, map(res, simplify)))

st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, rep(1, 13), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
  )

resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce1_null.rds")

st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, rep(1, 11)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce1_one_sup.rds")

st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, 1.5, 1.5, rep(1, 9)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce1_three_sup.rds")


st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, c(1, rep(1.5, 3), rep(1.25, 3), rep(1, 6)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce1_two_msg_benefit.rds")

st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, seq(1,1.5, length.out = 13), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce1_increase_seq.rds")

st <- system.time(
  res <- lapply(
    1:1000, 
    function(j) run_a_noninf_trial(
      j, c(1.25, rep(1, 12)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce1_ctrl_best.rds")


# 
# 
# resall0 %>% select("interim", "superior", "noninferior", starts_with("n.")) %>% summarise_all(mean)
# resall0 %>% 
#   select(starts_with("p_sup")) %>% 
#   summarise_all(function(x) mean(x > 0.9)) %>% 
#   matrix(13,13,byrow=T)
# resall0 %>% 
#   filter(superior == 1) %>%
#   select(starts_with("p_sup")) %>% 
#   summarise_all(function(x) mean(x > 0.9)) %>% 
#   matrix(13,13,byrow=T)
# 
# resall0 %>% 
#   filter(noninferior == 1) %>%
#   select(starts_with("p_sup")) %>% 
#   summarise_all(function(x) mean(x > 0.9)) %>% 
#   matrix(13,13,byrow=T)
# # When stopping for non-inferiority, only 1-3 is ever still active
# resall0 %>% 
#   filter(noninferior == 1) %>%
#   select(starts_with("active")) %>% 
#   summarise_all(mean)
# resall0 %>% 
#   filter(noninferior == 1) %>%
#   select(starts_with("active")) %>% 
#   mutate(`Total noninferior` = rowSums(.)) %>%
#   count(`Total noninferior`) %>%
#   mutate(p = n / sum(n))
