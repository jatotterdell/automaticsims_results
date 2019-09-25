library(parallel)
library(automaticsims)
library(tidyverse)

st <- system.time(
  res0 <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, rep(1, 13), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
  )

resall0 <- as_tibble(do.call(rbind, map(res0, simplify)))
saveRDS(resall0, "out/noninferior/sce1_null.rds")

st <- system.time(
  res1 <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, rep(1, 11)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall1 <- as_tibble(do.call(rbind, map(res1, simplify)))
saveRDS(resall1, "out/noninferior/sce1_one_sup.rds")

st <- system.time(
  res3 <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, 1.5, 1.5, 1.5, rep(1, 9)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall3 <- as_tibble(do.call(rbind, map(res3, simplify)))
saveRDS(resall3, "out/noninferior/sce1_three_sup.rds")


st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, c(1, rep(1.5, 3), rep(1.25, 3), rep(1, 6)), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce1_two_msg_benefit.rds")

st <- system.time(
  res <- lapply(
    1:10000, 
    function(j) run_a_noninf_trial(
      j, seq(1,1.5, length.out = 13), 0.1, 
      return_all = F, allocate_inactive = F, brar = T))
)
resall <- as_tibble(do.call(rbind, map(res, simplify)))
saveRDS(resall, "out/noninferior/sce1_increase_seq.rds")


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
