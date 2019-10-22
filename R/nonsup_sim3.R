library(parallel)
library(automaticsims)
library(tidyverse)

pars <- expand.grid(
  delta_sup = 0.1,
  delta_noninf = 0.1,
  kappa_act = 0.01,
  kappa_sup_0 = c(0.75, 0.85),
  kappa_sup_1 = 0.75,
  kappa_ctr = 0.95,
  kappa_nonsup = c(0.05, 0.1),
  kappa_noninf = c(0.6, 1),
  kappa_nonsup = 0.05,
  alloc_ctrl = c(1/13, 1/5)
)
pars$pars <- factor(as.character(1:nrow(pars)), levels = 1:nrow(pars))


for(a in 1:nrow(pars)) {
  assign(paste0("res", a),
         as_tibble(do.call(rbind, map(
           mclapply(
             1:1000,
             function(j) run_a_noninf_trial(
               j, c(1, rep(1.1, 3), rep(1, 9)), 
               delta_sup = pars$delta_sup[a],
               delta_noninf = pars$delta_noninf[a],
               kappa_act_0 = pars$kappa_act[a], kappa_act_1 = pars$kappa_act[a],
               kappa_sup_0 = pars$kappa_sup_0[a], kappa_sup_1 = pars$kappa_sup_1[a],
               kappa_ctr_0 = pars$kappa_ctr[a], kappa_ctr_1 = pars$kappa_ctr[a],
               kappa_noninf_0 = pars$kappa_noninf[a], kappa_noninf_1 = pars$kappa_noninf[a],
               kappa_nonsup_0 = pars$kappa_nonsup[a], kappa_nonsup_1 = pars$kappa_nonsup[a],
               ctrl_alloc = pars$alloc_ctrl[a],
               return_all = F, allocate_inactive = F, brar = T),
             mc.cores = parallel::detectCores() - 1),
           simplify))))
}


res <- bind_rows(lapply(1:nrow(pars), function(x) get(paste0("res", x))), .id = "pars")
rm(list = paste0("res", 1:nrow(pars)))
saveRDS(res, "out/nonsuperiority/mu_three_sup0.rds")

for(a in 1:nrow(pars)) {
  assign(paste0("res", a),
         as_tibble(do.call(rbind, map(
           mclapply(
             1:1000,
             function(j) run_a_noninf_trial(
               j, c(1, rep(1.25, 3), rep(1, 9)), 
               delta_sup = pars$delta_sup[a],
               delta_noninf = pars$delta_noninf[a],
               kappa_act_0 = pars$kappa_act[a], kappa_act_1 = pars$kappa_act[a],
               kappa_sup_0 = pars$kappa_sup_0[a], kappa_sup_1 = pars$kappa_sup_1[a],
               kappa_ctr_0 = pars$kappa_ctr[a], kappa_ctr_1 = pars$kappa_ctr[a],
               kappa_noninf_0 = pars$kappa_noninf[a], kappa_noninf_1 = pars$kappa_noninf[a],
               kappa_nonsup_0 = pars$kappa_nonsup[a], kappa_nonsup_1 = pars$kappa_nonsup[a],
               ctrl_alloc = pars$alloc_ctrl[a],
               return_all = F, allocate_inactive = F, brar = T),
             mc.cores = parallel::detectCores() - 1),
           simplify))))
}


res <- bind_rows(lapply(1:nrow(pars), function(x) get(paste0("res", x))), .id = "pars")
rm(list = paste0("res", 1:nrow(pars)))
saveRDS(res, "out/nonsuperiority/mu_three_sup1.rds")


for(a in 1:nrow(pars)) {
  assign(paste0("res", a),
         as_tibble(do.call(rbind, map(
           mclapply(
             1:1000,
             function(j) run_a_noninf_trial(
               j, c(1, rep(1.5, 3), rep(1, 9)),
               delta_sup = pars$delta_sup[a],
               delta_noninf = pars$delta_noninf[a],
               kappa_act_0 = pars$kappa_act[a], kappa_act_1 = pars$kappa_act[a],
               kappa_sup_0 = pars$kappa_sup_0[a], kappa_sup_1 = pars$kappa_sup_1[a],
               kappa_ctr_0 = pars$kappa_ctr[a], kappa_ctr_1 = pars$kappa_ctr[a],
               kappa_noninf_0 = pars$kappa_noninf[a], kappa_noninf_1 = pars$kappa_noninf[a],
               kappa_nonsup_0 = pars$kappa_nonsup[a], kappa_nonsup_1 = pars$kappa_nonsup[a],
               ctrl_alloc = pars$alloc_ctrl[a],
               return_all = F, allocate_inactive = F, brar = T),
             mc.cores = parallel::detectCores() - 1),
           simplify))))
}


res <- bind_rows(lapply(1:nrow(pars), function(x) get(paste0("res", x))), .id = "pars")
rm(list = paste0("res", 1:nrow(pars)))
saveRDS(res, "out/nonsuperiority/mu_three_sup2.rds")
