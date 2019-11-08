library(parallel)
library(automaticsims)
library(tidyverse)

pars <- expand.grid(
  delta_sup = 0.1,
  kappa_act = 0.01,
  kappa_sup_0 = 0.85,
  kappa_sup_1 = 0.75,
  kappa_ctr = 0.95,
  kappa_nonsup = 0.05,
  kappa_noninf = 0.5,
  alloc_ctrl = 1/13
)
pars$pars <- factor(as.character(1:nrow(pars)), levels = 1:nrow(pars))

assign(paste0("res"),
         as_tibble(do.call(rbind, map(
           mclapply(
             1:10000,
             function(j) run_a_noninf_trial_alt(
               j, rep(0, 13), 
               delta_sup = pars$delta_sup,
               delta_noninf = pars$delta_sup,
               kappa_act_0 = pars$kappa_act, kappa_act_1 = pars$kappa_act,
               kappa_sup_0 = pars$kappa_sup_0, kappa_sup_1 = pars$kappa_sup_1,
               kappa_ctr_0 = pars$kappa_ctr, kappa_ctr_1 = pars$kappa_ctr,
               kappa_noninf_0 = pars$kappa_noninf, kappa_noninf_1 = pars$kappa_noninf,
               kappa_nonsup_0 = pars$kappa_nonsup, kappa_nonsup_1 = pars$kappa_nonsup,
               ctrl_alloc = pars$alloc_ctrl, ind_comp_ctrl = T,
               return_all = F, allocate_inactive = F, brar = T),
             mc.cores = parallel::detectCores() - 1),
           simplify))))

saveRDS(res, "out/nonsuperiority/final_mu_none_sup1.rds")


assign("res",
       as_tibble(do.call(rbind, map(
         mclapply(
           1:10000,
           function(j) run_a_noninf_trial_alt(
             j, rep(1, 13), 
             delta_sup = pars$delta_sup,
             delta_noninf = pars$delta_sup,
             kappa_act_0 = pars$kappa_act, kappa_act_1 = pars$kappa_act,
             kappa_sup_0 = pars$kappa_sup_0, kappa_sup_1 = pars$kappa_sup_1,
             kappa_ctr_0 = pars$kappa_ctr, kappa_ctr_1 = pars$kappa_ctr,
             kappa_noninf_0 = pars$kappa_noninf, kappa_noninf_1 = pars$kappa_noninf,
             kappa_nonsup_0 = pars$kappa_nonsup, kappa_nonsup_1 = pars$kappa_nonsup,
             ctrl_alloc = pars$alloc_ctrl, ind_comp_ctrl = T,
             return_all = F, allocate_inactive = F, brar = T),
           mc.cores = parallel::detectCores() - 1),
         simplify))))

saveRDS(res, "out/nonsuperiority/final_mu_none_sup2.rds")