library(parallel)
library(automaticsims)
library(tidyverse)

nsup <- 0
mu1 <- 1.25
mu2 <- 1.5

pars <- expand.grid(
  delta_sup = 0.1,
  kappa_act = 0.01,
  kappa_sup_0 = c(0.85, 0.95),
  kappa_sup_1 = 0.75,
  kappa_ctr = 0.95,
  kappa_nonsup_0 = c(0.05, 0.01),
  kappa_nonsup_1 = 0.05,
  kappa_noninf = 0.5,
  alloc_ctrl = 1/13,
  Nfirst = c(500, 1500)
)
pars$pars <- factor(as.character(1:nrow(pars)), levels = 1:nrow(pars))

for(a in 1:nrow(pars)) {
  assign(paste0("res", a),
         as_tibble(do.call(rbind, map(
           mclapply(
             1:5,
             function(j) run_a_noninf_trial_alt(
               j, c(1, rep(mu1, nsup), rep(1, 12 - nsup)), 
               delta_sup = pars$delta_sup[a],
               delta_noninf = pars$delta_sup[a],
               kappa_act_0 = pars$kappa_act[a], kappa_act_1 = pars$kappa_act[a],
               kappa_sup_0 = pars$kappa_sup_0[a], kappa_sup_1 = pars$kappa_sup_1[a],
               kappa_ctr_0 = pars$kappa_ctr[a], kappa_ctr_1 = pars$kappa_ctr[a],
               kappa_noninf_0 = pars$kappa_noninf[a], kappa_noninf_1 = pars$kappa_noninf[a],
               kappa_nonsup_0 = pars$kappa_nonsup_0[a], kappa_nonsup_1 = pars$kappa_nonsup_1[a],
               Nseq = seq(pars$Nfirst[a], 10000, 500),
               ctrl_alloc = pars$alloc_ctrl[a], ind_comp_ctrl = T,
               return_all = F, allocate_inactive = F, brar = T),
             mc.cores = parallel::detectCores()),
           simplify))))
}
res <- bind_rows(lapply(1:nrow(pars), function(x) get(paste0("res", x))), .id = "pars")
rm(list = paste0("res", 1:nrow(pars)))
saveRDS(res, "out/nonsuperiority/final_mu_one_sup1.rds")


for(a in 1:nrow(pars)) {
  assign(paste0("res", a),
         as_tibble(do.call(rbind, map(
           mclapply(
             1:4,
             function(j) run_a_noninf_trial_alt(
               j, c(1, rep(mu2, nsup), rep(1, 12 - nsup)), 
               delta_sup = pars$delta_sup[a],
               delta_noninf = pars$delta_sup[a],
               kappa_act_0 = pars$kappa_act[a], kappa_act_1 = pars$kappa_act[a],
               kappa_sup_0 = pars$kappa_sup_0[a], kappa_sup_1 = pars$kappa_sup_1[a],
               kappa_ctr_0 = pars$kappa_ctr[a], kappa_ctr_1 = pars$kappa_ctr[a],
               kappa_noninf_0 = pars$kappa_noninf[a], kappa_noninf_1 = pars$kappa_noninf[a],
               kappa_nonsup_0 = pars$kappa_nonsup_0[a], kappa_nonsup_1 = pars$kappa_nonsup_1[a],
               Nseq = seq(pars$Nfirst[a], 10000, 500),
               ctrl_alloc = pars$alloc_ctrl[a], ind_comp_ctrl = T,
               return_all = F, allocate_inactive = F, brar = T),
             mc.cores = parallel::detectCores()),
           simplify))))
}
res <- bind_rows(lapply(1:nrow(pars), function(x) get(paste0("res", x))), .id = "pars")
rm(list = paste0("res", 1:nrow(pars)))
saveRDS(res, "out/nonsuperiority/final_mu_one_sup2.rds")