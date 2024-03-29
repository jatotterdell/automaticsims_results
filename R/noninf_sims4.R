library(parallel)
library(automaticsims)
library(tidyverse)

pars <- expand.grid(
  delta = c(0.05, 0.1, 0.15),
  kappa_act = c(0.01, 0.025),
  kappa_sup = c(0.75, 0.85),
  kappa_ctr = 0.95,
  kappa_noninf = c(0.5, 0.6),
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
               j, c(1, rep(1.25, 3), rep(1, 9)), pars$delta[a],
               kappa_act_0 = pars$kappa_act[a], kappa_act_1 = pars$kappa_act[a],
               kappa_sup_0 = pars$kappa_sup[a], kappa_sup_1 = pars$kappa_sup[a],
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
saveRDS(res, "out/noninferior/mu_three_sup1.rds")


for(a in 1:nrow(pars)) {
  assign(paste0("res", a),
         as_tibble(do.call(rbind, map(
           mclapply(
             1:1000,
             function(j) run_a_noninf_trial(
               j, c(1, rep(1.5, 3), rep(1, 9)), pars$delta[a],
               kappa_act_0 = pars$kappa_act[a], kappa_act_1 = pars$kappa_act[a],
               kappa_sup_0 = pars$kappa_sup[a], kappa_sup_1 = pars$kappa_sup[a],
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
saveRDS(res, "out/noninferior/mu_three_sup2.rds")
