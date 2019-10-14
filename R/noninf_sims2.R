library(parallel)
library(automaticsims)
library(tidyverse)

pars <- expand.grid(
  delta = c(0.05, 0.1, 0.15),
  kappa_lo = c(0.01, 0.025),
  kappa_hi = c(0.75, 0.85),
  kappa_no = c(0.5, 0.6),
  alloc_ctrl = c(1/13, 1/5)
)
pars$pars <- factor(as.character(1:nrow(pars)), levels = 1:nrow(pars))

# cl <- makeCluster(4)
# clusterEvalQ(cl, {library(automaticsims)})
# clusterExport(cl, "pars")
# for(a in 1:nrow(pars)) {
#   clusterExport(cl, "a")
#   assign(paste0("res", a),
#          as_tibble(do.call(rbind, map(
#            parLapply(
#             cl, 1:10,
#             function(j) run_a_noninf_trial(
#             j, rep(1, 13), pars$delta[a],
#             kappa_lo_0 = pars$kappa_lo[a], kappa_lo_1 = pars$kappa_lo[a],
#             kappa_hi_0 = pars$kappa_hi[a], kappa_hi_1 = pars$kappa_hi[a],
#             kappa_no_0 = pars$kappa_no[a], kappa_no_1 = pars$kappa_no[a],
#             ctrl_alloc = pars$alloc_ctrl[a],
#             return_all = F, allocate_inactive = F, brar = T)
#           ),
#         simplify))))
# }
# stopCluster(cl)



for(a in 1:nrow(pars)) {
  assign(paste0("res", a),
         as_tibble(do.call(rbind, map(
           mclapply(
             1:1000,
             function(j) run_a_noninf_trial(
               j, c(1.25, rep(1, 12)), pars$delta[a],
               kappa_lo_0 = pars$kappa_lo[a], kappa_lo_1 = pars$kappa_lo[a],
               kappa_hi_0 = pars$kappa_hi[a], kappa_hi_1 = pars$kappa_hi[a],
               kappa_no_0 = pars$kappa_no[a], kappa_no_1 = pars$kappa_no[a],
               ctrl_alloc = pars$alloc_ctrl[a],
               return_all = F, allocate_inactive = F, brar = T),
           mc.cores = parallel::detectCores() - 1),
           simplify))))
}


res <- bind_rows(lapply(1:nrow(pars), function(x) get(paste0("res", x))), .id = "pars")
rm(list = paste0("res", 1:nrow(pars)))

saveRDS(res, "out/noninferior/mu_ctrl_sup.rds")
