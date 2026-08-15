### 
# Script to cut off start of a chain and get it running again with updateMCMC

out <- readRDS("~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_20factors_run2_chain5_2026-08-13.rds")

# remove z and psi samples if present 
if(length(which(names(out) %in% c("z.samples", "psi.samples"))) > 0){
  rm_inds <- which(names(out) %in% c("z.samples", "psi.samples"))
  out <- out[-rm_inds]
}
out$monitors[c(4,5)] <- 0 # fix the monitors vector so it doesn't include these 

# remove first num_to_cut samples
num_to_cut <- 3500
dimensions <- sapply(out, dim)
for(i in 1:length(out)){
  if(out$n.post %in% dimensions[[i]]){
    if(length(dimensions[[i]]) == 2){
      out[[i]] <- out[[i]][-c(1:num_to_cut), ]
    } else if(length(dimensions[[i]]) == 3){
      out[[i]] <- out[[i]][-c(1:num_to_cut), , ]
    } else {
      print(paste0("wrong number of indices for item ", i))
    }
  }
}

# fix other values to match
out$n.post <- out$n.post - num_to_cut
out$n.burn <- out$n.burn + num_to_cut*out$n.thin

class(out) <- "sfJSDM"

saveRDS(out, "~/../../work/pi_twixson_umass_edu/nps_full_allCovs_15cmQuad_20factors_run2_chain5chopped3500_2026-08-14.rds")




