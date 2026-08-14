# Function to compute the psi values from other spOcc output 

psi_new <- w_star <-array(dim = dim(out$psi.samples))
inds <- 0:6 * 36
for(i in 1:num_species){
  for(j in 1:num_sites){
    for(k in 1:50){
      w_star[k, i, j] <- 
        out$lambda.samples[k,c(i, i + 36)] %*% out$w.samples[k,,j]
      inds_temp <- inds + i
      temp_x <- 
        out$X[j, ] %*% out$beta.samples[k, inds_temp] + w_star[k ,i ,j]
      psi_new[k,i,j] <- (1 + exp(-temp_x))^-1
    }
  }
}
