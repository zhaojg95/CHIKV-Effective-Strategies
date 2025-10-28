
#1. No intervention measures ----------------------------------------
CHIKV_model <- function(t, Y, parms) {
  #The CHIKV model is divided into three compartments: 
  # aquatic stage mosquitoes(Sa\Ia), adult stage mosquitoes(Sm\Em\Im),and human population(Sp\Ep\Ip\Rp)
  
  with(as.list(c(Y, parms)), {
    Nm <- Np*k
    # period function
    T_period <- 365
    T_shift  <- 30
    c <- 0.5 * (cos(2*pi*(t - T_shift)/T_period) + 1)
    
    # ODE function
    
    dSa  <- a * c * (Nm - n * Im) - lambda * e_p * Sa - (1-e_p)*Sa
    dIa  <- a * c * n * Im       - lambda * e_p * Ia - (1-e_p)*Ia
    
    dSm  <- lambda * e_p * Sa - beta_pm * Sm * (Ip + Ap) / Np - b * Sm
    dEm  <- beta_pm * Sm * (Ip + Ap) / Np - omega_m * Em - b * Em
    dIm  <- lambda * e_p * Ia + omega_m * Em - b * Im
    
    dSp  <- -beta_mp * Sp * Im / Np
    dEp  <-  beta_mp * Sp * Im / Np - omega_p * Ep
    dIp  <- (1 - q) * omega_p * Ep - gamma * Ip
    dAp  <- q * omega_p * Ep - gamma1 * Ap
    dRp  <- gamma * Ip + gamma1 * Ap
    
    #sum - new cases
    dX   <- (1 - q) * omega_p * Ep  
    
    list(c(dSa, dIa, dSm, dEm, dIm, dSp, dEp, dIp, dAp, dRp, dX))
  })
}



# 2.combined intervention measures ----------------------------------------
CHIKV_model_with_combind_intervention <- function(t, Y, parms) {
  
  with(as.list(c(Y, parms)), {
    # period function
    T_period <- 365
    T_shift  <- 30
    c <- 0.5 * (cos(2*pi*(t - T_shift)/T_period) + 1)
    
    #aquatic stage mosquitoes
    if(t <= control_time) ua_c = 0
    if(t > control_time) ua_c = ua_c
    
    #adult stage mosquitoes
    if(t <= control_time) um_c = 0
    if(t > control_time) um_c = um_c
    
    # reduce the bite rate
    if(t <= control_time) R_bite = 0
    if(t > control_time) R_bite = R_bite
    
    # shorten the infectious period
    if(t <= control_time) gamma = 1/7
    if(t <= control_time) gamma1 = 1/7
    if(t > control_time) gamma = R_gamma
    if(t > control_time) gamma1 = R_gamma1
    
    # ODE function
    
    dSa  <- a  * c * (Nm - n * Im) - lambda * e_p * Sa - (1-e_p)*Sa - ua_c*Sa 
    dIa  <- a * c * n * Im       - lambda * e_p * Ia - (1-e_p)*Ia- ua_c*Ia
    
    dSm  <- lambda * e_p * Sa - beta_pm*(1-R_bite) * Sm * (Ip + Ap) / Np - b * Sm - um_c*Sm
    dEm  <- beta_pm*(1-R_bite) * Sm * (Ip + Ap) / Np - omega_m * Em - b * Em - um_c*Em
    dIm  <- lambda * e_p * Ia + omega_m * Em - b * Im - um_c*Im
    
    dSp  <- -beta_mp*(1-R_bite) * Sp * Im / Np
    dEp  <-  beta_mp*(1-R_bite) * Sp * Im / Np - omega_p * Ep
    dIp  <- (1 - q) * omega_p * Ep - gamma * Ip
    dAp  <- q * omega_p * Ep - gamma1 * Ap
    dRp  <- gamma * Ip + gamma1 * Ap
    
    #sum - new cases
    dX   <- (1 - q) * omega_p * Ep  
    
    #sum - Removed aquatic & adult mosquitoes
    
    dMaqua <-  ua_c*Sa +ua_c*Ia
    dMadult <-  um_c*Sm + um_c*Em + um_c*Im

    list(c(dSa, dIa, dSm, dEm, dIm, dSp, dEp, dIp, dAp, dRp, dX, dMaqua , dMadult))
  })
}

