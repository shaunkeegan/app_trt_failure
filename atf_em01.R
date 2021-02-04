# Apparent Treatment Failure 'atf'
# Exploratory Model 01         'em01'
# 
# Shaun Keegan (keega.s.p@gmail.com/shaun.keegan@glasgow.ac.uk)
# November 2020
#
# This code produces a compartmental model investigating the possibility that R0
# can be so high as to allow reinfection to occur so rapidly that from a farmer's
# perspective treatment has failed. 

# PARAMETER KEY:
#   beta - transmission rate        
#   sigma - return to susceptible class after treatment
#   gamma - recovery rate           
#   tau - treatment of infected
#   epsilon - re-susceptibility of recovered
#



# Model Function

atf_em_01 <- function(times, init, parms){
  
  S <- init[1]
  Strt <- init[2]
  I <- init[3]
  Ireb <- init[4]
  R <- init[5]
  
  with(as.list(parms),{
    
    # population total
    N <- S + Strt + I + Ireb + R
    It <- I + Ireb
    
    # susceptibles
    dS.dt <- - beta * S * It/N  + sigma * Strt + epsilon * R
    
    # susceptibles after treatment
    dStrt.dt <- - beta * Strt * It/N  - sigma * Strt + tau * I
    
    # infected with susceptible strain
    dI.dt <- beta * S * It/N   - tau * I
    
    # infected with resistant strain
    dIreb.dt <- beta * Strt * It/N - gamma * Ireb
    
    # recovereds
    dR.dt <- gamma * Ireb - epsilon * R
    
    # model output
    dX <- c(dS.dt, dStrt.dt, dI.dt, dIreb.dt, dR.dt)
    list(dX)
  })
}
