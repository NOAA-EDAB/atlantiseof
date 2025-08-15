#Testing decision space simulation
library(dplyr)
set.seed(13)

sim_decision_space = function(spp.param,
                              fleet.param,
                              targeting.level,
                              EOL.prop,
                              eco.biomass.thresh,
                              spp.biomass.thresh,
                              fleet.rev.thresh,
                              fleet.consolidate.thresh,
                              time.n){
  # Calculate eveness of targeting
  evenness = function(x,n){
    a = x
    b = rep(1/n,n)
    diff = abs(a-b)
    c = (n*sum(diff))/(2*(n-1))
    return(c)
  }

  #Caclulate fleet.target based on targeting.level
  fleet.target = matrix(nrow = fleet.param$fleet.n, ncol = spp.param$spp.n, data = NA)
  for(f in 1: fleet.param$fleet.n){
    
    #for each fleet choose a target species based on targeting.level
    target.spp = sample(spp.param$spp.n, 1)/spp.param$spp.n
    
    #Generate a gamma distribution centered on fleet.target.sppp
    s1 = (target.spp * (targeting.level-1))+1
    s2 = ((1-target.spp) * (targeting.level-1))+1
    
    spp.probs = dbeta(seq(0.1,0.9,length.out = spp.param$spp.n),s1,s2)
    spp.probs.norm = spp.probs/sum(spp.probs)
    
    fleet.target[f,] = spp.probs.norm
  }
  fleet.which.max = apply(fleet.target, 1, which.max)
  fleet.eveness = apply(fleet.target, 1, function(x) evenness(x, spp.param$spp.n))

  # selectivity = function(x,sela,selb){
  #   sel = 1/(1+exp(-selb*(x-sela)))
  #   return(sel)
  # }
  # fleet.sela = round(rnorm(fleet.n,30,10))
  # fleet.selb = runif(fleet.n,0.15,0.4)
  
  #simulation parameters & outputs

  spp.biomass = matrix(NA, nrow = time.n+1, ncol = spp.param$spp.n)
  spp.biomass[1,] = spp.param$spp.biomass.init
  eco.biomass = numeric()
  EOL = numeric()
  fleet.quota = matrix(NA, nrow = time.n, ncol =  fleet.param$fleet.n)
  fleet.landings = array(NA, dim = c( fleet.param$fleet.n, spp.param$spp.n, time.n))
  fleet.f = array(NA, dim = c( fleet.param$fleet.n, spp.param$spp.n, time.n))
  fleet.rev = array(NA, dim = c( fleet.param$fleet.n, spp.param$spp.n, time.n))
  fleet.rev.tot = matrix(NA, nrow = time.n, ncol =  fleet.param$fleet.n)
  fleet.landings.tot = matrix(NA, nrow = time.n, ncol =  fleet.param$fleet.n)
  
  eco.biomass.test = numeric()
  spp.biomass.test = numeric()
  fleet.rev.test = numeric()
  fleet.consolidate.test = numeric()
  
  for(i in 1:time.n){
  
    #Calculate EOL and fleet.quota
    EOL[i] = sum(spp.biomass[i,]) * EOL.prop
    fleet.quota[i,] = EOL[i] *  fleet.param$fleet.quota.prop
    
    #Calculate Landings & revenue
    # fleet.landings.t = (fleet.target* fleet.q * spp.biomass[i,]) 
    # fleet.landings.t = sweep(fleet.landings.t, 1, fleet.effort, FUN = "*")
    # fleet.landings.t = sweep(fleet.landings.t, 1, fleet.sweptarea, FUN = "*")
    fleet.landings[,,i] = fleet.target * fleet.quota[i,]
    
    fleet.f[,,i] = fleet.landings[,,i]/spp.biomass[i,]
    
    fleet.rev[,,i] = fleet.landings[,,i] * spp.param$spp.price
    
    fleet.rev.tot[i,] = rowSums(fleet.rev[,,i])
    fleet.landings.tot[i,] = rowSums(fleet.landings[,,i])
    
    #Update Biomass
    spp.biomass[i+1,] = spp.biomass[i,] * spp.param$spp.growth - colSums(fleet.landings[,,i])
    spp.biomass[i+1,spp.biomass[i+1,] < 0] = 0 # no negative biomass
    eco.biomass[i] = sum(spp.biomass[i,],na.rm=T)
    
    #Update test stats
    ## ecosystem biomass
    eco.biomass.test[i] = ifelse(sum(spp.biomass[i+1,]) < (eco.biomass.thresh * spp.param$eco.biomass.init), 1, 0)
    ## species biomass
    spp.biomass.test[i] = ifelse(any(spp.biomass[i+1,] < (spp.biomass.thresh * spp.param$spp.biomass.init)), 1, 0)
    ## fleet revenue
    fleet.rev.test[i] = ifelse(any(fleet.rev.tot[i,] < (fleet.rev.thresh * fleet.rev.tot[1,])), 1, 0)
    ## fleet consolidation
    fleet.consolidate.test[i] = ifelse(any(fleet.rev.tot[i,]/sum(fleet.rev.tot[i,]) > fleet.consolidate.thresh ), 1, 0)
    
  }
  
  #Return results
  results = list(spp.biomass = spp.biomass,
                 eco.biomass = eco.biomass,
                 EOL = EOL,
                 fleet.quota = fleet.quota,
                 fleet.landings = fleet.landings,
                 fleet.f = fleet.f,
                 fleet.rev = fleet.rev,
                 fleet.rev.tot = fleet.rev.tot,
                 fleet.landings.tot = fleet.landings.tot,
                 eco.biomass.test = eco.biomass.test,
                 spp.biomass.test = spp.biomass.test,
                 fleet.rev.test = fleet.rev.test,
                 fleet.consolidate.test = fleet.consolidate.test,
                 targeting.level = targeting.level,
                 fleet.target = fleet.target,
                 fleet.which.max = fleet.which.max,
                 fleet.eveness = fleet.eveness)
  return(results)

}

#Run simulation

#Define species parameters
spp.n = 10
spp.init = round(rnorm(spp.n, 1E6, 1E5))
spp.param = list(spp.n = spp.n,
                 spp.names =  LETTERS[1:spp.n],
                 spp.biomass.init = spp.init,
                 eco.biomass.init = sum(spp.init),
                 spp.growth = runif(spp.n, 1.25, 1.75),
                 spp.price = round(rnorm(spp.n, 10, 3), 2))

#Define fleet parameters
# targeting.level = 8 # 1-Inf, where 1 is most even and Inf is most uneven
# EOL.prop = 0.3

# fleet.effort = round(runif(fleet.n,0,10))
# fleet.q = matrix(nrow = fleet.n, ncol = spp.n, 
#                  data = runif(fleet.n*spp.n,0.1,1))
# fleet.sweptarea = runif(fleet.n,0,0.5)

fleet.param = list(fleet.n = 3,
                   fleet.names = paste("Fleet", 1:3),
                   fleet.quota.prop =  rep(1/3,3)#c(0.5,0.3,0.2)
                   # fleet.effort = fleet.effort,
                   # fleet.q = fleet.q,
                   # fleet.sweptarea = fleet.sweptarea
)

#Define management rules

#Total system biomass doesn't drop below X% of initial biomass
eco.biomass.thresh = 0.3
#Individual species biomass doesn't drop below X% of initial biomass
spp.biomass.thresh = round(runif(spp.n, 0.1, 0.25),2)
#Fleet revenue doesn't drop below X% of initial revenue
fleet.rev.thresh = 0.2
#Fleet revenue isn't consolidate into a single fleet
fleet.consolidate.thresh = 0.9
time.n = 5

EOL.prop.v = seq(0.05, 1, length.out = 20)
targeting.level.v = seq(1,20,0.5)

sim.results = expand.grid(EOL.prop = EOL.prop.v,targeting.level = targeting.level.v)%>%
  mutate(eco.biomass.test = NA,
         spp.biomass.test = NA,
         fleet.rev.test = NA,
         fleet.consolidate.test = NA,
         revenue.tot = NA,
         eco.biomass = NA)


for(i in 1:nrow(sim.results) ){

  sim.output = sim_decision_space(spp.param = spp.param,
                                   fleet.param = fleet.param,
                                   targeting.level = sim.results$targeting.level[i],
                                   EOL.prop = sim.results$EOL.prop[i],
                                   eco.biomass.thresh = eco.biomass.thresh,
                                   spp.biomass.thresh =spp.biomass.thresh,
                                   fleet.rev.thresh = fleet.rev.thresh,
                                   fleet.consolidate.thresh = fleet.consolidate.thresh,
                                   time.n = time.n)
  
  # Score each simulation based on management rules
  sim.results$eco.biomass.test[i] = ifelse(any(sim.output$eco.biomass.test==1), 1, 0)
  sim.results$spp.biomass.test[i] = ifelse(any(sim.output$spp.biomass.test==1), 1, 0)
  sim.results$fleet.rev.test[i] = ifelse(any(sim.output$fleet.rev.test==1), 1, 0)
  sim.results$fleet.consolidate.test[i] = ifelse(any(sim.output$fleet.consolidate.test==1), 1, 0)
  sim.results$revenue.tot[i] = sum(sim.output$fleet.rev.tot)
  sim.results$eco.biomass[i] = sum(sim.output$eco.biomass)
  
}

#Combined tests
sim.results = sim.results %>%
  mutate(pass.all = ifelse(eco.biomass.test == 0 & spp.biomass.test == 0 & fleet.rev.test == 0 & fleet.consolidate.test == 0, 1, 0),
         pass.score = eco.biomass.test + spp.biomass.test + fleet.rev.test + fleet.consolidate.test)

#make 3D surface  plot of revenue.tot as a function of EOL.prop and targeting.level

rev.z = matrix(sim.results$revenue.tot, nrow = length(EOL.prop.v), ncol = length(targeting.level.v), byrow = F)
eco.biomass.test.z = matrix(sim.results$eco.biomass.test, nrow = length(EOL.prop.v), ncol = length(targeting.level.v), byrow = F)
spp.biomass.test.z = matrix(sim.results$spp.biomass.test, nrow = length(EOL.prop.v), ncol = length(targeting.level.v), byrow = F)
fleet.rev.test.z = matrix(sim.results$fleet.rev.test, nrow = length(EOL.prop.v), ncol = length(targeting.level.v), byrow = F)
fleet.consolidate.test.z = matrix(sim.results$fleet.consolidate.test, nrow = length(EOL.prop.v), ncol = length(targeting.level.v), byrow = F)

plotly::plot_ly(x = ~EOL.prop.v, y = ~targeting.level.v,z = ~rev.z) %>%
  add_surface() %>%
  layout(scene = list(xaxis = list(title = "EOL Prop"),
                      yaxis = list(title = "Targeting Level"),
                      zaxis = list(title = "Total Revenue")))

#Ecosystem Biomass
plot_ly(sim.results, x = ~EOL.prop, y = ~targeting.level, z = ~revenue.tot,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = ~eco.biomass.test, colorscale = 'red', opacity = 1)) %>%
  layout(scene = list(xaxis = list(title = "EOL Prop"),
                      yaxis = list(title = "Targeting Level"),
                      zaxis = list(title = "Total Revenue")),
         title = 'Ecosystem Biomass Test')

#Species Biomass
plot_ly(sim.results, x = ~EOL.prop, y = ~targeting.level, z = ~revenue.tot,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = ~spp.biomass.test, colorscale = 'red', opacity = 1)) %>%
  layout(scene = list(xaxis = list(title = "EOL Prop"),
                      yaxis = list(title = "Targeting Level"),
                      zaxis = list(title = "Total Revenue")),
         title = 'Species Biomass Test')

#Fleet Revenue
plot_ly(sim.results, x = ~EOL.prop, y = ~targeting.level, z = ~revenue.tot,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = ~fleet.rev.test, colorscale = 'red', opacity = 1)) %>%
  layout(scene = list(xaxis = list(title = "EOL Prop"),
                      yaxis = list(title = "Targeting Level"),
                      zaxis = list(title = "Total Revenue")),
         title = 'Fleet Revenue Test')

#Fleet Consolidation
plot_ly(sim.results, x = ~EOL.prop, y = ~targeting.level, z = ~revenue.tot,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = ~fleet.consolidate.test, colorscale = 'red', opacity = 1)) %>%
  layout(scene = list(xaxis = list(title = "EOL Prop"),
                      yaxis = list(title = "Targeting Level"),
                      zaxis = list(title = "Total Revenue")),
         title = 'Fleet Consolidation Test')

#Pass All Tests
plot_ly(sim.results, x = ~EOL.prop, y = ~targeting.level, z = ~revenue.tot,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = ~pass.all, colorscale = 'red', opacity = 1)) %>%
  layout(scene = list(xaxis = list(title = "EOL Prop"),
                      yaxis = list(title = "Targeting Level"),
                      zaxis = list(title = "Total Revenue")),
         title = 'Fleet Consolidation Test')

#Pass All Tests 
plot_ly(sim.results, x = ~EOL.prop, y = ~targeting.level, z = ~revenue.tot,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = ~pass.score, colorscale = 'viridis', opacity = 1)) %>%
  layout(scene = list(xaxis = list(title = "EOL Prop"),
                      yaxis = list(title = "Targeting Level"),
                      zaxis = list(title = "Total Revenue")),
         title = 'Fleet Consolidation Test') 



