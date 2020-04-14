# This file defines all the functions that are used in the simulation

# ----------------------------------------------------------------------------
# SetODEs_SEIR function:
# -----------------------
# Defines the system of differential equations describing the SEIR model 
# INPUT: p - named list of parameter values
# OUTPUT: list of derivatives of each variable

SetODEs_SEIR=function(t,y,p){
  S = y[1]
  E0 = y[2]
  E1 = y[3]
  I0 = y[4]
  I1 = y[5]
  I2 = y[6]
  I3 = y[7]
  R = y[8]
  D = y[9]
  
  with(as.list(p),{
    
    seas=(1 + seas.amp*cos(2*pi*(t-seas.phase)/365))
    
    dS.dt = -(be*E1+b0*I0+b1*I1+b2*I2+b3*I3)*S*seas
    dE0.dt=(be*E1+b0*I0+b1*I1+b2*I2+b3*I3)*S*seas-a0*E0
    dE1.dt=a0*E0-a1*E1
    dI0.dt=f*a1*E1-g0*I0
    dI1.dt=(1-f)*a1*E1-g1*I1-p1*I1
    dI2.dt=p1*I1-g2*I2-p2*I2
    dI3.dt=p2*I2-g3*I3-u*I3
    dR.dt=g0*I0+g1*I1+g2*I2+g3*I3
    dD.dt=u*I3
    
    return(list(c(dS.dt, dE0.dt, dE1.dt,dI0.dt,dI1.dt, dI2.dt, dI3.dt, dR.dt, dD.dt)))
  })
}


# ----------------------------------------------------------------------------
# GetSpread_SEIR function:
# --------------------
# This function numerically intergrates the system of differential equations for a given set of parameter values, initial conditions, and maximum time
# INPUT: p- named list of parameter values
#        Tmax - max time to integrate for
#        y0 - named list of initial conditions for each variable
# OUTPUT: Dataframe with rows as timepoints and columns as variables

GetSpread_SEIR = function(p,Tmax,y0){
  
  t = seq(from=0, to=Tmax, by=1)
  
  out = ode(y=y0, times=t, func=SetODEs_SEIR, parms=p)
  
  df = as.data.frame(out)
  
  return(df)
}


# ----------------------------------------------------------------------------
# GetModelParams function:
# --------------------
# Function to take the parameters entered by the user and turn them into the rate parameters used by the model
# INPUT: input - structure containing all the user entered information
# OUTPUT: named list consisting of the population size N and another list of the model parameters, pModel

GetModelParams = function(input){
  
  IncubPeriod=input$IncubPeriod  #Incubation period, days
  
    # proportions
  prop0to9 = input$prop0to9/100
  prop10to19 = input$prop10to19/100
  prop20to29 = input$prop20to29/100
  prop30to39 = input$prop30to39/100
  prop40to49 = input$prop40to49/100
  prop50to59 = input$prop50to59/100
  prop60to69 = input$prop60to69/100
  prop70to79 = input$prop70to79/100
  prop80p = input$prop80p/100
  prop_all = c(prop0to9,
               prop10to19,
               prop20to29,
               prop30to39,
               prop40to49,
               prop50to59,
               prop60to69,
               prop70to79,
               prop80p)
  
  #DurMildInf=input$DurMildInf #Duration of mild infections, days
  DurMildInf0to9 = input$DurMildInf0to9
  DurMildInf10to19 = input$DurMildInf10to19
  DurMildInf20to29 = input$DurMildInf20to29
  DurMildInf30to39 = input$DurMildInf30to39
  DurMildInf40to49 = input$DurMildInf40to49
  DurMildInf50to59 = input$DurMildInf50to59
  DurMildInf60to69 = input$DurMildInf60to69
  DurMildInf70to79 = input$DurMildInf70to79
  DurMildInf80p = input$DurMildInf80p
  DurMildInf_all = c(DurMildInf0to9,
                     DurMildInf10to19,
                     DurMildInf20to29,
                     DurMildInf30to39,
                     DurMildInf40to49,
                     DurMildInf50to59,
                     DurMildInf60to69,
                     DurMildInf70to79,
                     DurMildInf80p)

    #FracSevere=input$FracSevere/100 #Fraction of infections that are severe
  FracSevere0to9 = input$FracSevere0to9/100
  FracSevere10to19 = input$FracSevere10to19/100
  FracSevere20to29 = input$FracSevere20to29/100
  FracSevere30to39 = input$FracSevere30to39/100
  FracSevere40to49 = input$FracSevere40to49/100
  FracSevere50to59 = input$FracSevere50to59/100
  FracSevere60to69 = input$FracSevere60to69/100
  FracSevere70to79 = input$FracSevere70to79/100
  FracSevere80p = input$FracSevere80p/100
  FracSevere_all = c(FracSevere0to9,
                     FracSevere10to19,
                     FracSevere20to29,
                     FracSevere30to39,
                     FracSevere40to49,
                     FracSevere50to59,
                     FracSevere60to69,
                     FracSevere70to79,
                     FracSevere80p)
  
  
  #FracCritical=input$FracCritical/100 #Fraction of infections that are critical
  FracCritical0to9 = input$FracCritical0to9/100
  FracCritical10to19 = input$FracCritical10to19/100
  FracCritical20to29 = input$FracCritical20to29/100
  FracCritical30to39 = input$FracCritical30to39/100
  FracCritical40to49 = input$FracCritical40to49/100
  FracCritical50to59 = input$FracCritical50to59/100
  FracCritical60to69 = input$FracCritical60to69/100
  FracCritical70to79 = input$FracCritical70to79/100
  FracCritical80p = input$FracCritical80p/100
  FracCritical_all = c(FracCritical0to9,
                       FracCritical10to19,
                       FracCritical20to29,
                       FracCritical30to39,
                       FracCritical40to49,
                       FracCritical50to59,
                       FracCritical60to69,
                       FracCritical70to79,
                       FracCritical80p)
  
  #FracMild=1-FracSevere-FracCritical  #Fraction of infections that are mild
  FracMild0to9 = 1 - FracSevere0to9 - FracCritical0to9
  FracMild10to19 = 1 - FracSevere10to19 - FracCritical10to19
  FracMild20to29 = 1 - FracSevere20to29 - FracCritical20to29
  FracMild30to39 = 1 - FracSevere30to39 - FracCritical30to39
  FracMild40to49 = 1 - FracSevere40to49 - FracCritical40to49
  FracMild50to59 = 1 - FracSevere50to59 - FracCritical50to59
  FracMild60to69 = 1 - FracSevere60to69 - FracCritical60to69
  FracMild70to79 = 1 - FracSevere70to79 - FracCritical70to79
  FracMild80p = 1 - FracSevere80p - FracCritical80p
  FracMild_all = c(FracMild0to9,
                   FracMild10to19,
                   FracMild20to29,
                   FracMild30to39,
                   FracMild40to49,
                   FracMild50to59,
                   FracMild60to69,
                   FracMild70to79,
                   FracMild80p)

    #ProbDeath=input$ProbDeath  #Probability of dying given critical infection
  ProbDeath0to9 = input$ProbDeath0to9
  ProbDeath10to19 = input$ProbDeath10to19
  ProbDeath20to29 = input$ProbDeath20to29
  ProbDeath30to39 = input$ProbDeath30to39
  ProbDeath40to49 = input$ProbDeath40to49
  ProbDeath50to59 = input$ProbDeath50to59
  ProbDeath60to69 = input$ProbDeath60to69
  ProbDeath70to79 = input$ProbDeath70to79
  ProbDeath80p = input$ProbDeath80p
  ProbDeath_all = c(ProbDeath0to9,
                    ProbDeath10to19,
                    ProbDeath20to29,
                    ProbDeath30to39,
                    ProbDeath40to49,
                    ProbDeath50to59,
                    ProbDeath60to69,
                    ProbDeath70to79,
                    ProbDeath80p)
  
  #CFR=ProbDeath*FracCritical/100 #Case fatality rate (fraction of infections resulting in death)
  CFR0to9 = ProbDeath0to9*FracCritical0to9/100
  CFR10to19 = ProbDeath10to19*FracCritical10to19/100
  CFR20to29 = ProbDeath20to29*FracCritical20to29/100
  CFR30to39 = ProbDeath30to39*FracCritical30to39/100
  CFR40to49 = ProbDeath40to49*FracCritical40to49/100
  CFR50to59 = ProbDeath50to59*FracCritical50to59/100
  CFR60to69 = ProbDeath60to69*FracCritical60to69/100
  CFR70to79 = ProbDeath70to79*FracCritical70to79/100
  CFR80p = ProbDeath80p*FracCritical80p/100
  CFR_all = c(CFR0to9,
              CFR10to19,
              CFR20to29,
              CFR30to39,
              CFR40to49,
              CFR50to59,
              CFR60to69,
              CFR70to79,
              CFR80p)

    #TimeICUDeath=input$TimeICUDeath #Time from ICU admission to death, days
  TimeICUDeath0to9 = input$TimeICUDeath0to9
  TimeICUDeath10to19 = input$TimeICUDeath10to19
  TimeICUDeath20to29 = input$TimeICUDeath20to29
  TimeICUDeath30to39 = input$TimeICUDeath30to39
  TimeICUDeath40to49 = input$TimeICUDeath40to49
  TimeICUDeath50to59 = input$TimeICUDeath50to59
  TimeICUDeath60to69 = input$TimeICUDeath60to69
  TimeICUDeath70to79 = input$TimeICUDeath70to79
  TimeICUDeath80p = input$TimeICUDeath80p
  TimeICUDeath_all = c(TimeICUDeath0to9,
                       TimeICUDeath10to19,
                       TimeICUDeath20to29,
                       TimeICUDeath30to39,
                       TimeICUDeath40to49,
                       TimeICUDeath50to59,
                       TimeICUDeath60to69,
                       TimeICUDeath70to79,
                       TimeICUDeath80p)
  
  #DurHosp=input$DurHosp #Duration of hospitalization, days
  DurHosp0to9 = input$DurHosp0to9
  DurHosp10to19 = input$DurHosp10to19
  DurHosp20to29 = input$DurHosp20to29
  DurHosp30to39 = input$DurHosp30to39
  DurHosp40to49 = input$DurHosp40to49
  DurHosp50to59 = input$DurHosp50to59
  DurHosp60to69 = input$DurHosp60to69
  DurHosp70to79 = input$DurHosp70to79
  DurHosp80p = input$DurHosp80p
  DurHosp_all = c(DurHosp0to9,
                  DurHosp10to19,
                  DurHosp20to29,
                  DurHosp30to39,
                  DurHosp40to49,
                  DurHosp50to59,
                  DurHosp60to69,
                  DurHosp70to79,
                  DurHosp80p)

  N=input$N
  
  # If seasonality is allowed. If there is seasonality, the input beta values correspond to the current values. Must be adjusted to find the true (average) beta values
  
  if(input$AllowSeason=="Yes"){
    seas.amp=input$seas.amp/100 #relative amplitude of seasonal fluctuations, in [0,1]
    seas.phase=input$seas.phase #phase of seasonal fluctuations, measuered in days relative to time zero when peak will occur (0=peak occurs at time zero, 30 = peak occurs one month after time zero). Can be negative
  }else{
    seas.amp=0.0 
    seas.phase=0
  }
  seas0=(1 + seas.amp*cos(2*pi*seas.phase/365)) #value of seasonality coefficient at time zero
   # the validate function only works when Shiny package is loaded
    #  validate(
  #    need(seas0>0, 'With this seasonality pattern, the b1 value at time zero would be zero, so there would be no outbreak. Choose another phase or amplitude for seasonality')
  #  )
  
  # The transmission rates are changed from values per time to values per capita per time
  
  b1=input$b1/(N*seas0)
  b2=input$b2/(N*seas0)
  b3=input$b3/(N*seas0)
  
  #If asymptomatic infection is allowed
  if(input$AllowAsym=="Yes"){
    FracAsym=input$FracAsym/100 #Fraction of all infections that are asymptomatic
    DurAsym=input$DurAsym #Duration of asympatomatic infection
    b0=input$b0/(N*seas0)
  }else{
    FracAsym=0 #Fraction of all infections that are asymptomatic
    DurAsym=7 #Duration of asympatomatic infection
    b0 = 0 #Transmission rate (asymptomatic infections)
  }
  
  # If presymptomatic transmission is allowed
  if(input$AllowPresym=="Yes"){
    PresymPeriod=input$PresymPeriod #Length of infections phase of incubation period
    be=input$be/(N*seas0)
  }else{
    PresymPeriod=0 #Length of infectious phase of incubation period
    be = 0 #Transmission rate (pre-symptomatic)
  }

  pClin=list(IncubPeriod=IncubPeriod, prop_all=prop_all, 
          DurMildInf_all=DurMildInf_all,FracMild_all=FracMild_all, 
          FracSevere_all=FracSevere_all,
          FracCritical_all=FracCritical_all,CFR_all=CFR_all,
          TimeICUDeath_all=TimeICUDeath_all,DurHosp_all=DurHosp_all,
          FracAsym=FracAsym,PresymPeriod=PresymPeriod,DurAsym=DurAsym)
  
  # Turn these clinical parameters into the rate constants of the model
  
  pModel=GetParams_SEIR(pClin)
  
  pModel=c(be=be,b0=b0,b1=b1,b2=b2,b3=b3,pModel)
  
  pModel=c(pModel,seas.amp=seas.amp, seas.phase=seas.phase)

  return(list("N"=N,"pModel"=pModel))
  
}

# ----------------------------------------------------------------------------
# GetParams_SEIR function:
# --------------------
# Function to relate the clinical parameters entered by the user into the rate parameters used by the model
# INPUT: pClin - named list of the clinical parameters
# OUTPUT: named list of the model rate parameters, excluding the Betas

GetParams_SEIR = function(pClin){
  
  with(as.list(pClin),{
    
    a1=min(10^6,1/PresymPeriod) #presymptomatic period of transmission
    a0=min(10^6,(IncubPeriod-PresymPeriod)^(-1)) # true latent period, avoid infinity when no presymptomatic phase
    
    g1_all <- vector(mode="numeric", length=length(prop_all))
    g2_all <- vector(mode="numeric", length=length(prop_all))
    g3_all <- vector(mode="numeric", length=length(prop_all))
    p1_all <- vector(mode="numeric", length=length(prop_all))
    p2_all <- vector(mode="numeric", length=length(prop_all))
    u_all <- vector(mode="numeric", length=length(prop_all))

    for (i in 1:length(prop_all)) {
      g1_all[i]=(1/DurMildInf_all[i])*FracMild_all[i]
      p1_all[i]=(1/DurMildInf_all[i])-g1_all[i]
      
      if (FracSevere_all[i]+FracCritical_all[i] == 0){
        p2_all[i] = 0
      } else {
        p2_all[i]=(1/DurHosp_all[i])*(FracCritical_all[i]/(FracSevere_all[i]+FracCritical_all[i]))
      }
      g2_all[i]=(1/DurHosp_all[i])-p2_all[i]
      
      if(FracCritical_all[i]==0){
        u_all[i]=0
      }else{
        u_all[i]=(1/TimeICUDeath_all[i])*(CFR_all[i]/FracCritical_all[i])
      }
      
      g3_all[i]=(1/TimeICUDeath_all[i])-u_all[i]
    }
    
    #TODO: change to get separate returns based on age groups
    g1 <- 0
    g2 <- 0
    g3 <- 0
    p1 <- 0
    p2 <- 0
    u <- 0
    for (i in 1:length(prop_all)) {
      g1 = g1 + prop_all[i]*g1_all[i]
      g2 = g2 + prop_all[i]*g2_all[i]
      g3 = g3 + prop_all[i]*g3_all[i]
      p1 = p1 + prop_all[i]*p1_all[i]
      p2 = p2 + prop_all[i]*p2_all[i]
      u = u + prop_all[i]*u_all[i]
    }

    f=FracAsym
    
    g0=1/DurAsym
    
    return(c(a0=a0,a1=a1,f=f,g0=g0,g1=g1,g2=g2,g3=g3,p1=p1,p2=p2,u=u))
  })
  
}

# ----------------------------------------------------------------------------
# GetRo_SEIR function:
# --------------------
# Function to calculate the basic reporductive ratio (Ro) for the model
# INPUT: p - named list of the clinical parameters
#        N - total population size
# OUTPUT: Ro

GetRo_SEIR = function(p,N){

  with(as.list(p),{
    
    Ro=N*((be/a1)+f*(b0/g0)+(1-f)*((b1/(p1+g1))+(p1/(p1+g1))*(b2/(p2+g2)+ (p2/(p2+g2))*(b3/(u+g3)))))
    
    return(Ro)
  })
  
}

# ----------------------------------------------------------------------------
# GetRo_SEIR_Season function:
# --------------------
# Function to calculate the basic reporductive ratio (Ro) for the model with seasonality, returning it at current time, peak, and trough
# INPUT: p - named list of the clinical parameters
#        N - total population size
# OUTPUT: Ro

GetRo_SEIR_Season = function(p,N){
  
  with(as.list(p),{
    
    Ro.now=N*((b1/(p1+g1))+(p1/(p1+g1))*(b2/(p2+g2)+ (p2/(p2+g2))*(b3/(u+g3))))*(1 + seas.amp*cos(2*pi*(0-seas.phase)/365))
    Ro.max=N*((b1/(p1+g1))+(p1/(p1+g1))*(b2/(p2+g2)+ (p2/(p2+g2))*(b3/(u+g3))))*(1 + seas.amp)
    Ro.min=N*((b1/(p1+g1))+(p1/(p1+g1))*(b2/(p2+g2)+ (p2/(p2+g2))*(b3/(u+g3))))*(1 - seas.amp)
    Ro=list("Ro.now"=Ro.now,"Ro.max"=Ro.max,"Ro.min"=Ro.min)
    return(Ro)
  })
  
}

# ----------------------------------------------------------------------------
# Getr_SEIR function:
# --------------------
# Function to calculate the early exponential growth rate (r) for the model from parameters
# INPUT: p - named list of the clinical parameters
#        N - total population size
# OUTPUT: Ro

Getr_SEIR = function(p,N){

  with(as.list(p),{
    
    #  Compute the maximum eigenvalue, corresponding to r, and the corresponding eigenvector, which gives the ratios of the numbers of individuals in each class during the early growth phase
    
    # matrix representation of the linearized system when S=N
    JacobianMat=rbind(c(-a0, N*be, N*b0, N*b1, N*b2, N*b3, 0, 0), 
                      c(a0, -a1, 0, 0, 0, 0, 0, 0), 
                      c(0, a1*f, -g0, 0, 0, 0, 0, 0), 
                      c(0, a1 - a1*f, 0, -p1-g1, 0, 0, 0, 0), 
                      c(0, 0, 0, p1, -p2-g2, 0, 0, 0), 
                      c(0, 0, 0, 0, p2, -u-g3, 0, 0), 
                      c(0, 0, g0, g1, g2, g3 , 0, 0), 
                      c(0, 0, 0, 0, 0, u, 0, 0)
    )
    
    eig=eigen(JacobianMat)
    eig$values=Re(eig$values) #sometimes it add zero complex parts
    r=max(eig$values)
    MaxEigenVector=eig$vectors[,which.max(eig$values)]
    MaxEigenVector=MaxEigenVector/MaxEigenVector[length(MaxEigenVector)] #normalize to deaths
    MaxEigenVector=Re(MaxEigenVector)
    DoublingTime=log(2)/r
    
    return(list("r"=r,"DoublingTime"=DoublingTime,"MaxEigenVector"=MaxEigenVector))
    
  })
  
}

# ----------------------------------------------------------------------------
# SetHospitalCapacity function:
# --------------------
# Function to determine the capacity for hospital beds and ICU beds based on total beds and availability, and to # get ventilator capacity
# INPUT: input - structure containing all the user entered information
# OUTPUT: named list consisting of all the healthcare capacity parameters

SetHospCapacity=function(input){
  
  AvailHospBeds=input$HospBedper*(100-input$HospBedOcc*(1+input$IncFluOcc/100))/100 #Available hospital beds per 1000 ppl in US based on total beds and occupancy
  AvailICUBeds=input$ICUBedper*(100-input$ICUBedOcc*(1+input$IncFluOcc/100))/100 #Available ICU beds per 1000 ppl in US, based on total beds and occupancy. Only counts adult not neonatal/pediatric beds
  ConvVentCap=input$ConvMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using conventional protocols
  ContVentCap=input$ContMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using contingency protocols
  CrisisVentCap=input$CrisisMVCap #Estimated excess # of patients who could be ventilated in US (per 1000 ppl) using crisis protocols
  
  capParams=c("AvailHospBeds"=AvailHospBeds,"AvailICUBeds"=AvailICUBeds,"ConvVentCap"=ConvVentCap,"ContVentCap"=ContVentCap,"CrisisVentCap"=CrisisVentCap)
  
  return(capParams)
}

# ----------------------------------------------------------------------------
# SimSEIR function:
# --------------------
# Function to simulate the spread of infection using the model
# INPUT: input - structure containing all the user entered information
# OUTPUT: named list consisting of df - wide format of the timecourse of each variable, N, Ro, r, and doubling time

SimSEIR = function(input){
  
  ParamStruct=GetModelParams(input)
  pModel=ParamStruct$pModel
  N=ParamStruct$N
  Tmax=input$Tmax

  # Set initial conditions and time interval
  E00=input$InitInf
  S0 = N-E00
  y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
  
  #get Ro and r values
  Ro=GetRo_SEIR(pModel,N)
  r.out=Getr_SEIR(pModel,N)
  r=r.out$r
  DoublingTime=r.out$DoublingTime
  
  #run ODEs
  out.df=GetSpread_SEIR(pModel,Tmax,y0)
  
  if(input$AllowSeason=="Yes"){
    
    Ro.Season=GetRo_SEIR_Season(pModel,N)
    
    return(list("out.df"=out.df,"N"=N,"Ro"=Ro,"Ro.Season"=Ro.Season,"r"=r,"DoublingTime"=DoublingTime))
    
  }else{
    return(
      list("out.df"=out.df,"N"=N,"Ro"=Ro,"r"=r,"DoublingTime"=DoublingTime))
    
  }
  
}

# ----------------------------------------------------------------------------
# SimSEIRintB function:
# --------------------
# Function to simulate the spread of infection using the model, when an intervention to reduce Beta is implemented
# INPUT: input - structure containing all the user entered information
# OUTPUT: named list consisting of df - wide format of the timecourse of each variable, N, Ro, r, and doubling time

SimSEIRintB = function(input){
  
  ParamStruct=GetModelParams(input)
  pModel=ParamStruct$pModel
  N=ParamStruct$N
  Tmax=input$Tmax
  
  # start/end time of intervention
  Tint=input$Tint
  Tend=pmin(input$Tend,input$Tmax)
  
  # intervention parameters
  pModelInt=pModel
  pModelInt["be"]=pModelInt["be"]*(1-input$s0/100)
  pModelInt["b0"]=pModelInt["b0"]*(1-input$s0/100)
  pModelInt["b1"]=pModelInt["b1"]*(1-input$s1/100)
  pModelInt["b2"]=pModelInt["b2"]*(1-input$s2/100)
  pModelInt["b3"]=pModelInt["b3"]*(1-input$s3/100)
  
  # intervention Ro and r values
  RoInt=GetRo_SEIR(pModelInt,N)
  
  r.out=Getr_SEIR(pModelInt,N)
  rInt=r.out$r
  DoublingTimeInt=r.out$DoublingTime
  
  if(Tint==Tend){ # If the intervention starts and ends at the same time, just return baseline values
    
    # Set initial conditions and time interval
    E00=input$InitInf
    S0 = N-E00
    y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
    
    #run ODEs
    outInt.df=GetSpread_SEIR(pModel,Tmax,y0)
    
  }else{
    
    # First simulate model without intervention, if Tint>0
    
    if(Tint>0){
      
      E00=input$InitInf
      S0 = N-E00
      y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
      out.df=GetSpread_SEIR(pModel,Tint,y0)
      
      # Set initial conditions and time interval
      iInt=nrow(out.df)
      S0 = out.df[iInt,"S"]
      E00 = out.df[iInt,"E0"]
      E10 = out.df[iInt,"E1"]
      I00 = out.df[iInt,"I0"]
      I10 = out.df[iInt,"I1"]
      I20 = out.df[iInt,"I2"]
      I30 = out.df[iInt,"I3"]
      D0 = out.df[iInt,"D"]
      R0 = out.df[iInt,"R"]
      y0 = c(S=S0, E0=E00, E1=E10, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
    }else{
      
      E00=input$InitInf
      S0 = N-E00
      y0 = c(S=S0, E0=E00,  E1=0, I0=0, I1=0, I2=0, I3=0, R=0, D=0)
      
    }
    
    #Run intervention time course until Tend. Up to time Tint, use baseline solution
    Trun=Tend-Tint
    
    outInt.df=GetSpread_SEIR(pModelInt,Trun,y0)
    outInt.df$time=outInt.df$time+Tint
    
    # combine data from before and after intervention, if the intervention didn't start right away
    
    if(Tint>0){
      outInt.df=rbind(out.df,outInt.df)
    }
    
    #--After intervention ends, run with regular parameters up to time Tmax
    
    Trun2=Tmax-Tend
    
    if(Trun2==0){
      
    }else{
      #Set initial conditions and time interval
      #Round all numbers to lowest intergar, so if less than 1, go to zero
      iEnd=nrow(outInt.df)
      
      if(input$RoundOne=="True"){
        S0 = round(outInt.df[iEnd,"S"])
        E00 = round(outInt.df[iEnd,"E0"])
        E10 = round(outInt.df[iEnd,"E1"])
        I00 = round(outInt.df[iEnd,"I0"])
        I10 = round(outInt.df[iEnd,"I1"])
        I20 = round(outInt.df[iEnd,"I2"])
        I30 = round(outInt.df[iEnd,"I3"])
        D0 = round(outInt.df[iEnd,"D"])
        R0 = round(outInt.df[iEnd,"R"])
      }else{
        S0 = outInt.df[iEnd,"S"]
        E00 = outInt.df[iEnd,"E0"]
        E10 = outInt.df[iEnd,"E1"]
        I00 = outInt.df[iEnd,"I0"]
        I10 = outInt.df[iEnd,"I1"]
        I20 = outInt.df[iEnd,"I2"]
        I30 = outInt.df[iEnd,"I3"]
        D0 = outInt.df[iEnd,"D"]
        R0 = outInt.df[iEnd,"R"]
      }
      
      y0 = c(S=S0, E0=E00, E1=E10, I0=I00, I1=I10, I2=I20, I3=I30, R=R0, D=D0)
      
      #run with parameters back to baseline

      outIntOff.df=GetSpread_SEIR(pModel,Trun2,y0)
      outIntOff.df$time=outIntOff.df$time+Tend
      
      #combine data
      outInt.df=rbind(outInt.df,outIntOff.df)
    }
    
  }
  
  
  if(input$AllowSeason=="Yes"){
    
    RoInt.Season=GetRo_SEIR_Season(pModelInt,N)
    
    return(list("out.df"=outInt.df,"N"=N,"Ro"=RoInt,"Ro.Season"=RoInt.Season,"r"=rInt,"DoublingTime"=DoublingTimeInt))
    
  }else{
    
    return(list("out.df"=outInt.df,"N"=N,"Ro"=RoInt,"r"=rInt,"DoublingTime"=DoublingTimeInt))
    
  }
  
}



