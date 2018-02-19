## Take DO, Water Temp and Wind Speed to calculate GPP and R
## required:
## data.set = data frame name
## data.date = Date column name
## data.temperature = Temperature column name, units in C
## data.dissoxy = DO column name, units in mg L
## data.odo_sat = DO Saturation column nmae
## data.wind = Wind Speed column name
## lat and long = lattitude and longitude for site to calculate sunrise and sunsent values



library(dplyr)
library(lubridate) # Pull Month, Day, Year from date
library(ISOweek) # Determine Week number
library(tidyr)
library(maptools) # Used for determining sunrise sunset values
library(LakeMetabolizer)
library(R2jags)

# smoothDO = function(data, date = 'Date', temperature = 'Temp', dissoxy = 'ODO_mgL', odo_sat = 'ODO_sat', wind = 'Wind_Speed', smooth.set = 0.05){
#   ## Fill in missing DO numbers if DO saturation value exists
#   data = data %>% mutate(DO_calc = ifelse(is.na(data[[dissoxy]]), exp(-139.34411+((1.57570*10^5)/(data[[temperature]]+273.15))-((6.642308*10^7)/(data[[temperature]]+273.15)^2)+((1.243800*10^10)/(data[[temperature]]+273.15)^3)-((8.621949*10^11)/(data[[temperature]]+273.15)^4))*(data[[odo_sat]]/100), data[[dissoxy]]))
#   
#   ## Smooth DO data over a week
#   weeks = levels(as.factor(data$Week))
#   smooth.week = NULL
#   smooth.week = as.vector(smooth.week)
#   for (y in 1:length(weeks))
#   {
#     x = data %>% filter(Week == weeks[y])
#     z = lowess(x[[date]], x$DO_calc, smooth.set)
#     zy = z$y
#     smooth.week = append(smooth.week, zy)
#   }
#   data = data %>% mutate(smoothDO = smooth.week)
#   return(data)
# }

# K_wind = function(data, wind = 'Wind_Speed', temperature = 'Temp', dissoxy = 'DO_calc', gage_height = 4.5, method = 'bilinear'){
#   # From Wanninkof, 2014 LNO:Methods
#   data = data %>% mutate(Schmidt_Num = 1745.1-(124.34*data[[temperature]])+(4.8055*data[[temperature]]^2)-(0.10115*data[[temperature]]^3)+(0.00086842*data[[temperature]]^4))
# 
#   # Adjust for height of wind gage
#   data = data %>% mutate(Wind_cor = data[[wind]] * (10 / gage_height)^0.15)
#   
#   # Recommended via Fabrice Veron to use -2/3 for smooth surfaces instead of -1/2
#   # Originally from Wanninkof 1992
#   # k_coef is to convert cm/h to m/s for Schmidt number
#   # 600 is for freshwater
#   # From Crusius and Wanninkhof 2003, depending on wind speed will determine gas flux
#   # /100 to convert from cm h-1 to m h-1
#   if(method == 'bilinear'){
#     data = data %>% mutate(K_wind = ifelse(Wind_cor < 3.7, ((0.72 * Wind_cor) * (Schmidt_Num/600)^-0.5) / 100, (-13.3 + ((4.33 * Wind_cor)) * (Schmidt_Num/600)^-0.5) / 100))
#   } else if(method == 'power'){
#     data = data %>% mutate(K_wind = ((0.168 + 0.228 * Wind_cor^2.2) * (Schmidt_Num / 600)^-0.5) / 100)
#   } else if(method == 'static'){
#     data = data %>% mutate(K_wind = ifelse(Wind_cor < 3.7, (1 * (Schmidt_Num/600)^-0.5) / 100, (-17.9 + ((5.14 * Wind_cor)) * (Schmidt_Num/600)^-0.5) / 100))
#   } else if(method == 'temperature'){
#   	data = data %>% mutate(K_wind = ifelse(data[[temperature]] > lag(data[[temperature]]), ((-0.15 + 1.74 * Wind_cor) * (Schmidt_Num/600)^-0.5) / 100, ((-2.0 + 2.04 * Wind_cor) * (Schmidt_Num/600)^-0.5 / 100)))
#   }
#   return(data)
# }

production = function(data, Date = 'Date', wind = 'Wind_Speed', temperature = 'Temp', dissoxy = 'ODO_mgL', sat = 'C_sat', Kwind = 'K_wind', PAR = 'TotPAR', site.long = -75.510, site.lat = 38.988, method = c('classic', 'ols', 'mle', 'kalman', 'bayesian'), error.type = c('OE', 'PE')){
  
  days = levels(data$Date_2)
  days_length = as.numeric(length(days))
  data_all = NULL
  vec_data = NULL
  for(i in 1:days_length){
    data_day = data %>% filter(Date_2 == days[i]) %>% mutate(z.mix = 1)
    cat('Calculating Production for', days[i], '\n')
    nobs = length(data_day[[dissoxy]])
    do.diff = diff(data_day[[dissoxy]])
    lntemp = log(data_day[[temperature]])
    irr = data_day[[PAR]]
    if(method == 'ols'){

      inst_flux = (data_day[[Kwind]] / nobs) * (data_day[[sat]] - data_day[[dissoxy]])
      flux = inst_flux[-nobs]
      noflux.do.diff = do.diff - flux
      mod = lm(noflux.do.diff ~ irr[-nobs] + lntemp[-nobs] -1)
      rho = mod[[1]][[2]]
      iota = mod[[1]][[1]]
      mod.matrix = model.matrix(mod)
      
      gpp = mean(iota * mod.matrix[,1], na.rm=TRUE) * nobs
      resp = mean(rho * mod.matrix[,2], na.rm=TRUE) * nobs
      nep = gpp + resp
      
      vec_data = data.frame(Date = ymd(days[i], tz = 'EST'), GPP = gpp, NEP = nep, R = resp)
      data_all = bind_rows(data_all, vec_data)
      vec_data = NULL

    } else if(method == 'mle'){

      Q0 = ((diff(range(data_day[[dissoxy]], na.rm=TRUE)) - mean(data_day[[dissoxy]], na.rm=TRUE))^2 / nobs)
      guesses = c(1E-4, 1E-4, log(Q0))
      if(error.type == 'OE'){
        guesses = c(guesses, data_day[[dissoxy]][1]) 
      
        fit = optim(guesses, fn = mleNllOE, do.obs = data_day[[dissoxy]], do.sat = data_day[[sat]], k.gas = (data_day[[Kwind]]/nobs), z.mix = 1, irr = irr, wtr = data_day[[temperature]])
        pars0 = fit$par
      } else if(error.type == 'PE'){
        fit = optim(guesses, fn = mleNllPE, do.obs = data_day[[dissoxy]], do.sat = data_day[[sat]], k.gas = (data_day[[Kwind]]/nobs), z.mix = 1, irr = irr, wtr = data_day[[temperature]])
        pars0 = fit$par
        #pars = c("gppCoeff" = pars0[1], 'rCoeff' = pars0[2], 'Q' = exp(pars0[3]), 'nll'=fit$value)
      }

      gpp = mean(pars0[1] * data_day[[PAR]], na.rm = TRUE) * nobs
      resp = mean(pars0[2] * log(data_day[[temperature]]), na.rm = TRUE) * nobs
      nep = gpp + resp 
      
      vec_data = data.frame(Date = ymd(days[i], tz = 'EST'), GPP = gpp, NEP = nep, R = resp, 'gppCoeff' = pars0[1], 'rCoeff' = pars0[2], 'Q' =exp(pars0[3]), 'nll' = fit$value, 'doInit' = pars0[4])
      data_all = bind_rows(data_all, vec_data)
      vec_data = NULL

    } else if(method == 'classic'){
    	
      irr = as.integer(is.day(datetimes = data_day[[Date]], lat = site.lat))
    	dayI = irr  == 1L
      nightI = irr == 0L
      gas.flux = (data_day[[sat]] - data_day[[dissoxy]]) * (data_day[[Kwind]] / nobs)
      delta.do = do.diff - gas.flux[1:length(gas.flux)-1]
		  nep.day = delta.do[dayI]
		  nep.night = delta.do[nightI]
		  resp = mean(nep.night, na.rm = TRUE) * nobs
		  nep = mean(delta.do, na.rm = TRUE) * nobs
		  gpp = mean(nep.day, na.rm = TRUE) * nobs
		  vec_data = data.frame(Date = ymd(days[i], tz = 'EST'), GPP = gpp, NEP = nep, R = resp)
		  data_all = bind_rows(data_all, vec_data)
		  vec_data = NULL

    } else if(method == 'kalman'){
      guesses = c(1e-4, 1e-4, log(5), log(5))
      fit = optim(guesses, fn = KFnllDO, do.obs = data_day[[dissoxy]], do.sat = data_day[[sat]], k.gas = (data_day[[Kwind]]/nobs), z.mix = 1, irr = data_day[[PAR]], wtr = data_day[[temperature]])
      pars0 = fit$par
      smoothDO = KFsmoothDO(pars0, do.obs = data_day[[dissoxy]], do.sat = data_day[[sat]], k.gas = (data_day[[Kwind]]/nobs), z.mix = 1, irr = data_day[[PAR]], wtr = data_day[[temperature]])

      gpp = mean(pars0[1] * data_day[[PAR]], na.rm = TRUE) * nobs
      resp = mean(pars0[2] * log(data_day[[temperature]]), na.rm = TRUE) * nobs

      vec_data = data.frame(Date = ymd(days[i], tz = 'EST'), GPP = gpp, NEP = gpp + resp, R = resp, 'gppCoeff' = pars0[1], 'rCoeff' = pars0[2], 'Q' = exp(pars0[3]), 'H' = exp(pars0[4]))
      data_all = bind_rows(data_all, vec_data)
      vec_data = NULL

    } else if(method == 'bayesian'){
      #data_day = data_day %>% mutate(z.mix = 1)
      priors = c("gppMu"=0, "gppSig2"=1E5, "rMu"=0, "rSig2"=1E5, "kSig2"=NA)
      # Define model and write to file
      # Model choice depends on k values (all 0, all non-0, mixture)
      modfile = bayes.makeModel(k.gas = (data_day[[Kwind]] / nobs))
      jags.dir = system.file('jags', package='LakeMetabolizer')
      # ===========================================
      # = Define objects to be used in jags model =
      # ===========================================
      #Supply elements of U (PAR, log(temp))
      U = matrix(NA, nrow = length(data_day[[PAR]]), ncol=2)
      U[,1] = data_day[[PAR]] # PAR Values
      U[,2] = log(data_day[[temperature]]) # log(temp) values

      # Priors (kP) for k.gas
      kP = matrix(NA, nrow = length(data_day[[Kwind]]), ncol=2)
      kP[,1] = data_day[[Kwind]] / nobs

      if(is.na(priors["kSig2"])){
        k0.logic = !is.finite(1/kP[,1]) # test for when k is 0
        kP[,2] = sum(kP[,1])/sum(!k0.logic)*0.1 # k variance = mean of the non-zero K, times 0.1
        kP[k0.logic,2] = 1E-9
      }else{
        kP[,2] = priors["kSig2"]
      }
      
      # Priors for regression coefficients (cP)
      cP = matrix(NA, nrow=2, ncol=2)
      cP[1,1] = priors["gppMu"] # prior mean of GPP coefficient (C[1,1]*PAR=GPP)
      cP[1,2] = priors["gppSig2"] # prior variance of GPP coefficient
      cP[2,1] = priors["rMu"] # prior mean of R coefficient (C[2,1]*log(Temp)=R)
      cP[2,2] = priors["rSig2"] # prior variance of R coefficient
	  databayes = list(Y = data_day[[dissoxy]], N = length(data_day[[dissoxy]]), U = U, kP = kP, cP = cP, satO = data_day[[sat]], a0 = data_day[[dissoxy]][1], Zmix = data_day$z.mix)
	  params = c("C", "sigmaV", "sigmaW")

	  output = bayesFit(databayes, params, mf = modfile)

      print(c(output$GPP, output$NEP, output$R))
      vec_data = data.frame(Date = ymd(days[i], tz = 'EST'), GPP = output$GPP, GPPsd = output$GPPsd, NEP = output$NEP, NEPsd = output$NEPsd, R = output$R, Rsd = output$Rsd)
      data_all = bind_rows(data_all, vec_data)
    }
  }
  
  return(data_all)
}

mleNllOE = function(Params, do.obs, do.sat, k.gas, z.mix, irr, wtr, error.type){
  c1 = Params[1] #PAR coeff
  c2 = Params[2] #log(Temp) coeff
  Q = exp(Params[3]) # Variance of the process error
  
  # See KalmanDO_smooth.R comments for explanation of beta
  kz = k.gas/z.mix # K and Zmix are both vector of length nobs
  beta = exp(-kz) # This beta is for using the differential equation form
  
  # Set first true value equal to first observation
  alpha = rep(0, length(do.obs))
  alpha[1] = Params[4] #Free varying initial DO value
  
  alpha = mleLoopOE(alpha=alpha, doobs=do.obs, c1=c1, c2=c2, beta=beta, irr=irr, wtr=wtr, kz=kz, dosat=do.sat)
  
  return(-sum(dnorm(do.obs, alpha, sd=sqrt(Q), log=TRUE), na.rm=TRUE))
}#End function

mleLoopOE = function(alpha, doobs, c1, c2, beta, irr, wtr, kz, dosat){
  nobs = length(doobs)
  a.loop = .C('mleLoopCoe', alpha=as.double(alpha), as.double(doobs), as.double(c1), as.double(c2), as.double(beta), as.double(irr), as.double(wtr), as.double(kz), as.double(dosat), as.integer(nobs), PACKAGE='LakeMetabolizer')
  return(a.loop[['alpha']])
}

# ======================
# = Kalman filter/ nll =
# ======================
# Main recursion written in C
KFnllDO = function(Params, do.obs, do.sat, k.gas, z.mix, irr, wtr){
  
  # ===========================
  # = Unpack and set initials =
  # ===========================
  #!Pseudocode #1: Initial guesses for B, C, and Q t
  c1 = Params[1] #PAR coeff
  c2 = Params[2] #log(Temp) coeff
  Q = exp(Params[3]) # Variance of the process error
  H = exp(Params[4]) # Variance of observation error
  
  # See KalmanDO_smooth.R comments for explanation of beta
  kz = k.gas/z.mix # K and Zmix are both vector of length nobs
  # beta = 1-kz # beta is a vector of length nobs (this beta is for difference equation form)
  beta = exp(-kz) # This beta is for using the differential equation form
  
  # Set first true value equal to first observation
  alpha = do.obs[1]#Let's give this model some starting values
  
  # Set process covariance, P, equal to Q
  P = Q #starting value
  
  # Empty vector for nll's
  nlls = rep(0,length(do.obs))

  nlls = kalmanLoopR(nlls=nlls, alpha=alpha, doobs=do.obs, c1=c1, c2=c2, P=P, Q=Q, H=H, beta=beta, irr=irr, wtr=wtr, kz=kz, dosat=do.sat)

  return(sum(nlls)) # return the sum of nll's
}#End function



# ===================
# = Kalman Smoother =
# ===================
KFsmoothDO = function(Params, do.obs, do.sat, k.gas, z.mix, irr, wtr, Hfac=NULL){
  nobs = length(do.obs)
  d0 = double(nobs-1)
  # beta = 1-KO2zmix #do.obs_t = 1*do.obs_t-1 + -KO2zmix*do.obs_t-1 + Sea%*%Ewe + eta === (1-KO2zmix)*do.obs_t-1.....
  
  # Unpack parameters (these were previously fitted)
  c1 = Params[1] # irr Coeff
  c2 = Params[2] # log(wtr) Coeff
  Q = Params[3] # Variance of the Process Error
  if(is.null(Hfac)){
    H = Params[4]
  }else{
    H = Params[4]*Hfac
  }
   # Variance of Observation Error

  kz = k.gas/z.mix # K and Zmix are both vector of length nobs
  # beta = 1-kz # beta is a vector of length nobs (this beta is for difference equation form)
  beta = exp(-kz) # This beta is for using the differential equation form
  
  # Set first true value equal to first observation
  alpha = do.obs[1]
  
  # Set process covariance, P, equal to Q
  P = Q # starting value
  
  # Initial values
  aHat = c(alpha, d0) # aHat[t] == "a[t|t-1]" (estimate of a before updating)
  pHat = c(P, d0) # pHat[t] == "p[t|t-1]" (estimate of a before updating)
  aVec = aHat # aVec[t] == "a[t|t]" or "a[t]" (aVec is the "updated" version of aHat)
  pVec = pHat # pVec[t] == "P[t|t]" or "P[t]" (pVec is the "updated" version of pHat)
  etaVec = double(nobs)
  
  for(i in 2:nobs){
    # ===============
    # = Predictions =
    # ===============
    # Equations for Predictions from Harvey
    # a[t|t-1] = T[t]*a[t-1] + c[t] Harvey pg 105 eq. 3.2.2a
    # P[t|t-1] = T[t]*P[t-1]*T'[t] + R[t]*Q[t]*R'[t] Harvey pg 106 eq. 3.2.2b
    
    # Predictions where gas flux not split into beta etc.:
    # Uk <- K[i-1]*(do.sat[i-1] - alpha)/Zmix[i-1]
    # alpha <- alpha + c1*irr[i-1] + c2*log(wtr[i-1]) + Uk
    # aHat[i] <- alpha
    # P <- (Uk*P*Uk) + Q
    # pHat[i] <- P
    
    # Predictions where gas flux is split into beta (see explanation above):
    
    # Difference Equation Version:
    # alpha <- beta[i-1]*alpha + c1*irr[i-1] + c2*log(wtr[i-1]) + kz[i-1]*do.sat[i-1]
    
    # Differential Equation Version (see kalmanDO_nll.R for explanation):
    if(is.finite(1/kz[i-1])){
      
      a1 = c1*irr[i-1] + c2*log(wtr[i-1]) + kz[i-1]*do.sat[i-1]  
      alpha = a1/kz[i-1] + -beta[i-1]*a1/kz[i-1] + beta[i-1]*alpha # NOTE: beta==exp(-kz); kz=K/Zmix
      
    }else{
      
      alpha = c1*irr[i-1] + c2*log(wtr[i-1])
      
    }

    
    aHat[i] = alpha
    P = (beta[i-1]*P*beta[i-1]) + Q
    pHat[i] = P
  
    # ======================
    # = Updating Equations =
    # ======================
    # Updating Equations from Harvey
    # a[t] = a[t|t-1] + P[t|t-1]*Z'[t]*F[t]^-1(y[t] - Z[t]*a[t|t-1] - d[t]). Harvey, page 106, 3.2.3a
    # P[t] = P[t|t-1] - P[t|t-1]*Z'[t]*F[t]^-1*Z[t]*P[t|t-1] Harvey, page 106, eq. 3.2.3b
    # F[t] = Z[t]*P[t|t-1]*Z'[t] + H[t] Harvey, page 106, eq. 3.2.3c
        
    eta = do.obs[i] - alpha
    Eff = P + H
    alpha = alpha + P/Eff*eta
    P = P - P*P/Eff

    aVec[i] = alpha
    pVec[i] = P
    etaVec[i] = eta
  }
  
  #Kalman Smoother
  aSmooth = rep(NA,nobs)
  Psmooth = rep(NA,nobs)
  aSmooth[nobs] = aVec[nobs] # "starting" value for smoother (smoother starts at end and works backwards)
  # pSmooth[nobs] <- pVec[nobs]
  
  # Filtering is informed by past information
  # Smoothing includes the information from filtering (estimates of parameters), but also future information.
  # "The aim of filtering is to find the expected value of the state vector, alpha[t], conditional on the information available at time t, that is E(alpha[t]|Y[t]). The aim of smoothing is to take account of the information made available after time t. The mean of the distribution of alpha[t], conditional on all the sample, may be written as E(alpha[t]|Y[T]) and is known as the smoothed estimate. THe corresponding estimator is called the SMOOTHER. Since the smoother is based on more information than the filtered estimator, it will have a MSE which, in general, is smaller than that of the filtered estimator; it cannot be greater." ~ Harvey 1989, pgs 149-150.
   #a[t|T] = a[t] + Pstar[t]*(a[t+1|T] - T[t+1]*a[t])
  # P[t|T] = P[t] + Pstar[t]*(P[t+1|T] - P[t+1|t])*Pstar[t]
  # Pstar[t] = P[t]*T[t+1]/P[t+1|t]
  # t is current time step, T is last time step (when in []), T contains AR parameters (when NOT in [])
  
  for(i in length(d0):1){
    pStar = pVec[i]*beta[i+1]/pHat[i+1]
    aSmooth[i] = aVec[i] + pStar*(aSmooth[i+1] - aHat[i+1])
    
    # CAN ALSO SMOOTH P, WHICH GIVES THE SMOOTHED COVARIANCE MATRIX (not a matrix for univariate; gives estimate of accuracy of state estimate)
    # pSmooth[i] <- pVec[i] + pStar*(pSmooth[i+1] - pHat[i+1])*pStar
    }
  return(aSmooth) # return smoothed DO time series
}

# ===========================================
# = R function that calls the C loop for KF =
# ===========================================
# kalmanLoopC(double *alpha, double *doobs, double *c1, double *c2, double *P, double *Q, double *H,  double *beta, double *irr, double *wtr, double *kz, double *dosat, int *nobs)
kalmanLoopR = function(nlls, alpha, doobs, c1, c2, P, Q, H, beta, irr, wtr, kz, dosat){
  nobs = length(doobs)
  a.loop = .C('kalmanLoopC', nlls=as.double(nlls), as.double(alpha), as.double(doobs), as.double(c1), as.double(c2), as.double(P), as.double(Q), as.double(H), as.double(beta), as.double(irr), as.double(wtr), as.double(kz), as.double(dosat), as.integer(nobs), PACKAGE='LakeMetabolizer')
  return(a.loop[['nlls']])
}

bayes.makeModel = function(k.gas){
  
	if(!requireNamespace("R2jags")){
    stop("metab.bayesian requires R2jags.\ninstall.packages('R2jags')\n #Also install JAGS (http://mcmc-jags.sourceforge.net/)")
	}
	
	finite.1oK = is.finite(1/k.gas)
	
	choose.allK = all(finite.1oK)
	choose.noK = all(!finite.1oK)
	choose.bothK = any(finite.1oK) & any(!finite.1oK)
	
	choice.mod = c("allK", "noK", "bothK")[c(choose.allK, choose.noK, choose.bothK)]

	# Write the appropriate bayesian model into a temporary file
	#modfile <- tempfile('jags.metab.bayes')
	jags.dir = system.file('jags', package='LakeMetabolizer')
	switch(choice.mod,
		allK = {modfile = file.path(jags.dir, 'bayes.mod.allk.JAGS'); print("using allK model")},
		noK = {modfile = file.path(jags.dir, 'bayes.mod.nok.JAGS'); print("using noK model")},
		bothK = {modfile = file.path(jags.dir, 'bayes.mod.bothk.JAGS'); print("using bothK model")}
	)

	return(modfile)
}

bayesFit = function(data, params, mf, tend="median", ...){ #function that writes jags model, traces params, supplies data, etc
	
	bf.args = list(...)
	
	jags.m = R2jags::jags(data, NULL, parameters.to.save=params, mf)

	tF = function(x, tend){ # tendency function
		switch(tend,
			median=median(x), #median
			mean=mean(x), #mean
			mode1 = unique(x)[which.max(tabulate(match(x, unique(x))))], # mode --- most frequently observed value
			mode2 = { # mode --- based on the highest peak of the posterior probability (from density plot)
				xd <- density(x)
				xd$x[which.max(xd$y)]
			}
			)
	}
	# medSim <- matrix(apply(jags2.m$BUGSoutput$sims.matrix, 2, median)[-(1)], nrow=115, ncol=3)
	simOut = jags.m$BUGSoutput$sims.matrix
	ctSim = apply(simOut, 2, tF, tend) # the central tendency metric
	sdSim = apply(simOut, 2, sd)

	#Figure out the order of the sims.matrix columns ...
	n.obs = length(data$U[,1])
	GPP = mean(ctSim[1]*data$U[,1], na.rm=TRUE) * n.obs # gpp coef * par, then sum
	R = mean(ctSim[2]*data$U[,2], na.rm=TRUE) * n.obs # r coef * log(temp), then sum

	GPPsd = sqrt(sum(sdSim[1]^2*data$U[,1]^2))
	Rsd = sqrt(sum(sdSim[2]^2*data$U[,2]^2))
	NEPsd = sqrt(GPPsd^2 + Rsd^2)

	output = data.frame(GPP = GPP, GPPsd = GPPsd, NEP = GPP + R, NEPsd = NEPsd, R = R, Rsd = Rsd)
	# return(list(
	# 	"model" = jags.m, 
	# 	"params" = ctSim[1:2], 
	# 	"metab.sd" = matrix(c(GPPsd, Rsd, NEPsd), nrow=1, dimnames=list(NULL, c("GPPsd", "Rsd", "NEPsd"))),
	# 	"metab" = matrix(c(GPP, R, GPP+R), nrow=1, dimnames=list(NULL, c("GPP", "R", "NEP")))
	# )) # need to clean up format, and maybe include a return of the sd's of the estimates
	return(output)
}