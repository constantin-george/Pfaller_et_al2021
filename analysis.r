source("/Users/georgeglen/Documents/GitHub/Pfaller_et_al2021/functions.r")

## Load each dataset in individually
setwd("~/Documents/GitHub/Pfaller_et_al2021")
FULL <- fun$importExcel(
  fileName = "nesting_dat.xlsx",
  sheetNames = c("Recruitment","OCF", "ECF","RMI","BF","Production"),
  nskip = 0
);

## Seperate the datasets
## 1: Clutch Frequency
OCF <- FULL[[2]] %>% gather("Beach", "OCF")
OCF$Beach <- str_replace_all(OCF$Beach, "OCF.", "")
OCF <- OCF %>% transform(Beach = as.factor(Beach),
					   OCF 	 = as.numeric(OCF)) %>%
			 filter(!is.na(OCF))

ECF <- FULL[[3]] %>% gather("Beach", "ECF")
ECF$Beach <- str_replace_all(ECF$Beach, "ECF.", "")
ECF <- ECF %>% transform(Beach = as.factor(Beach),
					   ECF 	 = as.numeric(ECF)) %>%
			 filter(!is.na(ECF))

## 2: RMI
RMI <- FULL[[4]] %>% gather("Beach", "RMI")
RMI$Beach <- str_replace_all(RMI$Beach, "RI.", "")
RMI <- RMI %>% transform(Beach = as.factor(Beach),
						 RMI 	 = as.numeric(RMI)) %>%
			   filter(!is.na(RMI))
			   
## 3: BP
BF <- FULL[[5]] %>% gather("Beach", "BF")
BF$Beach <- str_replace_all(BF$Beach, "BF.", "")
BF <- BF %>% transform(Beach = as.factor(Beach),
					   BF 	 = as.numeric(BF)) %>%
			   filter(!is.na(BF))

#### ----------------------------------------- ####
#### Models
#### ----------------------------------------- ####

## Write out the model formulas
## 1: OCF
null.OCF.form <- formula(OCF ~ 1)
beach.OCF.form <- formula(OCF ~ Beach - 1)

null.ECF.form <- formula(ECF ~ 1)
beach.ECF.form <- formula(ECF ~ Beach - 1)

## 2: RMI 
null.RMI.form <- formula(RMI ~ 1)
beach.RMI.form <- formula(RMI ~ Beach - 1)

## 3: BP
null.BF.form <- formula(BF ~ 1)
beach.BF.form <- formula(BF ~ Beach - 1)

#### ----------------------------------------- ####
#### Model for OCF
#### ----------------------------------------- ####

## Create initial models
null.OCF.Model <- glm(null.OCF.form, data = OCF, family=poisson(link="log"))
OCF.Model      <- glm(beach.OCF.form, data = OCF, family=poisson(link="log"))

## check dispersion
testDispersion(null.OCF.Model); testDispersion(OCF.Model)

## Overdispersoin not present so we do not need to fit a negative binomial 
## model to account for overdispersion but underdispersion is present so lets 
## use a gen.pois model in the glmmTMB package and do distribution selection


ICtab(null.OCF.Model,
	  glmmTMB(formula = null.OCF.form, data= OCF, family=genpois(link="log"), na.action = na.exclude), ## UPDATE THE MODEL DISTRIBUTIONS
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("Poisson", "Generalized Poisson"))
	  
ICtab(OCF.Model,
	  glmmTMB(formula = beach.OCF.form, data= OCF, family=genpois(link="log"),na.action = na.exclude,
	          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))), ## UPDATE THE MODEL DISTRIBUTIONS
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("Poisson", "Generalized Poisson"))	  
	  
## The final null and full model for the OCF data is a genpois model
null.OCF.Model <- glmmTMB(formula = null.OCF.form, data= OCF, family=genpois(link="log"), na.action = na.exclude)
OCF.Model      <- glmmTMB(formula = beach.OCF.form, data= OCF, family=genpois(link="log"),
                          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")), na.action = na.exclude)
summary(null.OCF.Model); sigma(null.OCF.Model) ## returns the dispersion parameter
summary(OCF.Model); sigma(OCF.Model)

## Checking the models
simulationNULL <- simulateResiduals(fittedModel = null.OCF.Model, plot = F, n = 1000)
simulationALL  <- simulateResiduals(fittedModel = OCF.Model, plot = F, n = 1000)
plot(simulationNULL); plot(simulationALL)
testDispersion(simulationOutput = simulationNULL, alternative ="less") 
testDispersion(simulationOutput = simulationALL, alternative ="less") 
## underdispersion still present in the BEACH model

## Final comparison
ICtab(null.OCF.Model, OCF.Model,
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("NULL", "~Beach"))
## Model with beach is best between the two

## compute the evidence ratio
evidence(aictab(cand.set = list(null.OCF.Model, OCF.Model), 
                modnames = c("NULL", "FULL"), 
                second.ord = TRUE))

## compare simulated data to the observed data 
simBEACH <- lapply(simulate(OCF.Model, seed = 1, nsim = 1000), function(x){ 
	cbind(x, OCF, dist="gen.pois")
	})
simBEACH           <- do.call(rbind, simBEACH)[,-3]
OCF$dist           <- "observed"
colnames(simBEACH) <- colnames(OCF[,c(2,1,3)]) 
ssd                <-  rbind(simBEACH, OCF)

## plot
pal= c("#999999","#000000")
ggplot(ssd, aes(x=OCF, fill= dist)) + facet_wrap(~Beach) + 
	  geom_histogram(aes(y = ..density..), position="dodge", binwidth=1, colour=1) +
      xlab("Observed Clutch Frequency") + ylab(NULL) + 
      scale_fill_manual(values =pal) + 
      scale_x_continuous(limits = c(NA, 10)) +
      theme_classic(base_size=20) + 
      theme(legend.title=element_blank(), 
            legend.position = "top")

## compute profile CI and mean differences
confint(OCF.Model)
emmeans(OCF.Model, ~ Beach, transform = "response")

contrast(emmeans(OCF.Model, specs="Beach", transform = "response"),list(c(-1,1)))
(exp(0.811) / exp(1.265) ) ## % underestimated is 0.635
exp(0.811) - exp(1.265)    ## difference is on average 1.29

#### ----------------------------------------- ####
#### Model for ECF
#### ----------------------------------------- ####

null.ECF.Model <- glm(null.ECF.form, data = ECF, family=poisson(link="log"))
ECF.Model      <- glm(beach.ECF.form, data = ECF, family=poisson(link="log"))

## check dispersion
testDispersion(null.ECF.Model); testDispersion(ECF.Model)

## Overdispersoin not present so we do not need to fit a negative binomial 
## model to account for overdispersion but underdispersion is present so lets 
## use a gen.pois model in the glmmTMB package and do distribution selection
ICtab(null.ECF.Model,
	  glmmTMB(formula = null.ECF.form, data= ECF, family=genpois(link="log"), na.action = na.exclude), ## UPDATE THE MODEL DISTRIBUTIONS
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("Poisson", "Generalized Poisson"))
ICtab(ECF.Model,
	  glmmTMB(formula = beach.ECF.form, data= ECF, family=genpois(link="log"),na.action = na.exclude,
	          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))), ## UPDATE THE MODEL DISTRIBUTIONS
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("Poisson", "Generalized Poisson"))	  
	  
## The final null and full model for the ECF data is a genpois model
null.ECF.Model <- glmmTMB(formula = null.ECF.form, data= ECF, family=genpois(link="log"), na.action = na.exclude)
ECF.Model      <- glmmTMB(formula = beach.ECF.form, data= ECF, family=genpois(link="log"),
                          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")), na.action = na.exclude)
summary(null.ECF.Model); sigma(null.ECF.Model) ## returns the dispersion parameter
summary(ECF.Model); sigma(ECF.Model)

## Checking the models
testDispersion(null.ECF.Model); 
testDispersion(ECF.Model); 
## underdispersion still present in the BEACH model

simulationNULL <- simulateResiduals(fittedModel = null.ECF.Model, plot = F, n = 1000)
simulationALL  <- simulateResiduals(fittedModel = ECF.Model, plot = F, n = 1000)
plot(simulationNULL)
plot(simulationALL)

## Final comparison
ICtab(null.ECF.Model, ECF.Model,
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("NULL", "~Beach"))
## Model with beach is best

## compute the evidence ratio
evidence(aictab(cand.set = list(null.ECF.Model, ECF.Model), 
                modnames = c("NULL", "FULL"), second.ord = TRUE))


## compare simulated data to the observed data 
simBEACH <- lapply(simulate(ECF.Model, seed = 1, nsim = 1000), function(x){ 
	cbind(x, ECF, dist="gen.pois")
	})
simBEACH           <- do.call(rbind, simBEACH)[,-3]
ECF$dist           <- "observed"
colnames(simBEACH) <- colnames(ECF[,c(2,1,3)]) 
ssd                <-  rbind(simBEACH, ECF)

## plot
ggplot(ssd, aes(x=ECF, fill= dist)) + facet_wrap(~Beach) + 
	  geom_histogram(aes(y = ..density..), position="dodge", binwidth=1, colour=1) +
      xlab("Estimated Clutch Frequency") + ylab(NULL) + 
      scale_fill_manual(values =pal) + 
      scale_x_continuous(limits = c(NA, 10)) +
      theme_classic(base_size=20) + 
      theme(legend.title=element_blank(), 
            legend.position = "top")

## compute profile CI and mean differences
confint(ECF.Model)
emmeans(ECF.Model, ~ Beach, transform = "response")
contrast(emmeans(ECF.Model, specs="Beach", transform = "response"),list(c(-1,1)))
(exp(0.864) / exp(1.364) ) ## % underestimated is 0.6065307
exp(0.864) - exp(1.364)    ## difference is on average 1.539177

#### ----------------------------------------- ####
#### Model for RMI
#### ----------------------------------------- ####

null.RMI.Model <- glm(null.RMI.form, data = RMI, family=poisson(link="log"))
RMI.Model      <- glm(beach.RMI.form, data = RMI, family=poisson(link="log"))

## check dispersion
testDispersion(null.RMI.Model); testDispersion(RMI.Model)

## Overdispersoin not present so we do not need to fit a negative binomial 
## model to account for overdispersion but underdispersion is present so lets 
## use a gen.pois model in the glmmTMB package and do distribution selection
ICtab(null.RMI.Model,
	  glmmTMB(formula = null.RMI.form, data= RMI, family=genpois(link="log"), na.action = na.exclude, 
	          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))), ## UPDATE THE MODEL DISTRIBUTIONS
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("Poisson", "Generalized Poisson"))
ICtab(RMI.Model,
	  glmmTMB(formula = beach.RMI.form, data= RMI, family=genpois(link="log"), na.action = na.exclude,
	          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))), ## UPDATE THE MODEL DISTRIBUTIONS
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("Poisson", "Generalized Poisson"))	  
	  
## The final null and full model for the OCF data is a genpois model
null.RMI.Model <- glmmTMB(formula = null.RMI.form, data= RMI, family=genpois(link="log"),
                          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")),
                          na.action = na.exclude)
RMI.Model      <-  glmmTMB(formula = beach.RMI.form, data= RMI, family=genpois(link="log"),
						   control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")), na.action = na.exclude)
summary(null.RMI.Model); sigma(null.RMI.Model) ## returns the dispersion parameter
summary(RMI.Model); sigma(RMI.Model) ## returns the dispersion parameter
## exp(RMI.Model$sdr$par.fixed[3]) ## compute from the model output


## Checking the models
testDispersion(null.RMI.Model); 
testDispersion(RMI.Model )
simulationNULL <- simulateResiduals(fittedModel = null.RMI.Model, plot = F)
simulationALL  <- simulateResiduals(fittedModel = RMI.Model , plot = F)
plot(simulationNULL); 
plot(simulationALL)
## underdispersion still present in the both models

## Final comparison
ICtab(null.RMI.Model, RMI.Model ,
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("NULL", "~Beach"))
## NULL model is best

## compute the evidence ratio
evidence(aictab(cand.set = list(null.RMI.Model, RMI.Model), 
                modnames = c("NULL", "FULL"), second.ord = TRUE))

## compare simulated data to the observed data 
simBEACH <- lapply(simulate(RMI.Model, seed = 1, nsim = 1000), function(x){ 
	cbind(x, RMI, dist="gen.pois")
	})
simBEACH           <- do.call(rbind, simBEACH)[,-3]
RMI$dist           <- "observed"
colnames(simBEACH) <- colnames(RMI[,c(2,1,3)]) 
ssd                <-  rbind(simBEACH, RMI)

## plot
ggplot(ssd, aes(x=RMI, fill= dist)) + facet_wrap(~Beach) + 
	  geom_histogram(aes(y = ..density..), position="dodge", binwidth=1, colour=1) +
      xlab("Remigration Interval") + ylab(NULL) + 
      scale_fill_manual(values =pal) + 
      scale_x_continuous(limits = c(NA, 10)) +
      theme_classic(base_size=20) + 
      theme(legend.title=element_blank(), 
            legend.position = "top")

## compute profile CI and mea differences
confint(RMI.Model)
emmeans(RMI.Model, ~ 1, transform = "response")
emmeans(RMI.Model, ~ Beach, transform = "response")
contrast(emmeans(RMI.Model, specs="Beach", transform = "response"),list(c(-1,1)))

#### ----------------------------------------- ####
#### Model for BP
#### ----------------------------------------- ####
#### The number of breeding seasons for turtles across the 8-year study period

null.BF.Model <- glm(null.BF.form, data = BF, family=poisson(link="log"))
BF.Model      <- glm(beach.BF.form, data = BF, family=poisson(link="log"))

## check dispersion
testDispersion(null.BF.Model); testDispersion(BF.Model )

## Overdispersoin not present so we do not need to fit a negative binomial 
## model to account for overdispersion but underdispersion is present so lets 
## use a gen.pois model in the glmmTMB package and do distribution selection
ICtab(null.BF.Model,
	  glmmTMB(formula = null.BF.form, data= BF, family=genpois(link="log"), na.action = na.exclude,
	          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))), ## UPDATE THE MODEL DISTRIBUTIONS
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("Poisson", "Generalized Poisson"))
ICtab(BF.Model,
	  glmmTMB(formula = beach.BF.form, data= BF, family=genpois(link="log"), na.action = na.exclude,
	          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))), ## UPDATE THE MODEL DISTRIBUTIONS
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("Poisson", "Generalized Poisson"))	  
	  
## The final null and full model for the OCF data is a genpois model
null.BF.Model <- glmmTMB(formula = null.BF.form, data= BF, family=genpois(link="log"), na.action = na.exclude,
                          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))

BF.Model      <-  glmmTMB(formula = beach.BF.form, data= BF, family=genpois(link="log"),  na.action = na.exclude,
                          control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
summary(null.BF.Model); sigma(null.BF.Model) ## dispersion parameter
summary(BF.Model); sigma(BF.Model) ## dispersion parameter

## by hand, the dispersion parameter is computed as 
exp(BF.Model$sdr$par.fixed[3])


## Checking the models
testDispersion(null.BF.Model); 
testDispersion(BF.Model )
simulationNULL <- simulateResiduals(fittedModel = null.BF.Model, plot = F)
simulationALL  <- simulateResiduals(fittedModel = BF.Model , plot = F)
plot(simulationNULL); 
plot(simulationALL)
## underdispersion still present in the both models

## Final comparison
ICtab(null.BF.Model, BF.Model ,
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("NULL", "~Beach"))
## FULL model is best

## compute the evidence ratio
evidence(aictab(cand.set = list(null.BF.Model, BF.Model), 
                modnames = c("NULL", "FULL"), second.ord = TRUE))

## compare simulated data to the observed data 
simBEACH <- lapply(simulate(BF.Model, seed = 1, nsim = 1000), function(x){ 
	cbind(x, BF, dist="gen.pois")
	})
simBEACH           <- do.call(rbind, simBEACH)[,-3]
BF$dist           <- "observed"
colnames(simBEACH) <- colnames(BF[,c(2,1,3)]) 
ssd                <-  rbind(simBEACH, BF)

## plot
ggplot(ssd, aes(x=BF, fill= dist)) + facet_wrap(~Beach) + 
	  geom_histogram(aes(y = ..density..), position="dodge", binwidth=1, colour=1) +
      xlab("Breeding Frequency") + ylab(NULL) + 
      scale_fill_manual(values =pal) + 
      scale_x_continuous(limits = c(NA, 10)) +
      theme_classic(base_size=20) + 
      theme(legend.title=element_blank(), 
            legend.position = "top")

## compute profile CI and mean differences
confint(BF.Model)
emmeans(BF.Model, ~ Beach, transform = "response")
contrast(emmeans(BF.Model, specs="Beach", transform = "response"),list(c(-1,1)))
(exp(0.324) / exp(0.733))  ## % underestimated is 0.6643142
exp(0.733) - exp(0.324)    ## difference is on average 0.6986679

#### ----------------------------------------- ####
#### Model for RECRUITS
#### ----------------------------------------- ####

RECRUIT <- FULL[[1]] %>% gather("Beach", "NestingStatus", -Year)
RECRUIT$Beach <- str_replace_all(RECRUIT$Beach, "Status.", "")
RECRUIT <- RECRUIT %>% transform(Year 			= as.numeric(Year), 
                                 Beach 			= as.factor(Beach),
                                 NestingStatus 	= as.factor(NestingStatus))
RECRUIT <- as.data.frame(table(RECRUIT[,1:3])) %>% filter(Freq>0)
## FOCAL = FALSE + TRUE
## ALL = NEO
## FOCAL overestimate proportion of neophytes

## count the number of nesters annually
nester.counter <- function(dat){
  
  by(dat, dat$Year, FUN=function(x){
  
  ##FILTER BY BEACH 
  beach.filt <- x[which(x$Beach == "All"),]
  
  ## SUM BY Year
  dat <- sum(beach.filt$Freq)
  
  return(dat)
  })
}
tempAcounts <- data.frame(Year = as.factor(seq(2013, 2017, 1)),
                          NN   = apply(nester.counter(RECRUIT), 1, unlist))
RECRUIT <- left_join(RECRUIT, tempAcounts)

## Count the number of recruits on the focal nestinng beach
recruit.counter <- function(dat){
  
  by(dat, dat$Year, FUN=function(x){
  
  ##FILTER BY BEACH 
  beach.filt <- x[which(x$Beach == "Focal" & x$NestingStatus != "REM"), -5]
  
  ## SUM BY Year
  dat <- sum(beach.filt$Freq)
  
  return(dat)
  })
}
tempRcounts <- data.frame(Year  = as.factor(seq(2013, 2017, 1)),
                          Beach = "Focal",
                          REC   = apply(recruit.counter(RECRUIT), 1, unlist))
RECRUIT     <- left_join(RECRUIT, tempRcounts)
RECRUIT$REC <- ifelse(is.na(RECRUIT$REC), RECRUIT$Freq, RECRUIT$RE) 
Mod.dat <- RECRUIT %>% filter(NestingStatus != "REM") %>% dplyr::select(Year, Beach, REC, NN) %>% unique()
Mod.dat$Year <- as.numeric(as.character(Mod.dat$Year))
Mod.dat$Beach <- as.factor(Mod.dat$Beach)

## write out model formulas
null.REC.form <- formula(cbind(REC, NN-REC) ~ 1)
FULL.REC.form <- formula(cbind(REC, NN-REC) ~ Beach -1)

## fit binomial models
options(na.action = "na.fail") 
null.REC.Model <- glm(null.REC.form, data = Mod.dat, family=binomial(link="logit"))
REC.Model      <- glm(FULL.REC.form, data = Mod.dat, family=binomial(link="logit"))

summary(null.REC.Model);
summary(REC.Model);  

## Final comparison
ICtab(null.REC.Model, REC.Model,
	  type = "AICc", delta = TRUE, base = TRUE, logLik= TRUE,
	  weights=T, 
	  mnames=c("NULL", "~Beach"))
## beach model is best out of the set

## compute the evidence ratio
evidence(aictab(cand.set = list(null.REC.Model, REC.Model), 
                modnames = c("NULL", "RECRUIT"), second.ord = TRUE))

## compute profile CI and mean differences
confint(REC.Model)
emmeans(REC.Model, ~ 1, transform = "response")
emmeans(REC.Model, specs= c("Beach"), transform = "response")
contrast(emmeans(REC.Model, specs="Beach", transform = "response"), list(c(-1,1)))

### TABLE OF ALL THE MODELS
tab_model(null.BF.Model,  BF.Model,
          show.stat = T, show.se = T, show.dev = T, show.loglik = T, 
          show.icc =T, show.ngroups = T, show.re.var = T, p.style = "numeric",
          CSS = css_theme("regression"), collapse.ci = T, #transform = NULL,
          #bootstrap = T, seed = 123, iterations = 1000,
          vcov.type = "HC0", digits = 2, p.val = "wald")
tab_model(null.RMI.Model, RMI.Model,
          show.stat = T, show.se = T, show.dev = T, show.loglik = T, 
          show.icc =T, show.ngroups = T, show.re.var = T, p.style = "numeric",
          CSS = css_theme("regression"), collapse.ci = T, #transform = NULL,
          #bootstrap = T, seed = 123, iterations = 1000,
          vcov.type = "HC0", digits = 2, p.val = "wald")
tab_model(null.OCF.Model,  OCF.Model, 
          show.stat = T, show.se = T, show.dev = T, show.loglik = T, 
          show.icc =T, show.ngroups = T, show.re.var = T, p.style = "numeric",
          CSS = css_theme("regression"), collapse.ci = T, #transform = NULL,
          #bootstrap = T, seed = 123, iterations = 1000,
          vcov.type = "HC0", digits = 2, p.val = "wald")
tab_model(null.ECF.Model,  ECF.Model, 
          show.stat = T, show.se = T, show.dev = T, show.loglik = T, 
          show.icc =T, show.ngroups = T, show.re.var = T, p.style = "numeric",
          CSS = css_theme("regression"), collapse.ci = T, #transform = NULL,
          #bootstrap = T, seed = 123, iterations = 1000,
          vcov.type = "HC0", digits = 2, p.val = "wald")
tab_model(null.REC.Model,  REC.Model, 
          show.stat = T, show.se = T, show.dev = T, show.loglik = T, 
          show.icc =T, show.ngroups = T, show.re.var = T, p.style = "numeric",
          CSS = css_theme("regression"), collapse.ci = T, #transform = NULL,
          #bootstrap = T, seed = 123, iterations = 1000,
          vcov.type = "HC0", digits = 2, p.val = "wald")

## Plot model coefficients
## count models
plot_summs(OCF.Model, ECF.Model, RMI.Model, BF.Model, 
            ci_level = 0.95, scale = TRUE, robust = TRUE, 
             confint = TRUE, digits = 3, inner_ci_level = .9, exp=T, grid = TRUE, 
            model.names = c("OCF Model w/ BEACH","ECF Model w/ BEACH",
                             "RMI Model w/ BEACH","BF Model w/ BEACH")) +
             scale_x_continuous(breaks=seq(1, 5, 0.3)) +
             theme_sjplot2() + labs(x="Parameter Estimates", y="") + theme(text = element_text(size=25))

## proportion model
plot_summs(REC.Model, ci_level = 0.95, robust = TRUE, plot.distributions=T,
            confint = TRUE, digits = 3, inner_ci_level = .9, grid = TRUE) +
           theme_sjplot2() + labs(x="Parameter Estimates", y="") + theme(text = element_text(size=25))

# Compute disgnostic stats (square root of the variance of the residuals) for each model 
summaryALL <- data.frame(models=c("NULL OCF","OCF w/ BEACH",
                                  "NULL ECF","ECF w/ BEACH",
                                  "NULL RMI","RMI w/ BEACH",
                                  "NULL BF","BF w/ BEACH", 
                                  "NULL REC","REC w/ BEACH"), 
                         rmse_value = NA, AICc_value = NA, Deviance_Exp = NA, RRS_value = NA)
mod_names <- list(null.OCF.Model, OCF.Model, null.ECF.Model, ECF.Model, 
                  null.RMI.Model, RMI.Model, null.BF.Model, BF.Model, 
                  null.REC.Model, REC.Model)
library(ie2misc); library(qpcR)
for (i in seq_along(mod_names)) {
  summaryALL$rmse_value[i]   <- sjstats::rmse(mod_names[[i]])
  summaryALL$AICc_value[i]   <- AICc(mod_names[[i]])
  summaryALL$Deviance_Exp[i] <- AICc(mod_names[[i]])
  summaryALL$RRS_value[i]    <- sum(residuals(mod_names[[i]])^2) #RSS(mod_names[[i]]) # Weighted sum of squares
}
summaryALL$delta_AICc <- c(
		ICtab(null.OCF.Model, OCF.Model, k=2, sort=F, base=TRUE, weights=TRUE, logLik=TRUE,  type = "AICc")$dAICc,
		ICtab(null.ECF.Model, ECF.Model, k=2, sort=F, base=TRUE, weights=TRUE, logLik=TRUE, type = "AICc")$dAICc, 
		ICtab(null.RMI.Model, RMI.Model, k=2, sort=F, base=TRUE, weights=TRUE, logLik=TRUE,  type = "AICc")$dAICc,
		ICtab(null.BF.Model, BF.Model, k=2, sort=F, base=TRUE, weights=TRUE, logLik=TRUE,  type = "AICc")$dAICc,
		ICtab(null.REC.Model, REC.Model, k=2, sort=F, base=TRUE, weights=TRUE, logLik=TRUE,  type = "AICc")$dAICc)
summaryALL
