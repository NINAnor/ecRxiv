We can calculate national and region-wise means and their related standard errors. But note that calculating a simple mean would be inappropriate for these data. This is because:
  (i) the scaled data are bound between 0 and 1, and thus follow a beta-distribution rather than a Gaussian one, and
(ii) the ANO dataset has a nested structure

Therefore, we need to (i) use a beta-model, that (ii) can account for the nested structure of the data.
Here, we apply the following function using either a glmmTMB null-model with a beta-distribution, logit link, and the nesting as a random intercept, or a simple betareg null-model with logit link if the nesting is not extensive enough for a mixed model.
```{r}
expit <- function(L) exp(L) / (1+exp(L)) # since the beta-models use a logit link, we need to calculate the estimates back to the identity scale

# the function performs a glmmTMB if there is >= 5 random levels in the nesting structure
# if that is not the case, then the function performs a betareg if there is >= 2 observations
# if that is not the case either, then the function returns the value of the single observation with NA standard error

indmean.beta <- function(df) {
  
  st_geometry(df) <- NULL
  colnames(df) <- c("y","ran")
  
  # add/remove small increment to/from 0 or 1 for beta-regression
  df <- df %>% 
    mutate(y = ifelse(y == 0, 0.00001, y)) %>%
    mutate(y = ifelse(y == 1, 0.99999, y))
  
  # at least 2 observations to do a regression
  if ( nrow(df[!is.na(df[,"y"]),]) >= 2 ) {
    # at least 5 random levels to do a random effects regression 
    if ( nrow(unique(df[!is.na(df[,"y"]),"ran"])) >=5 ) {
      # glmmTMB
      mod1 <- glmmTMB(y ~ 1 +(1|ran), family=beta_family(), data=df)
      
      return(c(
        expit(summary( mod1 )$coefficients$cond[1]),
        
        expit( summary( mod1 )$coefficients$cond[1] + 
                 summary( mod1 )$coefficients$cond[2] )-
          expit( summary( mod1 )$coefficients$cond[1] ),
        
        nrow(df[!is.na(df$y),]),
        summary( mod1 )$coefficients$cond[1],
        summary( mod1 )$coefficients$cond[2]
      ))
      
    } else {
      # betareg
      mod2 <- betareg(y ~ 1, data=df)
      
      return(c(
        expit(summary( mod2 )$coefficients$mean[1]),
        expit( summary( mod2 )$coefficients$mean[1] + 
                 summary( mod2 )$coefficients$mean[2] )-
          expit( summary( mod2 )$coefficients$mean[1] ),
        nrow(df[!is.na(df$y),]),
        summary( mod2 )$coefficients$mean[1],
        summary( mod2 )$coefficients$mean[2]
      ))
      
    }
    
  } else {
    # no regerssion run
    #return only original value and n=1
    #return(c(df$y,NA,nrow(df[!is.na(df$y),]),NA,NA))
    #return only NAs
    return(c(NA,NA,nrow(df[!is.na(df$y),]),NA,NA))
    
  }
  
}

```

We define dataframes to hold the results and populate them with calculated indices

```{r, echo = FALSE}
## forest ecosystems
# list to hold the results
NO_FUNC_004_2022to2024 <- list()
NO_FUNC_004_2019to2021 <- list()
NO_FUNC_004_2022to2024[['mean']] <- NO_FUNC_004_2022to2024[['se']] <- NO_FUNC_004_2022to2024[['n']] <- NO_FUNC_004_2019to2021[['mean']] <- NO_FUNC_004_2019to2021[['se']] <- NO_FUNC_004_2019to2021[['n']] <-
  data.frame(Indicator=c("fpci.min","Moist1","Moist2","Nitrogen1","Nitrogen2"),
             'Norway'=NA,'Northern.Norway'=NA, 'Central.Norway'=NA, 'Western.Norway'=NA,'Eastern.Norway'=NA, 'Southern.Norway'=NA)
# calculation of the beta-means
for (i in 1:length(NO_FUNC_004_2022to2024[['mean']]$Indicator) ) {
  
  res <-  indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_004_2022to2024[['mean']]$Indicator[i]  & res.ANO.long$hovedoekosystem_rute=="Forest" & res.ANO.long$aar %in% c(2022, 2023, 2024),c("scaled_value","ano_flate_id")])
  NO_FUNC_004_2022to2024[['mean']][i,2] <- res[1]
  NO_FUNC_004_2022to2024[['se']][i,2] <- res[2]
  NO_FUNC_004_2022to2024[['n']][i,2] <- res[3]
  
  res <-  indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_004_2019to2021[['mean']]$Indicator[i]  & res.ANO.long$hovedoekosystem_rute=="Forest" & res.ANO.long$aar %in% c(2019, 2020, 2021),c("scaled_value","ano_flate_id")])
  NO_FUNC_004_2019to2021[['mean']][i,2] <- res[1]
  NO_FUNC_004_2019to2021[['se']][i,2] <- res[2]
  NO_FUNC_004_2019to2021[['n']][i,2] <- res[3]
  
  
  for (j in names(NO_FUNC_004_2022to2024[['mean']])[3:7] ) {
    res <- indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_004_2022to2024[['mean']]$Indicator[i] & res.ANO.long$hovedoekosystem_rute=="Forest" &  res.ANO.long$aar %in% c(2022, 2023, 2024) & res.ANO.long$region==j,c("scaled_value","ano_flate_id")])
    NO_FUNC_004_2022to2024[['mean']][i,j] <- res[1]
    NO_FUNC_004_2022to2024[['se']][i,j] <- res[2]
    NO_FUNC_004_2022to2024[['n']][i,j] <- res[3]
    
    res <- indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_004_2019to2021[['mean']]$Indicator[i] & res.ANO.long$hovedoekosystem_rute=="Forest" &  res.ANO.long$aar %in% c(2019, 2020, 2021) & res.ANO.long$region==j,c("scaled_value","ano_flate_id")])
    NO_FUNC_004_2019to2021[['mean']][i,j] <- res[1]
    NO_FUNC_004_2019to2021[['se']][i,j] <- res[2]
    NO_FUNC_004_2019to2021[['n']][i,j] <- res[3]
  }
}

## mountain ecosystems
# list to hold the results
NO_FUNC_002_2022to2024 <- list()
NO_FUNC_002_2019to2021 <- list()
NO_FUNC_002_2022to2024[['mean']] <- NO_FUNC_002_2022to2024[['se']] <- NO_FUNC_002_2022to2024[['n']] <- NO_FUNC_002_2019to2021[['mean']] <- NO_FUNC_002_2019to2021[['se']] <- NO_FUNC_002_2019to2021[['n']] <-
  data.frame(Indicator=c("fpci.min","Heat_requirement2","Light1","Light2","Nitrogen1","Nitrogen2"),
             'Norway'=NA,'Northern.Norway'=NA, 'Central.Norway'=NA, 'Western.Norway'=NA,'Eastern.Norway'=NA, 'Southern.Norway'=NA)
# calculation of the beta-means
for (i in 1:length(NO_FUNC_002_2022to2024[['mean']]$Indicator) ) {
  
  res <-  indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_002_2022to2024[['mean']]$Indicator[i]  & res.ANO.long$hovedoekosystem_rute=="Mountain" & res.ANO.long$aar %in% c(2022, 2023, 2024),c("scaled_value","ano_flate_id")])
  NO_FUNC_002_2022to2024[['mean']][i,2] <- res[1]
  NO_FUNC_002_2022to2024[['se']][i,2] <- res[2]
  NO_FUNC_002_2022to2024[['n']][i,2] <- res[3]
  
  res <-  indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_002_2019to2021[['mean']]$Indicator[i]  & res.ANO.long$hovedoekosystem_rute=="Mountain" & res.ANO.long$aar %in% c(2019, 2020, 2021),c("scaled_value","ano_flate_id")])
  NO_FUNC_002_2019to2021[['mean']][i,2] <- res[1]
  NO_FUNC_002_2019to2021[['se']][i,2] <- res[2]
  NO_FUNC_002_2019to2021[['n']][i,2] <- res[3]
  
  
  for (j in names(NO_FUNC_002_2022to2024[['mean']])[3:7] ) {
    res <- indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_002_2022to2024[['mean']]$Indicator[i] & res.ANO.long$hovedoekosystem_rute=="Mountain" &  res.ANO.long$aar %in% c(2022, 2023, 2024) & res.ANO.long$region==j,c("scaled_value","ano_flate_id")])
    NO_FUNC_002_2022to2024[['mean']][i,j] <- res[1]
    NO_FUNC_002_2022to2024[['se']][i,j] <- res[2]
    NO_FUNC_002_2022to2024[['n']][i,j] <- res[3]
    
    res <- indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_002_2019to2021[['mean']]$Indicator[i] & res.ANO.long$hovedoekosystem_rute=="Mountain" &  res.ANO.long$aar %in% c(2019, 2020, 2021) & res.ANO.long$region==j,c("scaled_value","ano_flate_id")])
    NO_FUNC_002_2019to2021[['mean']][i,j] <- res[1]
    NO_FUNC_002_2019to2021[['se']][i,j] <- res[2]
    NO_FUNC_002_2019to2021[['n']][i,j] <- res[3]
  }
}

## wetland ecosystems
# list to hold the results
NO_FUNC_003_2022to2024 <- list()
NO_FUNC_003_2019to2021 <- list()
NO_FUNC_003_2022to2024[['mean']] <- NO_FUNC_003_2022to2024[['se']] <- NO_FUNC_003_2022to2024[['n']] <- NO_FUNC_003_2019to2021[['mean']] <- NO_FUNC_003_2019to2021[['se']] <- NO_FUNC_003_2019to2021[['n']] <-
  data.frame(Indicator=c("fpci.min","Moist1","Moist2","Light1","Light2","pH1","pH2","Nitrogen1","Nitrogen2"),
             'Norway'=NA,'Northern.Norway'=NA, 'Central.Norway'=NA, 'Western.Norway'=NA,'Eastern.Norway'=NA, 'Southern.Norway'=NA)
# calculation of the beta-means
for (i in 1:length(NO_FUNC_003_2022to2024[['mean']]$Indicator) ) {
  
  res <-  indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_003_2022to2024[['mean']]$Indicator[i]  & res.ANO.long$hovedoekosystem_rute=="Wetland" & res.ANO.long$aar %in% c(2022, 2023, 2024),c("scaled_value","ano_flate_id")])
  NO_FUNC_003_2022to2024[['mean']][i,2] <- res[1]
  NO_FUNC_003_2022to2024[['se']][i,2] <- res[2]
  NO_FUNC_003_2022to2024[['n']][i,2] <- res[3]
  
  res <-  indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_003_2019to2021[['mean']]$Indicator[i]  & res.ANO.long$hovedoekosystem_rute=="Wetland" & res.ANO.long$aar %in% c(2019, 2020, 2021),c("scaled_value","ano_flate_id")])
  NO_FUNC_003_2019to2021[['mean']][i,2] <- res[1]
  NO_FUNC_003_2019to2021[['se']][i,2] <- res[2]
  NO_FUNC_003_2019to2021[['n']][i,2] <- res[3]
  
  
  for (j in names(NO_FUNC_003_2022to2024[['mean']])[3:7] ) {
    res <- indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_003_2022to2024[['mean']]$Indicator[i] & res.ANO.long$hovedoekosystem_rute=="Wetland" &  res.ANO.long$aar %in% c(2022, 2023, 2024) & res.ANO.long$region==j,c("scaled_value","ano_flate_id")])
    NO_FUNC_003_2022to2024[['mean']][i,j] <- res[1]
    NO_FUNC_003_2022to2024[['se']][i,j] <- res[2]
    NO_FUNC_003_2022to2024[['n']][i,j] <- res[3]
    
    res <- indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_003_2019to2021[['mean']]$Indicator[i] & res.ANO.long$hovedoekosystem_rute=="Wetland" &  res.ANO.long$aar %in% c(2019, 2020, 2021) & res.ANO.long$region==j,c("scaled_value","ano_flate_id")])
    NO_FUNC_003_2019to2021[['mean']][i,j] <- res[1]
    NO_FUNC_003_2019to2021[['se']][i,j] <- res[2]
    NO_FUNC_003_2019to2021[['n']][i,j] <- res[3]
  }
}

## open lowland ecosystems
# list to hold the results
NO_FUNC_001_2022to2024 <- list()
NO_FUNC_001_2019to2021 <- list()
NO_FUNC_001_2022to2024[['mean']] <- NO_FUNC_001_2022to2024[['se']] <- NO_FUNC_001_2022to2024[['n']] <- NO_FUNC_001_2019to2021[['mean']] <- NO_FUNC_001_2019to2021[['se']] <- NO_FUNC_001_2019to2021[['n']] <-
  data.frame(Indicator=c("fpci.min","CC1","CC2","SS1","SS2","RR1","RR2","Moist1","Moist2","Light1","Light2","pH1","pH2","Nitrogen1","Nitrogen2","Phosphorus1","Phosphorus2","Grazing_mowing1","Grazing_mowing2","Soil_disturbance1","Soil_disturbance2"),
             'Norway'=NA,'Northern.Norway'=NA, 'Central.Norway'=NA, 'Western.Norway'=NA,'Eastern.Norway'=NA, 'Southern.Norway'=NA)
# calculation of the beta-means
for (i in 1:length(NO_FUNC_001_2022to2024[['mean']]$Indicator) ) {
  
  res <-  indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_001_2022to2024[['mean']]$Indicator[i]  & res.ANO.long$hovedoekosystem_rute %in% c("Seminat","Natopen") & res.ANO.long$aar %in% c(2022, 2023, 2024),c("scaled_value","ano_flate_id")])
  NO_FUNC_001_2022to2024[['mean']][i,2] <- res[1]
  NO_FUNC_001_2022to2024[['se']][i,2] <- res[2]
  NO_FUNC_001_2022to2024[['n']][i,2] <- res[3]
  
  res <-  indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_001_2019to2021[['mean']]$Indicator[i]  & res.ANO.long$hovedoekosystem_rute %in% c("Seminat","Natopen") & res.ANO.long$aar %in% c(2019, 2020, 2021),c("scaled_value","ano_flate_id")])
  NO_FUNC_001_2019to2021[['mean']][i,2] <- res[1]
  NO_FUNC_001_2019to2021[['se']][i,2] <- res[2]
  NO_FUNC_001_2019to2021[['n']][i,2] <- res[3]
  
  
  for (j in names(NO_FUNC_001_2022to2024[['mean']])[3:7] ) {
    res <- indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_001_2022to2024[['mean']]$Indicator[i] & res.ANO.long$hovedoekosystem_rute %in% c("Seminat","Natopen") &  res.ANO.long$aar %in% c(2022, 2023, 2024) & res.ANO.long$region==j,c("scaled_value","ano_flate_id")])
    NO_FUNC_001_2022to2024[['mean']][i,j] <- res[1]
    NO_FUNC_001_2022to2024[['se']][i,j] <- res[2]
    NO_FUNC_001_2022to2024[['n']][i,j] <- res[3]
    
    res <- indmean.beta(df=res.ANO.long[res.ANO.long$fp_ind==NO_FUNC_001_2019to2021[['mean']]$Indicator[i] & res.ANO.long$hovedoekosystem_rute %in% c("Seminat","Natopen") &  res.ANO.long$aar %in% c(2019, 2020, 2021) & res.ANO.long$region==j,c("scaled_value","ano_flate_id")])
    NO_FUNC_001_2019to2021[['mean']][i,j] <- res[1]
    NO_FUNC_001_2019to2021[['se']][i,j] <- res[2]
    NO_FUNC_001_2019to2021[['n']][i,j] <- res[3]
  }
}

```