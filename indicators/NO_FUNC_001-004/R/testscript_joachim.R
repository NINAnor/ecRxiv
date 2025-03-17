indBoot.HeatOverhang <- function(sp,abun,ind, iter,obl,rat) {
  
  # data frame to hold the results
  heat.ind.b <- matrix(nrow=iter,ncol=length(colnames(abun)))
  colnames(heat.ind.b) <- colnames(abun)
  heat.ind.b <- as.data.frame(heat.ind.b)  
  
  # looping through the ecosystem types  
  for (j in 1:length(colnames(abun))) {
    
    # creating the species data for the respective ecosystem type
    dat <- cbind(sp,ind,abun[,j])
    dat <- dat[dat[,3]>0,]            # only species that are present in the ecoystem
    dat <- dat[!is.na(dat[,2]),]      # only species that have Heat requirement values
    dat <- as.data.frame(dat)
    colnames(dat) <- c('sp','ind','abun')  
          
    # calculating the cumulative sums for Heat requirement levels 9:1
    if ( nrow(dat)>2 ) {
      
      df <- dat[,c(2,3)]
      df[,2] <- df[,2]/sum(df[,2]) # scaling to total cover of 100%

      abun.cumsums <- abun.sums <- data.frame(ind=rev(1:13),cumsum=NA)
      
      df <- df %>% add_row(ind=1:13, abun=rep(0,13))
      abun.sums[,2] <- rev(with(df,tapply(abun,ind,sum)))
      abun.cumsums[,2] <- cumsum(abun.sums[,2])

      
    # bootstrapping, can specify to always keep certain abundance classes (obl) and how many species to sample (rat)
    for (i in 1:iter) {
      
      speciesSample <- sample(dat$sp[dat[,3] < obl], size=round( (length(dat$sp)-length(dat$sp[dat[,3] >= (obl)])) *rat,0), replace=F)  
      dat.b <- rbind(dat[dat[,3] >= (obl),],
                     dat[match(speciesSample,dat$sp),]
      )
      dat.b[,2] <- as.numeric(dat.b[,2])
      dat.b[,3] <- as.numeric(dat.b[,3])

      # calculating the cumulative cover of species with a certain level of heat requirement and above
      if ( nrow(dat.b)>2 ) {
        
        df <- dat.b[,c(2,3)]
        df[,2] <- df[,2]/sum(df[,2]) # scaling to total cover of 100%
        
        abun.cumsums.b <- abun.sums.b <- data.frame(ind=rev(1:13),cumsum=NA)

        df <- df %>% add_row(ind=1:13, abun=rep(0,13))
        abun.sums.b[,2] <- rev(with(df,tapply(abun,ind,sum)))
        abun.cumsums.b[,2] <- cumsum(abun.sums.b[,2])
        heat.ind.b[i,j] <- sum(abun.cumsums.b[1:11,2] - abun.cumsums[1:11,2])

      } else {heat.ind.b[i,j] <- NA}
      
      print(paste(i,"",j)) 
    }
   
    }
     
  }
  return(heat.ind.b)
}

#sp=,abun=,ind=NiN.wet_mount_forest_seminat.cov[,134:144],
#iter=1000,obl=1,rat=2/3,var.abun=T

indBoot.HeatOverhang(sp=NiN.wet_mount_forest_seminat.cov[,1],
                     abun=NiN.wet_mount_forest_seminat.cov[,43:69],
                     ind=NiN.wet_mount_forest_seminat.cov[,137],
                     iter=3,obl=4/5,rat=1/2)

heat.ind.b <- indBoot.HeatOverhang(sp=NiN.wet_mount_forest_seminat.cov[,1],
                                   abun=NiN.wet_mount_forest_seminat.cov[,43:69],
                                   ind=NiN.wet_mount_forest_seminat.cov[,137],
                                   iter=1000,obl=4/5,rat=1/2)




#        plot(abun.cumsums[,1],abun.cumsums[,2],
#             type='b',
#             xlab="Heat_requirement",
#             ylab="Cumulative relative abundance")
#        points(abun.cumsums.b[,1],abun.cumsums.b[,2],
#             type='b',
#             xlab="Heat_requirement",
#             ylab="Cumulative relative abundance",
#             co='red')


wet_mount_forest_seminat.ref.cov[["Heat_requirement"]] <- heat.ind.b
