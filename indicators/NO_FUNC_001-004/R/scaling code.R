#### calculating scaled and non-truncated values for the indicators based on the dataset ####
for (i in 1:nrow(ANO.all) ) {  #
  tryCatch({
    print(i)
    print(paste(ANO.all$ano_flate_id[i]))
    print(paste(ANO.all$ano_punkt_id[i]))
    #    ANO.all$Hovedoekosystem_sirkel[i]
    #    ANO.all$Hovedoekosystem_rute[i]
    
    
    
    # if the ANO.hovedtype exists in the wet_mount_forest_seminat reference
    if ( ANO.all$hovedtype_rute[i] %in% unique(sub("\\-.*", "", wet_mount_forest_seminat.ref.cov.val$grunn)) ) {
      
      # if there is any species present in current ANO point  
      if ( length(ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),'Species']) > 0 ) {
        
        
        # Light
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Light')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Light),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Light'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Light1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Light1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Light1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Light1'] <- scal() 
          results.ANO[['non-truncated']][i,'Light1'] <- scal.2() 
          results.ANO[['original']][i,'Light1'] <- val 
          
          # upper part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Light2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Light2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Light2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Light2'] <- scal() 
          results.ANO[['non-truncated']][i,'Light2'] <- scal.2() 
          results.ANO[['original']][i,'Light2'] <- val
        }
        
        
        # Moisture
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Moisture')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Moisture),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Moisture'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Moist1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Moist1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Moist1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Moist1'] <- scal() 
          results.ANO[['non-truncated']][i,'Moist1'] <- scal.2() 
          results.ANO[['original']][i,'Moist1'] <- val 
          
          # upper part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Moist2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Moist2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Moist2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Moist2'] <- scal() 
          results.ANO[['non-truncated']][i,'Moist2'] <- scal.2() 
          results.ANO[['original']][i,'Moist2'] <- val
        }
        
        
        # Soil_reaction_pH
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Soil_reaction_pH')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Soil_reaction_pH),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Soil_reaction_pH'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='pH1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='pH1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='pH1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'pH1'] <- scal() 
          results.ANO[['non-truncated']][i,'pH1'] <- scal.2() 
          results.ANO[['original']][i,'pH1'] <- val 
          
          # upper part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='pH2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='pH2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='pH2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'pH2'] <- scal() 
          results.ANO[['non-truncated']][i,'pH2'] <- scal.2() 
          results.ANO[['original']][i,'pH2'] <- val
        }
        
        
        # Nitrogen
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Nitrogen')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Nitrogen),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Nitrogen'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Nitrogen1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Nitrogen1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Nitrogen1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Nitrogen1'] <- scal() 
          results.ANO[['non-truncated']][i,'Nitrogen1'] <- scal.2() 
          results.ANO[['original']][i,'Nitrogen1'] <- val 
          
          # upper part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Nitrogen2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Nitrogen2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Nitrogen2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Nitrogen2'] <- scal() 
          results.ANO[['non-truncated']][i,'Nitrogen2'] <- scal.2() 
          results.ANO[['original']][i,'Nitrogen2'] <- val
        }
        

        # Phosphorus
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Phosphorus')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Phosphorus),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Phosphorus'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Phosphorus1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Phosphorus1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Phosphorus1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Phosphorus1'] <- scal() 
          results.ANO[['non-truncated']][i,'Phosphorus1'] <- scal.2() 
          results.ANO[['original']][i,'Phosphorus1'] <- val 
          
          # upper part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Phosphorus2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Phosphorus2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Phosphorus2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Phosphorus2'] <- scal() 
          results.ANO[['non-truncated']][i,'Phosphorus2'] <- scal.2() 
          results.ANO[['original']][i,'Phosphorus2'] <- val
        }
        
        
        # Grazing_mowing
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Grazing_mowing')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Grazing_mowing),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Grazing_mowing'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Grazing_mowing1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Grazing_mowing1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Grazing_mowing1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Grazing_mowing1'] <- scal() 
          results.ANO[['non-truncated']][i,'Grazing_mowing1'] <- scal.2() 
          results.ANO[['original']][i,'Grazing_mowing1'] <- val 
          
          # upper part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Grazing_mowing2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Grazing_mowing2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Grazing_mowing2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Grazing_mowing2'] <- scal() 
          results.ANO[['non-truncated']][i,'Grazing_mowing2'] <- scal.2() 
          results.ANO[['original']][i,'Grazing_mowing2'] <- val
        }
        
        
        # Soil_disturbance
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Soil_disturbance')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Soil_disturbance),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Soil_disturbance'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Soil_disturbance1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Soil_disturbance1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Soil_disturbance1' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Soil_disturbance1'] <- scal() 
          results.ANO[['non-truncated']][i,'Soil_disturbance1'] <- scal.2() 
          results.ANO[['original']][i,'Soil_disturbance1'] <- val 
          
          # upper part of distribution
          ref <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Soil_disturbance2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
          lim <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Soil_disturbance2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          maxmin <- wet_mount_forest_seminat.ref.cov.val[wet_mount_forest_seminat.ref.cov.val$Ind=='Soil_disturbance2' & wet_mount_forest_seminat.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Soil_disturbance2'] <- scal() 
          results.ANO[['non-truncated']][i,'Soil_disturbance2'] <- scal.2() 
          results.ANO[['original']][i,'Soil_disturbance2'] <- val
        }

      }
    }
    
    
    # if the ANO.hovedtype is a mountain ecosystem qualifying for the Heat_requirement indicator
    if ( ANO.all$hovedtype_rute[i] %in% c('T3','T7','T14','T22') ) {
      
      # if there is any species present in current ANO point  
      if ( length(ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),'Species']) > 0 ) {
        
        
        #Heat requirement overhang
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Heat_requirement')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Heat_requirement) & dat$art_dekning>0,]
        
        if ( nrow(dat)>0 ) {
          
          dat$Dekning <- dat$Dekning/sum(dat$Dekning)
          val <- sum(dat[dat$Heat_requirement>=heat.scal.tab.boot[heat.scal.tab.boot$NiN==as.character(unique(dat$Kartleggingsenhet_rute)),'ind_level'],'Dekning'])
          ref <- heat.scal.tab.boot[heat.scal.tab.boot$NiN==as.character(unique(dat$Kartleggingsenhet_rute)),'ref']
          lim <- heat.scal.tab.boot[heat.scal.tab.boot$NiN==as.character(unique(dat$Kartleggingsenhet_rute)),'lim']
          maxmin <- heat.scal.tab.boot[heat.scal.tab.boot$NiN==as.character(unique(dat$Kartleggingsenhet_rute)),'maxmin']
          
          # coercing x into results.mount dataframe
          result.mount[i,'Heat'] <- scal.2() 
          
        } else {
          result.mount[i,'Heat'] <- 1 
        }
        
        
      }
    }
    
    
    # if the ANO.hovedtype exists in the natopen reference
    if ( ANO.all$hovedtype_rute[i] %in% unique(sub("\\-.*", "", natopen.ref.cov.val$grunn)) ) {
      
      # if there is any species present in current ANO point  
      if ( length(ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),'Species']) > 0 ) {
        
        # Grime's C
        
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','CC')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$CC),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'CC'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'CC1'] <- scal() 
          results.ANO[['non-truncated']][i,'CC1'] <- scal.2() 
          results.ANO[['original']][i,'CC1'] <- val 
          
          # upper part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }          
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='CC2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'CC2'] <- scal() 
          results.ANO[['non-truncated']][i,'CC2'] <- scal.2() 
          results.ANO[['original']][i,'CC2'] <- val
        }
        
        
        # Grime's S
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','SS')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$SS),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'SS'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }          
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'SS1'] <- scal() 
          results.ANO[['non-truncated']][i,'SS1'] <- scal.2() 
          results.ANO[['original']][i,'SS1'] <- val 
          
          # upper part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }          
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='SS2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'SS2'] <- scal() 
          results.ANO[['non-truncated']][i,'SS2'] <- scal.2() 
          results.ANO[['original']][i,'SS2'] <- val
        }
        
        
        # Grime's R
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','RR')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$RR),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'RR'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }          
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'RR1'] <- scal() 
          results.ANO[['non-truncated']][i,'RR1'] <- scal.2() 
          results.ANO[['original']][i,'RR1'] <- val 
          
          # upper part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }          
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='RR2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'RR2'] <- scal() 
          results.ANO[['non-truncated']][i,'RR2'] <- scal.2() 
          results.ANO[['original']][i,'RR2'] <- val
        }
        
        
        # Light
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Light')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Light),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Light'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }            
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Light1'] <- scal() 
          results.ANO[['non-truncated']][i,'Light1'] <- scal.2() 
          results.ANO[['original']][i,'Light1'] <- val 
          
          # upper part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }            
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Light2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Light2'] <- scal() 
          results.ANO[['non-truncated']][i,'Light2'] <- scal.2() 
          results.ANO[['original']][i,'Light2'] <- val
        }
        
        
        # Nitrogen
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Nitrogen')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Nitrogen),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Nitrogen'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }            
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Nitrogen1'] <- scal() 
          results.ANO[['non-truncated']][i,'Nitrogen1'] <- scal.2() 
          results.ANO[['original']][i,'Nitrogen1'] <- val 
          
          # upper part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }            
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Nitrogen2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Nitrogen2'] <- scal() 
          results.ANO[['non-truncated']][i,'Nitrogen2'] <- scal.2() 
          results.ANO[['original']][i,'Nitrogen2'] <- val
        }
        
        
        # Soil_disturbance
        dat <- ANO.sp.ind[ANO.sp.ind$parentglobalid==as.character(ANO.all$globalid[i]),c('art_dekning','Soil_disturbance')]
        results.ANO[['original']][i,'richness'] <- nrow(dat)
        dat <- dat[!is.na(dat$Soil_disturbance),]
        
        if ( nrow(dat)>0 ) {
          
          val <- sum(dat[,'art_dekning'] * dat[,'Soil_disturbance'],na.rm=T) / sum(dat[,'art_dekning'],na.rm=T)
          # lower part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance1' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }           
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance1' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Soil_disturbance1'] <- scal() 
          results.ANO[['non-truncated']][i,'Soil_disturbance1'] <- scal.2() 
          results.ANO[['original']][i,'Soil_disturbance1'] <- val 
          
          # upper part of distribution
          if( ANO.all$kartleggingsenhet_1m2[i] %in% c("T2-C-7","T2-C-8") ) {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance2' & natopen.ref.cov.val$grunn==paste(as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),"_BN",sep=""),'Gv']
          } else {
            ref <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Rv']
            lim <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'Gv']
          }
          maxmin <- natopen.ref.cov.val[natopen.ref.cov.val$Ind=='Soil_disturbance2' & natopen.ref.cov.val$grunn==as.character(results.ANO[['original']][i,"kartleggingsenhet_1m2"]),'maxmin']
          # coercing x into results.ANO dataframe
          results.ANO[['scaled']][i,'Soil_disturbance2'] <- scal() 
          results.ANO[['non-truncated']][i,'Soil_disturbance2'] <- scal.2() 
          results.ANO[['original']][i,'Soil_disturbance2'] <- val
        }
        
      
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# for using both sides of the plant indicators
results.ANO[['2-sided']] <- results.ANO[['non-truncated']]

# remove values >1 for 2-sided indicators
results.ANO[['2-sided']]$Light1[results.ANO[['2-sided']]$Light1>1] <- NA
results.ANO[['2-sided']]$Light2[results.ANO[['2-sided']]$Light2>1] <- NA

results.ANO[['2-sided']]$Moist1[results.ANO[['2-sided']]$Moist1>1] <- NA
results.ANO[['2-sided']]$Moist2[results.ANO[['2-sided']]$Moist2>1] <- NA

results.ANO[['2-sided']]$pH1[results.ANO[['2-sided']]$pH1>1] <- NA
results.ANO[['2-sided']]$pH2[results.ANO[['2-sided']]$pH2>1] <- NA

results.ANO[['2-sided']]$Nitrogen1[results.ANO[['2-sided']]$Nitrogen1>1] <- NA
results.ANO[['2-sided']]$Nitrogen2[results.ANO[['2-sided']]$Nitrogen2>1] <- NA

