# species dictionary function
clean_species_from_dictionary <- function(x, dict = species_dict_pattern) {
  x_clean <- x
  for (k in seq_len(nrow(dict))) {
    pat  <- dict$pattern[k]
    repl <- dict$replacement[k]
    x_clean[grepl(pat, x_clean, fixed = TRUE)] <- repl
  }
  x_clean
}


# bootstrapping function
indBoot.freq <- function(sp, abun, ind, iter, obl, rat = 2/3,var.abun = F) {
  
  ind.b <- matrix(nrow=iter,ncol=length(colnames(abun)))
  colnames(ind.b) <- colnames(abun)
  ind.b <- as.data.frame(ind.b)  
  
  ind <- as.data.frame(ind)
  ind.list <- as.list(1:length(colnames(ind)))
  names(ind.list) <- colnames(ind)
  
  for (k in 1:length(colnames(ind)) ) {
    ind.list[[k]] <- ind.b }
  
  for (j in 1:length(colnames(abun)) ) {
    
    dat <- cbind(sp,abun[,j],ind)
    dat <- dat[dat[,2]>0,]            # only species that are present in the ecosystem
    dat <- dat[!is.na(dat[,3]),]      # only species that have indicator values
    
    for (i in 1:iter) {
      
      speciesSample <- sample(dat$sp[dat[,2] < obl], size=round( (length(dat$sp)-length(dat$sp[dat[,2]>=obl])) *rat,0), replace=F)  
      dat.b <- rbind(dat[dat[,2] >= obl,],
                     dat[match(speciesSample,dat$sp),]
      )
      
      if (var.abun==T) {
        for (m in 1:nrow(coverscale[-1,]) ) {
          xxx <- dat.b[dat.b[,2]==coverscale[-1,][m,2],2]
          if ( m==1 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0) ,size=length(xxx),replace=T) }
          if ( m==2 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.2, 0.3, 0.5, 0.0, 0.0, 0.0, 0.0) ,size=length(xxx),replace=T) }
          if ( m==3 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.0, 0.2, 0.3, 0.5, 0.0, 0.0, 0.0) ,size=length(xxx),replace=T) }
          if ( m==4 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.0, 0.0, 0.2, 0.3, 0.5, 0.0, 0.0) ,size=length(xxx),replace=T) }
          if ( m==5 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.0, 0.0, 0.0, 0.2, 0.3, 0.5, 0.0) ,size=length(xxx),replace=T) }
          if ( m==6 ) { dat.b[dat.b[,2]==coverscale[-1,][m,2],2] <- sample( c(0.01,coverscale[2:7,2]), prob = c(0.0, 0.0, 0.0, 0.0, 0.2, 0.3, 0.5) ,size=length(xxx),replace=T) }
        }
        dat.b[!is.na(dat.b[,2]) & dat.b[,2]<=(0),2] <- 0.01
        dat.b[!is.na(dat.b[,2]) & dat.b[,2]>1,2] <- 1
      }
      
      for (k in 1:length(colnames(ind))) {
        
        if ( nrow(dat.b)>2 ) {
          
          ind.b <- sum(dat.b[!is.na(dat.b[,2+k]),2] * dat.b[!is.na(dat.b[,2+k]),2+k] , na.rm=T) / sum(dat.b[!is.na(dat.b[,2+k]),2],na.rm=T)
          ind.list[[k]][i,j] <- ind.b
          
        } else {ind.list[[k]][i,j] <- NA}
        
      }
      
      #      print(paste(i,"",j)) 
    }
    
  }
  return(ind.list)
}

# expanding nin reference values
expand_shared_nin <- function(nin) {
  nin <- as.character(nin)
  
  # Match shared-lists:
  m <- stringr::str_match(
    nin,
    "^(.+?)-C(\\d+(?:C\\d+)*)$"
  )
  
  if (is.na(m[1, 1])) {
    return(nin)
  }
  
  main_type <- m[1, 2]
  code_string <- m[1, 3]
  
  # "21C6" becomes c("21", "6")
  codes <- stringr::str_split(code_string, "C")[[1]]
  
  # Return one properly formatted NiN code per component
  paste0(main_type, "-C-", codes)
}


# scaling function
build_ref_for_indicator <- function(ref_cov, ind_name, n_levels, indEll_n = 2) {
  
  # Choose quantiles based on indicator type
  if (ind_name %in% one_sided_indicators) {
    myQuantiles <- c(0.05, 0.5, 0.95)
  } else {
    myQuantiles <- c(0.025, 0.5, 0.975)
  }
  
  # Indicator matrix
  ind_mat <- ref_cov[[ind_name]]
  
  if (is.null(ind_mat)) {
    stop(sprintf("No reference matrix found for indicator: %s", ind_name))
  }
  
  all_names <- colnames(ind_mat)
  
  if (is.null(all_names) || length(all_names) == 0) {
    stop(sprintf(
      "Reference matrix for '%s' has no column names.",
      ind_name
    ))
  }
  
  # Strip a/b/c/d suffixes from column names:
  # T32-C21C6a -> T32-C21C6
  base_names <- stringr::str_remove(all_names, "[abcd]$")
  
  nin_map <- tibble::tibble(
    col_index = seq_along(all_names),
    col_name  = all_names,
    base_nin  = base_names
  )
  
  nin_counts <- nin_map |>
    dplyr::count(base_nin, name = "n_cols")
  
  nin_single <- nin_counts |>
    dplyr::filter(n_cols == 1) |>
    dplyr::pull(base_nin)
  
  nin_multi <- nin_counts |>
    dplyr::filter(n_cols > 1) |>
    dplyr::pull(base_nin)
  
  # Calculate quantiles for one base NiN.
  # If several reference-list columns represent the same base NiN
  # (e.g. ...a and ...b), their bootstrap values are pooled.
  calc_q_for_nin <- function(base_id) {
    
    idx <- nin_map |>
      dplyr::filter(base_nin == base_id) |>
      dplyr::pull(col_index)
    
    vals <- as.matrix(ind_mat[, idx, drop = FALSE])
    
    q <- stats::quantile(
      vals,
      probs = myQuantiles,
      na.rm = TRUE
    )
    
    tibble::tibble(
      NiN        = base_id,
      Q_low      = q[1],
      Q_med      = q[2],
      Q_high     = q[3],
      Q_low_dup  = q[1],
      Q_med_dup  = q[2],
      Q_high_dup = q[3]
    )
  }
  
  tab_single <- purrr::map_dfr(nin_single, calc_q_for_nin)
  tab_multi  <- purrr::map_dfr(nin_multi, calc_q_for_nin)
  
  tab <- dplyr::bind_rows(tab_single, tab_multi)
  
  # Expand shared generalised species lists.
  # Each resulting NiN type receives the same reference quantiles.
  tab <- tab |>
    dplyr::mutate(
      NiN_expanded = purrr::map(NiN, expand_shared_nin)
    ) |>
    tidyr::unnest_longer(NiN_expanded) |>
    dplyr::select(-NiN) |>
    dplyr::rename(NiN = NiN_expanded)
  
  # Restructure lower and upper reference limits
  y_low <- numeric(length = nrow(tab) * 2)
  y_low[seq(1, length(y_low), by = 2)] <- tab$Q_low
  y_low[seq(2, length(y_low), by = 2)] <- tab$Q_high
  
  y_ref <- numeric(length = nrow(tab) * 2)
  y_ref[seq(1, length(y_ref), by = 2)] <- tab$Q_low_dup
  y_ref[seq(2, length(y_ref), by = 2)] <- tab$Q_high_dup
  
  # Build indicator names
  ind_labels <- c(
    paste0(ind_name, "1"),
    paste0(ind_name, "2")
  )
  
  ind_ref <- data.frame(
    grunn  = rep(rep(tab$NiN, each = 2), indEll_n),
    county = rep("all", nrow(tab) * 2 * indEll_n),
    region = rep("all", nrow(tab) * 2 * indEll_n),
    Ind    = rep(ind_labels, nrow(tab) * indEll_n),
    Rv     = c(
      rep(tab$Q_med, each = 2),
      rep(tab$Q_med_dup, each = 2)
    ),
    Gv     = c(y_low, y_ref),
    maxmin = rep(c(1, n_levels), nrow(tab) * indEll_n)
  )
  
  ind_ref |>
    tibble::as_tibble() |>
    dplyr::mutate(
      grunn = as.factor(grunn),
      Ind   = as.factor(Ind)
    )
}
