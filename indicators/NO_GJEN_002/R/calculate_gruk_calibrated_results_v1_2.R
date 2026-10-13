suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
})

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 7L) stop("Expected seven arguments",call.=FALSE)
frame_path <- args[[1]]
polygon_path <- args[[2]]
v11_table_path <- args[[3]]
ano_path <- args[[4]]
output_dir <- args[[5]]
B <- as.integer(args[[6]])
seed <- as.integer(args[[7]])
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

VERSION <- "v1.2_gruk_calibrated"
MAX_OBS_YEAR <- 2024L
CLUSTER_M <- 10000
region_ids <- c(NA_integer_,1L,2L,3L,4L,5L)
region_names <- c("Norge","Nord-Norge","Midt-Norge","Østlandet","Vestlandet","Sørlandet")

read_delimited <- function(path) {
  first <- readLines(path,n=1L,warn=FALSE,encoding="UTF-8")
  semi <- if(length(first)) lengths(strsplit(first,";",fixed=TRUE))-1L else 0L
  comma <- if(length(first)) lengths(strsplit(first,",",fixed=TRUE))-1L else 0L
  sep <- if(semi>comma) ";" else ","
  decimal <- if(sep==";") "," else "."
  read.csv(path,sep=sep,stringsAsFactors=FALSE,check.names=FALSE,
           fileEncoding="UTF-8-BOM",na.strings=c("","NA","NaN"),dec=decimal)
}

weighted_mean_safe <- function(v,w) {
  k <- is.finite(v) & is.finite(w) & w>0
  if (!any(k)) return(NA_real_)
  sum(v[k]*w[k])/sum(w[k])
}

normalize_mapping <- function(x) {
  z <- toupper(trimws(as.character(x)))
  z[is.na(z)] <- ""
  gsub("[[:space:]_]","-",z)
}

is_gruk_code <- function(x) {
  z <- normalize_mapping(x)
  grepl("(^|[^A-Z0-9])T2-*C-*(7|8)([^0-9]|$)",z,perl=TRUE)
}

extract_main_type <- function(mapping_unit,main_type_label) {
  x <- paste(ifelse(is.na(mapping_unit),"",as.character(mapping_unit)),
             ifelse(is.na(main_type_label),"",as.character(main_type_label)))
  x <- toupper(trimws(x))
  pat <- "(?<![A-Z0-9])T(?:2|8|11|12|13|15|16|18|21|24|29|31|32|33|34|40|41)(?=-|[^0-9]|$)"
  vapply(x,function(value) {
    hit <- regexpr(pat,value,perl=TRUE)
    if(is.na(hit)||hit<1L) return(NA_character_)
    regmatches(value,hit)
  },character(1),USE.NAMES=FALSE)
}

message("Reading frozen V1.1G object results...")
analysis <- st_read(polygon_path,layer="polygon_results",quiet=TRUE,stringsAsFactors=FALSE)
frame <- st_read(frame_path,layer="combined_delivery_frame",quiet=TRUE,stringsAsFactors=FALSE)
if (st_crs(analysis)$epsg != 25833) analysis <- st_transform(analysis,25833)
if (st_crs(frame)$epsg != 25833) frame <- st_transform(frame,25833)
if (anyDuplicated(analysis$delivery_id)) stop("Duplicate analysis delivery_id",call.=FALSE)
if (anyDuplicated(frame$delivery_id)) stop("Duplicate frame delivery_id",call.=FALSE)

required_analysis <- c("delivery_id","source_dataset","source_id","region_id",
  "indicator_value","area_weight_m2","cluster_10km","included")
missing_analysis <- setdiff(required_analysis,names(analysis))
if(length(missing_analysis)) stop("V1.1G object layer lacks: ",paste(missing_analysis,collapse=", "),call.=FALSE)

analysis$included <- as.logical(analysis$included)
analysis$indicator_value <- as.numeric(analysis$indicator_value)
analysis$area_weight_m2 <- as.numeric(analysis$area_weight_m2)
analysis$region_id <- as.integer(analysis$region_id)
analysis <- analysis[analysis$included & is.finite(analysis$indicator_value) &
                     is.finite(analysis$area_weight_m2) & analysis$area_weight_m2>0,]
if(nrow(analysis)!=3759L) stop("Expected 3759 included V1.1G objects; found ",nrow(analysis),call.=FALSE)

message("Reading original ANO mapping units...")
layers <- st_layers(ano_path)$name
if(!("ANO_SurveyPoint" %in% layers)) stop("ANO_SurveyPoint layer is missing",call.=FALSE)
ano <- st_read(ano_path,layer="ANO_SurveyPoint",quiet=TRUE,stringsAsFactors=FALSE)
required_ano <- c("ano_punkt_id","aar","kartleggingsenhet_250m2","kartleggingsenhet_1m2",
                  "hovedtype_250m2","hovedtype_1m2")
missing_ano <- setdiff(required_ano,names(ano))
if(length(missing_ano)) stop("ANO source lacks: ",paste(missing_ano,collapse=", "),call.=FALSE)

ano$observation_year <- suppressWarnings(as.integer(ano$aar))
ano$main_250 <- extract_main_type(ano$kartleggingsenhet_250m2,ano$hovedtype_250m2)
ano$main_1 <- extract_main_type(ano$kartleggingsenhet_1m2,ano$hovedtype_1m2)
ano$main_selected <- ifelse(!is.na(ano$main_250),ano$main_250,ano$main_1)
ano$mapping_selected <- ifelse(!is.na(ano$main_250),
                               as.character(ano$kartleggingsenhet_250m2),
                               as.character(ano$kartleggingsenhet_1m2))
ano$gruk_t2_c7_c8 <- is_gruk_code(ano$mapping_selected)

valid_id <- !is.na(ano$ano_punkt_id) & trimws(as.character(ano$ano_punkt_id))!=""
ano <- ano[valid_id & !is.na(ano$observation_year) & ano$observation_year<=MAX_OBS_YEAR,]
if("utilgjengelig_punkt" %in% names(ano)) {
  inaccessible <- tolower(trimws(as.character(ano$utilgjengelig_punkt))) %in% c("ja","yes","true","1")
  ano <- ano[!inaccessible,]
}
date_rank <- if("registeringsdato" %in% names(ano)) {
  suppressWarnings(as.numeric(as.POSIXct(as.character(ano$registeringsdato),tz="UTC")))
} else rep(NA_real_,nrow(ano))
date_rank[!is.finite(date_rank)] <- -Inf
ord <- order(as.character(ano$ano_punkt_id),-ano$observation_year,-date_rank)
ano <- ano[ord,]
ano <- ano[!duplicated(as.character(ano$ano_punkt_id)),]
ano_attr <- st_drop_geometry(ano)
ano_key <- as.character(ano_attr$ano_punkt_id)

# Use the complete ANO delivery frame as the target population denominator.
# This preserves the approved open-lowland and Moen 1-5 frame selection and
# avoids estimating the share only from objects with complete imagery.
ano_frame <- frame[frame$source_dataset=="ANO",]
idx <- match(as.character(ano_frame$source_id),ano_key)
if(anyNA(idx)) stop(sum(is.na(idx))," ANO frame IDs were not found in original ANO source",call.=FALSE)
ano_frame$mapping_selected <- ano_attr$mapping_selected[idx]
ano_frame$gruk_t2_c7_c8 <- ano_attr$gruk_t2_c7_c8[idx]
if(anyNA(ano_frame$gruk_t2_c7_c8)) stop("NA GRUK classification in ANO frame",call.=FALSE)
ano_frame$region_id <- as.integer(ano_frame$region_id)
fc <- st_coordinates(st_centroid(st_geometry(ano_frame)))
ano_frame$cluster_10km <- paste(floor(fc[,1]/CLUSTER_M),floor(fc[,2]/CLUSTER_M),sep="_")
if(!any(ano_frame$gruk_t2_c7_c8)) {
  stop("No T2-C-7/T2-C-8 points were identified in the ANO open-lowland frame",call.=FALSE)
}
mapping_audit <- as.data.frame(table(
  mapping_selected=ifelse(is.na(ano_frame$mapping_selected)|ano_frame$mapping_selected=="",
                          "<missing>",ano_frame$mapping_selected),
  gruk_t2_c7_c8=ano_frame$gruk_t2_c7_c8),stringsAsFactors=FALSE)
mapping_audit <- mapping_audit[mapping_audit$Freq>0,]
mapping_audit <- mapping_audit[order(-mapping_audit$Freq,mapping_audit$mapping_selected),]

# Mark every outcome object that belongs to the GRUK domain. This includes the
# mapped GRUK source and ANO probability-sample points classified as T2-C-7/8.
analysis$gruk_domain <- analysis$source_dataset=="GRUK"
ano_analysis <- which(analysis$source_dataset=="ANO")
if(length(ano_analysis)) {
  aidx <- match(as.character(analysis$source_id[ano_analysis]),ano_key)
  if(anyNA(aidx)) stop("An included ANO result lacks original mapping data",call.=FALSE)
  analysis$gruk_domain[ano_analysis] <- ano_attr$gruk_t2_c7_c8[aidx]
}
analysis$calibration_domain <- ifelse(analysis$gruk_domain,"GRUK_T2_C7_C8","OTHER_OPEN_LOWLAND")

v11 <- read_delimited(v11_table_path)
v11$reporting_area <- as.character(v11$reporting_area)
old_value_col <- intersect(c("indicator_value","Indeks","index"),names(v11))[1]
if(is.na(old_value_col)) stop("Could not identify V1.1G indicator column",call.=FALSE)

bootstrap_calibrated <- function(d,f,B,seed_offset) {
  clusters <- sort(unique(c(as.character(d$cluster_10km),as.character(f$cluster_10km))))
  if(length(clusters)<2L) return(c(NA_real_,NA_real_,length(clusters)))
  di <- match(as.character(d$cluster_10km),clusters)
  fi <- match(as.character(f$cluster_10km),clusters)
  set.seed(seed+seed_offset)
  ans <- rep(NA_real_,B)
  for(b in seq_len(B)) {
    cw <- rexp(length(clusters),rate=1)
    fw <- cw[fi]
    p <- sum(as.numeric(f$gruk_t2_c7_c8)*fw)/sum(fw)
    dw <- d$area_weight_m2*cw[di]
    mg <- weighted_mean_safe(d$indicator_value[d$gruk_domain],dw[d$gruk_domain])
    mo <- weighted_mean_safe(d$indicator_value[!d$gruk_domain],dw[!d$gruk_domain])
    if(is.finite(p) && is.finite(mg) && is.finite(mo)) ans[b] <- p*mg+(1-p)*mo
  }
  if(sum(is.finite(ans)) < 0.99*B) stop("Too many invalid bootstrap replicates",call.=FALSE)
  c(unname(quantile(ans,c(.025,.975),na.rm=TRUE)),length(clusters))
}

result_rows <- list()
share_rows <- list()
analysis$calibration_weight_national <- NA_real_
analysis$calibration_weight_region <- NA_real_

for(i in seq_along(region_names)) {
  rid <- region_ids[[i]]; label <- region_names[[i]]
  dk <- if(is.na(rid)) rep(TRUE,nrow(analysis)) else analysis$region_id==rid
  fk <- if(is.na(rid)) rep(TRUE,nrow(ano_frame)) else ano_frame$region_id==rid
  d <- analysis[dk,]; f <- ano_frame[fk,]
  if(!nrow(d) || !nrow(f)) stop("Empty outcome or ANO frame for ",label,call.=FALSE)
  p <- mean(f$gruk_t2_c7_c8)
  ng <- sum(f$gruk_t2_c7_c8); nt <- nrow(f)
  mg <- weighted_mean_safe(d$indicator_value[d$gruk_domain],d$area_weight_m2[d$gruk_domain])
  mo <- weighted_mean_safe(d$indicator_value[!d$gruk_domain],d$area_weight_m2[!d$gruk_domain])
  if(!is.finite(mg)||!is.finite(mo)) stop("A calibration domain has no estimate for ",label,call.=FALSE)
  adjusted <- p*mg+(1-p)*mo
  old_match <- match(label,v11$reporting_area)
  if(is.na(old_match) && nrow(v11)>=i) old_match <- i
  old_reported <- as.numeric(v11[[old_value_col]][old_match])
  old <- weighted_mean_safe(d$indicator_value,d$area_weight_m2)
  if(!is.finite(old_reported)||!is.finite(old))
    stop("Could not recover V1.1G comparison value for ",label,call.=FALSE)
  if(abs(old-old_reported)>5e-5)
    stop("Recomputed V1.1G value differs from reported table for ",label,
         ": ",old," versus ",old_reported,call.=FALSE)
  ci <- bootstrap_calibrated(d,f,B,i)

  wg <- d$area_weight_m2[d$gruk_domain]
  wo <- d$area_weight_m2[!d$gruk_domain]
  local_weights <- numeric(nrow(d))
  local_weights[d$gruk_domain] <- p*wg/sum(wg)
  local_weights[!d$gruk_domain] <- (1-p)*wo/sum(wo)
  if(abs(sum(local_weights)-1)>1e-10) stop("Calibration weights do not sum to one for ",label,call.=FALSE)
  if(is.na(rid)) analysis$calibration_weight_national[dk] <- local_weights
  else analysis$calibration_weight_region[dk] <- local_weights

  result_rows[[i]] <- data.frame(
    reporting_area=label,
    v1_1g_direct_area_weighted=old,
    gruk_share_from_ano=p,
    gruk_domain_mean=mg,
    other_open_lowland_mean=mo,
    v1_2_gruk_calibrated=adjusted,
    ci95_lower=ci[[1]],ci95_upper=ci[[2]],
    ano_open_lowland_points=nt,ano_gruk_t2_c7_c8_points=ng,
    included_outcome_objects=nrow(d),
    included_gruk_domain_objects=sum(d$gruk_domain),
    included_other_objects=sum(!d$gruk_domain),
    spatial_clusters=as.integer(ci[[3]]),
    uncertainty_method=paste0("Bayesian spatial cluster bootstrap, 10 km blocks; ANO share re-estimated per replicate; B=",B),
    stringsAsFactors=FALSE)
  share_rows[[i]] <- data.frame(
    reporting_area=label,region_id=ifelse(is.na(rid),"NATIONAL",as.character(rid)),
    ano_open_lowland_points=nt,ano_gruk_t2_c7_c8_points=ng,
    gruk_share=p,other_share=1-p,stringsAsFactors=FALSE)
}

results <- do.call(rbind,result_rows)
shares <- do.call(rbind,share_rows)
if(any(!is.finite(results$v1_2_gruk_calibrated))) stop("Non-finite calibrated result",call.=FALSE)
if(any(results$v1_2_gruk_calibrated<0|results$v1_2_gruk_calibrated>1)) stop("Calibrated result outside 0-1",call.=FALSE)
if(any(abs(shares$gruk_share+shares$other_share-1)>1e-12)) stop("Shares do not sum to one",call.=FALSE)
if(anyNA(analysis$calibration_weight_national)) stop("Missing national calibration weights",call.=FALSE)
if(anyNA(analysis$calibration_weight_region)) stop("Missing regional calibration weights",call.=FALSE)
if(abs(sum(analysis$calibration_weight_national)-1)>1e-10) stop("National object weights do not sum to one",call.=FALSE)
for(rid in 1:5) if(abs(sum(analysis$calibration_weight_region[analysis$region_id==rid])-1)>1e-10)
  stop("Regional object weights do not sum to one for region ",rid,call.=FALSE)

write.csv2(results,file.path(output_dir,"NO_GJEN_002_delivery_table_2024_gpu_v1_2_gruk_calibrated.csv"),
           row.names=FALSE,fileEncoding="UTF-8")
write.csv2(shares,file.path(output_dir,"NO_GJEN_002_gruk_share_from_ANO_v1_2.csv"),
           row.names=FALSE,fileEncoding="UTF-8")
write.csv2(mapping_audit,file.path(output_dir,"NO_GJEN_002_ANO_mapping_code_audit_v1_2.csv"),
           row.names=FALSE,fileEncoding="UTF-8")
write.csv2(st_drop_geometry(analysis),file.path(output_dir,"delivery_polygon_statistics_gpu_v1_2_gruk_calibrated.csv"),
           row.names=FALSE,fileEncoding="UTF-8")
st_write(analysis,file.path(output_dir,"delivery_polygon_results_gpu_v1_2_gruk_calibrated.gpkg"),
         layer="polygon_results",delete_dsn=TRUE,quiet=TRUE)

provenance <- list(
  version=VERSION,created_utc=format(Sys.time(),tz="UTC",usetz=TRUE),
  baseline="V1.1G; frozen object indicator values; no inference rerun",
  correction_request="GRUK share equals T2-C-7/T2-C-8 share among ANO open-lowland points",
  target_population="approved ANO open-lowland delivery frame within Moen zones 1-5",
  point_estimator="p_ANO_GRUK * area-weighted GRUK-domain mean + (1-p_ANO_GRUK) * area-weighted other-open-lowland mean",
  gruk_domain="all mapped GRUK-source objects plus ANO objects classified T2-C-7/T2-C-8",
  other_domain="remaining ANO plus ASO sample-event objects; relative V1.1G area weights retained within domain",
  uncertainty=list(method="Bayesian spatial cluster bootstrap",cluster_grid_m=CLUSTER_M,
                   ano_share_reestimated_each_replicate=TRUE,replicates=B,seed=seed),
  inputs=list(frame=normalizePath(frame_path,winslash="/"),
              polygon_results_v1_1g=normalizePath(polygon_path,winslash="/"),
              v1_1g_table=normalizePath(v11_table_path,winslash="/"),
              ano_source=normalizePath(ano_path,winslash="/")),
  safeguards=list(v1_1g_unchanged=TRUE,gpu_inference_rerun=FALSE,
                  frozen_references_changed=FALSE,maximum_observation_year=MAX_OBS_YEAR))
write_json(provenance,file.path(output_dir,"delivery_results_provenance_gpu_v1_2_gruk_calibrated.json"),
           pretty=TRUE,auto_unbox=TRUE)

fmt <- function(x) ifelse(is.finite(x),sprintf("%.4f",x),"NA")
lines <- c(
  "NO_GJEN_002 GRUK-CALIBRATED DELIVERY RESULTS 2024 GPU V1.2","",
  "V1.1G is preserved unchanged as the direct-area-weighted comparison.",
  "GRUK weight is calibrated to the T2-C-7/T2-C-8 share among ANO open-lowland frame points.",
  "The ANO share is re-estimated in every 10 km spatial bootstrap replicate.","",
  "RESULTS")
for(i in seq_len(nrow(results))) {
  z <- results[i,]
  lines <- c(lines,sprintf("%s: V1.2=%s (95%% CI %s-%s); ANO GRUK share=%.4f (%d/%d); V1.1G=%s",
    z$reporting_area,fmt(z$v1_2_gruk_calibrated),fmt(z$ci95_lower),fmt(z$ci95_upper),
    z$gruk_share_from_ano,z$ano_gruk_t2_c7_c8_points,z$ano_open_lowland_points,
    fmt(z$v1_1g_direct_area_weighted)))
}
lines <- c(lines,"","INTERPRETATION",
  "V1.2 corrects the over-representation of intensively mapped GRUK polygons.",
  "It does not change object indicator values, GPU predictions, calibration, QA or frozen references.",
  "The result remains coverage-limited to the V1.1G imagery and reference scope.")
writeLines(lines,file.path(output_dir,"NO_GJEN_002_DELIVERY_RESULTS_2024_GPU_V1_2_GRUK_CALIBRATED.txt"),useBytes=TRUE)
cat(paste(lines,collapse="\n"),"\n")
