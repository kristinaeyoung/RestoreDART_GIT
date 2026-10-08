#
d_dir <- '../RestoreDART_DATA/pixelwise_dart_results_25Feb2025'
d_fls <- list.files(d_dir, pattern = 'trt_effects\\.csv', full.names = T)

out_df <- data.frame()
out_fl <- '../RestoreDART_DATA/DART_combined_BEM_04032026.csv'

cnm <-c("response", "point.pred", "point.pred.lower", "point.pred.upper", 
        "point.effect", "point.effect.lower", "point.effect.upper", "year.index", 
        "target_id", "fun_group", "trtYear", "PolyID")
ccl <- c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
         "numeric", "integer", "integer", "character", "integer", "integer")

for (i in seq_along(d_fls)) {
  
  cat('\r', round(i / length(d_fls) * 100, 2))
  ii <- d_fls[i]
  
  idf <- read.csv(ii)
  iid <- as.integer(unlist(strsplit(basename(ii), '_'))[2])
  stopifnot(
    length(unique(idf$PolyID)) == 1,
    iid == unique(idf$PolyID),
    identical(colnames(idf), cnm),
    identical(as.character(lapply(idf, class)), ccl)
  )
  
  if (i == 1) {
    out_df <- idf
  } else {
    out_df <- rbind(out_df, idf)
  }
  
  #break
  
}

write.csv(out_df, out_fl, row.names = F)
#