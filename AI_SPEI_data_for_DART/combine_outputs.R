
AI_fls <- list.files(pattern = 'AI_21_24_chunk')
SPEI_fls <- list.files(pattern = 'SPEI_21_24_chunk')

AI_df <- data.frame()
SPEI_df <- data.frame()

out_AI <- '../../RestoreDART_DATA/CLIMATE_DATA/AI_21_24.csv'
out_SPEI <- '../../RestoreDART_DATA/CLIMATE_DATA/SPEI_21_24.csv'

for (i in seq_along(AI_fls)) {
  ii <- AI_fls[i]
  
  i0 <- read.csv(ii)
  
  colnames(i0) <- gsub('X0_yr_mean_ai', 'X2021', colnames(i0))
  colnames(i0) <- gsub('X1_yr_mean_ai', 'X2022', colnames(i0))
  colnames(i0) <- gsub('X2_yr_mean_ai', 'X2023', colnames(i0))
  colnames(i0) <- gsub('X3_yr_mean_ai', 'X2024', colnames(i0))
  
  if (i == 1) {
    AI_df <- i0
  } else {
    AI_df <- rbind(AI_df, i0)
  }
  
}

for (i in seq_along(SPEI_fls)) {
  ii <- SPEI_fls[i]
  
  i0 <- read.csv(ii)

  colnames(i0) <- gsub('X0_yr_mean_spei', 'X2021', colnames(i0))
  colnames(i0) <- gsub('X1_yr_mean_spei', 'X2022', colnames(i0))
  colnames(i0) <- gsub('X2_yr_mean_spei', 'X2023', colnames(i0))
  colnames(i0) <- gsub('X3_yr_mean_spei', 'X2024', colnames(i0))
  
  if (i == 1) {
    SPEI_df <- i0
  } else {
    SPEI_df <- rbind(SPEI_df, i0)
  }
  
}

write.csv(AI_df, out_AI, row.names = F)
write.csv(SPEI_df, out_SPEI, row.names = F)
