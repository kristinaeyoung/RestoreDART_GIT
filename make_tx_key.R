

in_fl <- '../provided/count_by_trt_methods.csv'

df0 <- read.csv(in_fl)
# drop the one weird NA
df0 <- df0[!is.na(df0$coarse_trt_group), ]


length(unique(df0$coarse_trt_group))
# 14 coarse treatment groups rather than the ~145 currently
unique(unlist(strsplit(df0$coarse_trt_group, ';')))
# but only 4 actual categories: "Prescribed Burn", "Seeding", "Soil Disturbance", "Vegetation Removal"

df0 <- df0[, c('combined_TREATMENT_ASSIGNMENT', 'coarse_trt_group', 'coarse_num_polygons', 'coarse_num_pixels')]
colnames(df0) <- c('tx_fine', 'tx_coarse', 'npoly_coarse', 'npix_coarse')
df0$tx_fine <- tolower(df0$tx_fine)
df0$tx_coarse <- tolower(df0$tx_coarse)

# split tx_fine into logicals
df0$prescribed_burn <- grepl('prescribed burn', df0$tx_coarse)
df0$seeding <- grepl('seeding', df0$tx_coarse)
df0$soil_disturbance <- grepl('soil disturbance', df0$tx_coarse)
df0$vegetation_removal <- grepl('vegetation removal', df0$tx_coarse)


write.csv(df0, '../results/tx_key_BEM.csv', row.names = F)
