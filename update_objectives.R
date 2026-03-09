# BEM note: write this so that it can be called from a parent script using source() without leaving a messy Global env

library(readxl)

obj_dir <- '../RestoreDART_DATA/objectives/'
obj_fl <- 'MARCH2025_OBJECTIVES_26022026.xlsx'
data_file <- '../RestoreDART_DATA/MIXED_MODELS/2_combined_filter_input_data.csv'

objs <- read_excel(file.path(obj_dir, obj_fl), 1, na = c('', 'NA'))

d0 <- read.csv(data_file)

# which column names are shared?
#colnames(objs)[which(colnames(objs) %in% colnames(d0))]
# just OBJECTIVE and combined_TREATMENT_assignment
# are all polygon ID's in the current data in the objective file?
stopifnot(all(unique(d0$PolyID) %in% objs$polyID_unique))
colnames(objs)[which(colnames(objs) == 'polyID_unique')] <- 'PolyID'

# should be able to join now

# reduce size of objs
objs <- objs[which(colnames(objs) %in% colnames(d0))]
objs <- objs[complete.cases(objs), ]
objs <- objs[order(objs$PolyID), ]

# check to see if the current data matches the new data for entries that exist
objs_old <- objs[which(objs$PolyID %in% d0$PolyID), ]
objs_old <- as.data.frame(objs_old)
d0_old <- d0[which(d0$PolyID %in% objs_old$PolyID), c('PolyID', 'OBJECTIVE', 'combined_TREATMENT_ASSIGNMENT')]
d0_old <- d0_old[!duplicated(d0_old), ]
d0_old <- d0_old[order(d0_old$PolyID), ]
row.names(d0_old) <- NULL
stopifnot(nrow(objs_old) == nrow(d0_old))
stopifnot(all.equal(objs_old$PolyID, d0_old$PolyID))

if (!isTRUE(all.equal(objs_old$OBJECTIVE, d0_old$OBJECTIVE))) {
  print('Objective mismatch:')
  data.frame(
    PolyID = objs_old$PolyID[objs_old$OBJECTIVE != d0_old$OBJECTIV],
    objs_old = objs_old$OBJECTIVE[objs_old$OBJECTIVE != d0_old$OBJECTIVE],
    d0_old = d0_old$OBJECTIVE[objs_old$OBJECTIVE != d0_old$OBJECTIVE]
    )
}
# per KY, use the new objectives

# leave ONLY the new data in the Global env
rm(obj_dir, obj_fl, data_file, d0, objs, objs_old, d0_old)
gc()