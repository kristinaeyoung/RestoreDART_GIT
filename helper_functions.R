split_by_objective <- function(df_in, obj_col = "objective", fun_col = "fun_group") {
  
  stopifnot(
    is.data.frame(df_in),
    nrow(df_in) > 0,
    is.character(obj_col),
    length(obj_col) == 1,
    is.character(fun_col),
    length(fun_col) == 1
  )
  
  all_objectives <- unique(unlist(strsplit(df_in[[obj_col]], ', ')))
  all_fun_groups <- unique(df_in[[fun_col]])
  
  combos <- expand.grid(objective = all_objectives, fun_group = all_fun_groups, stringsAsFactors = F)
  combos$match_fun_group <- toupper(gsub('.*_', '', combos$objective))
  combos$match_fun_group[which(combos$fun_group == 'BAR')] <- 'BAR'
  combos <- combos[combos$match_fun_group == combos$fun_group, ]

  result <- vector("list", nrow(combos))
  
  for (i in seq_len(nrow(combos))) {
    
    i_obj <- combos$objective[i]
    i_fg  <- combos$fun_group[i]
    
    # selects all rows where ANY objectives matches the target objective
    i_x <- df_in[[obj_col]]
    i_out <- df_in[grepl(i_obj, i_x), ]
    # seelcts rows for target functional group
    i_out <- i_out[i_out[[fun_col]] == i_fg, ]
    # only use post-intervention data
    i_out <- i_out[i_out$year_diff > 0, ]
    
    result[[i]] <- i_out
    
  }
  
  result <- setNames(result, paste(combos$objective, combos$fun_group, sep = '_'))
  
  return(result)
}