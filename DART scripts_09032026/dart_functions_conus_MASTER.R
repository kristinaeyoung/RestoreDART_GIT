## DART for large polygons - updated to use sf and terra
## CONUS dart mods - soil ("filter") vars incorporated into the gower, along w/ topo vars
## G. Tyree - gtyree@usgs.gov - 01/31/2024

##################################################
## DART function for parallel processing         #
##################################################

#
# 1. Generate dart pixels with GOWER. 
#    Output file : x.dart  -- padpoly,bufrast,toposim
# 2. Extract timeseries rasters from polygons
#    Output file : x.extract
#

dart_fn <- function(id) {    
  
  cat(' ** running dart for id ', id, '\n')   
  filename <- file.path( dpar$outdir, paste0(dpar$prefix, id, '.RData'))
  if(file.exists(filename)){ return() }
  
  #internal timer
  tstart <- Sys.time()
  cat(Sys.time(), ' ', id, '\n', file = '~/snowlog.txt', append = TRUE)
  
  # get polygon
  padpoly <- get_poly(dpar, id)
  
  # get raster data
  zone <- getRasterData(padpoly, dpar, dpar$rad)

  # get target pixels
  pad <- get_treatment(padpoly, dpar, zone)   
  padpixels <- pad$padpixels
  
  ## Get padstk as an object to use in the while loop below
  padstk <- pad$padstk
  
  # candidate params
  tries <- dpar$tries
  lastTry <- FALSE
  searchRadius <- dpar$rad
  
  #placeholder to see which pixels omitted due to lack of data
  ignoredTreatedPixels <- 0
  
  # get candidates
  while( tries > 0){
    
    if ( tries == 1 ) lastTry = TRUE
    cat(' ** looking for candidates\n')
    cat('\t searchRadius:', searchRadius, '\n')       
    
    candidates <- try({ get_candidates(dpar, searchRadius, padpoly, zone) })

    # check to see if any candidates were found
    if('try-error' %in% class(candidates)){
      if(grepl('ERROR1', candidates)){
        cat(' \tno non NA candidates. Doubling search radius\n');
        tries <- tries - 1
        searchRadius <- searchRadius  + dpar$rad
        zone <- getRasterData(padpoly, dpar, searchRadius)
        next()
      } else {
        stop(candidates)
      }
    } 
    
    cat( '\t finished search\n')      
    break() 
    
  }
  
  # get similarity matrix for top 100 ref pixels for each target pixel
  cat('getting similarity index \n')
  top.sims <- getGowerByGroup(padpixels, candidates, c(dpar$filterVars, dpar$topoVars))

  chosenCandidates <- candidates[unique(c(top.sims$index)),]
  extraction <- extract_TS(padpoly, padpixels, chosenCandidates, top.sims)  # Using rapr extraction
  synthControl <- getSC(target = extraction$extractedTarget, 
                        reference = extraction$extractedReference, 
                        treatmentYear = padpoly$trtYear
                        )
  
  timeElapsed <- Sys.time() - tstart
  cat('saving data to ', filename, '\n')
  
  candidates <- candidates[,1]
  
  save( 
    padpoly, padpixels, searchRadius, candidates, 
    chosenCandidates, top.sims, 
    timeElapsed, dpar, extraction, synthControl,
    ignoredTreatedPixels, 
    file = filename) 
  
  # generate metadata and log to SQL
  cat('saving metadata\n')
  meta <- gen_metadata()
  insert_table(meta, 'meta')
  
  # save raw response timeseries
  cat('saving timeseries\n')
  ts <- gen_ts()
  ts$insertDate <- as.Date(Sys.time())
  insert_table(ts, 'ts')
  
  cat(' time elapsed: ', timeElapsed, '\n')
  cat(' number of treatment pixels: ', nrow(padpixels), '\n')
  cat(' number of pixels removed: ', length(ignoredTreatedPixels), '\n')
  cat(' final search radius: ', searchRadius, '\n')
  cat(' #COMPLETED# \n')    
  gc()
}



#############################################
## Supporting functions ###
gen_ts <- function( env = parent.frame() ){
  # input : extraction
  # output : long table - polyID, pixelID, years_after, year, variable, value
  extraction <- get('extraction', env)
  padpoly <- get('padpoly', env)
  D = list()
  for( variable in names(extraction$extractedTarget)){
    
    e <- extraction$extractedTarget[[variable]]
    # identify years since treatment
    y <- as.integer(year(Sys.Date()))
    yy <- (y - ncol(e)):(y - 1)      # aka (currentYear - number of years of data in e) to previous year       
    library(lubridate)
    years_after <- 1:length(yy) - grep(as.integer(year(padpoly$trtYear)), yy)
    
    
    D[[variable]] <- data.table(polyID = padpoly$polyID,
                                pixelID = rep( 1:nrow(e), ncol(e)),
                                years_after = rep(years_after, each = nrow(e)),
                                year = rep(yy, each = nrow(e)),
                                variable = variable,
                                value = c(e)) 
  }
  
  d.out <- do.call(rbind, D)
  
  d.out
  
  
}

gen_metadata <- function(env = parent.frame()){
  
  # format metadata
  require(data.table)
  
  ## load env variables
  dpar <- get('dpar', env)
  padpoly <- get('padpoly', env)
  extraction <- get('extraction', env)
  padpixels <- get('padpixels', env)
  filename <- get('filename', env)
  
  ## get metadata  ##
  meta <- padpixels
  
  # remove columns
  meta <- meta[ , which(!names(meta) %in% dpar$dontKeep)]
  
  # add coordinates
  xy <- st_coordinates(st_centroid(padpixels) )
  meta$x <- xy[,1]
  meta$y <- xy[,2]
  meta$pixelID <- 1:nrow(meta)
  meta$file <- filename
  
  # add metadata
  for(p in dpar$metaCols){
    try({
      meta[[p]] <- padpoly[[p]]
    })
  }
  
  #################################
  #################################
  
  ## static covariates
  cat('extracting static variables\n')
  for( v in names(dpar$staticVars)){
    cat('\t',v,'\n')
    R <- rast(dpar$staticVars[[v]])
    meta[[v]] <- terra::extract(R, padpixels)[,1]
  }
  
  
  ################################
  ################################
  
  ## get pre-treatment mean and sd for each extraction
  
  for( variable in names(extraction$extractedTarget)){
    
    # variable <- "pfg"
    e <- extraction$extractedTarget[[variable]]
    
    # identify years since treatment
    currentYr <- lubridate::year(Sys.Date())
    yy <- (currentYr - ncol(e)):(currentYr - 1)    
    ## e could be an extraction from a multi-layer raster with nlyrs corresponding to number of years
    ## probably not supposed to have the ID col in the extraction...
    y <- 1:length(yy) - grep(padpoly$trtYear, yy)

    meta[[ paste0('preMean.',variable)]] <-rowMeans(e[,y<0], na.rm = TRUE)
    meta[[ paste0('preSD.', variable) ]] <- apply(e[,y<0],1, sd, na.rm = TRUE)
    meta[[ paste0('preMed.', variable)]] <- apply(e[,y<0],1,median, na.rm = TRUE)
  }
  
  meta$insertDate = as.Date(Sys.time())
  
  return ( meta )
}

insert_table <- function(data, tableName){
  # insert data into table 'tableName' for dpar$dbfile
  require(RSQLite)
  dbFile <- dpar$dbfile
  CON <- dbConnect(RSQLite::SQLite(), dbFile)
  # write to tables
  cat('writing table ', tableName, ' in ', dbFile, '\n')
  retry <- F
  while( retry ){
    res <- try({ dbWriteTable(CON, tableName, value = data, append = TRUE) } )
    if( class(res) != 'try-error' ) {
      retry <- FALSE
      cat('\tsuccess\n')
    } else {
      cat('\twrite try failed -- sleeping for 5 seconds\n')
      Sys.sleep(5)
    }
  }
  dbDisconnect(CON)
  cat('finished\n')
  
}

getGowerByGroup <- function(padpixels, candidates, vars){
  ## Make an empty matrix in which to store the pix with the top 100 gower results
  gower.out <- list( index = matrix(NA, 100, nrow(padpixels)), distance =  matrix(NA, 100, nrow(padpixels)) )
  
  # toponames <- gsub(".tif","", names(dpar$topoVars))
  names <- gsub(".tif","", names(vars))
  w <- rep(x=1, times=length(names))      #Weights for the gower (all set the same here)

    j <- padpixels  # Use all pad pixels
    k <- candidates # Use all candidates
    # select comparison vars in pad     #GLT - modify for all candidate pix
    dat1 <- st_drop_geometry(padpixels[j, c(names)])
    # select buffered candidate references    #GLT - modify for all candidate pix
    dat2 <- st_drop_geometry(candidates[k,c(names)])
    # get similarity matrix for top 100 ref pixels for each target pixel
    cat('getting similarity index \n')
    gower.out <- gower_topn(x=dat1, y=dat2, n=100, nthread = 1, 
                            weights = w)   

  gower.out
}

get_poly <- function(dpar, id){
  cat(' ** loading polygon \n')
  polygons <- st_read(dpar$polygons)
  polygons$id <- polygons$polyID
  padpoly <- polygons[ which(polygons$id == id),]
  return(padpoly)
}

get_fun <- function(x){
  # For all mask variables x, apply a function to their path names y:
  s <- lapply( names(x), function(y) { 
    cat('getting ', y, '\n')
    rpath <- x[[y]]
    r <- rast(rpath)
  })
  rast(s)
}

getRasterData <- function(padpoly, dpar, radius){
  cat(' ** loading raster data\n')
  toponames <- gsub(".tif","", names(dpar$topoVars))
  masks <- get_fun(dpar$maskVars)
  names(masks) <- names(dpar$maskVars)
  topovars <- get_fun(dpar$topoVars)
  names(topovars) <- names(dpar$topoVars)
  ## Keep here, still need to process the filter rasts
  filtervars <- get_fun(dpar$filterVars)
  names(filtervars) <- names(dpar$filterVars)

  rast.proj <- crs(masks[['refrast']])
  b <- c(masks, topovars, filtervars)

  # Reproject the polys if needed:
  padpoly <- st_transform(padpoly, rast.proj)

  ## Prepare neighborhood
  padpolybuf <- st_buffer(padpoly, dist = radius)
  padpolybuf$rastval <- 1

  ## crop
  cat('\tcropping predictors\n')
  zone <- crop(b, padpolybuf)

  return( zone )
}

get_treatment <- function(padpoly, dpar, zone){

  cat(' ** getting treated pixels\n')
  # apply interior buffer
  padpolyi <- st_buffer(padpoly, dist = -1*dpar$innerRad)

  if(is.null(padpolyi)){
    stop('ERROR not enough pixels in polygon\n')
  }
  ## Create rasters
  padpolyi$rastval <- 1
  padrast <- rasterize(padpolyi, crop(zone[[1]], padpolyi),field=padpolyi$rastval, datatype='INT1U')

  # make raster stack
  padstk <- crop(zone, padpolyi)

  # mask out unwanted  pixels in treated area
  # Function to mask NLCD
  nlcd_fn <- function(x, y) {
    ind <- ifel(x!=21 & x!=22 & x!=24 & x!=81 & x!=82 & x!=11 & x!=12 & y==1, 1, NA)
    return(ind)
  }
  ###
  padMask <- padrast
  for( variable in dpar$interiorMaskVars){
    if(variable == 'nlcd'){
      padMask  <- nlcd_fn(padstk[[variable]], padMask)
    } else {
      padMask  <- padstk[[variable]] * padMask
    }
  }

  padMask[padMask[] == 0] <- NA
  # get subsample of target pixels
  if( dpar$subSample ){
    samp1 = spatSample(padMask, size=ncell(padMask) / 16, method = "regular", as.points = TRUE)
    samp2 = shift(samp1, 2*(res(padMask)[1]), 2*(res(padMask)[2]))
    dots <- st_as_sf(rbind(samp1, samp2)) %>% st_intersection(padpolyi)
    dots <- mask(padMask, dots)
    padMask <- dots
  }

  padstk <- padMask * padstk
  names(padstk) <-  names(zone)

  # check that there are enough padpixels
  cat('\tchecking for enough non-na pixels in treatment area\n')
  if(all(is.na(padrast[]))) stop( 'ERROR2: not enough pixels in treatment polygon;')

  # if(length(which(!is.na(padrast[]))) == 1){}
  cat('\tconverting treatment raster to sf\n')
     padpixels <- as.polygons(x = padstk, trunc=F, dissolve=F, values=T, na.rm=T, na.all=F, extent=F) %>% st_as_sf()
     # trying without the aggregate function to see if that fixes things?? 
   #padpixels <- as.polygons(x = padstk, aggregate = F, trunc=F, dissolve=F, values=T, na.rm=T, na.all=F, extent=F) %>% st_as_sf()
    return( list(padpixels = padpixels, padpoly = padpoly, padstk = padstk))

}

get_candidates <- function(dpar, radius, padpoly, zone){
  #Generate raster brick of masked candidate pixels
  padpolybuf <- st_buffer(padpoly, dist = dpar$rad)
  padpolybuf$rastval <- 1
  padbufrast <- rasterize(padpolybuf, zone[[1]], field=padpolybuf$rastval, datatype='INT1U')
  ## Get the mask, topo, and filter variables
  masks <- get_fun(dpar$maskVars)        
  names(masks) <- names(dpar$maskVars)
  topovars <- get_fun(dpar$topoVars)
  names(topovars) <- names(dpar$topoVars)
  filtervars <- get_fun(dpar$filterVars)
  names(filtervars) <- names(dpar$filterVars)
  
  ## Screen out unwanted disturbances in buffer zone
  # mask out unwanted  pixels in treated area
  # Function to mask NLCD
  nlcd_fn <- function(x, y) {
    ind <- ifel(x!=21 & x!=22 & x!=24 & x!=81 & x!=82 & x!=11 & x!=12 & y==1, 1, NA)
    return(ind)
  }
  
  for( msk in names(masks)){
    if( msk == 'refrast') next()
    cat('\tmasking ', msk,'\n')
    if( msk == 'nlcd'){
      padbufrast <- nlcd_fn(zone[[msk]], padbufrast)
    } else {
      padbufrast <- padbufrast * zone[[msk]]
    }
  }
  
  # mask everything - check this...
  cat('\tmasking buffer\n')
  padbufrast[which(padbufrast[] == 0)] <- NA
  padbufstk <- padbufrast * zone
  
  # make donut
  cat('\tmaking donut\n')
  padpolyb <- st_buffer(padpoly, dist = dpar$buffer)
  padbufstk <- mask(padbufstk, padpolyb, inverse = TRUE)
  
  # propagate names
  names(padbufstk) <- names(zone)  
  
  # checks
  cat('\tchecking number of candidates in buffer\n')
  if(all(is.na(padbufrast[]))) stop( 'ERROR1: no non-na candidate pixels in buffer (every pixel masked)')    
  
  cat('\tconverting buffer to sf \n')
  padbufpixels <- as.polygons(x = padbufstk, trunc=F, dissolve=F, values=T, na.rm=T, na.all=F, extent=F) %>% st_as_sf()
  
  #padbufpixels <- as.polygons(x = padbufstk, aggregate = F, trunc=F, dissolve=F, values=T, na.rm=T, na.all=F, extent=F) %>% st_as_sf()

  return( padbufpixels )
}

extract_TS <- function(padpoly, padpixels, allpixels, toposim){  
  ext = ext(rbind(padpixels[,1], allpixels[,1]))
  # ext[1] <- ext[1] - 100
  # ext[2] <- ext[2] + 100
  # ext[3] <- ext[3] - 100
  # ext[4] <- ext[4] + 100
  
  box <- as.polygons(ext)
  crs(box) <- crs(padpixels)
  
  cat('loading response variables\n'); flush.console()
  
  require("rapr")
  b <- box
  trtYear <- as.integer(padpoly$trtYear)
  y0 = trtYear - dpar$trtYrBuff
  if(y0 < 1986) {as.integer(1986) -> y0} else {y0 -> y0}  
  
  y1 = trtYear + dpar$trtYrBuff
  if(y1 > lubridate::year(Sys.Date()) - 1) {as.integer(lubridate::year(Sys.Date()) - 1) -> y1} else {y1 -> y1}

  s <- get_rap(x = b,   
               product = "vegetation-cover",
               version = "v3",
               year = seq(y0,y1,1),
               progress = TRUE
               )

  s <- project(s, crs(padpixels), threads=TRUE)

  st <- Sys.time()
  
  cat('extracting padvals\n') ; flush.console()
  padvals <- terra::extract(s, padpixels, ID=T, xy=T)
  
  cat('extracting reference vals\n') ; flush.console()
  allvals <- terra::extract(s, allpixels, ID=T, xy=T)
  
  a <- toposim$index
  aindex <- unique(c(a)) # unique reference pixels
  timeElapsed <- Sys.time() - st
  
  
  return ( list( refIndex = aindex, extractedTarget =padvals, extractedReference=allvals, ExtractedTimeElapsed = timeElapsed) ) 
  
}

getSC <- function(target, reference, treatmentYear) {
  
  cat('run causal impact model\n')
  
  tgt.tbl <- target %>%
    pivot_longer(cols = !contains(c('ID', 'x', 'y')), names_to = "name", values_to = "value") %>%
    mutate(var = substr(name, 1, nchar(name) - 8),
           year = str_sub(name, -7, -4),
           .before = value) %>%
    dplyr::select(-name) %>% 
    group_by(var, year) %>% summarise(value = mean(value)) %>%
    ungroup() %>% rename(tgt = value)
  
  ref.tbl <- reference %>%
    pivot_longer(cols = !contains(c('ID', 'x', 'y')), names_to = "name", values_to = "value") %>%
    mutate(var = substr(name, 1, nchar(name) - 8),
           year = str_sub(name, -7, -4),
           .before = value) %>%
    dplyr::select(-name) %>% 
    group_by(var, year) %>% summarise(value = mean(value)) %>%
    ungroup() %>% rename(ref = value)
  
  tbls <- left_join(tgt.tbl, ref.tbl, by = c("var", "year"))
  vars <- unique(tbls$var)
  sc_extraction <- list()
  
  for (i in 1:length(vars)) {
    
    var.tbl <- tbls %>% filter(var == vars[i]) %>% dplyr::select(-c(var, year))
    sc_extraction[[i]] <- var.tbl
    
  }
  
  names(sc_extraction) <- vars
  
  ## Get pre- and post-treatment periods. 
  years <- unique(tbls$year)
  pre <- c(1, grep(pattern=treatmentYear, x=years) - 1)
  post <- c(grep(treatmentYear, years),  length(years))
  
  # require(CausalImpact)
  CI <- function(x) {return (CausalImpact::CausalImpact(as.matrix(x), pre.period=pre, post.period=post))}
  
  sc_models <- lapply(sc_extraction, CI)
  sc_series <- lapply(sc_models, 
                      function(x) {
                        series_data <- as.data.table(x$series[, c('point.effect', 'point.effect.upper', 'point.effect.lower', 'cum.effect', 'cum.effect.lower', 'cum.effect.upper')])
                        series_data[, year := seq_len(nrow(series_data))] # Add a year column (adjust as needed)
                        return(series_data)
                      })
  names(sc_models) <- vars
  # names(sc_series) <- vars
  ## Export the SC model and series results
  return(list( models = sc_models, data = sc_series) )
}

