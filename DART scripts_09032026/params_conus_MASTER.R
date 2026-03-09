# run DART
# these are the parameters to specify for a DART run

######################################################
# Parameters template
dpar <- list(
    ## Number of cores
    ncores = parallel::detectCores() - 4, # do not make changes unless you really need to
    
    ## Output directory -- this is the name of the output directory for DART results
    outdir = "outdir_4",

    ## Treatment areas
    # Your shapefile here
    polygons =  "/90daydata/aeroldc/GH/DART/DART_regions_of_interest/WRI_test/test_dart_polys_sf_NAD83.shp",
    
    #polygons =  "C:/Users/gharrison/OneDrive - USDA/Documents/RAP Research/RestoreNM_RAP/Data/RNM_trt_forDART.shp",
    #polygons =  "C:/Users/gharrison/OneDrive - USDA/Documents/RAP Research/RestoreNM_RAP/Data/RNM_field_leaveouts_for_DART.shp",   

    polygon_id_column = 'polyID',

    ##log file
    logfile = paste0('~/dart_logfile_', format(Sys.time(), '%Y-%m-%d-%H-%M-%S'), '.txt'),
    ## Output Prefix
    prefix = 'ID_',
    
    
    ## Neighborhood radius (m) -- size in m to search for reference pixels
    rad = 3000, # default is 3000
    ## Reduce number of treatment pixels?
    subSample = FALSE,
    ## Number of times to expand search radius if no candidates found
    tries = 4,
    ## Target variable name
    varname = NULL,
    
    ## Inner buffer radius (shrink treatment area to avoid edge effects) (in m)
    innerRad = 100, # defaul is 100 
    buffer = 300, # default is 300
    
    ## Predictions -- dont need to mess with 
    n_x = 20, # n columns of tiles
    n_y = 20, # n rows of tiles
    
    ## Gower settings ## how many ref pixels per target pixels are selected
    topn = 100,
    
    ## number of years to grab response (RAP) data for 
    ## Number of years with which to buffer treatment year (for extraction)
    trtYrBuff = 35,
    
    # masking variables
    # masking variables: 1 = ok, 0 = mask
    # these are only for external (reference) masks
    maskVars = list(
      refrast = "/90daydata/aeroldc/DART_MASTER/DART_rasters/refrast_conus.tif",
      nlcd = "/90daydata/aeroldc/DART_MASTER/DART_rasters/nlcd_100m_mode.tif",
      mtbs_wildfires = "/90daydata/aeroldc/DART_MASTER/DART_rasters/wildfire_mask_MTBS_81_22.tif"
      #wildfires = "/90daydata/aeroldc/GH/DART/DART_rasters/wildfire_mask_MTBS_81_22.tif"
      #rnm_trts = "/90daydata/aeroldc/GH/DART/DART_rasters/RNM_trt_mask.tif", 
      #other_rnm_trts = paste0("D:/DART_rasters/trt_masks/trt_mask_",id, ".tif"),
     ),
    
    # masking variables for inside treated area
    #interiorMaskVars = c('nlcd'),
    interiorMaskVars = c('nlcd','mtbs_wildfires'),

    #vars to filter by
    filterVars = list(
      # CONUS ext:
      soilec = "/90daydata/aeroldc/DART_MASTER/DART_rasters/ec_0to30cm_p.tif",
      fragVol = "/90daydata/aeroldc/DART_MASTER/DART_rasters/fragvol_0to30cm_p.tif",
      resdep = "/90daydata/aeroldc/DART_MASTER/DART_rasters/resdept_all_cm_p.tif",
      sand = "/90daydata/aeroldc/DART_MASTER/DART_rasters/sandtotal_0to30cm_p.tif",
      silt = "/90daydata/aeroldc/DART_MASTER/DART_rasters/silttotal_0to30cm_p.tif",
      clay = "/90daydata/aeroldc/DART_MASTER/DART_rasters/claytotal_0to30cm_p.tif"
    ),
   
   # variables for distance matrix
    topoVars = list(
      ## SoilGrids100m topo covars
        CAlog_10 = "/90daydata/aeroldc/DART_MASTER/DART_rasters/log10_ca.tif",
        PLANC = "/90daydata/aeroldc/DART_MASTER/DART_rasters/planc_2.tif",
        PROFC = "/90daydata/aeroldc/DART_MASTER/DART_rasters/profc_2.tif",
        SWI = "/90daydata/aeroldc/DART_MASTER/DART_rasters/swi_10.tif",
        RELELEV02 = "/90daydata/aeroldc/DART_MASTER/DART_rasters/relelev_2.tif",
        RELELEV16 = "/90daydata/aeroldc/DART_MASTER/DART_rasters/relelev_16.tif",
        RELELEV32 = "/90daydata/aeroldc/DART_MASTER/DART_rasters/relelev_32.tif",
        MODGMRPH = "/90daydata/aeroldc/DART_MASTER/DART_rasters/modal_gmrph_ms_30_NAD83albers.tif",
        ELEVm = "/90daydata/aeroldc/DART_MASTER/DART_rasters/DEMNED6.tif",
        SLOPE = "/90daydata/aeroldc/DART_MASTER/DART_rasters/SLPNED6.tif",
        VALLEDEP = "/90daydata/aeroldc/DART_MASTER/DART_rasters/VDPNED6.tif",
        NTOPOPEN = "/90daydata/aeroldc/DART_MASTER/DART_rasters/NEGNED6.tif",
        PTOPOPEN = "/90daydata/aeroldc/DART_MASTER/DART_rasters/POSNED6.tif",
        MELTON = "/90daydata/aeroldc/DART_MASTER/DART_rasters/MRNNED6.tif",
        VDCN = "/90daydata/aeroldc/DART_MASTER/DART_rasters/VDCNED6.tif",
        DVMNED = "/90daydata/aeroldc/DART_MASTER/DART_rasters/DVMNED6.tif",
        TPI = "/90daydata/aeroldc/DART_MASTER/DART_rasters/TPINED6.tif"
        ),
   respVars = list(NULL),
   staticVars = list( NULL)
)

## database file
dpar$dbfile = file.path( dirname( dpar$outdir ), paste0(basename(dpar$outdir), '.sqlite'))
