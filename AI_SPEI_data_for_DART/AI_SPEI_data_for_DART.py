#!/usr/bin/env python
# coding: utf-8

# authenticate 
import ee

ee.Authenticate()
ee.Initialize()

#import modules
import geemap
import os

chunk = 15

shp_file = '/home/bmcnellis/Documents/RestoreDART/RestoreDART_DATA/SPATIAL_DATA/LTDL_WRI_shp_250/plyID_chunk_'+ str(chunk) + '.shp'

polys = geemap.shp_to_ee(shp_file)
coll_gridmet =  ee.ImageCollection('IDAHO_EPSCOR/GRIDMET')

srt_yr = 2021
end_yr = 2024

# define a function to calcualte aridity index (ai)
def ai(image):
    image = image.addBands(image.expression(
            '(precip/(et))',
        {
                'precip': image.select('pr'),
                'et': image.select('eto'),
                 }).rename('ai'))\
                .copyProperties(image, ['system:time_start'])

    return image

coll = coll_gridmet.select(['pr', 'eto'])
coll_ai = coll.map(ai)     

years = ee.List.sequence(srt_yr, end_yr)

def yr_mean_ai(year):
    image = coll_ai.filter(ee.Filter.calendarRange(year, year, 'year')).select('ai').mean().rename('yr_mean_ai')
    return image

# map this function over all image colelction to year yearly mean ai
coll_ai_yr_mean = coll_ai.fromImages(years.map(yr_mean_ai))

# for spei
# filter based on date range, band, and feature
coll_gridmet_drought =  ee.ImageCollection('GRIDMET/DROUGHT')
band = ['spei1y']

coll_spei = coll_gridmet_drought.select(band)

def yr_mean_spei(year):
    image = coll_spei.filter(ee.Filter.calendarRange(year, year, 'year')).select('spei1y').mean().rename('yr_mean_spei')
    return image

coll_spei_yr_mean = coll_spei.fromImages(years.map(yr_mean_spei))

out_ai_stats = os.path.join(os.getcwd(), f'GRIDMET_Annual_AI_21_24_chunk_{chunk}.csv')
geemap.zonal_statistics(coll_ai_yr_mean.select('yr_mean_ai'), polys, out_ai_stats, statistics_type = 'MEAN', scale=1000)

out_spei_stats = os.path.join(os.getcwd(), f'GRIDMET_Annual_SPEI_21_24_chunk_{chunk}.csv')
geemap.zonal_statistics(coll_spei_yr_mean.select('yr_mean_spei'), polys, out_spei_stats, statistics_type = 'MEAN', scale=1000)

# end
