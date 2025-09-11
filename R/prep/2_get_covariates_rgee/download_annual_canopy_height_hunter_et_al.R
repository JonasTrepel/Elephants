#download canopy height 
library(httr2)



dt_url <- data.frame(
  urls = c(
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20000101_20001231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20010101_20011231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20020101_20021231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20030101_20031231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20040101_20041231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20050101_20051231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20060101_20061231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20070101_20071231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20080101_20081231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20090101_20091231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20100101_20101231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20110101_20111231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20120101_20121231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20130101_20131231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20140101_20141231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20150101_20151231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20160101_20161231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20170101_20171231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20180101_20181231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20190101_20191231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20200101_20201231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20210101_20211231_go_epsg.4326_v1.tif",
    "https://s3.opengeohub.org/gpw/arco/gpw_short.veg.height_egbt_m_30m_s_20220101_20221231_go_epsg.4326_v1.tif"
  ),
  names = c(
    "canopy_height_2000_30m.tif",
    "canopy_height_2001_30m.tif",
    "canopy_height_2002_30m.tif",
    "canopy_height_2003_30m.tif",
    "canopy_height_2004_30m.tif",
    "canopy_height_2005_30m.tif",
    "canopy_height_2006_30m.tif",
    "canopy_height_2007_30m.tif",
    "canopy_height_2008_30m.tif",
    "canopy_height_2009_30m.tif",
    "canopy_height_2010_30m.tif",
    "canopy_height_2011_30m.tif",
    "canopy_height_2012_30m.tif",
    "canopy_height_2013_30m.tif",
    "canopy_height_2014_30m.tif",
    "canopy_height_2015_30m.tif",
    "canopy_height_2016_30m.tif",
    "canopy_height_2017_30m.tif",
    "canopy_height_2018_30m.tif",
    "canopy_height_2019_30m.tif",
    "canopy_height_2020_30m.tif",
    "canopy_height_2021_30m.tif",
    "canopy_height_2022_30m.tif"
  )
)


dt_url <- dt_url[8:16, ]
library(tidyverse)
library(httr2)
for (i in 1:nrow(dt_url)) {
  
  url <- dt_url[i, ]$urls
  name = dt_url[i, ]$names
  file_path = paste0("data/spatial_data/time_series/", name)
  

  res <- request(url) %>%
    req_perform(path = file_path, verbosity = 3)
  
  message("Saved: ", file_path)
}
