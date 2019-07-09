#####################################################
# UK HOUSES PRICES * 16 - Download INSPIRE Polygons #
#####################################################

# load packages
pkgs <- c('data.table', 'fst', 'rvest', 'sf', 'sp')
lapply(pkgs, require, char = TRUE)

# set constants
bnd_path <- file.path(Sys.getenv('PUB_PATH'), 'boundaries', 'uk', 'INSPIRE')
start_id <- 1

message('Downloading list of names...')
y <- read_html('https://www.gov.uk/government/collections/download-inspire-index-polygons') %>% 
        html_nodes('.gem-c-document-list__item-title') %>% 
        html_text() %>% 
        as.data.table() %>% 
        setnames('LAD')
y[, LAD := gsub(',| INSPIRE.*', '', LAD)][, LAD := gsub(' ', '_', LAD)]

for(idx in start_id:nrow(y)){
    lad <- y[idx]
    message('===============================================================')
    message('Processing <', gsub('_', ' ', lad), '> [' ,  idx, ' out of ', nrow(y), ']')
    message(' + Downloading zip file...')
    tmp <- tempfile()
    download.file(paste0('http://data.inspire.landregistry.gov.uk/', lad, '.zip'), destfile = tmp)
    fzname <- setDT(unzip(tmp, list = TRUE))[order(-Length)][1, Name]
    message(' + Unzipping file...')
    unzip(tmp, files = fzname, exdir = bnd_path)
    unlink(tmp)
    message(' + Reading boundaries...')
    bnd <- read_sf(file.path(bnd_path, fzname))
    message(' + Deleting not informative columns...')
    bnd <- bnd[, c('gml_id', 'INSPIREID')]
    bnd$gml_id <- gsub('PREDEFINED.fid-', '', bnd$gml_id)
    message(' + Converting coordinates system...')
    bnd <- st_transform(bnd, CRS('+init=epsg:4326'))
    message(' + Converting to "sp" format...')
    bnd <- as(bnd, 'Spatial')
    message(' +         Saving as RDS file...')
    fname <- file.path(bnd_path, lad)
    if(file.exists(fname)) file.remove(fname)
    saveRDS(bnd, fname)
    message('Done!')
    message(' ')
}

message('===============================================================')
message('Job Complete! Cleaning and exit')
rm(list = ls())
gc()
