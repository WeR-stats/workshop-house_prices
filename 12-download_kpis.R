#########################################
# UK HOUSES PRICES * 12 - Download KPIs #
#########################################

# load packages
pkgs <- c('data.table', 'fst')
lapply(pkgs, require, char = TRUE)

# load variables names
lcn <- fread('./data/locations.csv')
vars <- fread('./data/vars.csv')

# check last version url here https://www.gov.uk/government/collections/uk-house-price-index-reports
message('Reading file from website...')
dts <- fread('http://publicdata.landregistry.gov.uk/market-trend-data/house-price-index-data/UK-HPI-full-file-2019-04.csv')
              
message('Checking Location Names...')
y <- dts[, .(AreaCode, RegionName)]
print( unique(lcn[y, on = c(location_id = 'AreaCode')][name != RegionName]) )
print( unique(y[lcn, on = c(AreaCode = 'location_id')])[name != RegionName] )

# drop "RegionName"
dts[, RegionName := NULL]

message('Renaming columns according to predefined...')
# setnames(dts, c('datefield', 'location_id', paste0('X', formatC(1:(ncol(dts) - 2), width = 2, flag = '0') ) ) )
y <- data.table(name = names(dts)[3:ncol(dts)])
y <- y[vars, on = 'name']
setnames(dts, c('datefield', 'location_id',  y[, var_id]))

message('Changing dataset shape to long form...')
dts <- melt.data.table(dts, id.vars = 1:2, variable.name = 'var_id', na.rm = TRUE)
dts <- dts[order(-datefield, location_id, var_id)]

message('Recoding date and location id...')
dts[, `:=`( datefield = as.Date(datefield, '%d/%m/%Y'), location_id = as.factor(location_id) ) ]

message('Saving as fst...')
write_fst(dts, './data/kpis', 100)

message('Done! Cleaning and exit')
rm(list = ls())
gc()
