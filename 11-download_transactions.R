#################################################
# UK HOUSES PRICES * 11 - Download Transactions #
#################################################

# Load packages
pkgs <- c('data.table', 'fasttime', 'fst', 'ISOweek', 'lubridate')
lapply(pkgs, require, char = TRUE)

# set constants
out_path <- file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')
geo_path <- file.path('~', 'uk_geography', 'datasets')

message('Reading file from website... (relax, it is more than 4GB)')
dts <- fread('http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv', 
             header = FALSE, na.string = '',
             select = c(2:7, 15),
             col.names = c('price', 'tn_date', 'postcode', 'property_type', 'is_newly_built', 'is_freehold', 'is_standard_price')
)

message('Cleaning postcode and convert it to fixed 7-chars...')
dts[, postcode := toupper(gsub('[[:punct:]| ]', '', postcode))]
dts[!grepl('[[:digit:]][[:alpha:]][[:alpha:]]$', postcode), postcode := NA] # Apr-2019: 35,983
dts[grepl('^[0-9]', postcode), postcode := NA]                              # Apr-2019: 0
dts[nchar(postcode) < 5 | nchar(postcode) > 7, postcode := NA]              # Apr-2019: 0
dts[nchar(postcode) == 5, postcode := paste0( substr(postcode, 1, 2), '  ', substring(postcode, 3) ) ]
dts[nchar(postcode) == 6, postcode := paste0( substr(postcode, 1, 3), ' ', substring(postcode, 4) ) ]

message('Adding output area...')
pc <- read_fst(file.path(geo_path, 'postcodes'), columns = c('OA', 'postcode'), as.data.table = TRUE)
dts <- pc[dts, on = 'postcode'] # NA for Apr-2019 (minus above): 2,218 (total: 38,201)

message('Applying various recoding...')

# recode is_newly_built as Y=1-Yes / N=2-No
dts[ data.table( is_newly_built = c('N', 'Y'), to = 0:1), on = 'is_newly_built', is_newly_built := i.to ]

# recode is_freehold as F=1-Yes / <other>=2-No
dts[is_freehold == 'F', is_freehold := '1' ]
dts[is_freehold != '1', is_freehold := '0' ]

# recode is_standard_price as A=1-Yes / B=2-No
dts[ data.table( is_standard_price = c('B', 'A'), to = as.character(0:1)), on = 'is_standard_price', is_standard_price := i.to ]

message('Reordering records by date, postcode, and price...')
setorderv(dts, c('tn_date', 'postcode', 'price'))

message('Recoding date and categoricals...')
dts <- dts[, `:=`(
    postcode = factor(postcode), 
    tn_date = as.IDate(fastPOSIXct(tn_date)),
    property_type = factor(property_type, 
        levels = c('D', 'S', 'T', 'F', 'O'), 
        labels = c('Detached', 'Semi-Detached', 'Terraced', 'Flat', 'Other'),
        ordered = TRUE
    ),
    is_newly_built = factor(is_newly_built, labels = c('No','Yes') ), 
    is_freehold = factor(is_freehold, labels = c('No', 'Yes') ), 
    is_standard_price = factor(is_standard_price, labels = c('No', 'Yes') )
)]

message('Adding various location codes, deleting unmatched...')
oas <- read_fst(file.path(geo_path, 'output_areas'), as.data.table = TRUE)
dts <- oas[dts, on = 'OA'][!is.na(OA)]

message('Adding time related fields...')
md <- min(year(dts$tn_date))
Md <- max(year(dts$tn_date))
ord_days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
ord_months <- apply( expand.grid(month.abb, md:Md), 1, function(x) paste(x, collapse = ' ') )
ord_qrtrs <- sort(apply( expand.grid(md:Md, 1:4), 1, function(x) paste(x, collapse = ' Q') ))
dts[, `:=`(
    # year (YYYY)
    tn_year = year(tn_date),
    # quarter of year (Qx)
    tn_qoy = factor(quarters(tn_date), ordered = TRUE),
    # month of year ()
    tn_moy = factor(months(tn_date), levels = month.name, ordered = TRUE),
    # week of year ()
    tn_woy = week(tn_date),
    # week ()
    tn_week = factor(ISOweek(tn_date), ordered = TRUE),
    # day of week ()
    tn_dow = factor(weekdays(tn_date), levels = ord_days, ordered = TRUE),
    # day of month ()
    tn_dom = mday(tn_date)
)][, `:=`(
    # quarters (YYYY Qx)
    tn_quarter = factor(paste(tn_year, tn_qoy), levels = ord_qrtrs, ordered = TRUE),
    # month ()
    tn_month = factor(paste(substr(tn_moy, 1, 3), tn_year), levels = ord_months, ordered = TRUE) 
)]
# delete last month
dts <- dts[tn_month < max(tn_month)]
# delete all missing factors' levels
dts <- droplevels(dts)

# retrocumulative number of years, months, weeks, and days
dts <- unique(dts[, .(tn_year)])[order(-tn_year)][, years_to := 1:.N][dts, on = 'tn_year']
dts <- unique(dts[, .(tn_month)])[order(-tn_month)][, months_to := 1:.N][dts, on = 'tn_month']
dts <- unique(dts[, .(tn_week)])[order(-tn_week)][, weeks_to := 1:.N][dts, on = 'tn_week']
dts <- unique(dts[, .(tn_date)])[order(-tn_date)][, days_to := 1:.N][dts, on = 'tn_date']

message('Reordering columns...')
cols_geo <- names(oas)
setcolorder(dts, c(
    'tn_date', 'property_type', 'is_newly_built', 'is_freehold', 'is_standard_price', 'price', 
    'postcode', cols_geo,
    'tn_week', 'tn_month', 'tn_quarter', 'tn_year', 'tn_dow', 'tn_dom', 'tn_woy', 'tn_moy', 'tn_qoy', 
    'days_to', 'weeks_to', 'months_to', 'years_to'
))

message('Saving as fst with index on RGN, LAD, and year...')
setorderv(dts, c('RGN', 'LAD', 'tn_year'))
yx <- dts[, .N, .(RGN, LAD, tn_year)]
yx[, n2 := cumsum(N)][, n1 := shift(n2, 1L, type = 'lag') + 1][is.na(n1), n1 := 1]
setcolorder(yx, c('RGN', 'LAD', 'tn_year', 'N', 'n1', 'n2'))
write_fst(yx, file.path(out_path, 'transactions.idx'))
write_fst(dts, file.path(out_path, 'transactions'), 100)

message('Saving as fst with index on year and month...')
setorderv(dts, c('tn_year', 'tn_month'))
yx <- dts[, .N, .(tn_year, tn_month)]
yx[, n2 := cumsum(N)][, n1 := shift(n2, 1L, type = 'lag') + 1][is.na(n1), n1 := 1]
setcolorder(yx, c('tn_year', 'tn_month', 'N', 'n1', 'n2'))
write_fst(yx, file.path(out_path, 'transactions_ym.idx'))
write_fst(dts, file.path(out_path, 'transactions_ym'), 100)

message('Done! Cleaning and exit')
rm(list = ls())
gc()
