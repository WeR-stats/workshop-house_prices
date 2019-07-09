######################################################
# UK HOUSES PRICES * 21 - Calculate Metrics for MSOA #
######################################################

# Load packages
pkgs <- c('data.table', 'fst')
lapply(pkgs, require, char = TRUE)

# set constants 
data_path <- file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')
out_path <- file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')

message('Reading files...')
dts <- read_fst(file.path(data_path, 'transactions'), as.data.table = TRUE)

maxQ <- as.numeric(max(dts$tn_quarter))
y <- data.table(
        'MSOA' = character(0), 'group' = character(0), 'label' = character(0), tn_quarter = character(0),
        'sales' = integer(0), 'median' = numeric(0), 'mean' = numeric(0), 'mean_upto90pct' = numeric(0)
)
for(qe in 4:maxQ){
    qen <- levels(dts$tn_quarter)[qe]
    message('================================================')
    message('Processing MSOA for year ending ', qen, ' (', qe - 4, ' out of ', maxQ - 4, ')')
    message(' + Extracting records...')
    y0 <- dts[as.numeric(tn_quarter) %in% (qe - 3):qe]
    message(' + Calculating Totals...')
    y1 <- y0[, .(
        group = 'Totals', label = 'All', tn_quarter = qen,
        sales = .N, median = as.double(median(price)), mean = mean(price), mean_upto90pct = mean(price[price <= quantile(price, 0.9)])
    ), .(MSOA)]
    message(' + Calculating Property Type...')
    y2 <- y0[, .(
        group = 'Type',  tn_quarter = qen,
        sales = .N, median = as.double(median(price)), mean = mean(price), mean_upto90pct = mean(price[price <= quantile(price, 0.9)])
    ), .(MSOA, property_type)]
    message(' + Calculating Newly Built...')
    y3 <- y0[, .(
        group = 'New',  tn_quarter = qen,
        sales = .N, median = as.double(median(price)), mean = mean(price), mean_upto90pct = mean(price[price <= quantile(price, 0.9)])
    ), .(MSOA, is_newly_built)]
    message(' + Calculating Freehold...')
    y4 <- y0[, .(
        group = 'Freehold', tn_quarter = qen, 
        sales = .N, median = as.double(median(price)), mean = mean(price), mean_upto90pct = mean(price[price <= quantile(price, 0.9)])
    ), .(MSOA, is_freehold)]
    message(' + Binding all together...')
    y <- rbindlist(list(y, y1, y2, y3, y4), use.names = FALSE)
}

# MSOA metrics ending quarters (see also ONS )
setorderv(y, 'tn_quarter')
yx <- y[, .N, .(tn_quarter)]
yx[, n2 := cumsum(N)][, n1 := shift(n2, 1L, type = 'lag') + 1][is.na(n1), n1 := 1]
setcolorder(yx, c('tn_quarter', 'N', 'n1', 'n2'))
write_fst(yx, file.path(out_path, 'MSOA.idx'))
write_fst(y, file.path(out_path, 'MSOA'), 100)

# LONDON all transactions
london <- dts[RGN ==  'E12000007']
setorderv(london, c('LAD', 'tn_year'))
yx <- london[, .N, .(LAD, tn_year)]
yx[, n2 := cumsum(N)][, n1 := shift(n2, 1L, type = 'lag') + 1][is.na(n1), n1 := 1]
setcolorder(yx, c('LAD', 'tn_year', 'N', 'n1', 'n2'))
write_fst(yx, file.path(out_path, 'london.idx'))
write_fst(london, file.path(out_path, 'london'), 100)

message('Done! Cleaning and exit')
rm(list = ls())
gc()
