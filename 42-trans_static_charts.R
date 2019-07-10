#######################################################
# UK HOUSES PRICES * 42 - TRANSACTIONS: Static Charts #
#######################################################

# load packages
pkgs <- c('classInt', 'data.table', 'fst', 'ggplot2', 'ggthemes', 'scales')
lapply(pkgs, require, char = TRUE)

# load data
oas <- read_fst('~/uk_geography/datasets/output_areas', as.data.table = TRUE)
lcn <- read_fst('~/uk_geography/datasets/locations', as.data.table = TRUE)

# define functions
get_tns_lad <- function(lid, ys = NA, ye = NA, cols = NULL, dpath = file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')){
    yx <- read_fst(file.path(dpath, 'transactions.idx'), as.data.table = TRUE)
    if(is.na(ys)) ys <- min(yx$tn_year)
    if(is.na(ye)) ye <- max(yx$tn_year)
    ns <- yx[LAD == lid & tn_year == ys, min(n1)]
    ne <- yx[LAD == lid & tn_year == ye, max(n2)]
    read_fst(file.path(dpath, 'transactions'), from = ns, to = ne, columns = cols, as.data.table = TRUE)
}
get_tns <- function(lid, is_rgn = TRUE, ys = NA, ye = NA, cols = NULL, dpath = file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')){
    yx <- read_fst(file.path(dpath, 'transactions.idx'), as.data.table = TRUE)
    y <- data.table()
    if(is_rgn){
        lids <- unique(yx[RGN == lid, LAD])
        for(id in lids)
            y <- rbindlist(list( y, get_tns_lad(id, ys, ye, cols, dpath)))
    } else {
        for(id in lid)
            y <- rbindlist(list( y, get_tns_lad(id, ys, ye, cols, dpath)))
    }
    return(y)
}

# load london
dts <- get_tns('E12000007', ys = 2015, ye = 2018)
y <- dts[, .N, .(X = tn_dom, Y = tn_moy)]
ggplot(y, aes(X, Y, fill = N)) + 
    geom_tile(color = 'white') + 
    coord_equal() + 
    scale_fill_distiller(palette = 'BuGn', direction = 1) + 
    theme_solarized()

# load inner london
dts <- get_tns(unique(oas[CTY == 'E13000001', LAD]), is_rgn = FALSE)
y <- dts[, .(Y = mean(price/1000) * 1000), .(X = tn_year, LAD)]
y <- lcn[, .(location_id, Z = name)][y, on = c(location_id = 'LAD')]

# time series
ggplot(y[location_id == 'E09000007'], aes(X, Y)) + 
    geom_line() 

ggplot(y, aes(X, Y)) + 
    geom_line(aes(colour = Z))

ggplot(y, aes(X, Y)) + 
    geom_line(aes(colour = Z)) +
    labs(
        title = 'Average House Price for Boroughs in Inner London', 
        x = 'year', 
        y = 'price',
        colour = 'London Boroughs'
    ) +
    scale_y_continuous(labels = comma) +
    theme_clean() +
    theme(legend.position = 'bottom') # 'none' to remove legend

ggplot(y, aes(X, Y)) + 
    geom_line() +
    scale_y_continuous(
        breaks = pretty_breaks(),
        labels = unit_format(unit = 'K', scale = 1e-3, sep = '', big.mark = ',')
    ) +
    facet_wrap(~Z) +
    theme_fivethirtyeight()

ggplot(y, aes(X, Y)) + 
    geom_line(aes(colour = Z)) +
    scale_y_continuous(
        breaks = pretty_breaks(),
        labels = unit_format(unit = 'K', scale = 1e-3, sep = '', big.mark = ',')
    ) +
    facet_wrap(~Z, scale = 'free_y') +
    theme_fivethirtyeight()

# dygraphs
library(dygraphs)
library(xts)

y <- dcast(y[, location_id := NULL], X~Z)
y[, X := as.Date(paste0(X, '0101'), '%Y%m%d')]
y <- as.data.frame(y)
y <- xts(y[, -1], order.by = y[, 1])
dygraph(y)

# boxplot
y <- dts[price <= quantile(price, 0.9)]

ggplot(y, aes(x = tn_dow, y = price)) +
    geom_boxplot() +
    scale_y_continuous(labels = comma) +
    theme_solarized()

ggplot(y, aes(x = reorder(tn_dow, -price, median), y = price)) +
    geom_boxplot() +
    scale_y_continuous(labels = comma) +
    theme_solarized()

ggplot(y, aes(x = tn_dow, y = price)) +
    geom_boxplot(aes(fill = property_type)) +
    scale_y_continuous(labels = comma) +
    theme_solarized()

# histogram
ggplot(dts, aes(x = price)) +
    geom_histogram() 

ggplot(y, aes(x = price)) +
    geom_histogram(binwidth = 10000) 

ggplot(y, aes(x = price)) +
    geom_histogram(binwidth = 10000) +
    facet_wrap(~is_newly_built)

ggplot(y, aes(x = price, colour = is_freehold)) +
    geom_histogram(binwidth = 10000)

ggplot(y, aes(x = price, fill = is_freehold)) +
    geom_histogram(binwidth = 10000)
