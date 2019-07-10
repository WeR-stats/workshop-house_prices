##############################################
# UK HOUSES PRICES * 31 - KPIS: Display Data #
##############################################

# load packages
pkgs <- c('classInt', 'data.table', 'DT', 'fst')
lapply(pkgs, require, char = TRUE)

# set constants
data_path <- file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')

# define functions
get_tns <- function(lid, is_rgn = TRUE, ys = NA, ye = NA, cols = NULL, dpath = file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')){
    yx <- fst::read_fst(file.path(dpath, 'transactions.idx'), as.data.table = TRUE)
    if(is.na(ys)) ys <- min(yx$tn_year)
    if(is.na(ye)) ye <- max(yx$tn_year)
    if(is_rgn){
        ns <- yx[RGN == lid & tn_year == ys, min(n1)]
        ne <- yx[RGN == lid & tn_year == ye, max(n2)]
        fst::read_fst(file.path(dpath, 'transactions'), from = ns, to = ne, columns = cols, as.data.table = TRUE)
    } else {
        
    }
}

get_msoa <- function(lids, dpath = file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')){
    yx <- fst::read_fst(file.path(dpath, 'MSOA.idx'), as.data.table = TRUE)
    dts <- NULL
    for(lid in lids){
        y <- read_fst(file.path(dpath, 'MSOA'), from = , to = , as.data.table = )
    }
}
