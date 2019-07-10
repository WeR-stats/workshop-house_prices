########################################################
# UK HOUSES PRICES * 46 - TRANSACTIONS Choropleth Maps #
########################################################

# load packages
pkgs <- c('classInt', 'data.table', 'fst', 'htmltools', 'leaflet', 'leaflet.extras', 'RColorBrewer', 'scales', 'sp', 'stringr')
lapply(pkgs, require, char = TRUE)

# load data
lcns <- fread('./data/lcns.csv', stringsAsFactors = TRUE)

# load boundaries
bnd_path <- file.path('~/uk_geography/boundaries/')
bnd <- readRDS(file.path(bnd_path, 'MSOA'))

# define functions
get_msoa <- function(q = NA, dpath = file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')){
    yx <- read_fst(file.path(dpath, 'MSOA.idx'), as.data.table = TRUE)
    if(is.na(q)) q <- max(yx$tn_quarter)
    read_fst(file.path(dpath, 'MSOA'), from = yx[tn_quarter == q, n1], to = yx[tn_quarter == q, n2], as.data.table = TRUE)
}
query_metric <- function(grp = NA, lbl = NA, q = NA, dpath = file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')){
    y <- get_msoa(q, dpath)
    if(is.na(grp)) return( y[group == 'All'] )
    if(is.na(lbl)) return( y[group == grp] )
    return(y[group == grp & label == lbl])
}
build_poly_label <- function(x){
    out <- paste0(
        '<hr>',
            '<b>MSOA</b>: ', y$name[x], '<br>',
            '<b>', mtc, '</b>: ', format(round(y$X[x], 2),  big.mark = ','), '<br>',
        '<hr>'
    )
    HTML(out)
}
get_legend <- function(Y, brks_lim, n_brks) {
    lbl_brks <- format(round(brks_lim, 3), nsmall = 1)
    pad <- max(nchar(lbl_brks))
    sapply(1:n_brks,
        function(x)
            paste0(
                str_pad(lbl_brks[x], pad), ' ÷ ', str_pad(lbl_brks[x + 1], pad),
                ' (', length(Y[!is.na(Y) & Y >= as.numeric(gsub(',', '', lbl_brks[x])) & Y < as.numeric(gsub(',', '', lbl_brks[x + 1])) ] ), ')'
            )
    )
}

# set parameters
mtc <- 'median'  # one in: 'sales', 'median', 'mean', 'mean_upto90pct'
bsz <- 6         # BORDER Width (pixels)
bcl <- '#727272' # BORDER Colour
btp <- 5         # BORDER Transparency (1-10)
hcl <- 'white'   # HIGHLIGHT BORDER Colour
hsz <- 8         # HIGHLIGHT BORDER Width (pixels)
ftp <- 7         # FILL Transparency (1-10)
smt <- 1         # smoothFactor
n_brks <- 9
class.method <- 'quantile'
## - 'Fixed' = 'fixed',                  need an additional argument fixedBreaks that lists the n+1 values to be used
## - 'Equal Intervals' = 'equal',        the range of the variable is divided into n part of equal space
## - 'Quantiles' = 'quantile',           each class contains (more or less) the same amount of values
## - 'Pretty Integers' = 'pretty',       sequence of about ‘n+1’ equally spaced ‘round’ values which cover the range of the values in ‘x’. The values are chosen so that they are 1, 2 or 5 times a power of 10.
## - 'Natural Breaks' = 'jenks',         seeks to reduce the variance within classes and maximize the variance between classes
## - 'Hierarchical Cluster' = 'hclust',  Cluster with short distance
## - 'K-means Cluster' = 'kmeans'        Cluster with low variance and similar size
fixed_brks <- seq(0, 5, 0.5)
use_palette <- TRUE
# pal_pkg <- ''
pal_name <- 'BrBG'
rev_pal <- TRUE
map_colours <- c('#0AC75F', '#ECFF59', '#CC9710')

# query dataset
dts <- query_metric('Type', 'Flat')

# merge with boundaries
y <- merge(bnd, dts[, .(MSOA, X = get(mtc))], by.x = 'id', by.y = 'MSOA')

# build palette for polygons
brks_poly <- 
    if(class.method == 'fixed'){
        classIntervals( y$X[!is.na(y$X)], n = 11, style = 'fixed', fixedBreaks = fixed_brks)
    } else {
        classIntervals(y$X[!is.na(y$X)], n_brks, class.method)
    }
col_codes <- 
    if(use_palette){
        yc <-
            if(n_brks > brewer.pal.info[pal_name, 'maxcolors']){
                colorRampPalette(brewer.pal(brewer.pal.info[pal_name, 'maxcolors'], pal_name))(n_brks)
            } else {
                brewer.pal(n_brks, pal_name)
            }
        if(rev_pal) yc <- rev(yc)
        yc
    } else {
        colorRampPalette(c(map_colours[1], map_colours[2], map_colours[3]))(n_brks)
    }
pal_poly <- findColours(brks_poly, col_codes)

# create map
mp <- leaflet(options = leafletOptions(minZoom = 6)) %>%
        # center and zoom
        fitBounds(y@bbox[1], y@bbox[2], y@bbox[3], y@bbox[4]) %>%
        # add maptiles
    	addProviderTiles(providers$CartoDB.Positron, group = 'CartoDB Positron') %>%
    	addProviderTiles(providers$OpenStreetMap.Mapnik, group = 'OSM Mapnik') %>%
    	addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = 'OSM B&W') %>%
    	addProviderTiles(providers$Stamen.Toner, group = 'Stamen Toner') %>%
    	addProviderTiles(providers$Stamen.TonerLite, group = 'Stamen Toner Lite') %>% 
    	addProviderTiles(providers$Hydda.Full, group = 'Hydda Full') %>%
    	addProviderTiles(providers$Hydda.Base, group = 'Hydda Base') %>% 
        # add map reset button 
        addResetMapButton() %>% 
        # add polygons layer
        addPolygons(
    		data = y,
            stroke = TRUE,
            color = bcl,
            opacity = 1 - as.numeric(btp) / 10,
            weight = as.integer(bsz) / 10,
            smoothFactor = smt,
            fill = TRUE,
    		fillColor = pal_poly, 
            fillOpacity = 1 - as.numeric(ftp) / 10,
            highlightOptions = highlightOptions(
                color = hcl,
                weight = as.integer(hsz),
                opacity = 1,
                bringToFront = TRUE
            ),
            label = lapply(1:nrow(y), build_poly_label),
        	labelOptions = labelOptions(
        		textsize = '12px',
        		direction = 'right',
        		sticky = FALSE,
        		offset = c(80, -50),
        		style = list('font-weight' = 'normal', 'padding' = '2px 6px')
        	)
        ) %>% 
        # add Legend
    	addLegend(
            colors = col_codes,
            labels = get_legend(y$X, brks_poly$brks, n_brks),
    		position = 'bottomright',
            title = mtc,
    		opacity = 0.8
    	) %>% 
        # add layer control
    	addLayersControl(
    		baseGroups = c('CartoDB Positron', 'OSM Mapnik', 'OSM B&W', 'Stamen Toner', 'Stamen Toner Lite', 'Hydda Full', 'Hydda Base'),
    		options = layersControlOptions(collapsed = TRUE)
    	) %>% 
        # add title
    	addControl(
    		tags$div(HTML(paste0(
    		    '<div style="font-size:20px;background-color:#F7E816;">',
    		        paste('<p style="padding:10px 5px 5px 10px;margin:0px;">Distribution of', mtc, 'by MSOA</p>'),
    		    '</div>'
    		))), 
    		position = 'bottomleft'
    	)
        
# show map
print(mp)

# save map
# saveWidget(mp, file = '')
