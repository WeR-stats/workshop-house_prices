#################################################
# UK HOUSES PRICES * 36 - KPIS: Choropleth Maps #
#################################################

# load packages
pkgs <- c('classInt', 'data.table', 'fst', 'htmltools', 'leaflet', 'leaflet.extras', 'RColorBrewer', 'scales', 'sp', 'stringr')
lapply(pkgs, require, char = TRUE)

# load data
kpis <- read_fst('./data/kpis', as.data.table = TRUE)
vars <- fread('./data/vars.csv', na.strings = '', stringsAsFactors = TRUE)
lcns <- fread('./data/lcns.csv', stringsAsFactors = TRUE)

# load boundaries
bnd_path <- file.path('~/uk_geography/boundaries/')
ltypes <- c('Local Authority' = 'LAD', 'County' = 'CTY', 'Region' = 'RGN')
bnd <- lapply(ltypes, function(x) readRDS(file.path(bnd_path, x)))
names(bnd) <- ltypes

# define functions
query_metric <- function(mtc, month = NA, ltype = 'LAD'){
    if(!ltype %in% ltypes) stop('Wrong type for location!')
    if(is.na(month)) month <- kpis[var_id == mtc, max(datefield)]
    lid <- lcns[type == ltype, location_id]
    y <- kpis[var_id == mtc & datefield == month & location_id %in% lid, .(location_id, value)]
    y <- lcns[, .(location_id, name)][y, on = 'location_id']
    droplevels(y)
    return(y)        
}
build_poly_label <- function(x){
    out <- paste0(
        '<hr>',
            '<b>', names(which(ltypes == area_code)), '</b>: ', y$name[x], '<br>',
            '<b>', vname, '</b>: ', 
                ifelse(y$value[x] > 0, '', '<font color="red">'), 
                format(round(y$value[x], 2),  big.mark = ','),
                ifelse(y$value[x] > 0, '', '</font>'),
                '<br>',
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
vid <- 'X403'
vname <- vars[var_id == vid, description]
vsname <- ifelse(
    is.na(vars[var_id == vid, subdomain]), 
    NA, 
    paste0(vars[var_id == vid, domain], ': ', vars[var_id == vid, subdomain])
)
area_code <- 'LAD'
dts <- query_metric(vid, ltype = area_code)

# merge with boundaries
y <- merge(bnd[[area_code]], dts, by.x = 'id', by.y = 'location_id')

# build palette for polygons
brks_poly <- 
    if(class.method == 'fixed'){
        classIntervals( y[['value']][!is.na(y$value)], n = 11, style = 'fixed', fixedBreaks = fixed_brks)
    } else {
        classIntervals(y[['value']][!is.na(y$value)], n_brks, class.method)
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
            labels = get_legend(y[['value']], brks_poly$brks, n_brks),
    		position = 'bottomright',
            title = vname,
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
    		        paste('<p style="padding:10px 5px 5px 10px;margin:0px;">Distribution of', vname, 'by', names(which(ltypes == area_code)), '</p>'),
    		        ifelse(is.na(vsname), '', paste('<p style="padding:5px 5px 10px 10px;margin:0px;">', vsname, '</p>')),
    		    '</div>'
    		))), 
    		position = 'bottomleft'
    	)
        
# show map
print(mp)
