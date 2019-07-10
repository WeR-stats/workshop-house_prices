###############################################
# UK HOUSES PRICES * 32 - KPIS: Static Charts #
###############################################

# load packages
pkgs <- c('classInt', 'data.table', 'fst', 'ggplot2', 'ggthemes', 'scales')
lapply(pkgs, require, char = TRUE)

# load data
kpis <- read_fst('./data/kpis', as.data.table = TRUE)
vars <- fread('./data/vars.csv', stringsAsFactors = TRUE)
locations <- fread('./locations.csv', stringsAsFactors = TRUE)

# define functions
query_metric <- function(mtc, lid = NULL, parent = FALSE){
    y <- kpis[var_id == mtc][, var_id := NULL]
    if(!is.null(lid)){
        if(parent){
            y <- y[location_id %in% locations[parent_id %in% lid, location_id]]
        } else {
            y <- y[location_id %in% lid]
        }
    } 
    y <- locations[, .(location_id, name)][y, on = 'location_id']
    return(y)        
}

# set parameters
mtc <- 'X101'
lid <- 'E09000001'
pid <- c('E13000001', 'E13000002')

dts <- query_metric(mtc, lid)
ggplot(dts, aes(datefield, value)) +
    geom_line() + 
    scale_y_continuous(labels = comma) +
    labs(title = '',  vars, x = 'Month', y = vars[var_id == mtc, description]) +
    theme_minimal()

dts <- query_metric(mtc, pid, TRUE)
ggplot(dts, aes(datefield, value)) +
    geom_line() + 
    scale_y_continuous(
        breaks = pretty_breaks(), 
        labels = unit_format(unit = 'K', scale = 1e-3, sep = '', big.mark = ',')
    ) +
    facet_wrap(~ name, scale = 'free_y', nrow = 6) +
    labs(
        title = paste(vars[var_id == mtc, description], 'by London Boroughs'), 
        subtitle = paste('From', format(min(dts$datefield), '%B %Y'), 'to', format(max(dts$datefield), '%B %Y')),
        x = 'Month', y = 'Average Price',
        caption = '@2019 Land Registry'
    ) +
    theme_solarized() +
    theme(
        plot.title = element_text(margin = margin(b = 0), size = 20), 
        plot.subtitle = element_text(margin = margin(t = 5, b = 10))
    ) 

dts <- dts[year(datefield) == 2018, .(value = mean(value)), name]
ggplot(dts, aes(reorder(name, value), value)) +
    geom_col() + 
    geom_hline(aes(yintercept = median(value)), linetype = 'solid', color = 'blue', size = 1) +
    scale_y_continuous(breaks = pretty_breaks(), labels = comma) +
    labs(
        title = vars[var_id == mtc, description], 
        x = 'London Boroughs', y = 'Average Price',
        caption = '@2019 Land Registry'
    ) +
    coord_flip() +
    theme_tufte() 
