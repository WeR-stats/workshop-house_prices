# load packages
pkgs <- c('data.table', 'dygraphs', 'fst', 'ggthemes', 'scales', 'shiny', 'shinydashboard', 'xts')
lapply(pkgs, require, char = TRUE)

# load data
oas <- read_fst('~/uk_geography/datasets/output_areas', as.data.table = TRUE)
lcn <- read_fst('~/uk_geography/datasets/locations', as.data.table = TRUE)

# set constants
dpath <- file.path(Sys.getenv('PUB_PATH'), 'datasets', 'uk_land_registry')

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

ui <- dashboardPage(

    dashboardHeader(title = "UK House Prices"),

    dashboardSidebar(sidebarMenu(menuItem("tab title", tabName = "tab", icon = icon("globe")))),

    dashboardBody(

        tabItems(

            tabItem(tabName = "tab",

                fluidRow( 
                    
                    box(dygraphOutput("graph"), width = 9 ),

                    box(textOutput("legendDivID"), title = "Legend", collapsible = TRUE, width = 3)

                )
            )

        )
    )
    
)

server <- function(input, output) {
    
    output$graph <- renderDygraph({
    
        dts <- get_tns(unique(oas[CTY == 'E13000001', LAD]), is_rgn = FALSE)
        y <- dts[, .(Y = mean(price/1000) * 1000), .(X = tn_year, LAD)]
        y <- lcn[, .(location_id, Z = name)][y, on = c(location_id = 'LAD')]
        y <- dcast(y[, location_id := NULL], X~Z)
        y[, X := as.Date(paste0(X, '0101'), '%Y%m%d')]
        y <- as.data.frame(y)
        y <- xts(y[, -1], order.by = y[, 1])

        withProgress(message = "Loading...", {
            dygraph(y, main = 'House Prices', ylab = 'Year') %>% 
                dyRangeSelector() %>%
                dyLegend(labelsDiv = "legendDivID")
        })
        
    })
    
}

shinyApp(ui, server)
