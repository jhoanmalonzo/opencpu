app <- function() {
  library("data.table", character.only = TRUE)
  library("DT", character.only = TRUE)
  library("lubridate", character.only = TRUE)
  library("pipeR", character.only = TRUE)
  library("purrr", character.only = TRUE)
  library("RColorBrewer", character.only = TRUE)
  library("shiny", character.only = TRUE)
  library("solrium")
  library("aggregator")
  
  solr_client <- SolrClient$new()
  
  campaign_data_query_dt <- solr_client$search(
    name = "campaign",
    params = list(q = "adw_campaign_id:965415296",
                  rows = "40671",
                  fl = paste("manager_mcc_id",
                             "brand_id",
                             "client_mcc_id",
                             "adw_account_id",
                             "adw_campaign_id",
                             "adw_adgroup_id",
                             "adw_ad_id",
                             "adw_gender",
                             "adw_camp_impressions",
                             "adw_camp_views",
                             "adw_camp_clicks",
                             "adw_camp_cost",
                             "adw_date",
                             "day_of_week",
                             "week_of_year",
                             "month",
                             "year",
                             sep = ","))
  
  )
  setDT(campaign_data_query_dt)
  campaign_data_query_dt[, adw_date := as.character(date(ymd_hms(adw_date)))]
  
  campaign_data_query_dt <- melt(campaign_data_query_dt,
                                 measure.vars = c("adw_date",
                                                  "day_of_week",
                                                  "week_of_year",
                                                  "month",
                                                  "year"),
                                 variable.name = "factor_name",
                                 variable.factor = FALSE,
                                 value.name = "factor_value")
  campaign_data_query_dt <- campaign_data_query_dt[order(factor_value, decreasing = FALSE)]
  factor_names <- campaign_data_query_dt[, unique(factor_name)]
  
  ui <- fluidPage(
    # Application title
    titlePanel("Capital One 2H 2017 Campaign: Venture - Did You Know - Desktop"),
  
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "factor_name",
                    label = "Time factor:",
                    choices = factor_names,
                    selected = "day_of_week"),
        width = 3
      ),
  
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput(outputId = "ddm_table")
      )
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    color_scheme_name <- "YlOrRd"
    color_scheme <- brewer.pal(brewer.pal.info[color_scheme_name, "maxcolors"], color_scheme_name)
    break_probs <- seq(0, 1, length.out = length(color_scheme) - 1)
  
    # Create data table
    output$ddm_table <- renderDataTable({
      req(input$factor_name)
  
      ddm_table <-
        campaign_data_query_dt[factor_name == input$factor_name,
                               list(timp = sum(adw_camp_impressions),
                                    tviews = sum(adw_camp_views),
                                    tclicks = sum(adw_camp_clicks),
                                    tcost = sum(adw_camp_cost)),
                               by = "factor_value"]
  
      ddm_table <- set_calc_statistics(ddm_table)
  
      style_datatable_heatmap_columns <- pipeline({
        ddm_table[, !"factor_value"]
        (Map(
          function(col_values, col_name) {
            partial(formatStyle,
                    columns = col_name,
                    backgroundColor = styleInterval(
                      quantile(col_values[is.finite(col_values)],
                               probs = break_probs,
                               na.rm = TRUE),
                      color_scheme
                    ))
          },
          .,
          colnames(.)
        ))
        (do.call(compose, .))
      })
  
      two_decimal_cols <- colnames(ddm_table) %>>%
        (.[!(. %in% c("factor_value", "timp",
                      "tviews", "tclicks", "ctr"))])
      cols_with_sep <- colnames(ddm_table) %>>%
        (.[(. %in% c("timp", "tviews", "tclicks",
                     "tcost", "mimp", "mviews", "mclicks", "mcost"))])
  
      pipeline({
        datatable(data = ddm_table,
                  options = list(paging = FALSE),
                  rownames = FALSE)
        style_datatable_heatmap_columns
        formatCurrency(columns = cols_with_sep, interval = 3,
                       mark = ",", currency = "", digits = 0)
        formatRound(columns = two_decimal_cols, digits = 2)
        formatRound(columns = "ctr", digits = 5)
  
      })
    })
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
}
