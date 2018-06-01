#### load necessary packages ####
library(shiny) # underlies the whole application
library(dplyr) # used for data handling
library(tidyr) # used for tidying some data for presentation
library(rCharts) # used for interactive charts, not hosted on CRAN so must be installed from github, see http://ramnathv.github.io/rCharts/
library(DT) # used for interactive tables
library(ggplot2)
library(ggthemes)
library(htmlwidgets)


################################################## 
#### this function 'region_state' corrects     ###
####  State and Region names in 'budget.level' ###  
##################################################

#the firstline defines the name of the function
#and the name of the input data which it processes 
#throughout the script
#the second line  tells R to process the function
#from i to the number of rows. Practically this means
#the fucntion is repeated for each row in the dataframe 
#processed by the function. For example 
#the function first runs with i as 1, then as 2, 
#then as 3 ...until i equals the last row  
#of the dataframe (as defined by nrows()) 
region_state <- function(raw_data) {
  for (i in 1:nrow(raw_data)) {
    
 #the following 'if' and 'else if' commands 
 #tests each row defined above (i in nrow)
 #to ensure States and Regions are named correctly
 #################################################    
 #In the example below raw_data[i,"budget.level"] tells R to 
 #to check if row i equals "Ayeyawady". If the condition 
 #is TRUE, row i is replaced with 'Ayeyawady Region' using the <- operator  
    if (raw_data[i,"budget.level"] == "Ayeyawady") {
      raw_data[i,"budget.level"] <- "Ayeyawady Region"
    }
    
    else if (raw_data[i,"budget.level"] == "Bago") {
      raw_data[i,"budget.level"] <- "Bago Region"
    }
    
    else if (raw_data[i,"budget.level"] == "Chin") {
      raw_data[i,"budget.level"] <- "Chin State"
    }
    
    else if (raw_data[i,"budget.level"] == "Kachin") {
      raw_data[i,"budget.level"] <- "Kachin State"
    }
    
    else if (raw_data[i,"budget.level"] == "Kayah") {
      raw_data[i,"budget.level"] <- "Kayah State"
    }
    
    else if (raw_data[i,"budget.level"] == "Kayin") {
      raw_data[i,"budget.level"] <- "Kayin State"
    }
    
    else if (raw_data[i,"budget.level"] == "Magway") {
      raw_data[i,"budget.level"] <- "Magway Region"
    }
    
    else if (raw_data[i,"budget.level"] == "Mandalay") {
      raw_data[i,"budget.level"] <- "Mandalay Region"
    }
    
    else if (raw_data[i,"budget.level"] == "Mon") {
      raw_data[i,"budget.level"] <- "Mon State"
    }
    
    else if (raw_data[i,"budget.level"] == "Rakhine") {
      raw_data[i,"budget.level"] <- "Rakhine State"
    }
    
    else if (raw_data[i,"budget.level"] == "Sagaing") {
      raw_data[i,"budget.level"] <- "Sagaing Region"
    }
    
    else if (raw_data[i,"budget.level"] == "Shan") {
      raw_data[i,"budget.level"] <- "Shan State"
    }
    
    else if (raw_data[i,"budget.level"] == "Tanintharyi") {
      raw_data[i,"budget.level"] <- "Tanintharyi Region"
    }
    
    else if (raw_data[i,"budget.level"] == "Yangon") {
      raw_data[i,"budget.level"] <- "Yangon Region"
    }
    
  }
  #the final 'raw_data' command below returns the updated
  # data when the function has run through all rows
  raw_data
}

#This function merges ministries and agencies to cope with changes in different administrations. 
agency_fix <- function(raw_data) {
  for (i in 1:nrow(raw_data)) {
    
  
    if (raw_data[i,"budget.entry"] == "Agriculture and Irrigation") {
      raw_data[i,"budget.entry"] <- "Agriculture, Livestock and Irrigation"
    }
    
    else if (raw_data[i,"budget.entry"] == "Livestock, Fisheries and Rural Development") {
      raw_data[i,"budget.entry"] <- "Agriculture, Livestock and Irrigation"
    }
    
    else if (raw_data[i,"budget.entry"] == "Cooperative") {
      raw_data[i,"budget.entry"] <- "Agriculture, Livestock and Irrigation"
    }
    
    else if (raw_data[i,"budget.entry"] == "Communications and Information Technology") {
      raw_data[i,"budget.entry"] <- "Transport and Communications"
    }
    
    else if (raw_data[i,"budget.entry"] == "Transport") {
      raw_data[i,"budget.entry"] <- "Transport and Communications"
    }
    
    else if (raw_data[i,"budget.entry"] == "Labour, Employment and Social Security") {
      raw_data[i,"budget.entry"] <- "Labour, Immigration and Population"
    }
    
    else if (raw_data[i,"budget.entry"] == "Immigration and Populations") {
      raw_data[i,"budget.entry"] <- "Labour, Immigration and Population"
    }
    
    else if (raw_data[i,"budget.entry"] == "Culture") {
      raw_data[i,"budget.entry"] <- "Religious Affairs and Culture"
    }
    
    else if (raw_data[i,"budget.entry"] == "Religious Affairs") {
      raw_data[i,"budget.entry"] <- "Religious Affairs and Culture"
    }
    
    else if (raw_data[i,"budget.entry"] == "Culture") {
      raw_data[i,"budget.entry"] <- "Religious Affairs and Culture"
    }
    
    else if (raw_data[i,"budget.entry"] == "Electric Power") {
      raw_data[i,"budget.entry"] <- "Electricity and Energy"
    }
    
    else if (raw_data[i,"budget.entry"] == "Electric Power No.(2)") {
      raw_data[i,"budget.entry"] <- "Electricity and Energy"
    }
    
    else if (raw_data[i,"budget.entry"] == "Energy") {
      raw_data[i,"budget.entry"] <- "Electricity and Energy"
    }
    
    else if (raw_data[i,"budget.entry"] == "National Planning and Economic Development") {
      raw_data[i,"budget.entry"] <- "Planning and Finance"
    }
    
    else if (raw_data[i,"budget.entry"] == "Finance") {
      raw_data[i,"budget.entry"] <- "Planning and Finance"
    }
    
    else if (raw_data[i,"budget.entry"] == "Finance and Revenue") {
      raw_data[i,"budget.entry"] <- "Planning and Finance"
    }
    
    else if (raw_data[i,"budget.entry"] == "Health") {
      raw_data[i,"budget.entry"] <- "Health and Sports"
    }
    
    else if (raw_data[i,"budget.entry"] == "Sports") {
      raw_data[i,"budget.entry"] <- "Health and Sports"
    }
    
    else if (raw_data[i,"budget.entry"] == "Environmental Conservation and Forestry") {
      raw_data[i,"budget.entry"] <- "Natural Resources and Environmental Conservation"
    }
    
    else if (raw_data[i,"budget.entry"] == "Mines") {
      raw_data[i,"budget.entry"] <- "Natural Resources and Environmental Conservation"
    }
    
    else if (raw_data[i,"budget.entry"] == "Sports") {
      raw_data[i,"budget.entry"] <- "Health and Sports"
    }
    
    else if (raw_data[i,"budget.entry"] == "Sports") {
      raw_data[i,"budget.entry"] <- "Health and Sports"
    }
    
    else if (raw_data[i,"budget.entry"] == "State Counsellor's Office") {
      raw_data[i,"budget.entry"] <- "Office of the State Counsellor"
    }
    
  }
  #the final 'raw_data' command below returns the updated
  # data when the function has run through all rows
  raw_data
}

### Read prepared Union and Subnatioanl budget data from a CSV file  
budget.data <- na.omit(read.csv('myanmar budget data.csv', stringsAsFactors = FALSE, na.strings="NA"))

####################################################################### 
#### Seperate the data into 'subnational' and 'union' budget data  ####
#######################################################################

# the 'filter' function is from the dplyr package and allows data to be
# seperated based on whether it meets a series of conditions 
# see here for how filter works https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# For an overview of the 'logical operators' such as &, == and !=
#see http://www.statmethods.net/management/operators.html or http://www.programiz.com/r-programming/operator

# Create separate subnational budget data
subbudget.data <- filter(budget.data, 
budget.level != 'Union'
& budget.category != "Total" 
& flow.type != "Total Supplemental"
& budget.category !='Grand Total' 
& flow.type !='Total'
)

# Create separate union budget data 
unionbudget.data <- filter(budget.data, 
                           budget.level == 'Union'
                           & budget.category != "Total" 
                           & flow.type != "Total Supplemental"
                           & budget.category !='Grand Total' 
                           & flow.type !='Total')

unionbudget.data <- agency_fix(unionbudget.data)

#create subnational income and expenditure data tables
sub.income_data <- filter(subbudget.data, flow == 'income')
sub.expenditure_data <- filter(subbudget.data, flow == 'expenditure')


#create union income and expenditure data tables without tax and SEE data
u.income_data <- filter(unionbudget.data, flow == 'income'
                        & flow.type != 'Receipts from SEEs'
                        & flow.type != 'Taxes'
                        & budget.category != 'SEEs out of Union Funds'
                        & budget.category != 'SEEs')

u.expenditure_data <- filter(unionbudget.data, flow == 'expenditure'
                             & flow.type != 'Receipts from SEEs'
                             & flow.type != 'Taxes'
                             & budget.category != 'Union Transfers to States and Regions'
                             & budget.category != 'SEEs out of Union Funds'
                             & budget.category != 'SEEs')

#extract data for graphing union SEEs
u.SEE.income_data <- filter(unionbudget.data, flow == 'income'
                        & budget.category == 'SEEs out of Union Funds'
                        | flow == 'income'
                        & budget.category == 'SEEs')

u.SEE.expenditure_data <- filter(unionbudget.data, flow == 'expenditure'
                             & budget.category == 'SEEs'
                             | flow=="expenditure"
                             & budget.category=="SEEs out of Union Funds"
                             )


###################################################
#######  start of server processing code    #######
###################################################
#Server code is standard for Shiny data visualisation. 
#It essentially tells R that this is where the calculations are conducted 

shinyServer(function(input, output, session) {
  ## see the bottom of ShinyServer() function for definitions of all reactive functions used 
######################################################
####  Start of Subnational Budget Explorer Code   ####
######################################################

  #The income and expenditure pie functions below involves three steps:
  # - 1.) Using subnational budget data it creates data (rev.pie.data) for graphing 
  # - 2.) the rev.pie.data is then used to create the pie graphs 'income_pie' 
  # - 3.) It then displays the pie graph 
  # it is then stored as an outputs 'rev.summary_pie' and 'exp.summary_pie' for the ui.R file
  #A function is used so that it can be repeated 
  #each time a user selects a different state or region  

    # Produces the 'rev.summary_pie' graph to display on the dashboard (processed by the UI.R) 
  output$rev.summary_pie <- renderChart2({
    
    # the following code selects the data 'rev.pie_data' to be used in the pie chart 
    # please note that the the 'pipe' function %>% is used to take the results of one
    # function and input it as the data used in another. In practice this means it is similar to
    # 'nesting' a function. For instance, x(y(z)) is the same as z %>% y %>% x in the example below this 
    # means that the output from the first line is used as an input into the second, 
    # the output from the second line as input into the third and so on.
    
    #the 'select' function takes the 'sub.income.data' and drops any variables not listed
    rev.pie_data <- select(sub.income_data, budget.level, flow.type, fin.year, value) %>%
      #the 'complete' function processes the dataset to remove any rows with missing data   
      complete(budget.level, flow.type, fill = list(value = 0)) %>%
      #groups the data according to 'flow.type' for summarizing below
      group_by(flow.type) %>%
      #filter function returns data according to the financial year and state/region selected
      # by the user (defined in the ui.R file)
      filter(budget.level %in% input$region_summary_select,
             fin.year %in% input$region_.fin.year_select) %>%
      #The function below summarises the data so that it can be shown as a pie graph
      summarise(value = sum(value))
    
    # produce Rcharts pie chart using RCharts 
    chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
    income_pie <- hPlot(
      x = "flow.type",
      y = "value",
      data = rev.pie_data,
      type = "pie",
      #Note that the title uses the 'paste' function to paste together text such as 
      #the financial year and state/region seelected by the user (via ui.R)
      title = paste(as.numeric(input$region_.fin.year_select)-1,"-",input$region_.fin.year_select, "Income by Budget Classification - ",input$region_summary_select) 
    )
    
    income_pie$set(width=chart_width)
    # set pie chart tool tip options - this code displays the numbers on the pie graph when the user puts their cursor over it
    income_pie$tooltip(useHTML = T,
                       formatter = "#! function() { return Highcharts.numberFormat(this.y,0) + 'm Kyat from ' + this.key ; } !#")
    
    # return pie chart object
    income_pie
  })
  
  ## the following code is almost identical to that used above, except it uses expenditure data to create the graph
  output$exp.summary_pie <- renderChart2({
    
    # prep pie chart data
    exp.pie_data <- select(sub.expenditure_data, budget.level, flow.type, fin.year, value) %>%
      complete(budget.level, flow.type, fill = list(value = 0)) %>%
      group_by(flow.type) %>%
      filter(budget.level %in% input$region_summary_select,
             fin.year %in% input$region_.fin.year_select) %>%
      summarise(value = sum(value))
    
    # produce Rcharts pie chart
    chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
    expenditure_pie <- hPlot(
      x = "flow.type",
      y = "value",
      data = exp.pie_data,
      type = "pie",
      title = paste(as.numeric(input$region_.fin.year_select)-1,"-",input$region_.fin.year_select, " Expenditure by Budget Classification - ",input$region_summary_select)
    )
    
    expenditure_pie$set(width=chart_width)
    # set pie chart tool tip options
    expenditure_pie$tooltip(useHTML = T,
                       formatter = "#! function() { return Highcharts.numberFormat(this.y,0) + 'm Kyat from ' + this.key ; } !#")
    
    # return pie chart object
    expenditure_pie
  })  
  
  ## Top ten income barchart function
  output$top_income_items <- renderPlot({
    #The '%>%' or 'pipe' function sends the results of a command
    #to the next function. eg x(y(4)) is the same as y %>% x
    
    # prepare top ten income barchart data
    #the code below first selects the data based on 
    #the selected region and financial year using the 'filter' function
    #This data is then organized and summarised by budget.level and budget.entry 
    #using the 'group_by' function and 'summarise' 
    #The top ten entries are then returned according to value
    top_10 <- sub.income_data  %>%
      filter(budget.level == input$region_summary_select & fin.year== input$region_.fin.year_select) %>%
      group_by(budget.level, budget.entry)  %>%
      summarise(value = sum(value))  %>%
      top_n(value, n = 10)  %>%
      arrange(desc(value))
    
    # produce ggplot top ten income barchart
    #Please see the tutorials below on the #ggplot2 graphing package 
    #for an overview of how this code is structured 
    #https://rstudio-pubs-static.s3.amazonaws.com/123139_f3559c860bf74fd0bd13b5396ce8bcf0.html#/
    #https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
    
    top_plot <- ggplot(top_10,
                       aes(
                         x = reorder(budget.entry, value),
                         y = value,
                         fill = budget.level
                       )) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      theme_hc() +
      ggtitle(paste("Largest Budgeted Income in", input$region_summary_select)) +
      scale_fill_manual(values = c("#0066cc", "#6789483")) +
      labs(x = NULL, y = "Million Kyat") +
      theme(
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none"
      )
    
    # return top ten income barchart
    return(top_plot)
    
  })
  
  ## Top ten expenditure barchart function
  output$top_exp_items <- renderPlot({
    
    
    #The '%>%' or 'pipe' function sends the results of a command
    #to the next function. eg x(y(4)) is the same as y %>% x
    
    # prepare top ten expenditure barchart data
    #the code below first selects the data based on 
    #the selected region and financial year using the 'filter' function
    #This data is then organized and summarised by budget.level and budget.entry 
    #using the 'group_by' function and 'summarise' 
    #The top ten entries are then returned according to value
    
    top_10 <- sub.expenditure_data  %>%
      filter(budget.level == input$region_summary_select & fin.year== input$region_.fin.year_select) %>%
      group_by(budget.level, budget.entry)  %>%
      summarise(value = sum(value))  %>%
      top_n(value, n = 10)  %>%
      arrange(desc(value))
    
    # produce ggplot top ten expenditure barchart
    #Please see the tutorials below on the #ggplot2 graphing package 
    #for an overview of how this code is structured 
    #https://rstudio-pubs-static.s3.amazonaws.com/123139_f3559c860bf74fd0bd13b5396ce8bcf0.html#/
    #https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
    top_plot <- ggplot(top_10,
                       aes(
                         x = reorder(budget.entry, value),
                         y = value,
                         fill = budget.level
                       )) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      theme_hc() +
      ggtitle(paste(
        "Largest Budgeted Expenditure in", input$region_summary_select
      )) +
      scale_fill_manual(values = c("#cc3300", "#6789483")) +
      labs(x = NULL, y = "Million Kyat") +
      theme(
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "none"
      )
        #returns the top ten expenditure barchart
    print(top_plot)
    })

  ######################################################
  #####  End of Subnational Budget Explorer Code   #####
  ######################################################    

####  SUBNATIONAL Budget Item Code ####
  ## The following code produces a bar chart based on the subnational income data
  # The resulting chart is then stored as 'item_income_chart' for use in UI.R code
  output$item_income_chart <- renderChart2({
    
    # prepare NVD3 income chart data using budget.entry_data_prep function (defined below)
    item_sub.income_data <-
      budget.entry_data_prep()(sub.income_data)   # see https://github.com/rstudio/shiny/issues/858 for why two sets of brackets are used
    
    # produce NVD3 income Chart using data created above with labels based on inputs selected (region_.fin.year_select and item_select.b.entry)
    #note that the chart is produced using a user defined function called "budget.entry_Chart" (see below)  
        budget.entry_Chart(item_sub.income_data, 
paste(as.numeric(input$region_.fin.year_select)-1,"-",input$region_.fin.year_select," Budgeted Income - ",input$item_select.b.entry ,sep=""))
    
  })
  
  ## The following code produces a bar chart based on the subnational expenditure data selected 
  # The resulting chart is then stored as 'item_expenditure_chart' for use in UI.R code

  ## produce NVD3 expenditure chart (function code at bottom of app)
  output$item_expenditure_chart <- renderChart2({
    
    # prepare NVD3 expenditure chart data
    item_sub.expenditure_data <-
      budget.entry_data_prep()(sub.expenditure_data)
    
    # produce NVD3 expenditure Chart using data created above with labels based on inputs selected (region_.fin.year_select and item_select.b.entry)
    #note that the chart is produced using a user defined function called "budget.entry_Chart" (see below)  
        budget.entry_Chart(item_sub.expenditure_data, 
paste(as.numeric(input$region_.fin.year_select)-1,"-",input$region_.fin.year_select," Budgeted Expenditure - ", input$item_select.b.entry ,sep=""))
    
  })

  
  #### UNION Budget Item Code ####
  ##Note that the function to render the chart is ordered so that income is second
  # to correct conflicts that were occuring when trying to visualize only items with income 
  #(such as taxes) but this then presents a problem with unmatched expendiutre items
  

  ## The following code produces a bar chart based on the Union expenditure data selected 
  # The resulting chart is then stored as 'u.item_expenditure_chart' for use in UI.R code
  output$u.item_expenditure_chart <- renderChart2({
    
    # prepare NVD3 Union expenditure chart data
    u.item_expenditure_data <-
      u.budget.entry_data_prep()(u.expenditure_data)
    
    # produce NVD3 expenditure Chart using data created above with labels based on inputs selected (item_select.u.b.entry)
    #note that the chart is produced using a user defined function called "u.budget.entry_Chart" (see below)  
    u.budget.entry_Chart(u.item_expenditure_data, 
                         paste(" Budgeted Expenditure - ",input$item_select.u.b.entry, sep=""))
    
  })
  
  ## The following code produces a bar chart based on the Union incomeb data selected 
  # The resulting chart is then stored as 'u.item_income_chart' for use in UI.R code
  output$u.item_income_chart <- renderChart2({
    
    # prepare NVD3 income chart data
    u.item_income_data <- 
      u.budget.entry_data_prep()(u.income_data)       # see https://github.com/rstudio/shiny/issues/858 for why two sets of brackets are used

    # produce NVD3 expenditure Chart using data created above with labels based on inputs selected (item_select.u.b.entry)
    #note that the chart is produced using a user defined function called "u.budget.entry_Chart" (see below)  
        u.budget.entry_Chart(u.item_income_data, 
                          paste(" Budgeted Income - ", input$item_select.u.b.entry,sep=""))
    
  })  
  
  ## The following code produces a bar chart based on the Union income data selected 
  # The resulting chart is then stored as 'u.SEE.item_expenditure_chart' for use in UI.R code
  output$u.SEE.item_expenditure_chart <- renderChart2({
    
    # prepare NVD3 expenditure chart data
    u.SEE.item_expenditure_data <-
      u.SEE.budget.entry_data_prep()(u.SEE.expenditure_data)
    
    # produce NVD3 expenditure Chart using data created above with labels based on inputs selected (item_select.u.b.entry)
    #note that the chart is produced using a user defined function called "u.budget.entry_Chart" (see below)  
    u.budget.entry_Chart(u.SEE.item_expenditure_data, paste(" Budgeted Expenditure - ",input$item_select.u.SEE.b.entry))
    
  })
  ## The following code produces a bar chart based on the Union income data selected 
  # The resulting chart is then stored as 'u.SEE.item_income_chart' for use in UI.R code
  output$u.SEE.item_income_chart <- renderChart2({
    
    # prepare NVD3 income chart data
    u.SEE.item_income_data <-
      u.SEE.budget.entry_data_prep()(u.SEE.income_data)       # see https://github.com/rstudio/shiny/issues/858 for why two sets of brackets are used
    
    # produce NVD3 expenditure Chart using data created above with labels based on inputs selected (item_select.u.b.entry)
    #note that the chart is produced using a user defined function called "u.budget.entry_Chart" (see below)  
        u.budget.entry_Chart(u.SEE.item_income_data, paste(" Budgeted Income - ",input$item_select.u.SEE.b.entry))
  })
  

  
  ##############################################
  ##### SUBNATIONAL Data Table Filter Code ##### 
  ##############################################  
  
  ## creates a data table 'table_data' for viewing and download in response to user specifications using the 'reactive' function
  # see here for description of reactive: http://shiny.rstudio.com/reference/shiny/latest/reactive.html
  table_data <- reactive({
    
    #the following code takes the options selected by the user from UI.R
    #and creates values suitable for filtering the data via the filter function below 
    #for the interactive datatable. For instance, if the user selects 'All' it returns
    #a list of list of all unique items listed in the variable. 
    
    #the first line creates an object 'fin.year_switched' which will contain either 
    #the option selected by the user or all possible values of the variable (in this case all values from fin.year)
    fin.year_switched <- switch(input$table_fin.year,
                              All = unique(as.character(subbudget.data$fin.year)),
                              input$table_fin.year)
    
    region_switched <- switch(input$table_region,
                              All = unique(as.character(subbudget.data$budget.level)),
                              input$table_region)
    
    flow_switched <- switch(input$table_flow,
                            All = unique(as.character(subbudget.data$flow)),
                            input$table_flow)
    
    item_switched <- switch(input$table_budget.entry,
                            All = unique(as.character(subbudget.data$budget.entry)),
                            input$table_budget.entry)
    
    budget.category_switched <- switch(input$table_budget.category,
                              All = unique(as.character(subbudget.data$budget.category)),
                              input$table_budget.category)
    
    #the following filter funciton creates a data table 'temp_table_data' based on
    #the selection chosen by the user in 'switch' code above. 
    #the %in% function essentially selects where something exists within each variable
    #for instance is the financial years selected via UI.R present in the data
    temp_table_data <- filter(
      subbudget.data,
      fin.year %in% fin.year_switched,
      budget.category %in% budget.category_switched,
      budget.level %in% region_switched,
      flow %in% flow_switched,
      budget.entry %in% item_switched
    )
    

    #this uses the user defined 'region_state' function (above) 
    #to name the region/state variables correctly
    temp_table_data <- region_state(temp_table_data)
    
    # this returns the selected data as the result of the function such that table_data equals temp_table_data
    temp_table_data
  })
    
  ## display DataTable object from 'table_data' 
  output$budget_table <- renderDataTable({
    
    datatable(table_data(), options = list(searching = FALSE))
    
  })
  

############################################
##### SUBNATIONAL Download Button Code ##### 
############################################  
  ## use the 'table_data' data selected via the function above to create a csv file for download.
  ## Resulting datatable is then accessed via UI.R via the download_table_data output
  output$download_table_data <- downloadHandler(
    
    filename = 'Myanmar Budget Data (Filtered).csv',
    content = function(file) {
      write.csv(table_data(), file)
    }
  )
  
  #ensure states and regions are all appropriately named  
  full_table_data <- region_state(subbudget.data)
  
  ## prepare full data frame appropriate for download
  #Note this draws the data directly from the csv imported at start of code 
  # add appropriate area title (region or date)
  
  ## Dusplay full data download button
  output$download_subbudget.data <- downloadHandler(
    filename = 'Myanmar Budget Data (Full).csv',
    content = function(file) {
      write.csv(full_table_data, file)
    }
  )

##############################################
### END - SUBNATIONAL Download Button Code ###  
##############################################        

  ## creates a data table 'utable_data' for viewing and download in response to user specifications using the 'reactive' function
  ## see here for description of reactive: http://shiny.rstudio.com/reference/shiny/latest/reactive.html  
  utable_data <- reactive({
    
    #the following code takes the options selected by the user from UI.R
    #and creates values suitable for filtering the data via the filter function below 
    #for the interactive datatable. For instance, if the user selects 'All' it returns
    #a list of list of all unique items listed in the variable. 
    
    #the first line creates an object 'uyear_switched' which will contain either 
    #the option selected by the user or all possible values of the variable (in this case all values from fin.year)
    uyear_switched <- switch(input$table_uyear,
                              All = unique(as.character(unionbudget.data$fin.year)),
                              input$table_uyear)
    
    uflow_switched <- switch(input$table_uflow,
                            All = unique(as.character(unionbudget.data$flow)),
                            input$table_uflow)
    
    uitem_switched <- switch(input$table_ubudget.entry,
                            All = unique(as.character(unionbudget.data$budget.entry)),
                            input$table_ubudget.entry)
    
    u.budget.category_switched <- switch(input$table_ubudget.category,
                              All = unique(as.character(unionbudget.data$budget.category)),
                              input$table_ubudget.category)
    
    #the following filter funciton creates a data table 'temp_table_uniondata' based on
    #the selection chosen by the user in 'switch' code above. 
    #the %in% function essentially selects where something exists within each variable
    #for instance is the financial years selected via UI.R present in the data
    temp_table_uniondata <- filter(
      unionbudget.data,
      budget.category %in% u.budget.category_switched,
      fin.year %in% uyear_switched,
      flow %in% uflow_switched,
      budget.entry %in% uitem_switched
    )
    

    # this returns the selected data as the result of the function such that utable_data equals temp_table_uniondata
    temp_table_uniondata
  })
  
 
  
  ## produce DataTable object from selected data for Union data
  output$ubudget_table <- renderDataTable({
    
    datatable(utable_data(), options = list(searching = FALSE))
    
  })
  
  #### Download Button Code ####  
  
  ############################################
  ######## UNION Download Button Code ####### 
  ############################################ 
  ## use the 'utable_data' data selected via the function above to create a csv file for download.
  ## Resulting datatable is then accessed via UI.R via the 'download_utable_data' output
  
  output$download_utable_data <- downloadHandler(
    
    filename = 'Myanmar Union Budget Data (Filtered).csv',
    content = function(file) {
      write.csv(utable_data(), file)
    }
  )
  outputOptions(output, 'download_utable_data', suspendWhenHidden=FALSE)
  
  ##this prepares a full data frame for download 
  #note that this data is the same which is imported at the beginning of the code 
  full_utable_data <- unionbudget.data
  
  ## Display full data download button
  output$download_unionbudget.data <- downloadHandler(
    filename = 'Myanmar Union Budget Data (Full).csv',
    content = function(file) {
      write.csv(full_utable_data, file)
    }
  )
  outputOptions(output, 'download_unionbudget.data', suspendWhenHidden=FALSE)
  
  
  
  ############################################
  ##### END UNION Download Button Code ####### 
  ############################################  
  
#### set up reactive functions used multiple times above ####
  ## Essentially each time 'function is called it is given a name (eg budget.entry_data_prep) 
  ## and a list of inputs ('unformatted_data). The reason functions are useful is as they 
  ## can be resused multiple time. In this instance the functions below are called each time
  ## a new graph needs to be drawn based on the user inputs selected via UI.R and the dataset 
  ## used (which is represented by the 'unformmatted_data' object defined in the function). 
  
  ## prep data for budget.entry NVD3 Barcharts
  budget.entry_data_prep <- reactive({
    
    # function to return formatted for subnational dashboard data
    function(unformatted_data) {
      unformatted_data %>%
        filter(budget.entry == input$item_select.b.entry & fin.year == input$region_.fin.year_select) %>%
        group_by(budget.level, flow.type) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        complete(budget.level, flow.type, fill = list(value = 0))
      
    }
  })
  
  u.budget.entry_data_prep <- reactive({
    
    # function to return formatted for union dashboard data
    #
    function(unformatted_data) {
      validate(need(input$item_select.u.b.entry %in% unformatted_data$budget.entry,
                    "Sorry! We know no such agency! Maybe it's named differently."))
      unformatted_data %>%
        filter(budget.entry == input$item_select.u.b.entry) %>%
        group_by(fin.year, flow.type) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        complete(fin.year, flow.type, fill = list(value = 0))
      
    }
  })
  
  u.SEE.budget.entry_data_prep <- reactive({
    
    # function to return formatted for union dashboard data
    function(unformatted_data) {
      
      unformatted_data %>%
        filter(budget.entry == input$item_select.u.SEE.b.entry) %>%
        group_by(fin.year, flow.type) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        complete(fin.year, flow.type, fill = list(value = 0))
      
    }
  })
 
   
  ##Start code for drawing union and subnational budget chartser 
  ## The following to functions use the budget data selected by the 
  ## User above to create interactive charts under the rCharts framework 
  ## Note that options for each chart are defined below in a 
  ##'layered' fashion similar to ggplot2 
  ## see http://ramnathv.github.io/posts/rcharts-nvd3/ for simplified examples  
  
  ## plot NVD3 barcharts for State and Region budget.entry Tab
  
  budget.entry_Chart <- function(chart_data, Title) {
    chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
    item_rchart <- nPlot(value ~ budget.level, group = 'flow.type',
                         data = chart_data,
                         type = 'multiBarChart', width=chart_width)
    
    item_rchart$chart(reduceXTicks = FALSE)
    item_rchart$xAxis(rotateLabels = -45)
        item_rchart$xAxis(axisLabel = 'State or Region')
    item_rchart$yAxis(axisLabel = 'Kyat (Millions)', width = 75)
    item_rchart$templates$script <-
      "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
    item_rchart$set(title = Title)
    item_rchart$chart(stacked = TRUE)
    item_rchart$chart(margin = list(left = 75,bottom = 75))
    
    item_rchart$chart(tooltipContent = "#! function(key, x, y, e, graph){
                      return '<h3>' + key + '</h3>' +
                      '<p>' + y + 'm Kyat spent in'  + x + '</p>'
  } !#")
    
    return(item_rchart)
}
 
  ## plot NVD3 barcharts for Union budget.entry Tab
  
  u.budget.entry_Chart <- function(chart_data, Title) {
    chart_width <- 0.95*as.numeric(JS('window.innerWidth'))
    item_rchart <- nPlot(value ~ fin.year, group = 'flow.type',
                         data = chart_data,
                         type = 'multiBarChart', width=chart_width)
    item_rchart$chart(reduceXTicks = FALSE)
    item_rchart$xAxis(rotateLabels = -45)
    item_rchart$xAxis(axisLabel = 'Financial Year')
    item_rchart$yAxis(axisLabel = 'Kyat (Millions)', width = 75)
    item_rchart$set(title = Title)
    item_rchart$chart(stacked = TRUE)
    item_rchart$chart(margin = list(left = 75,bottom = 75))
    item_rchart$templates$script <-
      "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
    return(item_rchart)
  }  
  
  output$download_text_subnat <- renderTable({ 
    table <- table_data()
      }, spacing = 'm', width = '50%')
  output$download_text_union <- renderTable({ 
    table <- utable_data()
    }, spacing = 'm', width = '50%')
  

})
