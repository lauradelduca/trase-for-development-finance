#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rCharts)
library(dplyr)
library(sankeyD3)



# Define UI for application that draws a sankey and displays a downloadable table of the results
ui <- fluidPage(
   
   # Application title
   titlePanel("OECD CRS database 1973 - 2015"),
   
   # Explain
   # shows sankey and table of the 50 largest flows, by USD commitment or USD disbursement
   # can download full table
   # in millions USD
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("range",
                     "Years:",
                     min = 1973,
                     max = 2015,
                     value = c(1973, 2015)),
         
         # should default to all...
         selectizeInput('donor', 'Donor', 
                        choices = c("Adaptation Fund", "African Development Bank", "African Development Fund", 
                                    "Arab Bank for Economic Development in Africa", "Arab Fund (AFESD)", "AsDB Special Funds", 
                                    "Asian Development Bank", "Australia", "Austria", "Azerbaijan", "Belgium", 
                                    "Bill & Melinda Gates Foundation", "Bulgaria", "Canada", "Caribbean Development Bank", 
                                    "Chinese Taipei", "Climate Investment Funds", "Council of Europe Development Bank",
                                    "Croatia", "Cyprus", "Czech Republic", "Denmark", "Dutch Postcode Lottery", "Estonia", 
                                    "EU Institutions", "European Bank for Reconstruction and Development", "Finland", 
                                    "Food and Agriculture Organisation", "France", "Germany", 
                                    "Global Alliance for Vaccines and Immunization", "Global Environment Facility", 
                                    "Global Fund", "Global Green Growth Institute", "Greece", "Green Climate Fund", 
                                    "Hungary", "Iceland", "IDB Invest", "IDB Special Fund", "IFAD",  
                                    "IMF (Concessional Trust Funds)", "Inter-American Development Bank", 
                                    "International Atomic Energy Agency", "International Bank for Reconstruction and Development", 
                                    "International Development Association", "International Finance Corporation", 
                                    "International Labour Organisation", "Ireland", "Islamic Development Bank", "Israel",
                                    "Italy", "Japan", "Kazakhstan", "Korea", "Kuwait", "Kuwait (KFAED)", "Latvia", 
                                    "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Montreal Protocol", "Netherlands", 
                                    "New Zealand", "Nordic Development Fund", "Norway", "OPEC Fund for International Development", 
                                    "OSCE", "People's Postcode Lottery", "Poland", "Portugal", "Romania", "Russia", 
                                    "Saudi Arabia", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
                                    "Swedish Postcode Lottery", "Switzerland", "Thailand", "Timor-Leste", "Turkey", 
                                    "UN Peacebuilding Fund", "UNAIDS", "UNDP", "UNECE", "UNEP", "UNFPA", "UNHCR", 
                                    "UNICEF", "United Arab Emirates", "United Kingdom", "United States", "UNRWA", "WFP", 
                                    "World Health Organisation", "World Tourism Organisation"), 
                        multiple = TRUE),
         
         
         
         selectizeInput('destination', 'Destination', 
                        choices = list("Africa" = c("Africa, regional", "Algeria", "Angola", "Benin", "Botswana", 
                                                    "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", 
                                                    "Central African Republic", "Chad", "Comoros", "Congo",
                                                    "Côte d'Ivoire", "Democratic Republic of the Congo", "Djibouti", 
                                                    "East African Community", "Egypt", "Equatorial Guinea", "Eritrea", 
                                                    "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                                                    "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                                                    "Mali", "Mauritania", "Mauritius", "Mayotte", "Morocco", "Mozambique", 
                                                    "Namibia", "Niger", "Nigeria", "North of Sahara, regional", "Rwanda", 
                                                    "Saint Helena", "Sao Tome and Principe", "Senegal", "Seychelles", 
                                                    "Sierra Leone", "Somalia", "South Africa", "South of Sahara, regional", 
                                                    "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", 
                                                    "Uganda", "Zambia", "Zimbabwe"),
                                       
                                       "America" = c("America, regional", "Anguilla", "Antigua and Barbuda", "Argentina", 
                                                     "Aruba", "Bahamas", "Barbados", "Belize", "Bermuda", "Bolivia", 
                                                     "Brazil", "British Virgin Islands", "Cayman Islands", "Chile", 
                                                     "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", 
                                                     "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti", 
                                                     "Honduras", "Jamaica", "Mexico", "Montserrat", "Netherlands Antilles", 
                                                     "Nicaragua", "North & Central America, regional", "Panama", "Paraguay", 
                                                     "Peru", "Saint Kitts and Nevis", "Saint Lucia", 
                                                     "Saint Vincent and the Grenadines", "South America, regional", "Suriname", 
                                                     "Trinidad and Tobago", "Turks and Caicos Islands", "Uruguay", "Venezuela", 
                                                     "West Indies, regional"),
                                       
                                       "Asia" = c("Afghanistan", "Armenia", "Asia, regional", "Azerbaijan", "Bahrain", 
                                                  "Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", 
                                                  "Central Asia, regional", "China (People's Republic of)", "Chinese Taipei", 
                                                  "Democratic People's Republic of Korea", "Far East Asia, regional", "Georgia",
                                                  "Hong Kong, China", "India", "Indonesia", "Iran", "Iraq", "Israel", "Jordan", 
                                                  "Kazakhstan", "Korea", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", 
                                                  "Lebanon", "Macau, China", "Malaysia", "Maldives", "Middle East, regional", 
                                                  "Mongolia", "Myanmar", "Nepal", "Oman", "Pakistan", "Philippines", "Qatar", 
                                                  "Saudi Arabia", "Singapore", "South & Central Asia, regional", 
                                                  "South Asia, regional", "Sri Lanka", "Syrian Arab Republic", "Tajikistan", 
                                                  "Thailand", "Timor-Leste", "Turkmenistan", "United Arab Emirates", "Uzbekistan", 
                                                  "Viet Nam", "West Bank and Gaza Strip", "Yemen"),
                                       
                                       "Europe" = c("Albania", "Belarus", "Bosnia and Herzegovina", "Croatia", "Cyprus", 
                                                    "Europe, regional", "Former Yugoslav Republic of Macedonia", "Gibraltar", 
                                                    "Kosovo", "Malta", "Moldova", "Montenegro", "Serbia", "Slovenia", 
                                                    "States Ex-Yugoslavia", "Turkey", "Ukraine"),
                                       
                                       "Oceania" = c("Cook Islands", "Fiji", "French Polynesia", "Kiribati", "Marshall Islands",
                                                     "Micronesia", "Nauru", "New Caledonia", "Niue", "Northern Mariana Islands",
                                                     "Oceania, regional", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", 
                                                     "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna"),
                                       
                                       "Regional and Unspecified" = c("Bilateral, unspecified")),
                        
                        multiple = TRUE), 
                        
         
         radioButtons("variable", "Additional Variable (optional)",
                      choices = c("None selected", "Agency.Name", "Region.Name", "Income.Group.Name", "Flow.Name", "Purpose.Name", "Sector.Name"),
                      selected = NULL),
         
         downloadButton("downloadData", "Download")
         
      ),
      
      # Show a plot and downloadable table
      mainPanel(
         chartOutput('sankey', 'C:/Users/laura.delduca/d3_sankey'),
         #sankeyNetworkOutput('sankey', width = '100\%', height = '500px'),
         br(), br(),
         tableOutput("results")
      )
   )
)

# Define server logic required to draw a sankey and display a downloadable table
server <- function(input, output) {
        
        data <- read.csv("CRS_1973_2015_data.csv", sep = ';')
        
        
        filtered <- reactive({
  
                if (is.null(input$range)) {return(NULL)}
                if (is.null(input$donor)) {return(NULL)} 
                if (is.null(input$destination)) {return(NULL)}
                if (is.null(input$variable)) {return(NULL)} 
                
                a <- subset(data, data$Year >= input$range[1])
                a <- subset(a, a$Year <= input$range[2])
                a <- subset(a, a$'Donor.Name' == input$donor)
                a <- subset(a, a$'Recipient.Name' == input$destination)
                
                a <- a[order(a$Year, -as.numeric(a$'USD.Commitment')),]
                
                return( a[1:min( nrow(a), 30 ),] )

        })
        
        
        sankeydata <- reactive({
                
                if (is.null(input$range)) {return(NULL)}
                if (is.null(input$donor)) {return(NULL)} 
                if (is.null(input$destination)) {return(NULL)}
                if (is.null(input$variable)) {return(NULL)}
                
                if (input$variable == 'None selected') {

                        a <- subset(data, data$Year >= input$range[1])
                        a <- subset(a, a$Year <= input$range[2])
                        a <- subset(a, a$'Donor.Name' == input$donor)
                        a <- subset(a, a$'Recipient.Name' == input$destination)
                        
                        a <- a[, c('Donor.Name', 'Recipient.Name', 'USD.Commitment')]
                        colnames(a) <- c('source', 'target', 'value')
                        
                        return(a[1:min( nrow(a), 30 ),])
                }
                
                else {
                        a <- subset(data, data$Year >= input$range[1])
                        a <- subset(a, a$Year <= input$range[2])
                        a <- subset(a, a$'Donor.Name' == input$donor)
                        a <- subset(a, a$'Recipient.Name' == input$destination)
                        
                        a1 <- a[, c('Donor.Name', input$variable, 'USD.Commitment')]
                        a2 <- a[, c(input$variable, 'Recipient.Name', 'USD.Commitment')]
                        colnames(a1) <- colnames(a2) <- c('source', 'target', 'value')
                        a <- rbind(a1, a2)
                
                        return(a[1:min( nrow(a), 30 ),])
                }
                
        })
        
        
        output$sankey <- renderChart({
                
                #if (is.null(input$range)) {return(NULL)}
                #if (is.null(input$donor)) {return(NULL)} 
                #if (is.null(input$destination)) {return(NULL)}
                #if (is.null(input$variable)) {return(NULL)} 
                
                
                sankeyPlot <- rCharts$new()
                sankeyPlot$setLib('C:/Users/laura.delduca/d3_sankey')
                
                
                test <- data.frame(c('a','b','c'), c('d','e','f'), c(2,1,1))
                colnames(test) <- c('source', 'target', 'value')
                
                
                sankeyPlot$set(
                        #data = sankeydata,
                        data = test,
                        nodeWidth = 15,
                        nodePadding = 10,
                        layout = 32,
                        width = 750,
                        height = 500,
                        labelFormat = ".1%"
                )
                
                sankeyPlot
                
                
                #sankeyNetwork(
                #        Links = sankeydata,
                #        Nodes = ,
                #        Source = 'source',
                #        Target = 'target',
                #        Value = 'value',
                #        NodeID = 'name',
                #        units = 'million USD',
                #        fontSize = 12,
                 #       nodeWidth = 30
                #),
                
                #env = parent.frame(),
                
                #quoted = FALSE
                
        })
        
        
        output$results <- renderTable( sankeydata() )
        
        output$downloadData <- downloadHandler(
                
                filename = function() {
                        paste('OECD_CRS_1973_2015_', Sys.Date(), '.csv', sep = '')
                },
                content = function(file) {
                        write.csv2(filtered(), file, row.names = FALSE, quote = FALSE)
                }
        )
   
}


# Run the application 
shinyApp(ui = ui, server = server)

