library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(psych)
library(tidytext)
library(party)
library(rpart)
library(rpart.plot)
library(gplots)
library(plotly)
library(heatmaply)
library(ggcorrplot)
library(fst)
library(data.table)
library(DT)
library(factoextra)
library(FactoMineR)
library(shinycssloaders)
library(shinyjs)
library(xlsx)
library(tidymodels)
library(sqldf)

ip <- read_fst("fonocortesia.fst")
vowels <- readRDS("vowels_fonocortesia.rds")
# prosody <- read_fst("prosody_fonocortesia.fst")

# prosody <- read_fst("prosodyameresco.fst")

ui <- dashboardPage(skin = "blue",
                    
    dashboardHeader(title = "Oralstats"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Info", tabName = "info", icon = icon("info-sign", lib = "glyphicon"),startExpanded = FALSE),
            menuItem("Create", tabName = "creation", icon = icon("pencil", lib = "glyphicon"),startExpanded = FALSE),
            menuItem("Filter", tabName = "filter", icon = icon("search", lib = "glyphicon")),
            menuItem("N-Grams", tabName = "ngrams", icon = icon("th", lib = "glyphicon")),
            menuItem("Basic methods", tabName = "basic", icon = icon("bold", lib = "glyphicon"),
                     startExpanded = FALSE,
            menuSubItem("Descriptive by group", tabName = "frequencies", icon = icon("list-alt", lib = "glyphicon")),
            menuSubItem("Variable description", tabName = "descriptive", icon = icon("eye-open", lib = "glyphicon")),
            menuSubItem("Crosstabs", tabName = "crosstabs", icon = icon("resize-horizontal", lib = "glyphicon"))),
            menuItem("Advanced methods", tabName = "advanced", icon = icon("star", lib = "glyphicon"),
                     startExpanded = FALSE,
            menuSubItem("Heatmap", tabName = "heatmap", icon = icon("map-marker", lib = "glyphicon")),
            menuSubItem("Tree", tabName = "tree", icon = icon("tree-conifer", lib = "glyphicon")),
            menuSubItem("correlation", tabName = "correlations", icon = icon("chevron-right", lib = "glyphicon")),
            menuSubItem("ANOVA", tabName = "anova", icon = icon("font", lib = "glyphicon")),
            menuSubItem("PCA", tabName = "PCA", icon = icon("resize-small", lib = "glyphicon")),
            menuSubItem("MFA", tabName = "MFA", icon = icon("resize-small", lib = "glyphicon")),
            menuSubItem("Logistic regression", tabName = "logistic", icon = icon("resize-full", lib = "glyphicon"))),
            menuItem("Prosody", tabName = "prosodyview", icon = icon("music", lib = "glyphicon"),
                     startExpanded = FALSE,
                     menuSubItem("pitch raw",
                                 tabName = "pitchraw"),
                     menuSubItem("Linear prosody",
                                 tabName = "linearprosody"),
                     menuSubItem("pitch contour",
                                 tabName = "pitchcontour"))
                     
                     ,
            menuItem("Credits", tabName = "credit", icon = icon("thumbs-up", lib = "glyphicon"),
                     startExpanded = FALSE,
                     menuSubItem("Libraries",
                                 tabName = "credits"),
                     menuSubItem("Inspirational work",
                                 tabName = "inspirational")
                     )
            
        )
    ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName="info",
                    fluidRow(box(width=12,status = "danger", solidHeader = TRUE,title="About Oralstats",collapsible = TRUE,collapsed = TRUE,uiOutput("aboutoralstatsui"))),
                    fluidRow(box(width=12,status = "primary", solidHeader = TRUE,title="Phonopoliteness database",uiOutput("aboutdatabaseui"))),
                    fluidRow(box(status = "info", solidHeader = TRUE,title="Project manager",uiOutput("projectui")),box(status = "info", solidHeader = TRUE,title="Author", uiOutput("authorui"))
                    ),
                    fluidRow(box(status = "warning", solidHeader = TRUE,title="License", uiOutput("licenseui")),
                    box(status = "warning", solidHeader = TRUE,title="Requirements", uiOutput("requirementsui"))
                    
                    )),
            tabItem(tabName = "creation",
                    fluidRow(box(width=12,status = "danger", solidHeader = TRUE,title="Important message",tags$p("This section does not include a fully complete version of the Oralstats creation script; you can transform only one file with its correspondent pitch and intensity files. If you prefer more advanced features, you can use instead Oralstats.create local script available on Github"))),
                    fluidRow(
                        
                        box(width="3", title ="Create files", status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                        selectInput("phonemes","Are your data including phonemes, words or other tiers?",choices = c("yes","no")),
                        selectInput("phonemes","Are you adding pitch and intensity values?",choices = c("yes","no")),
                        selectInput("phonemes","Are you adding metadata values?",choices = c("yes","no")),
                        selectInput("tagged words","Are you uploading an already pos tagged/reviewed file?",choices = c("yes","no")),
                        fileInput("tabbedtext","Upload tabbed text file from ELAN",multiple=FALSE),
                        actionButton("createbutton","Create files")),
                        box(width="3",title="Prosody",status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                 fileInput("pitchfile","upload pitch file",multiple = TRUE),
                                 fileInput("intensityfile","upload intensity file",multiple = TRUE)
                            # ,
                            # actionButton("joinprosodicdfbutton","Create prosodic df")
                            ),
                        box(width="3",title="Other files", status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,fileInput("metadata","upload metadata file"),
                        fileInput("posfile","upload pos tagged file (not available)")
                        ),
                        box(width="3",title="Collapse hierarchy (not available)", status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,textInput("collapse1","Collapse 1"),
                            textInput("collapse2","Collapse2"),textInput("collapse3","Collapse 3"),textInput("collapse4","Collapse 4"),textInput("collapse5","Collapse 5")
                        )
                    )
                    ,
            fluidRow(box(width="12",title = "Download files csv",status = "warning", solidHeader = TRUE,downloadButton("downloadwords","Download words (not available)"),
                    downloadButton("downloadip","Download ip"),
                     downloadButton("downloadphonemes","Download phonemes (not available)"),downloadButton("downloadvowels","Download vowels (not available)"),downloadButton("downloadprosody","Download pitch-intensity df"))),
            # fluidRow(box(width="12",title = "Download files fst",status = "warning", solidHeader = TRUE,downloadButton("downloadwordsfst","Download words"),
            #              downloadButton("downloadipfst","Download ip"),
            #              downloadButton("downloadphonemesfst","Download phonemes"),downloadButton("downloadvowelsfst","Download vowels"),downloadButton("downloadprosodyfst","Download pitch-intensity df"))),
            fluidRow(box(width=12, title="Original df",dataTableOutput("dforiginal") )),
            fluidRow(box(width=12, title="Transformed df",shinycssloaders::withSpinner(dataTableOutput("dftransform")) ))
            
            )
        
            
            ,
            tabItem(tabName = "filter",
                    # fluidRow( box(width=4,actionButton("filterbutton","PRESS HERE TO DEPLOY OR FILTER CORPUS"))),
                    fluidRow(
                        
                        column(width=3,
                            
                            box(status = "primary", solidHeader = TRUE,title="Filter",collapsible = TRUE,collapsed = TRUE,width = NULL,
                            # The id lets us use input$tabset1 on the server to find the current tab
                            
                                shinyjs::useShinyjs(),
                                id = "side-panel",
                           
                            actionButton("filterbutton","Filter"),actionButton("resetbutton","Reset"),
                            varSelectInput("var1","Filter 1 (select)",multiple = FALSE,data = ip%>%select(where(is.character)),selected = "corpus"),
                            uiOutput("choice1"),
                            varSelectInput("var2","Filter 2 (select)",multiple = FALSE,data = ip%>%select(where(is.character)),selected = "corpus"),
                            uiOutput("choice2"),
                            varSelectInput("var3","Filter 3 (select)",multiple = FALSE,data = ip%>%select(where(is.character)),selected = "corpus"),
                            uiOutput("choice3"),
                            varSelectInput("var6","Filter 4 (Select a variable and write [regex available])",multiple = FALSE,data = ip%>%select(where(is.character))),
                                     textInput("filter6", label = ""),
                             varSelectInput("var4","Filter 5 (numeric)",multiple = FALSE,data = ip%>%select(where(is.numeric))),
                                     uiOutput("choice4"),
                           varSelectInput("var5","Filter 6 (numeric)",multiple = FALSE,data = ip%>%select(where(is.numeric))),
                                     uiOutput("choice5")
                            )),
                        
                        column(width = 9,
                            
                            box(width=NULL,status="info", solidHeader = TRUE,title="Add variables to preview",actionButton("modal1button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("databasesel",label = "",multiple=TRUE,data = ip,selected = ip%>%select(c(1,2))),downloadButton("downloaddata",label = "Download complete or filtered database",icon=icon("download"))),
                            box(width = NULL,title = "Database", status = "info",collapsible = TRUE,collapsed = FALSE, solidHeader = TRUE,shinycssloaders::withSpinner(dataTableOutput("database")))
                            
                            
                        )
                      
                           
                            
                        )),
                        
                        # box(width = 2,title = "Filter", status = "info", solidHeader = TRUE,
                        #     varSelectInput("var1","",multiple = FALSE,data = ip%>%select(where(is.character)),selected = "corpus"),
                        #     uiOutput("choice1")),
                    #     box(width = 2,title = "Filter", status = "info", solidHeader = TRUE,varSelectInput("var2","",multiple = FALSE,data = ip%>%select(where(is.character)),selected = "corpus"),
                    #         uiOutput("choice2")),
                    #     box(width = 2,title = "Filter", status = "info", solidHeader = TRUE,varSelectInput("var3","",multiple = FALSE,data = ip%>%select(where(is.character)),selected = "corpus"),
                    #         uiOutput("choice3")),
                    #     box(width = 2,title = "Filter", status = "info", solidHeader = TRUE,varSelectInput("var6","",multiple = FALSE,data = ip%>%select(where(is.character))),
                    #         
                    #         textInput("filter6", label = "")),
                    #     # box(width=9,title = "Database", status = "info", solidHeader = TRUE,dataTableOutput("database"))
                    #   box(width = 2,title = "Filter", status = "info", solidHeader = TRUE,
                    #         varSelectInput("var4","",multiple = FALSE,data = ip%>%select(where(is.numeric))),
                    #         uiOutput("choice4")),
                    #       box(width = 2,title = "Filter", status = "info", solidHeader = TRUE,varSelectInput("var5","",multiple = FALSE,data = ip%>%select(where(is.numeric))),
                    #         uiOutput("choice5")
                    #         )
                    # ),
                    
            
            tabItem(tabName = "ngrams",
            fluidRow(box(width=3,status = "info", solidHeader = TRUE,title="Select variable to get ngrams",actionButton("modal2button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("varngrams","",multiple = FALSE,data = ip%>%select(where(is.character)),selected = "corpus"),
                     numericInput("ngramsize","Select ngrams size",min = 1,max = 10,step=1,value = 1),
                     actionButton("ngramsbutton",label = "Calculate ngrams")),
                     box(width=9,title="N-grams",status = "warning", solidHeader = TRUE,shinycssloaders::withSpinner(dataTableOutput("ngrams1"))))
              
                    
        
                    ),
            tabItem(tabName = "frequencies",
                    fluidRow(box(width=3,title = "Group by", status = "info", solidHeader = TRUE,actionButton("modal3button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("frequ","Group by",multiple = TRUE,data = ip%>%select(where(is.character))),
                                 uiOutput("varmeansui")),
                             box(width=9,title = "Frequency table", status = "primary", solidHeader = TRUE, dataTableOutput("frequency"))
                             )
                    # ,
                    # fluidRow(box(width=12,title="Barplot (only two variables)",status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE, plotOutput("frequencyplot"))),
                   
    
                    
                    )
            ,
            tabItem(tabName = "descriptive",
                    
                    fluidRow(box(width=3,title = "Group by", status = "info", solidHeader = TRUE,
                                 # varSelectInput("descriptivegroup","Group by",multiple = FALSE,data = ip%>%select(where(is.character))),
                                 actionButton("modal4button","help",icon = icon("question-sign", lib = "glyphicon")),
                                 varSelectInput("descriptivenum","Select variables",multiple = TRUE,data = ip%>%select(where(is.numeric))))),
                   fluidRow(box(width=12,title = "Descriptive statistics", status = "primary", solidHeader = TRUE, dataTableOutput("descriptivestats"))
                    )
                    
                    ),
            tabItem(tabName = "crosstabs",
                    
                    fluidRow(
                        
                        box(title = "Select one variable",status="info",solidHeader = TRUE,actionButton("modal5button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("crosstabsdep","Variable1",multiple = FALSE,data = ip%>%select(where(is.character))),
                            uiOutput("crosstabssel")),
                        box(title = "Select other variable",status="info",solidHeader = TRUE,varSelectInput("crosstabsindep","Variable2",multiple = FALSE,data = ip%>%select(where(is.character)))
                            ,uiOutput("crosstabssel2"))),
            
            
            
            fluidRow(box(width=3,actionButton("crosstabsbutton","Generate crosstabs",icon = icon("flash", lib = "glyphicon"))
            )),
            fluidRow(
                
                column(width=5,box(title = "Chi-squarevalue", status = "primary", solidHeader = TRUE,width = NULL,verbatimTextOutput("crosstabschi")),
                       box(title = "Frequency", status = "primary", solidHeader = TRUE,width = NULL,verbatimTextOutput("crosstabsobserved")),
                       box( title = "Residuals", status = "primary", solidHeader = TRUE,width = NULL,verbatimTextOutput("crosstabsresiduals"))),
                column(width=7,box(title = "Frequency Barplot", status = "warning", solidHeader = TRUE, width = NULL,plotlyOutput("crosstabsplot"))
            ))),
                            
tabItem(tabName = "correlations",
fluidRow(box(title = "Select numeric variables", status = "info", solidHeader = TRUE,actionButton("modal6button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("correlationsdep","Select variable",multiple = TRUE,data = ip%>%select(where(is.numeric))),
)),
fluidRow(box(actionButton("correlationsbutton","Generate correlations",icon = icon("flash", lib = "glyphicon"))
)),
fluidRow(box(width=12,title = "Correlation plot", status = "warning", solidHeader = TRUE,plotOutput("correlationsplot"))
)),

tabItem(tabName="anova",
        
        fluidRow(box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("modal7button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("anovadep","Select variable",multiple = FALSE,data = ip%>%select(where(is.character))),
            uiOutput("anovasel")),
        box(title = "Select numeric variable", status = "info", solidHeader = TRUE,varSelectInput("anovaindep","Select variable",multiple = FALSE,data = ip%>%select(where(is.numeric)))
            )),
        fluidRow(box(actionButton("anovabutton","Calculate ANOVA",icon = icon("flash", lib = "glyphicon")))),
        fluidRow(box(title = "Tukey test results", status = "primary", solidHeader = TRUE,width="12",verbatimTextOutput("anovaresults"))),
        fluidRow(box(title = "Boxplot", status = "warning", solidHeader = TRUE,width="12",plotlyOutput("anovaplot")))
        
        
        )
,
                            tabItem(tabName = "heatmap",
                                    
                                    fluidRow(
                                        
                                        box(title = "Select numeric variables", status = "info", solidHeader = TRUE,actionButton("modal8button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("heatmapdep","Select variable",multiple = FALSE,data = ip%>%select(where(is.character))),
                                            uiOutput("heatmapsel")),
                                        box(title = "Select group variable", status = "info", solidHeader = TRUE,varSelectInput("heatmapindep","Select variable",multiple = TRUE,data = ip%>%select(where(is.numeric))))),
                                    fluidRow(box(actionButton("heatmapbutton","Generate heatmap",icon = icon("flash", lib = "glyphicon"))
                                    )),
                                    fluidRow(box(title = "Heatmap plot", status = "warning", solidHeader = TRUE,width=12, shinycssloaders::withSpinner(plotlyOutput("heatmap")))
                                             
                                    )
                                    
                            ),
                            tabItem(tabName = "tree",
                                    
                                    fluidRow(
                                        
                                        box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("modal9button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("var6tree","Select variable",multiple = FALSE,data = ip%>%select(where(is.character))),
                                            uiOutput("choice6")),
                                        box(title = "Select variables", status = "info", solidHeader = TRUE,varSelectInput("var7","Select variable",multiple = TRUE,data = ip))),
                                    fluidRow(box(actionButton("treebutton","Generate decision tree",icon = icon("flash", lib = "glyphicon"))
                                    )),
                                    fluidRow(box(title = "Decision Tree plot (Rpart library)", status = "warning", solidHeader = TRUE,width=12, shinycssloaders::withSpinner(plotOutput("treeplot")))),
                                             fluidRow(box(title = "Decision Tree plot (Party library)", status = "warning", solidHeader = TRUE,width=12, shinycssloaders::withSpinner(plotOutput("treeplotparty")))),
                                                      fluidRow(box(title = "Decision Tree table (Party library)", status = "warning", solidHeader = TRUE,width=12, verbatimTextOutput("treeparty")))
                                                               
                                                      
                                    )
                                    
                            ,

tabItem(tabName = "PCA",
        
        fluidRow(
            
            box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("modal10button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("var6pca","Select variable",multiple = FALSE,data = ip%>%select(where(is.character))),
                uiOutput("choicepca")),
        box(title = "Select numeric variables", status = "info", solidHeader = TRUE,varSelectInput("var7pca","Select variable",multiple = TRUE,data = ip%>%select(where(is.numeric))))),
        fluidRow(box(width=3,actionButton("pcabutton","Generate PCA",icon = icon("flash", lib = "glyphicon"))
        )),
        fluidRow(box(title = "Contributions", status = "primary", solidHeader = TRUE,shinycssloaders::withSpinner(verbatimTextOutput("pcarcontrib"))),
                 box(title = "Coordinates", status = "primary", solidHeader = TRUE,shinycssloaders::withSpinner(verbatimTextOutput("pcarcoord")))),
        fluidRow(box(title = "Dimensions and variance plot", status = "warning", solidHeader = TRUE,width=6, shinycssloaders::withSpinner(plotOutput("pcaploteigen"))),
                     box(title = "Individuals plot", status = "warning", solidHeader = TRUE,width=6, shinycssloaders::withSpinner(plotOutput("pcaplotind")))
                 
        ),
        fluidRow(box(title = "Variables pca plot", status = "warning", solidHeader = TRUE,width=6,actionButton("pcavarbutton","Generate PCA var chart",icon = icon("flash", lib = "glyphicon")),
                     plotOutput("pcaplotvar")),
                 box(title = "Cluster from pca plot", status = "warning", solidHeader = TRUE,width=6, actionButton("pcaclusterbutton","Generate PCA cluster chart",icon = icon("flash", lib = "glyphicon")),
                     shinycssloaders::withSpinner(plotOutput("pcaplotcluster")),
                     tableOutput("pcaclustable"),
                     dataTableOutput("pcaclustable2")))
        
),
tabItem(tabName = "MFA",
        
        fluidRow(
            
            box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("modal11button","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("var6mfa","Select variable",multiple = FALSE,data = ip%>%select(where(is.character))),
                uiOutput("choicemfa")),
            box(title = "Select numeric variables", status = "info", solidHeader = TRUE,varSelectInput("var7mfa","Select variable",multiple = TRUE,data = ip))),
        fluidRow(box(width=3,actionButton("mfabutton","Generate mfa",icon = icon("flash", lib = "glyphicon"))
        )),
        fluidRow(box(title = "Contributions", status = "primary", solidHeader = TRUE,shinycssloaders::withSpinner(verbatimTextOutput("mfarcontrib"))),
                 box(title = "Coordinates", status = "primary", solidHeader = TRUE,shinycssloaders::withSpinner(verbatimTextOutput("mfarcoord")))),
        fluidRow(box(title = "Dimensions and variance plot", status = "warning", solidHeader = TRUE,width=6, shinycssloaders::withSpinner(plotOutput("mfaploteigen"))),
                 box(title = "Individuals plot", status = "warning", solidHeader = TRUE,width=6, shinycssloaders::withSpinner(plotOutput("mfaplotind")))
                 
        ),
        fluidRow(box(title = "Variables mfa plot", status = "warning", solidHeader = TRUE,width=6,actionButton("mfavarbutton","Generate mfa var chart",icon = icon("flash", lib = "glyphicon")),
                     plotOutput("mfaplotvar")),
                 box(title = "Cluster from mfa plot", status = "warning", solidHeader = TRUE,width=6, actionButton("mfaclusterbutton","Generate mfa cluster chart",icon = icon("flash", lib = "glyphicon")),
                     shinycssloaders::withSpinner(plotOutput("mfaplotcluster")),
                     tableOutput("mfaclustable"),
                     dataTableOutput("mfaclustable2")))
        
),
tabItem(tabName = "logistic",
        
        fluidRow(
            
            box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("logbuttonhelp","help",icon = icon("question-sign", lib = "glyphicon")),varSelectInput("var6log","Select variable",multiple = FALSE,data = ip%>%select(where(is.character))),
                uiOutput("choicelog")),
            box(title = "Select numeric variables", status = "info", solidHeader = TRUE,varSelectInput("var7log","Select variable",multiple = TRUE,data = ip),
                actionButton("logbutton","Generate logistic regression",icon = icon("flash", lib = "glyphicon"))
            )),
        
        fluidRow(box(width=8,status = "warning", solidHeader = TRUE,title="Logistic regression summary",verbatimTextOutput("logisticresults")),
                 box(width=4,status = "warning", solidHeader = TRUE,title="Logistic regression accuracy",verbatimTextOutput("logisticpredicted"),verbatimTextOutput("logisticaccuracy")))
        
)
,
tabItem(tabName = "pitchraw",fluidRow(
    column(width=3,
    box(width=NULL,title="Select parameters",status = "info", solidHeader = TRUE,actionButton("modal12button","help",icon = icon("question-sign", lib = "glyphicon")),uiOutput("rawfilter")),
                                      ),
    column(width=9,box(width=NULL,title="Datatable",status = "primary", solidHeader = TRUE, dataTableOutput("rawtable"),uiOutput("rawaudio")),
        box(width=NULL,title="pitch or intensity plot",status = "warning", solidHeader = TRUE,plotlyOutput("rawplot"))))
        ),
tabItem(tabName = "linearprosody",
fluidRow(
    column(width=3,
    box(width=NULL,title="Select variables",status = "info", solidHeader = TRUE,
actionButton("prosviewbutton","Generate chart"),
varSelectInput("prosviewfile1","Select variable",multiple = FALSE,data = ip%>%select(where(is.character))),
uiOutput("chosprosview"),
varSelectInput("prosview2","Select variable",multiple = FALSE,data = ip%>%select(where(is.character))),
varSelectInput("prosview3","Select variable",multiple = FALSE,data = ip%>%select(where(is.numeric))))),
                                               
column(width=9,box(width=NULL,title="Prosodic linear chart",status = "warning", solidHeader = TRUE,shinycssloaders::withSpinner(plotlyOutput("plotprosview")))
))),

tabItem(tabName = "pitchcontour",
     fluidRow(
         
         column(width=3,box(width=NULL,title="Filter",status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed=FALSE,
                            actionButton(inputId =  "contplot", label =  "Show chart"),
                varSelectInput("cont1","Select unit variable", vowels%>%select(where(is.character)), multiple = FALSE, selected="ip_id"),
                  uiOutput("filterscontour"),
                  varSelectizeInput("cont3","Select x variable", vowels%>%select(where(is.numeric)), multiple = FALSE, selected="tmin"),
                  varSelectizeInput("cont4","Select y variable", vowels%>%select(where(is.numeric)), multiple = FALSE, selected = "PimnSt"),
                  varSelectizeInput("cont5","Select linetype variable", vowels%>%select(where(is.character),where(is.factor)), multiple = FALSE,selected="w_id"),
                  varSelectizeInput("cont6","Select factor variable", vowels%>%select(where(is.character),where(is.factor)), multiple = FALSE, selected="w_id"),
                  varSelectizeInput("cont7","Select size variable", vowels%>%select(where(is.numeric)), multiple = FALSE, selected="Imn"),
                  varSelectizeInput("cont8","Select text variable", vowels%>%select(where(is.character),where(is.factor)), multiple = FALSE, selected="phon"),
                  varSelectizeInput("cont9","Select text variable", vowels%>%select(where(is.character),where(is.factor)), multiple = FALSE, selected="ip")
                  
                  )),
    column(width=9,box(width=NULL,title="Melody plot",status = "warning", solidHeader = TRUE,shinycssloaders::withSpinner(plotlyOutput("melodyplot"))),
     box(width=NULL,title="Table",status = "warning", solidHeader = TRUE,
         varSelectizeInput("cont10","Select variables to show", vowels%>%select(where(is.character),where(is.factor)), multiple = TRUE),
         actionButton(inputId =  "contDT", label =  "Show table"),
         dataTableOutput("dataframemel")))
     )
        ),
tabItem(tabName = "credits",
        
        fluidRow(box(width=12,title="Libraries",status = "info", solidHeader = TRUE,uiOutput("creditsui")))),

tabItem(tabName="inspirational",fluidRow(box(width=12,title="Inspirational",status = "info", solidHeader = TRUE, uiOutput("inspirationalui"))))

                            
                            
                            
                            
                            
                            # Second tab content
                            # tabItem(tabName = "widgets",
                            #         h2("Widgets tab content"),
                            #         dataTableOutput("table")
                            # )
                       )
                )
            )
            
            
            server <- function(input, output) {
                options(shiny.maxRequestSize=30*1024^2) 
                
                output$authorui <- renderUI({div(
                    img(src = "cabedo2.jpeg", style="display: block;margin-left: auto;margin-right: auto;", height = 150, width = 120),br(),
                    p(strong("Adrián Cabedo Nebot"),align="center"),p(a("adrian.cabedo@uv.es",href="adrian.cabedo@uv.es"),align="center"),p("Universitat de València",align="center"))})
                
                output$projectui <- renderUI({div(
                    img(src = "hidalgo.jpeg", style="display: block;margin-left: auto;margin-right: auto;", height = 150, width = 150),br(),
                    p(strong("Antonio Hidalgo Navarro"),align="center"),p(a("antonio.hidalgo@uv.es",href="antonio.hidalgo@uv.es"),align="center"),p("Universitat de València",align="center"))})
                
                
                output$requirementsui <- renderUI({div(p("If you download the code, you’ll need most recent versions of libraries (cited in credits section) along with RStudio and R installed on your computer.
If you prefer to use the web version, you’ll just need a common web browser like Google Chrome, Mozilla Firefox or Safari. Although Oralstats can be launched on mobile devices, we recommend to use a laptop or, at least, a bigger screen. This specific webpage, uploaded to shinyapps.io, is just an online sample done for exemplification purposes."))})
                
                output$aboutdatabaseui <- renderUI({div(p("Data proudly visualized with Oralstats. Versión beta: 1.0 (expect any kind of bugs). Free available at", a("https://github.com/acabedo/oralstats.", href="https://github.com/acabedo/oralstats"),"This dashboard includes 278 intonational phrases from Fonocortesia (Phonopoliteness), a Spanish research project ended in 2013. The owner of this database is Antonio Hidalgo Navarro (Universitat de València)."))})
                
                output$aboutoralstatsui <- renderUI({div(p("Oralstats is a data exploratory tool developed to achieve two main operations at discourse level analysis: 1) modify and transform transcribed speech data and join this data with intensity or pitch values; and 2) visualize and explore this data using several statistical and visualization methods (decision trees, heatmaps, boxplots, and so on)."))})
                
                
                # output$table <- renderDataTable(
                #     d <- ifelse(input$filterbutton,ip%>%filter(grepl(input$filter6,!!input$var6),!!input$var1%in%input$filter1,!!input$var2%in%input$filter2,!!input$var3%in%input$filter3,between(!!input$var4,input$filter4[1],input$filter4[2]),between(!!input$var5,input$filter5[1],input$filter5[2]),ip)),options = list(scrollX = TRUE))
                # 
                observeEvent(input$resetbutton, {
                    shinyjs::reset("side-panel")
                })
                
            
                
                ipframefilt <- eventReactive(input$filterbutton,{
                
                        
                        ip%>%filter(grepl(input$filter6,!!input$var6),!!input$var1%in%input$filter1,!!input$var2%in%input$filter2,!!input$var3%in%input$filter3,between(!!input$var4,input$filter4[1],input$filter4[2]),between(!!input$var5,input$filter5[1],input$filter5[2]))
                        
                    
                })
                
                databaseexport <- reactive({
                    
                   if (input$filterbutton) {
                    ipframefilt()%>%select(!!!input$databasesel) 
                } else {
                    ip%>%select(!!!input$databasesel) }
                }
                )
                
                ip2 <- reactive({
                    
                    p<- 
                        
                            if (input$filterbutton) {
                                ipframefilt()
                            } else {
                                ip
                            }
                            
                        
                        
                        })
                
                output$database <- renderDataTable({
                    
                   # p <- if (input$filterbutton) {
                   #     datatable(ipframefilt()%>%select(!!!input$databasesel),options = list(scrollX = TRUE)) 
                   # } else {
                   #     datatable(ip%>%select(!!!input$databasesel),options = list(scrollX = TRUE)) 
                       p<-datatable(ip2()%>%select(!!!input$databasesel),options = list(scrollX = TRUE))
                   }
                       
                       
                     # datatable(ipframe()%>%select(!!!input$databasesel),options = list(scrollX = TRUE)) 
                    )
                   
                   
                
                output$downloaddata <- downloadHandler(
                   
                    filename = function() {
                        paste("oralstats-", Sys.Date(), ".xlsx", sep="")
                    },
                    content = function(file) {
                 
                        write.xlsx2(databaseexport(), file)
              # write.xlsx(ip,file)
                    # write.table(ipframe(), file,sep=sep,quote = TRUE)
                        }
                    
                )
                
                output$choice1 <- renderUI(pickerInput("filter1",choices = unique(ip%>%select(!!input$var1)),multiple = TRUE,selected = "corpus"))
                output$choice2 <- renderUI(pickerInput("filter2",choices = unique(ip%>%select(!!input$var2)),multiple = TRUE,selected = "corpus"))
                output$choice3 <- renderUI(pickerInput("filter3",choices = unique(ip%>%select(!!input$var3)),multiple = TRUE,selected = "corpus"))
                output$choice4 <- renderUI(sliderInput("filter4","",min = min(ip%>%select(!!input$var4),na.rm = TRUE),max = max(ip%>%select(!!input$var4),na.rm = TRUE),value=c(min(ip%>%select(!!input$var4),na.rm = TRUE),max(ip%>%select(!!input$var4),na.rm = TRUE))))
                output$choice5 <- renderUI(sliderInput("filter5","",min = min(ip%>%select(!!input$var5),na.rm = TRUE),max = max(ip%>%select(!!input$var5),na.rm = TRUE),value=c(min(ip%>%select(!!input$var5),na.rm = TRUE),max(ip%>%select(!!input$var5),na.rm = TRUE))))
                output$choice6 <- renderUI(pickerInput("filter7",choices = unique(ip%>%select(!!input$var6tree)),multiple = TRUE))
                output$choicepca <- renderUI(pickerInput("filter8",choices = unique(ip%>%select(!!input$var6pca)),multiple = TRUE))
                output$choicemfa <- renderUI(pickerInput("filter9",choices = unique(ip%>%select(!!input$var6mfa)),multiple = TRUE))
                output$choicelog <- renderUI(pickerInput("filterlog",choices = unique(ip%>%select(!!input$var6log)),multiple = TRUE))
                output$heatmapsel <- renderUI(pickerInput("heatmappicker",choices = unique(ip%>%select(!!input$heatmapdep)),multiple = TRUE))
                output$crosstabssel <- renderUI(pickerInput("crosstabspicker",choices = unique(ip%>%select(!!input$crosstabsdep)),multiple = TRUE))
                output$crosstabssel2 <- renderUI(pickerInput("crosstabspicker2",choices = unique(ip%>%select(!!input$crosstabsindep)),multiple = TRUE))
                output$anovasel <- renderUI(pickerInput("anovapicker",choices = unique(ip%>%select(!!input$anovadep)),multiple = TRUE))
                output$frequency <- renderDataTable({
                    
                  p<- ip2()%>%group_by(!!!input$frequ)%>%select(!!!input$varmeans)%>%na.omit()%>%summarise(freq= n(), across(everything(),list(mean = mean, median = median, std = sd, max=max, min=min)))%>%select(-freq_mean,-freq_median,-freq_max,-freq_min,-freq_std)%>%mutate(across(where(is.numeric), round, 1))
                   p <-  setDT(p)
                  datatable(p,options = list(scrollX = TRUE)) })
               
                output$descriptivestats <- renderDataTable({
                    
                    p <- ip2()
                    d <- describe(ip2()%>%select(!!!input$descriptivenum))
                    d<- setDT(d, keep.rownames = "variable")
                    d<- d %>% mutate(across(where(is.numeric), round, 2))
                    datatable(d,options = list(scrollX = TRUE)
                    )}
                )
                
                 output$frequencyplot <- renderPlot({
                    p<- ip2()%>%group_by(!!!input$frequ)%>%summarise(freq=n())%>%rename(var1=1,var2=2,freq=3)
        
                d <-    ggplot(p, aes(fill=var1, y=freq, x=var2)) +
                        geom_bar(position='dodge', stat='identity')
                    d
                })
                 # output$correlationssel <- renderUI(pickerInput("crosstabspicker",choices = unique(ip2()%>%select(!!input$correlationsdep)),multiple = TRUE))
                # output$correlationssel2 <- renderUI(pickerInput("crosstabspicker2",choices = unique(ip2()%>%select(!!input$correlationsindep)),multiple = TRUE))
                
                
                ngrams1 <- eventReactive(input$ngramsbutton,{
                    
                   
                    p<-   ip2() %>% unnest_tokens(ngram,!!input$varngrams,token="ngrams", n= input$ngramsize)%>%group_by(ngram)%>%summarise(cantidad=n())%>%arrange(desc(cantidad))%>%filter(ngram!="")
                    
                    setDT(p)
                    p <- datatable(head(p,n = 100))
                    
                })
                
                output$ngrams1 <- renderDataTable({
                    
                    ngrams1()
                 
                })
                
                
                
                
                treeselect <- eventReactive(input$treebutton,{p <- ip2()%>%mutate(varx = !!input$var6tree)%>%filter(varx%in%!!input$filter7)%>%select(varx,!!!input$var7)%>%na.omit()
                d <-rpart(varx~., data= p,cp=.02)
                d}
                )
                
                crosstabsch <- eventReactive(input$crosstabsbutton,{
                    p <-  ip2()%>%mutate(varx = !!input$crosstabsdep, vary = !!input$crosstabsindep)%>%filter(varx%in%!!input$crosstabspicker,vary%in%!!input$crosstabspicker2)
                    d<-chisq.test(p$varx,p$vary)
                    d
                })
                
                crosstabsobserve <- eventReactive(input$crosstabsbutton,{
                    p <-  ip2()%>%mutate(varx = !!input$crosstabsdep, vary = !!input$crosstabsindep)%>%filter(varx%in%!!input$crosstabspicker,vary%in%!!input$crosstabspicker2)
                    d<-chisq.test(p$varx,p$vary)
                    d$observed
                })
                
                crosstabsresidual <- eventReactive(input$crosstabsbutton,{
                    p <-  ip2()%>%mutate(varx = !!input$crosstabsdep, vary = !!input$crosstabsindep)%>%filter(varx%in%!!input$crosstabspicker,vary%in%!!input$crosstabspicker2)
                    d<-chisq.test(p$varx,p$vary)
                    d$residuals
                })
                
                crosstabplot <- eventReactive(input$crosstabsbutton,{
                    p <-  ip2()%>%mutate(varx = !!input$crosstabsdep, vary = !!input$crosstabsindep)%>%filter(varx%in%!!input$crosstabspicker,vary%in%!!input$crosstabspicker2)
                    p<- p%>% ggplot(aes(varx, ..count..)) + geom_bar(aes(fill = vary), position = "dodge")
                    p})
                
                
                output$crosstabsresiduals <- renderPrint(crosstabsresidual())
                output$crosstabsobserved <- renderPrint(crosstabsobserve())
                output$crosstabschi <- renderPrint(crosstabsch())
                output$crosstabsplot <- renderPlotly(crosstabplot())
                
                correlations <- eventReactive(input$correlationsbutton,{
                    
                    p <- ip2()%>%select(!!!input$correlationsdep)%>%na.omit()
                    p.mat <- cor_pmat(p)
                    corr <- round(cor(p),1)
                    ggcorrplot(corr,hc.order = TRUE, type = "lower",
                               p.mat = p.mat)
                })
                
                output$correlationsplot <- renderPlot(correlations())
                
                
                
                anovaplots <- eventReactive(input$anovabutton,{

                    p <-  ip2()%>%mutate(varx = !!input$anovadep, vary = !!input$anovaindep)%>%filter(varx%in%!!input$anovapicker)
                    
                  d <-  ggplot(p, aes(x=varx, y=vary, fill=varx)) + 
                        geom_boxplot(alpha=0.3, outlier.shape = NA) +
                        theme(legend.position="none")
d
                })
                anovaresult <- eventReactive(input$anovabutton,{
                    
                    p <-  ip2()%>%mutate(varx = !!input$anovadep, vary = !!input$anovaindep)%>%filter(varx%in%!!input$anovapicker)
                    d <- TukeyHSD(aov(vary~varx, data=p))
                    d
                })
                
                output$anovaplot <- renderPlotly(anovaplots())
                output$anovaresults <- renderPrint(anovaresult())
                
                output$treeplot <- renderPlot(rpart.plot(treeselect(),extra = 101, box.palette="RdBu", shadow.col="gray", nn=TRUE))
                
                treeselectparty <- eventReactive(input$treebutton,{p <- ip2()%>%mutate(varx = !!input$var6tree)%>%filter(varx%in%!!input$filter7)%>%select(varx,!!!input$var7)%>%na.omit()%>%mutate(across(where(is_character),as_factor))
                d <-ctree(varx~., data= p, control = ctree_control(maxdepth = 4))
                d}
                )
                
                output$treeplotparty <- renderPlot(plot(treeselectparty()))
                output$treeparty <-renderPrint(treeselectparty())
                
                mapa <- eventReactive(input$heatmapbutton,{
                    
                    p <- ip2()%>%mutate(varx = !!input$heatmapdep)%>%filter(varx%in%!!input$heatmappicker)%>%group_by(varx)%>%select(varx,!!!input$heatmapindep)%>% summarize_if(., is.numeric, mean, na.rm = TRUE)
                    p <- p %>%column_to_rownames(var="varx")
                    # mapa2 <- p%>%select(-varx)
                    # rownames(mapa2) <- p$varx
                    # mapa <- as.matrix(mapa2)
                }
                )
                
                output$heatmap <- renderPlotly(heatmaply(
                    mapa(), 
                    xlab = "Features",
                    ylab = "Unit", 
                    scale = "column",
                    main = "Data transformation using 'scale'"
                ))
                
                pcaresult <- eventReactive(input$pcabutton,{
                    
                    p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
                   d <- prcomp(p%>%select(-varx), center = TRUE,scale. = TRUE)
                    
                    summary(d)
                    
                    
                })
                
                pcaresultfacto <- eventReactive(input$pcabutton,{
                    
                    p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
                    d <- prcomp(p%>%select(-varx), center = TRUE,scale. = TRUE)
                    
                    res.var <- get_pca_var(d)
                    
                    
                })
                
                pcaresultfacto2 <- eventReactive(input$pcabutton,{
                    
                    p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
                    # d <- prcomp(p%>%select(-varx), center = TRUE,scale. = TRUE)
                    
                    d<- PCA(p%>%select(-varx), ncp = 3, graph = FALSE)
                    
                    
                })
                
                output$pcarcontrib <- renderPrint(pcaresultfacto()$contrib)
                output$pcarcoord <- renderPrint(pcaresultfacto()$coord)
                output$pcaploteigen <- renderPlot({d<-fviz_eig(pcaresultfacto2())
                                                  d})
                output$pcaplotind<- renderPlot(
                    {p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
                    
                    res.pca <- pcaresultfacto2()
                    groups <- as.factor(p$varx)
                   d<- fviz_pca_ind(res.pca,
                                 col.ind = groups, # color by groups
                                 palette = c("#00AFBB",  "#FC4E07"),
                                 legend.title = "Groups",
                                 repel = TRUE)
                d
                    }
                )
                
pcaplotvar <-eventReactive(input$pcavarbutton,
                    {
                        # p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
                        # res.pca <- PCA(p%>%select(-varx), ncp = 3, graph = FALSE)
                        # Graph of variables including supplementary variables
                        d<-plot.PCA(pcaresultfacto2(), axes=c(1, 2), choix="var")
                        d
                    }
                    
                )                
                
                output$pcaplotvar <- renderPlot(
                    {pcaplotvar()
                        }
                    
                )
                pcaplotcluster <- eventReactive(input$pcaclusterbutton,{
                    
                    # p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
                    # # Compute hierarchical clustering on principal components
                    # res.pca <- PCA(p%>%select(-varx), ncp = 3, graph = FALSE)
                    res.hcpc <- HCPC(pcaresultfacto2(), graph = FALSE)
                    d<-fviz_dend(res.hcpc, 
                                 cex = 0.7,                     # Label size
                                 palette = "jco",               # Color palette see ?ggpubr::ggpar
                                 rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                                 rect_border = "jco",           # Rectangle color
                                 labels_track_height = 0.8) # Augment the room for labels
                    d
                    
                })
                    
                
                
          pcaclustable <- eventReactive(input$pcaclusterbutton,{
              
              p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
              # # Compute hierarchical clustering on principal components
              # res.pca <- PCA(p%>%select(-varx), ncp = 3, graph = FALSE)
              res.hcpc <- HCPC(pcaresultfacto2(), graph = FALSE)
              table(res.hcpc$data.clust$clust,p$varx)
              
          })
          
          pcaclustable2 <- eventReactive(input$pcaclusterbutton,{
              
              # p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
              # # Compute hierarchical clustering on principal components
              # res.pca <- PCA(p%>%select(-varx), ncp = 3, graph = FALSE)
              res.hcpc <- HCPC(pcaresultfacto2(), graph = FALSE)
              p<- res.hcpc$data.clust%>%group_by(clust)%>%summarise(freq=n(),across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE),2)))
              p <-  setDT(p)
             d<-  datatable(p,options = list(scrollX = TRUE))
              
              
          })
           output$pcaplotcluster <-         renderPlot({pcaplotcluster()                })
                
           output$pcaclustable <- renderTable({pcaclustable()})
           output$pcaclustable2 <- renderDataTable({pcaclustable2()})
           
           mfaresult <- eventReactive(input$mfabutton,{
               
               p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%select(varx,!!!input$var7mfa)%>%na.omit()
               d <- FAMD(p%>%select(-varx), graph = FALSE) 
               
               summary(d)
               
               
           }) 
           
           mfaresultfacto <- eventReactive(input$mfabutton,{
               
               p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%select(varx,!!!input$var7mfa)%>%na.omit()
               d <- FAMD(p%>%select(-varx), graph = FALSE)
               
               res.var <- get_famd_var(d)
              
               
               
           })
           
           mfaresultfacto2 <- eventReactive(input$mfabutton,{
               
               p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%select(varx,!!!input$var7mfa)%>%na.omit()
               # d <- prcomp(p%>%select(-varx), center = TRUE,scale. = TRUE)
               
               d<- FAMD(p%>%select(-varx), ncp = 3, graph = FALSE)
               
               
           })
           
           output$mfarcontrib <- renderPrint(mfaresultfacto()$contrib)
           output$mfarcoord <- renderPrint(mfaresultfacto()$coord)
           output$mfaploteigen <- renderPlot({d<-fviz_eig(mfaresultfacto2())
           d})
           output$mfaplotind<- renderPlot({
               p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%select(varx,!!!input$var7mfa)%>%na.omit()
               
               res.mfa <- mfaresultfacto2()
               groups <- as.factor(p$varx)
               d<- fviz_famd_ind(res.mfa,
                                col.ind = groups, # color by groups
                                palette = c("#00AFBB",  "#FC4E07"),
                                legend.title = "Groups",
                                repel = TRUE)
               d
               }
           )
           
           mfaplotvar <-eventReactive(input$mfavarbutton,
                                      {
                                          # p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7mfa)%>%na.omit()
                                          # res.mfa <- mfa(p%>%select(-varx), ncp = 3, graph = FALSE)
                                          # Graph of variables including supplementary variables
                                          d<-plot.FAMD(mfaresultfacto2(), axes=c(1, 2), choix="var")
                                          d
                                      }
                                      
           )                
           
           output$mfaplotvar <- renderPlot(
               {mfaplotvar()
               }
               
           )
           mfaplotcluster <- eventReactive(input$mfaclusterbutton,{
               
               # p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7mfa)%>%na.omit()
               # # Compute hierarchical clustering on principal components
               # res.mfa <- mfa(p%>%select(-varx), ncp = 3, graph = FALSE)
               res.hcpc <- HCPC(mfaresultfacto2(), graph = FALSE)
               d<-fviz_dend(res.hcpc, 
                            cex = 0.7,                     # Label size
                            palette = "jco",               # Color palette see ?ggpubr::ggpar
                            rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                            rect_border = "jco",           # Rectangle color
                            labels_track_height = 0.8) # Augment the room for labels
               d
               
           })
           
           
           
           mfaclustable <- eventReactive(input$mfaclusterbutton,{
               
               p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%select(varx,!!!input$var7mfa)%>%na.omit()
               # # Compute hierarchical clustering on principal components
               # res.mfa <- mfa(p%>%select(-varx), ncp = 3, graph = FALSE)
               res.hcpc <- HCPC(mfaresultfacto2(), graph = FALSE)
               table(res.hcpc$data.clust$clust,p$varx)
               
           })
           
           mfaclustable2 <- eventReactive(input$mfaclusterbutton,{
               
               # p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7mfa)%>%na.omit()
               # # Compute hierarchical clustering on principal components
               # res.mfa <- mfa(p%>%select(-varx), ncp = 3, graph = FALSE)
               res.hcpc <- HCPC(mfaresultfacto2(), graph = FALSE)
               p<- res.hcpc$data.clust%>%group_by(clust)%>%summarise(freq=n(),across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE),2)))
               p <-  setDT(p)
               d<-  datatable(p,options = list(scrollX = TRUE))
               
               
           })
           output$mfaplotcluster <-         renderPlot({mfaplotcluster()                })
           
           output$mfaclustable <- renderTable({mfaclustable()})
           output$mfaclustable2 <- renderDataTable({mfaclustable2()})
                          
                # output$treeplot <- renderPlot(plot(treeselect()))
           output$chosprosview <- renderUI(pickerInput("prosviewfileselect",choices = unique(ip2()%>%select(!!input$prosviewfile1)),multiple = FALSE))  
           plotprosview <- eventReactive(input$prosviewbutton,{
               
               dataframe <- ip2()%>% filter(!!input$prosviewfile1%in%!!input$prosviewfileselect)%>% mutate(selection = !!input$prosview3, selection2 = !!input$prosview2) 
               
               d<- ggplot(dataframe, aes(x=tmin, y=selection,color=selection2)) +
                   geom_segment(aes(x=tmin, xend=tmin, y=mean(selection,na.rm=TRUE), yend=selection), color="grey") +
                   geom_point (size=4) +
                   theme_light() +
                   theme(
                       panel.grid.major.x = element_blank(),
                       panel.border = element_blank(),
                       axis.ticks.x = element_blank()
                   ) +
                   xlab("") +
                   ylab("Prosodic variable") 
           d
               
           })
           output$plotprosview <- renderPlotly({plotprosview()})
           
           output$filterscontour <- renderUI({
               

pickerInput("cont2","Select group", choices = unique(vowels[input$cont1]), options =  list(`actions-box` = TRUE),multiple = F, selected = NULL)

          })

melodyplot <- eventReactive(input$contplot, {
    
    dataframe <- vowels%>% filter(!!input$cont1%in%!!input$cont2)%>%arrange(!!input$cont3)%>% mutate(xaxe = !!input$cont3, yaxe = !!input$cont4, linety = !!input$cont5, factormel = !!input$cont6, sizemel = !!input$cont7, textcolap = !!input$cont8, wrapping = !!input$cont9)
    
    ggplot(data=dataframe, aes(x=xaxe, y=yaxe, linetype=linety)) +
        geom_line(color="black", size=1)+ ylim(70,120)+
        geom_point(aes(colour = factor(factormel), size=sizemel))+
        geom_text(aes(label=textcolap)) + facet_wrap(~str_wrap(wrapping)) })

dataframemel <- eventReactive(input$contDT, {
    dataframe <- vowels%>% filter(!!input$cont1%in%!!input$cont2)%>%arrange(!!input$cont3)%>%select(!!!input$cont10)
})
           
            
            
output$dataframemel <- renderDataTable({dataframemel()})
            
output$melodyplot    <-     renderPlotly({melodyplot()})

output$creditsui <- renderText({

    includeHTML("grateful-citations.html")

    
})

output$licenseui <- renderUI({
    div(
    p("As practically all the used libraries on Oralstats are GNU", a("GNU General Public License v3.0",href="https://github.com/acabedo/oralstats/blob/main/LICENSE"),"we use a GNU license in our software.")
    )
    
})

output$inspirationalui <- renderUI({
    div(
    p("Here you will find a list of corpus platforms that have inspired the creation of Oralstats. Sometimes it could have been the disposal of the webpage, other times it could have been simply the database infrastructure. We want to give some credit to these previous works because they were a basis for our work."),
    
    a("Radiant",href="https://github.com/vnijs/radiant"),br(),
    a("Corpus del español",href="https://www.corpusdelespanol.org/"),br(),
    a("Spokes",href="http://spokes.clarin-pl.eu/"),br(),
    a("Eslora",href="http://eslora.usc.es/"),br(),
    a("CQPWeb",href="https://cqpweb.lancs.ac.uk/"),br(),
    a("ETITOBI",href="http://stel3.ub.edu/labfon/amper/eti_ToBI/"))
    
})


output$rawfilter <- renderUI({
   div(
    # varSelectInput("filenameraw","Select filename variable",multiple=FALSE,ip2()%>%select(where(is.character))),
    selectInput("filenamerawselect","Select file", multiple=FALSE,choices=unique(ip2()%>%select(filename))),
    varSelectInput("rawtext","Select text variable",multiple=FALSE, ip2()%>%select(where(is.character))),
    varSelectInput("rawspk","Select speaker variable",multiple=FALSE, ip2()%>%select(where(is.character))),
    varSelectInput("rawx","Select x or time variable",multiple=FALSE, prosody%>%select(where(is.numeric))),
    varSelectInput("rawy","Select pitch or intensity variable",multiple=FALSE,prosody%>%select(where(is.numeric))),
    sliderInput("sliderraw","Adjust times",min = 0,max=500000, value = c(0,10000)),
    actionButton("rawbutton","Create pitch or intensity plot"))
    })

rawplot <- eventReactive(input$rawbutton,{
    
    
    p <- prosody%>%filter(filename %in% !!input$filenamerawselect)%>%mutate(varx = !!input$rawx, vary=!!input$rawy,varfile=filename)%>%filter(varx>=!!input$sliderraw[1],varx<=!!input$sliderraw[2])
    d<- ggplot(data=p, aes(x=varx))+ geom_point(aes(y = vary), color = "blue")+ ylim(70,120)
    d
})

rawtable <- eventReactive(input$rawbutton,{
    
    
    p <- ip2()%>%filter(filename %in% !!input$filenamerawselect)%>%select(filename,!!input$rawspk,!!input$rawtext)
    
})

output$rawplot <- renderPlotly({rawplot()})
output$rawtable <- renderDataTable({rawtable()})

observeEvent(input$modal1button, {
    showModal(modalDialog(
        title = "About filter",
        "If you want to filter the database, you just need to use the filtering sidebar on the left. Any filter parameter will then populate the following sections of Oralstats. If you do not need to filter, but you want to explore the data, click the Add variables to preview and select any variable from the database. It will appear on section below.",
        easyClose = TRUE
    ))
})

observeEvent(input$modal2button, {
    showModal(modalDialog(
        title = "About ngrams",
        "Select the variable you want to tokenize or get the ngrams from. After that you can select the number of ngrams.",
        easyClose = TRUE
    ))
})

observeEvent(input$modal3button, {
    showModal(modalDialog(
        title = "About frequencies",
        "In this section you can select any character variable you want, or a bunch of them, and also some numeric variables. Oralstats will tell you the frequencies by category and also several descriptive statistics, like the mean, the median, the standard deviation, the minimum and maximum.",
        easyClose = TRUE
    ))
})

observeEvent(input$modal4button, {
    showModal(modalDialog(
        title = "About variables description",
        "In this section you can select any variable to get common descriptive statistics. This section presents the overall values of the variable, without being grouped by category",
        easyClose = TRUE
    ))
})

observeEvent(input$modal5button, {
    showModal(modalDialog(
        title = "About crosstabs and chi square",
        "You can combine two variables and get chi square variable; you will get chi square statistics values and also the residuals. A chart will also being generated.",
        easyClose = TRUE
    ))
})

observeEvent(input$modal6button, {
    showModal(modalDialog(
        title = "About correlations",
        "You can select multiple files and Oralstats will project a chart with the specific correlations. An X on chart means no significant correlation at all.",
        easyClose = TRUE
    ))
})

observeEvent(input$modal7button, {
    showModal(modalDialog(
        title = "About ANOVA",
        "You can apply ANOVA test to separate groups. This section will offer you a boxplot by category",
        easyClose = TRUE
    ))
})

observeEvent(input$modal8button, {
    showModal(modalDialog(
        title = "About Heatmap",
        "You can apply heatmap to observe the relation between some numeric variables and some categories. Data will be standardized",
        easyClose = TRUE
    ))
})

observeEvent(input$modal9button, {
    showModal(modalDialog(
        title = "About Decision trees",
        "You can generate decision trees from two libraries: Rpart and Party.",
        easyClose = TRUE
    ))
})

observeEvent(input$modal10button, {
    showModal(modalDialog(
        title = "About PCA",
        "You can generate principal component analysis with Factominer library. In addition, you will be able to get some charts and even a cluster analysis to get groups from PCA analysis. This is offered just with an exploratory aim.",
        easyClose = TRUE
    ))
})

observeEvent(input$modal11button, {
    showModal(modalDialog(
        title = "About MFA",
        "You can generate mixed factor analysis with Factominer library. In addition, you will be able to get some charts and even a cluster analysis to get groups from MFA analysis. This is offered just with an exploratory aim.",
        easyClose = TRUE
    ))
})

observeEvent(input$modal12button, {
    showModal(modalDialog(
        title = "About linear",
        "You can get the raw pitch/intensity values for selected times in a file. Some mods of Oralstats offer the option to hear also this pitch alignment",
        easyClose = TRUE
    ))
})

observeEvent(input$logbuttonhelp, {
  showModal(modalDialog(
    title = "About binary logistic regression",
    "You can get results for binary logistic regression. Only the significant variable will be kept on the resulting model. You would be able to see a correct vs incorrect classification on data with the generated  model.",
    easyClose = TRUE
  ))
})



logisticresults <- eventReactive(input$logbutton,{
    
    p <- ip2()%>%mutate(varx = !!input$var6log)%>%filter(varx%in%!!input$filterlog)%>%select(varx,!!!input$var7log)%>%na.omit()%>%mutate(across(where(is_character),as_factor))
    
    logit_1<- logistic_reg() %>%set_engine("glm") %>% set_mode("classification") %>% fit(varx~., data = p)
    tidy(logit_1)%>%
        filter(p.value < 0.05)
}
)

output$logisticresults <- renderPrint({logisticresults()})

logisticpredicted <- eventReactive(input$logbutton,{
    
    p <- ip2()%>%mutate(varx = !!input$var6log)%>%filter(varx%in%!!input$filterlog)%>%select(varx,!!!input$var7log)%>%na.omit()%>%mutate(across(where(is_character),as_factor))
    
    logit_1<- logistic_reg() %>%set_engine("glm") %>% set_mode("classification") %>% fit(varx~., data = p)
    
    pred_class <- predict(logit_1,
                          new_data = p,
                          type = "class")
    pred_proba <- predict(logit_1,
                          new_data = p,
                          type = "prob")
    
    diabetes_results <- p %>%
        select(varx) %>%
        bind_cols(pred_class, pred_proba)
    
   d <- conf_mat(diabetes_results, truth = varx,
             estimate = .pred_class)
    d
    })
    
output$varmeansui <- renderUI({
  
  varSelectInput("varmeans","means",multiple = TRUE,data = ip%>%select(where(is.numeric)))
  
})
output$logisticpredicted <- renderPrint(logisticpredicted())   

    # Calculating Accuracy
logisticaccuracy <- eventReactive(input$logbutton,{
    
    p <- ip2()%>%mutate(varx = !!input$var6log)%>%filter(varx%in%!!input$filterlog)%>%select(varx,!!!input$var7log)%>%na.omit()%>%mutate(across(where(is_character),as_factor))
    
    logit_1<- logistic_reg() %>%set_engine("glm") %>% set_mode("classification") %>% fit(varx~., data = p)
    
    pred_class <- predict(logit_1,
                          new_data = p,
                          type = "class")
    pred_proba <- predict(logit_1,
                          new_data = p,
                          type = "prob")
    
    diabetes_results <- p %>%
        select(varx) %>%
        bind_cols(pred_class, pred_proba)
    
    d<- accuracy(diabetes_results, truth = varx,
             estimate = .pred_class)
    d
})

output$logisticaccuracy <- renderPrint(logisticaccuracy())    

pitchdf <- reactive({
    

    p <- map2_df(input$pitchfile$name, input$pitchfile$datapath, 
                            ~fread(.y)%>% mutate(filename = .x))
    p <- p%>% rename(pitch=V2,time=V1)%>%rownames_to_column(.,"id")%>%mutate(time = round(as.numeric(time),2), pitchst = round((12*log2(pitch/1)),2),pitch=round(pitch,2))%>%distinct(filename,time,.keep_all = TRUE)%>%mutate(time_ms = time*1000, filename=gsub(".txt","",filename))
    
  
  
})



intensitydf <- reactive({
    

    p <- map2_df(input$intensityfile$name, input$intensityfile$datapath, 
                 ~fread(.y)%>% mutate(filename = .x))
    p <- p%>%mutate(time = round(as.numeric(time),2), intensity=round(intensity,2))%>%distinct(filename,time,.keep_all = TRUE)%>%mutate(time_ms = time*1000, filename=gsub(".txt","",filename))
    
    
})

pitchintensitydf <- reactive({
    
    p <- pitchdf()%>%dplyr::left_join(intensitydf()%>%select(filename,time,intensity),by=c("filename","time"))%>%filter(pitch<330,intensity<90)
    
})

# output$downloadintensity <- downloadHandler(
#     
#     filename = function() {
#         paste("prosodydf-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#         
#         write.csv(intensitydf(), file,quote=FALSE,row.names = FALSE)
#         # write.xlsx(ip2(),file)
#         # write.table(ipframe(), file,sep=sep,quote = TRUE)
#     }
#     
# )
# 
# output$downloadpitch <- downloadHandler(
#     
#     filename = function() {
#         paste("prosodydf-", Sys.Date(), ".csv", sep="")
#     },
#     content = function(file) {
#         
#         write.csv(pitchdf(), file,quote=FALSE,row.names = FALSE)
#         # write.xlsx(ip2(),file)
#         # write.table(ipframe(), file,sep=sep,quote = TRUE)
#     }
#     
# )

output$downloadprosody <- downloadHandler(
    
    filename = function() {
        paste("prosodydf-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
        
        write.csv(pitchintensitydf()%>%select(filename,time, pitch, pitchst,intensity), file,quote=FALSE,row.names = FALSE)
        # write.xlsx(ip2(),file)
        # write.table(ipframe(), file,sep=sep,quote = TRUE)
    }
    
)

rawaudio <- eventReactive(input$rawbutton,{
    
    p <- prosody%>%filter(filename %in% !!input$filenamerawselect)%>%mutate(varx=filename)%>%select(varx)
    p <- p %>%group_by(varx)%>%summarise(n())
    paste("audios/",p$varx,".mp3",sep="")
    
})

output$rawaudio <- renderUI({ div(tags$audio(src=rawaudio(),type = "audio/mp3",autoplay = NA, controls = NA))
    
    })

tabbeddf <- reactive({
    
    orst <- read_delim(input$tabbedtext$datapath,"\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
    orst <- orst %>% rename(
        tier = X1,
        spk = X2,
        tmin_ms = X3,
        tmax_ms = X4,
        dur = X5,
        annotation = X6,
        filename = X7
    ) %>% mutate(filename = gsub(".eaf","",filename), corpus="corpus")
    
})




metadatafile <- reactive({ 
    
    metadata <- read_delim(input$metadata$datapath,"\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
    
    
    })

dfprosody <- eventReactive(input$createbutton,{
    
    
    orstip <- tabbeddf()
    prosodydb <- pitchintensitydf()
    # orstip <- subset(orst, !grepl("phon\\/phon$",tier)&!grepl("word$",tier)&!grepl("acto",tier))
    orstip <- orstip%>%group_by(filename)%>%mutate(ip_id= paste("ip_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))

    ippros<- sqldf(
        "select x.*,y.pitch, y.intensity, y.time_ms from orstip x left join prosodydb as y on (y.time_ms >= (x.tmin_ms)) and (y.time_ms <= (x.tmax_ms)) and (x.filename = y.filename)")
    orstiprn <-
        ippros%>% 
        group_by(ip_id) %>% summarise(
            spk = max(spk),
            tmin = max(tmin_ms),
            tmax = max(tmax_ms),
            dur = max(dur),
            phon = max(annotation),
            file = max(filename),
            PirHz = round(max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),2),
            PirSt = round(12*log2(max(pitch,na.rm=TRUE)/min(pitch, na.rm = TRUE)),2),
            Pimd = round(median(pitch, na.rm = TRUE),2),
            PimnHz = round(mean(pitch, na.rm = TRUE),2),
            PimnSt = round(12*log2(mean(pitch,na.rm = TRUE)/1),2),
            Imd = round(median(intensity, na.rm = TRUE),2),
            Imn = round(mean(intensity, na.rm = TRUE),2),
            dcln = round(last(pitch)-first(pitch),2),
            dcln_st = round(12*log2(last(pitch)/first(pitch)),2),
            
        ) %>%ungroup()%>% arrange(file,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(file)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
    # col_means <- lapply(orstalofrn%>%select(-transition_post, -transition_prev), mean, na.rm = TRUE)
    # col_means <- lapply(col_means, round,2)
    # orstalofrn <- replace_na(orstalofrn, col_means)
    orstiprn <-
        orstiprn %>% mutate(
            RdprSt = PirSt - lag(PirSt),
            Idpr = Imn - lag(Imn, 1),
            spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
            spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
            ppr = ifelse(spkpr =="mismo",trpr,NA),
            ppst = ifelse(spkpst =="mismo",trpst,NA)
        )
    orstiprn <-
        orstiprn %>% group_by(file, spk) %>% mutate(
            # Rng_mean_sp = round(mean(pitch_range_St,na.rm = TRUE), 2),
            Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
            Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
            # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
            # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
            PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
            PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
            # Pdspkp = round(Pit_diff_sp_Hz / mean(pitch_mean_Hz,na.rm = TRUE), 2) * 100
            # pitch_mean_sp = round(mean(pitch_mean_Hz,na.rm = TRUE), 2)
        ) %>% ungroup()
    
ip <- orstiprn%>%left_join(metadatafile(), by="spk")
    
})

output$downloadip <- downloadHandler(
    
    filename = function() {
        paste("ipdf-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
        
        write.csv(dfprosody(), file,quote=FALSE,row.names = FALSE)
        # write.xlsx(ip2(),file)
        # write.table(ipframe(), file,sep=sep,quote = TRUE)
    }
    
)

output$dforiginal <- renderDataTable({datatable(tabbeddf(),options = list(scrollX = TRUE))})
output$dftransform <- renderDataTable({
    d <- dfprosody()
    datatable(d,options = list(scrollX = TRUE))})

   }         
            shinyApp(ui, server)
