
# Load dependencies -------------------------------------------------------

library(shiny)

library(wordcloud2)
library(shinydashboard)
# library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
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
# library(sqldf)
library(xlsx)
library(tidymodels)
library(udpipe)
library(markdown)
library(RSQLite)
library(phonfieldwork)

# Load databases from RSQLite file -------------------------------------------------------

db <- dbConnect(RSQLite::SQLite(), dbname = "oralstats.sqlite")
fonocortesiadb <- tbl(db,"fonocortesiadb")%>%collect()
fonocortesiadb <- fonocortesiadb%>%mutate(tmin_ms=0,tmax_ms=10000)
vowels <- tbl(db,"vowels_fonocortesia")%>%collect()
fonocortesiaprosody <- tbl(db,"prosody_fonocortesia")%>%collect()
picodeorodb <- tbl(db,"picodeorodb")%>%collect()
picodeorodbvowels <- tbl(db,"picodeorodbvowels")%>%collect()
picodeoroprosody <- tbl(db,"picodeoroprosody")%>%collect()


dbDisconnect(db)


# UI section --------------------------------------------------------------


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Oralstats"),
                    ## Sidebar content
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Info", tabName = "info", icon = icon("info-sign", lib = "glyphicon"),startExpanded = FALSE),
                        menuItem("How to use", tabName = "help", icon = icon("header", lib = "glyphicon"),startExpanded = FALSE),
                        # menuItem("Select database", tabName = "selecttab", icon = icon("play-circle", lib = "glyphicon"),startExpanded = FALSE),
                        
                        menuItem("Select database & Filter", tabName = "filter", icon = icon("search", lib = "glyphicon")),
                        menuItem("N-Grams", tabName = "ngrams", icon = icon("th", lib = "glyphicon")),
                        menuItem("Basic methods", tabName = "basic", icon = icon("bold", lib = "glyphicon"),
                                 startExpanded = FALSE,
                                 menuSubItem("Descriptive by group", tabName = "frequencies", icon = icon("list-alt", lib = "glyphicon")),
                                 menuSubItem("Variable description", tabName = "descriptive", icon = icon("eye-open", lib = "glyphicon")),
                                 menuSubItem("Crosstabs and Chi-Square", tabName = "crosstabs", icon = icon("resize-horizontal", lib = "glyphicon"))),
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
                        menuItem("Upload", tabName = "uploadtab", icon = icon("upload", lib = "glyphicon"),startExpanded = FALSE),
                        menuItem("Create", tabName = "creation", icon = icon("pencil", lib = "glyphicon"),startExpanded = FALSE),
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
                      tags$head(tags$style(".modal-dialog{ width:80%}")),
                      tags$head(tags$style(".modal-body{ min-height:80%}")),
                      tabItems(
                        tabItem(
                          tabName="info",
                          fluidRow(
                            shinydashboard::box(width=2,status = "primary", solidHeader = TRUE,title="Author", includeHTML("html/author.html"),
                                                tags$a(href = "https://github.com/acabedo/",
                                                       tags$img(src="github.png",width="20")),
                                                #                     socialButton(
                                                #   href = "https://github.com/acabedo/oralstats",
                                                #   icon = icon("github")
                                                # ),
                                                tags$a(href = "https://www.researchgate.net/profile/Adrian-Cabedo-Nebot",
                                                       tags$img(src="researchgate.png",width="20"))
                                                  
                                                ,
                                                tags$a(
                                                  href="https://canea.blogs.uv.es/", 
                                                  tags$img(src="uv.gif", 
                                                           title="Universitat de València", 
                                                           width="20",
                                                           height="20")
                                                ))
                            ,
                            shinydashboard::box(width=10,status = "info", solidHeader = TRUE,title="Oralstats",
                                                
                                                shinydashboard::box(width=NULL,status = "info", solidHeader = FALSE,title="About",tags$img(src="oralstats.png",width="150"),collapsible = FALSE,collapsed = FALSE,tags$br(),includeHTML("html/about.html")),
                                                
                                                shinydashboard::box(width=NULL,status = "info", solidHeader = FALSE,title="License & requirements", includeHTML("html/license.html"), br(), includeHTML("html/requirements.html"))
                            )),
                          fluidRow(shinydashboard::box(width=12,status = "warning", solidHeader = TRUE,title="Sample databases",collapsible = TRUE,collapsed = FALSE,includeHTML("html/projects.html")))
                          
                          
                          
                          
                          
                        ),
                        tabItem(tabName="help",
                                fluidRow(shinydashboard::box(width=12,status = "info",style='height:800px;overflow-y: scroll;', solidHeader = TRUE,title="How to use",collapsible = TRUE,collapsed = FALSE,includeMarkdown("help.md"))),
                                
                        ),
                        tabItem(tabName="selecttab",
                                
                                
                        ),
                        tabItem(tabName="uploadtab",
                                fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,title="Instructions",collapsible = FALSE,collapsed = FALSE,
                                                             uiOutput("instructionsui"),actionButton("tutorialupload","Video tutorial",icon = icon("film", lib = "glyphicon")))),
                                fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,title="Upload files",collapsible = FALSE,collapsed = FALSE,
                                                             fileInput("uploadRDSip","Upload a RDS file to explore",multiple = FALSE),
                                                             fileInput("uploadRDSvowels","Upload a RDS vowels file",multiple = FALSE),
                                                             fileInput("uploadRDSprosody","Upload a RDS prosody file",multiple = FALSE)
                                                             # actionButton("uploadbutton","Upload file")
                                )),
                                
                        ),
                        tabItem(tabName = "creation",
                                
                                # fluidRow(shinydashboard::box(width=12,status = "danger", solidHeader = TRUE,title="Important message",tags$p("This section does not include a fully complete version of the Oralstats creation script; you can transform only one file with its correspondent pitch and intensity files. If you prefer more advanced features, you can use instead Oralstats.create local script available on Github"))),
                                fluidRow(shinydashboard::box(width="12", title ="Create files", status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,actionButton("tutorialcreate","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                             
                                                             
                                                             
                                                             div(tabBox(width=NULL,
                                                                        title = "Input data",
                                                                        # The id lets us use input$tabset1 on the server to find the current tab
                                                                        id = "tabsetcreate",
                                                                        tabPanel("Create",
                                                                          
                                                                                 
                                                                                 splitLayout( fileInput("tabbedtext","Upload tabbed text file from ELAN/PRAAT/SUBTITLES",multiple=TRUE),                                                           radioButtons("radio1bttn","Select file format",choices = c("TABTXT","TextGrid","EAF"),selected = "TABTXT"),
                                                                                                                                                         radioButtons("radio2bttn","Columns name",choices = c("yes","no"),selected = "no") 
                                                                        ))
                                                             ,
tabPanel("Add files",
                      splitLayout(                                       fileInput("pitchfile","upload pitch file",multiple = TRUE),
                                                             fileInput("intensityfile","upload intensity file",multiple = TRUE),
                                                             textInput("audiofile2","Write a link to the audio",placeholder = "Write here the link")),

         splitLayout( fileInput("metadata","upload metadata file"),
fileInput("sentfile","upload sentiment lexicon"),
fileInput("ipafile","upload phonetic lexicon"),
fileInput("udpipefile","upload Language Pipe file"),
fileInput("posfile","upload pos tagged file"))),
tabPanel("Identify your units",
         tags$h4("Information:"),tags$p(" Here you can include how did you mark your speech units. For example, 'ip' could be a identifier for intonational phrase, 'word', for words, and so on"),
         tags$br(),
         splitLayout(
         textInput("ipsname","Ips identifier"),
         textInput("wordsname","Words identifier"),
         textInput("phonemesname","Phonemes identifier"),
textInput("collapse1","Tag 1 identifier"),
textInput("collapse2","Tag 2 identifier"))),
tabPanel("Sample files to play with",
         tags$a(href="sample_files/pzorrocarharla.zip", target='blank', 'pzorrocarharla', download = 'pzorrocarharla.zip')
,tags$br(),
         tags$p("In the previous zip file you will find a folder with one Tabbed TXT file and two folder including pitch and intensity files. Take a look at the structure expected by Oralstats")
         
         ),
tabPanel("Variables and categories description",
         
         includeHTML("html/variables.html")
         )
                                                             
                                                             
                                                             
                                )),
tags$p(tags$strong("Note: "),"If you want to create files with Oralstats, you need to select an option at Database and filter section"))),
                                # fluidRow(
                                #   
                                #   
                                #   shinydashboard::box(width="3",title="Prosody",status = "warning", solidHeader = TRUE,collapsible = FALSE,collapsed = TRUE,
                                #                       
                                #                       
                                #   ),
                                #   shinydashboard::box(width="3",title="Other files", status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
                                #                      
                                #   ),
                                #   shinydashboard::box(width="3",title="Collapse hierarchy", status = "warning", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                #                       # textInput("collapse3","Collapse 3"),
                                #                       # textInput("collapse4","Collapse 4"),textInput("collapse5","Collapse 5")
                                #   )
                                # )
                                # ,
                                
                                
                                # fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,collapsible = FALSE,collapsed = FALSE, title="Original df",
                                #                              
                                #                              shinycssloaders::withSpinner(dataTableOutput("dforiginal")) )),
                                # fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,collapsible = FALSE,collapsed = TRUE, title="IP Transformed df",
                                #              actionButton("createbutton","Create files"),
                                #              shinycssloaders::withSpinner(dataTableOutput("dftransform")) )),
                                fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE, title="Data frames",
                                                             div(tabBox(width=NULL,
                                                                        title = "Data frames",
                                                                        # The id lets us use input$tabset1 on the server to find the current tab
                                                                        id = "tabset1",
                                                                        tabPanel("Original",shinycssloaders::withSpinner(dataTableOutput("dforiginal"))),
                                                                        tabPanel("Ips", actionButton("createbutton","Create ips"),
                                                                                 downloadButton("downloadipxlsx","Download XLSX"),
                                                                                 downloadButton("downloadiprds","Download RDS"),
                                                                                 actionButton("createbutton2","Combine with vowels (vowels df needed)"),
                                                                                 downloadButton("downloadipjoinedxlsx","Download combined XLSX"),
                                                                                 downloadButton("downloadipjoinedrds","Download combined RDS"),
                                                                                 shinycssloaders::withSpinner(dataTableOutput("dftransform")),
                                                                                 
                                                                                 shinycssloaders::withSpinner(dataTableOutput("dftransform2"))
                                                                                 
                                                                                 ),
                                                                        tabPanel("Words", actionButton("createwords","Create words"),
                                                                                 downloadButton("downloadwordsxlsx","Download XLSX"),
                                                                                 downloadButton("downloadwordsrds","Download RDS"),
                                                                                 actionButton("createwords2","Combine with vowels (vowels df needed)"),
                                                                                 downloadButton("downloadwordsjoinedxlsx","Download combined XLSX"),
                                                                                 downloadButton("downloadwordsjoinedrds","Download combined RDS"),
                                                                                 shinycssloaders::withSpinner(dataTableOutput("wordsdt")),
                                                                                 shinycssloaders::withSpinner(dataTableOutput("wordsdt2"))),
                                                                        tabPanel("Vowels (TOBI+MAS)", actionButton("createphonemes","Create vowels"),
                                                                                 downloadButton("downloadphonemesxlsx","Download XLSX"),
                                                                                 downloadButton("downloadphonemesrds","Download RDS"),
                                                                                 shinycssloaders::withSpinner(dataTableOutput("phonemesdt"))),
                                                                        tabPanel("Collapse1", actionButton("createcollapse1","Create collapse 1 df"),
                                                                                 downloadButton("downloadcollapse1xlsx","Download XLSX"),
                                                                                 downloadButton("downloadcollapse1rds","Download RDS"),
                                                                                 actionButton("createcollapsejoin","Combine with vowels (vowels df needed"),
                                                                                 downloadButton("downloadcollapse1joinedxlsx","Download XLSX "),
                                                                                                                                  downloadButton("downloadcollapse1joinedrds","Download RDS"),
                                                                                                                                  
                                                                                 shinycssloaders::withSpinner(dataTableOutput("collapse1dt")),
                                                                                 shinycssloaders::withSpinner(dataTableOutput("collapse1dtjoined")),),
                                                                        tabPanel("Collapse2", actionButton("createcollapse2","Create collapse 2 df"),
                                                                                 downloadButton("downloadcollapse2xlsx","Download XLSX"),
                                                                                 downloadButton("downloadcollapse2rds","Download RDS"),
                                                                                 actionButton("createcollapsejoin2","Combine with vowels (vowels df needed"),
                                                                                 downloadButton("downloadcollapse2joinedxlsx","Download XLSX "),
                                                                                 downloadButton("downloadcollapse2joinedrds","Download RDS"),
                                                                                 shinycssloaders::withSpinner(dataTableOutput("collapse2dtjoined")),
                                                                                 shinycssloaders::withSpinner(dataTableOutput("collapse2dt"))),
                                                                        tabPanel("Prosody",
                                                                                 downloadButton("downloadprosodycsv","Download pitch-intensity df CSV"),
                                                                                 downloadButton("downloadprosodyrds","Download pitch-intensity df RDS"))
                                                             )
                                                             
                                                             
                                                             )))
                                # fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE, title="Words Transformed df",
                                #                              div(tabBox(width=NULL,
                                #                                         title = "Words data frame",
                                #                                         # The id lets us use input$tabset1 on the server to find the current tab
                                #                                         id = "tabset1",
                                #                                         tabPanel("Words", actionButton("createwords","Create words"),
                                #                                                  downloadButton("downloadwordsxlsx","Download XLSX"),
                                #                                                  downloadButton("downloadwordsrds","Download RDS"),
                                #                                                  shinycssloaders::withSpinner(dataTableOutput("wordsdt"))),
                                #                                         tabPanel("Words with prosody", actionButton("createwords2","Combine with phonemes"),shinycssloaders::withSpinner(dataTableOutput("wordsdt2")))
                                #                              )
                                #                              
                                #                              
                                #                              ))),
                                # fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE, title="Phonemes Transformed df",
                                #                              actionButton("createphonemes","Create phonemes"),
                                #                              downloadButton("downloadphonemesxlsx","Download XLSX"),
                                #                              downloadButton("downloadphonemesrds","Download RDS"),
                                #                              shinycssloaders::withSpinner(dataTableOutput("phonemesdt")) )),
                                # # fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE, title="Vowels Transformed df",
                                # #                              actionButton("createvowels","Create vowels"),
                                # #                              downloadButton("downloadvowelsxlsx","Download XLSX"),
                                # #                              downloadButton("downloadvowelsrds","Download RDS"),
                                # #                              shinycssloaders::withSpinner(dataTableOutput("vowelsdt")) )),
                                # fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE, title="Collapse1 Transformed df",
                                #                              div(tabBox(width=NULL,
                                #                                         title = "Collapse1 data frame",
                                #                                         # The id lets us use input$tabset1 on the server to find the current tab
                                #                                         id = "tabset1",
                                #                                         tabPanel("Collapse1", actionButton("createcollapse1","Create collapse1"),
                                #                                                  downloadButton("downloadcollapse1xlsx","Download XLSX"),
                                #                                                  downloadButton("downloadcollapse1rds","Download RDS"),
                                #                                                  shinycssloaders::withSpinner(dataTableOutput("collapse1dt"))),
                                #                                         tabPanel("Collapse 1 with words prosody", actionButton("createcollapsejoin","Combine with phonemes"),
                                #                                                  downloadButton("downloadcollapse1joinedrds","Download RDS"),
                                #                                                  shinycssloaders::withSpinner(dataTableOutput("collapse1dtjoined")))
                                #                              )
                                #                              
                                #                              
                                #                              )
                                #                              
                                #                              # actionButton("createcollapse1","Create collapse1"),
                                #                              # actionButton("createcollapsejoin","Create collapse1 joined"),
                                #                              # 
                                #                              # shinycssloaders::withSpinner(dataTableOutput("collapse1dt")) 
                                # )),
                                # fluidRow(shinydashboard::box(width=12,status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE, title="Collapse2 Transformed df",
                                #                              div(tabBox(width=NULL,
                                #                                         title = "Collapse2 data frame",
                                #                                         # The id lets us use input$tabset1 on the server to find the current tab
                                #                                         id = "tabset2",
                                #                                         tabPanel("Collapse2", actionButton("createcollapse2","Create collapse2"),
                                #                                                  downloadButton("downloadcollapse2xlsx","Download XLSX"),
                                #                                                  downloadButton("downloadcollapse2rds","Download RDS"),
                                #                                                  shinycssloaders::withSpinner(dataTableOutput("collapse2dt"))),
                                #                                         tabPanel("Collapse 1 with words prosody", actionButton("createcollapsejoin2","Combine with phonemes"),shinycssloaders::withSpinner(dataTableOutput("collapse2dtjoined")))
                                #                              )
                                #                              
                                #                              
                                #                              )
                                #                              
                                # )),
                                # fluidRow(shinydashboard::box(width="12",title = "Download prosody",status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = FALSE,
                                #                              ))
                                # fluidRow(shinydashboard::box(width="12",title = "Download RDS files",status = "primary", solidHeader = TRUE,
                                #              
                                #              
                                #              
                                #              
                                #              
                                #              )),
                                
                                
                                
                        )
                        
                        
                        ,
                        tabItem(tabName = "filter",
                                fluidRow(shinydashboard::box(width=12,status = "info", solidHeader = TRUE,title="Select data",collapsible = TRUE,collapsed = FALSE,
                                                             div(
                                                               pickerInput("selectdata","Select data frame to use", 
                                                                           choices = c("fonocortesiadb",
                                                                                       "picodeorodb",
                                                                                       "I will use ips from create section",
                                                                                       "I will use words from create section",
                                                                                       "I will use vowels from create section",
                                                                                       "I will use collapse1 from create section",
                                                                                       "I will use collapse2 from create section",
                                                                                       "I will upload my data at Upload Section"),
                                                                           multiple = FALSE,selected = "fonocortesiadb"),
                                                               pickerInput("joinedsel","Use hierarchy (only for create section)?",
                                                                           choices = c("yes","no"),
                                                                           multiple = FALSE,selected = "no"))                                                            
                                                             ,actionButton("oralstatify","Load or create"))),

                                  
fluidRow(shinydashboard::box(width = 12,title = "Database", status = "info",collapsible = TRUE,collapsed = FALSE, solidHeader = TRUE,
                                                             
                                                             actionButton("filterbutton","Filter"),
                                                             actionButton("resetbutton","Reset"),

                             tags$table(width="100%",
                               
                              tags$tr(
                              tags$th(width="15%",uiOutput("var1ui")),
                              tags$th(width="15%",uiOutput("filter1ui")),
                              tags$th(width="15%",uiOutput("var2ui")),
                              tags$th(width="15%",uiOutput("filter2ui")),
                              tags$th(width="15%",uiOutput("var5ui")),
                              tags$th(width="10%",uiOutput("filternum1ui")),
                              tags$th(width="10%",uiOutput("filternum2ui"))),
                              tags$tr(
                                tags$th(uiOutput("var3ui")),
                                tags$th(uiOutput("filter3ui")),
                                
                              
                            
                                tags$th(uiOutput("var4ui")),
                                tags$th(uiOutput("filter4ui")),
                                tags$th(uiOutput("var6ui")),
                                tags$th(uiOutput("filternum3ui")),
                                tags$th(uiOutput("filternum4ui")))),
                             
                                                       
                              shinycssloaders::withSpinner(uiOutput("databaseselfilter")), shinycssloaders::withSpinner(DTOutput("database")), uiOutput("selected"), textOutput("selectedtext"))
                                         # uiOutput("rawaudio2")
                                  )
                                  
                                  
                                )
                                
                                
                                
                        ,
                        
                        
                        tabItem(tabName = "ngrams",
                                fluidRow(shinydashboard::box(width=3,status = "info", solidHeader = TRUE,title="Select variable to get ngrams",actionButton("modal2button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialngrams","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                             uiOutput("varngrams"),
                                                             uiOutput("ngramsize"),
                                                             actionButton("ngramsbutton",label = "Calculate ngrams")),
                                         shinydashboard::box(width=9,title="N-grams",status = "warning", solidHeader = TRUE,
                                                             shinycssloaders::withSpinner(dataTableOutput("ngrams1table")))),
                                fluidRow(box(width= 12, status = "info",solidHeader = TRUE,title="Word cloud", wordcloud2Output("ngramswcloud")
                                             )
                                         
                                         
                                         
                                )),
                        tabItem(tabName = "frequencies",
                                fluidRow(shinydashboard::box(width=3,title = "Group by", status = "info", solidHeader = TRUE,actionButton("modal3button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialfreqgroups","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                             
                                                             uiOutput("frequ"),
                                                             uiOutput("varmeansui")),
                                         shinydashboard::box(width=9,title = "Frequency table", status = "primary", solidHeader = TRUE, dataTableOutput("frequency"))
                                )
                                
                                
                                
                        )
                        ,
                        tabItem(tabName = "descriptive",
                                
                                fluidRow(shinydashboard::box(width=3,title = "Group by", status = "info", solidHeader = TRUE,
                                                             actionButton("modal4button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialdescriptive","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                             uiOutput("descriptivenum"))),
                                
                                fluidRow(shinydashboard::box(width=12,title = "Descriptive statistics", status = "primary", solidHeader = TRUE, dataTableOutput("descriptivestats"))
                                )
                                
                        ),
                        tabItem(tabName = "crosstabs",
                                
                                fluidRow(
                                  
                                  shinydashboard::box(title = "Select one variable",status="info",solidHeader = TRUE,actionButton("modal5button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialcrosstabs","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                      uiOutput("crosstabsdep"),
                                                      uiOutput("crosstabssel")),
                                  shinydashboard::box(title = "Select other variable",status="info",solidHeader = TRUE,
                                                      uiOutput("crosstabsindep"),
                                                      
                                                      uiOutput("crosstabssel2"),actionButton("crosstabsbutton","Generate crosstabs",icon = icon("flash", lib = "glyphicon"))),
                                  
                                  
                                  
                                  # fluidRow(shinydashboard::box(width=3,)
                                  # )
                                ),
                                fluidRow(
                                  
                                  column(width=5,shinydashboard::box(title = "Chi-squarevalue", status = "primary", solidHeader = TRUE,width = NULL,verbatimTextOutput("crosstabschi")),
                                         shinydashboard::box(title = "Frequency", status = "primary", solidHeader = TRUE,width = NULL,verbatimTextOutput("crosstabsobserved")),
                                         shinydashboard::box( title = "Residuals", status = "primary", solidHeader = TRUE,width = NULL,verbatimTextOutput("crosstabsresiduals"))),
                                  column(width=7,shinydashboard::box(title = "Frequency Barplot", status = "warning", solidHeader = TRUE, width = NULL,shinycssloaders::withSpinner(plotlyOutput("crosstabsplot")))
                                  ))),
                        
                        tabItem(tabName = "correlations",
                                fluidRow(shinydashboard::box(title = "Select numeric variables", status = "info", solidHeader = TRUE,actionButton("modal6button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialcorrelations","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                             uiOutput("correlationsdep"),
                                )),
                                fluidRow(shinydashboard::box(actionButton("correlationsbutton","Generate correlations",icon = icon("flash", lib = "glyphicon"))
                                )),
                                fluidRow(shinydashboard::box(width=12,title = "Correlation plot", status = "warning", solidHeader = TRUE,plotOutput("correlationsplot"))
                                )),
                        
                        tabItem(tabName="anova",
                                
                                fluidRow(shinydashboard::box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("modal7button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialanova","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                             uiOutput("anovadep"),
                                                             uiOutput("anovasel")),
                                         shinydashboard::box(title = "Select numeric variable", status = "info", solidHeader = TRUE,
                                                             uiOutput("anovaindep"),
                                         )),
                                fluidRow(shinydashboard::box(actionButton("anovabutton","Calculate ANOVA",icon = icon("flash", lib = "glyphicon")))),
                                fluidRow(shinydashboard::box(title = "Tukey test results", status = "primary", solidHeader = TRUE,width="12",verbatimTextOutput("anovaresults"))),
                                fluidRow(shinydashboard::box(title = "Boxplot", status = "warning", solidHeader = TRUE,width="12",plotlyOutput("anovaplot")))
                                
                                
                        )
                        ,
                        tabItem(tabName = "heatmap",
                                
                                fluidRow(
                                  
                                  shinydashboard::box(title = "Select numeric variables", status = "info", solidHeader = TRUE,actionButton("modal8button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialheatmap","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                      uiOutput("heatmapdep"),
                                                      uiOutput("heatmapsel")),
                                  shinydashboard::box(title = "Select group variable", status = "info", solidHeader = TRUE,
                                                      uiOutput("heatmapindep")
                                  )),
                                fluidRow(shinydashboard::box(actionButton("heatmapbutton","Generate heatmap",icon = icon("flash", lib = "glyphicon"))
                                )),
                                fluidRow(shinydashboard::box(title = "Heatmap plot", status = "warning", solidHeader = TRUE,width=12, shinycssloaders::withSpinner(plotlyOutput("heatmap")))
                                         
                                )
                                
                        ),
                        tabItem(tabName = "tree",
                                
                                fluidRow(
                                  
                                  shinydashboard::box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("modal9button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialtree","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                      uiOutput("var6tree"),
                                                      uiOutput("choice6")),
                                  shinydashboard::box(title = "Select variables", status = "info", solidHeader = TRUE,
                                                      uiOutput("var7"))),
                                fluidRow(shinydashboard::box(actionButton("treebutton","Generate decision tree",icon = icon("flash", lib = "glyphicon"))
                                )),
                                fluidRow(shinydashboard::box(title = "Decision Tree plot (Rpart library)", status = "warning", solidHeader = TRUE,width=12, shinycssloaders::withSpinner(plotOutput("treeplot")))),
                                fluidRow(shinydashboard::box(title = "Decision Tree plot (Party library)", status = "warning", solidHeader = TRUE,width=12, shinycssloaders::withSpinner(plotOutput("treeplotparty")))),
                                fluidRow(shinydashboard::box(title = "Decision Tree table (Party library)", status = "warning", solidHeader = TRUE,width=12, verbatimTextOutput("treeparty")))
                                
                                
                        )
                        
                        ,
                        
                        tabItem(tabName = "PCA",
                                
                                fluidRow(
                                  
                                  shinydashboard::box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("modal10button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialpca","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                      uiOutput("var6pca"),
                                                      uiOutput("choicepca")),
                                  shinydashboard::box(title = "Select numeric variables", status = "info", solidHeader = TRUE,
                                                      uiOutput("var7pca"))),
                                fluidRow(shinydashboard::box(width=3,actionButton("pcabutton","Generate PCA",icon = icon("flash", lib = "glyphicon"))
                                )),
                                fluidRow(shinydashboard::box(title = "Contributions", status = "primary", solidHeader = TRUE,shinycssloaders::withSpinner(verbatimTextOutput("pcarcontrib"))),
                                         shinydashboard::box(title = "Coordinates", status = "primary", solidHeader = TRUE,shinycssloaders::withSpinner(verbatimTextOutput("pcarcoord")))),
                                fluidRow(shinydashboard::box(title = "Dimensions and variance plot", status = "warning", solidHeader = TRUE,width=6, shinycssloaders::withSpinner(plotOutput("pcaploteigen"))),
                                         shinydashboard::box(title = "Individuals plot", status = "warning", solidHeader = TRUE,width=6, shinycssloaders::withSpinner(plotOutput("pcaplotind")))
                                         
                                ),
                                fluidRow(shinydashboard::box(title = "Variables pca plot", status = "warning", solidHeader = TRUE,width=6,actionButton("pcavarbutton","Generate PCA var chart",icon = icon("flash", lib = "glyphicon")),
                                                             plotOutput("pcaplotvar")),
                                         shinydashboard::box(title = "Cluster from pca plot", status = "warning", solidHeader = TRUE,width=6, actionButton("pcaclusterbutton","Generate PCA cluster chart",icon = icon("flash", lib = "glyphicon")),
                                                             shinycssloaders::withSpinner(plotOutput("pcaplotcluster")),
                                                             tableOutput("pcaclustable"),
                                                             dataTableOutput("pcaclustable2")))
                                
                        ),
                        tabItem(tabName = "MFA",
                                
                                fluidRow(
                                  
                                  shinydashboard::box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("modal11button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialmfa","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                      uiOutput("var6mfa"),
                                                      uiOutput("choicemfa")),
                                  shinydashboard::box(title = "Select numeric variables", status = "info", solidHeader = TRUE,
                                                      uiOutput("var7mfa"))),
                                fluidRow(shinydashboard::box(width=3,actionButton("mfabutton","Generate mfa",icon = icon("flash", lib = "glyphicon"))
                                )),
                                fluidRow(shinydashboard::box(title = "Contributions", status = "primary", solidHeader = TRUE,shinycssloaders::withSpinner(verbatimTextOutput("mfarcontrib"))),
                                         shinydashboard::box(title = "Coordinates", status = "primary", solidHeader = TRUE,shinycssloaders::withSpinner(verbatimTextOutput("mfarcoord")))),
                                fluidRow(shinydashboard::box(title = "Dimensions and variance plot", status = "warning", solidHeader = TRUE,width=6, shinycssloaders::withSpinner(plotOutput("mfaploteigen"))),
                                         shinydashboard::box(title = "Individuals plot", status = "warning", solidHeader = TRUE,width=6, shinycssloaders::withSpinner(plotOutput("mfaplotind")))
                                         
                                ),
                                fluidRow(shinydashboard::box(title = "Variables mfa plot", status = "warning", solidHeader = TRUE,width=6,actionButton("mfavarbutton","Generate mfa var chart",icon = icon("flash", lib = "glyphicon")),
                                                             plotOutput("mfaplotvar")),
                                         shinydashboard::box(title = "Cluster from mfa plot", status = "warning", solidHeader = TRUE,width=6, actionButton("mfaclusterbutton","Generate mfa cluster chart",icon = icon("flash", lib = "glyphicon")),
                                                             shinycssloaders::withSpinner(plotOutput("mfaplotcluster")),
                                                             tableOutput("mfaclustable"),
                                                             dataTableOutput("mfaclustable2")))
                                
                        ),
                        tabItem(tabName = "logistic",
                                
                                fluidRow(
                                  
                                  shinydashboard::box(title = "Select group variable", status = "info", solidHeader = TRUE,actionButton("logbuttonhelp","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutoriallogistic","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                      uiOutput("var6log"),
                                                      uiOutput("choicelog")),
                                  shinydashboard::box(title = "Select numeric variables", status = "info", solidHeader = TRUE,
                                                      uiOutput("var7log"),
                                                      actionButton("logbutton","Generate logistic regression",icon = icon("flash", lib = "glyphicon"))
                                  )),
                                
                                fluidRow(shinydashboard::box(width=8,status = "warning", solidHeader = TRUE,title="Logistic regression summary",verbatimTextOutput("logisticresults")),
                                         shinydashboard::box(width=4,status = "warning", solidHeader = TRUE,title="Logistic regression accuracy",verbatimTextOutput("logisticpredicted"),verbatimTextOutput("logisticaccuracy")))
                                
                        )
                        ,
                        tabItem(tabName = "pitchraw",fluidRow(
                          column(width=3,
                                 shinydashboard::box(width=NULL,title="Select parameters",status = "info", solidHeader = TRUE,actionButton("modal12button","help",icon = icon("question-sign", lib = "glyphicon")),actionButton("tutorialpitchraw","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                     uiOutput("filenamerawselect"),
                                                     uiOutput("filenamerawchoices"),
                                                     uiOutput("rawfilter"))
                          ),
                          column(width=9,
                                 shinydashboard::box(width=NULL,title="pitch or intensity plot",status = "warning", solidHeader = TRUE,
                                                     actionButton("listenbutton","Escuchar"),
                                                     uiOutput("rawaudio"),
                                                     shinycssloaders::withSpinner(plotlyOutput("rawplot"))),
                                 
                                 shinydashboard::box(width=NULL,title="Datatable",status = "primary", solidHeader = TRUE, 
                                                     uiOutput("dtrawsel"),
                                                     dataTableOutput("rawtable")
                                                     
                                 )
                          )
                        )),
                        tabItem(tabName = "linearprosody",
                                fluidRow(
                                  column(width=3,
                                         shinydashboard::box(width=NULL,title="Select variables",status = "info", solidHeader = TRUE,
                                                             actionButton("tutoriallinear","Video tutorial",icon = icon("film", lib = "glyphicon")),actionButton("prosviewbutton","Generate chart"),
                                                             uiOutput("prosviewfile1"),
                                                             uiOutput("chosprosview"),
                                                             uiOutput("prosview2"),
                                                             uiOutput("prosview3")
                                         )),
                                  
                                  column(width=9,shinydashboard::box(width=NULL,title="Prosodic linear chart",status = "warning", solidHeader = TRUE,
                                                                     shinycssloaders::withSpinner(plotlyOutput("plotprosview"))),
                                         shinydashboard::box(width=NULL,title="Data table",status = "primary", solidHeader = TRUE,
                                                             uiOutput("dtlinearsel"),
                                                             shinycssloaders::withSpinner(dataTableOutput("dtlinear"))
                                                             
                                                             
                                         )))),
                        
                        tabItem(tabName = "pitchcontour",
                                fluidRow(
                                  
                                  column(width=3,shinydashboard::box(width=NULL,title="Filter",status = "info", solidHeader = TRUE,collapsible = TRUE,collapsed=FALSE,
                                                                     actionButton("tutorialpitchcontour","Video tutorial",icon = icon("film", lib = "glyphicon")),
                                                                     actionButton(inputId =  "contplot", label =  "Show chart"),
                                                                     uiOutput("cont1"),
                                                                     uiOutput("filterscontour"),
                                                                     uiOutput("cont3"),
                                                                     uiOutput("cont4"),
                                                                     uiOutput("cont5"),
                                                                     uiOutput("cont6"),
                                                                     uiOutput("cont7"),
                                                                     uiOutput("cont8"),
                                                                     uiOutput("cont9")
                                                                     
                                  )),
                                  column(width=9,shinydashboard::box(width=NULL,title="Melody plot",status = "warning", solidHeader = TRUE,shinycssloaders::withSpinner(plotlyOutput("melodyplot"))),
                                         shinydashboard::box(width=NULL,title="Table",status = "warning", solidHeader = TRUE,
                                                             uiOutput("cont10"),
                                                             actionButton(inputId =  "contDT", label =  "Show table"),
                                                             dataTableOutput("dataframemel")))
                                )
                        ),
                        tabItem(tabName = "credits",
                                
                                fluidRow(shinydashboard::box(width=12,title="Libraries",status = "info", solidHeader = TRUE,uiOutput("creditsui")))),
                        
                        tabItem(tabName="inspirational",fluidRow(shinydashboard::box(width=12,title="Inspirational",status = "info", solidHeader = TRUE, uiOutput("inspirationalui"))))
                      )
                    )
                    # footer = dashboardFooter(
                    #   left = "By Adrián Cabedo Nebot",
                    #   right = "Valencia, 2021-"
                    # )
)



server <- function(input, output) {
  
  
  
  # Set limit upload file size ----  
  
  options(shiny.maxRequestSize=30*1024^2) 
  
  
  # UI text parts (texts to be included on APP) ----------------------------------
  
  output$instructionsui <- renderUI({
    
    div(
      
      p("You can upload here RDS files (R format) with data frames for vowels, prosody and units.")
      
    )
    
  })
  

  output$creditsui <- renderText({
    
    includeHTML("html/grateful-citations.html")
    
    
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
  # Reset filter panel ------------------------------------------------------
  
  # observeEvent(input$resetbutton, {
  #     shinyjs::reset("filter1")
  # })
  
  # Create data frame -------------------------------------------------------
  
  ## Data frame from ELAN text file ------
  
  # Expected data frame: tab file with 7 variables without column names.
  # Although there is no column names, the variables should refer to:
  # X1 = tier, X2 = spk/participant, X3 = tmin, X4 = tmax, X5 = dur, X6 = annotation, X7 = filename
  # IMPORTANT NOTE: tiers can refer to basic intonational phrases or include also words and phonemes.
  
  tabbeddf <- reactive({
    
    if(input$radio1bttn == "TABTXT" & input$radio2bttn == "no"){
    
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
    orst} else if (input$radio1bttn == "TABTXT" & input$radio2bttn == "yes") {
      
      orst <- read_delim(input$tabbedtext$datapath,"\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
      
    } else if (input$radio1bttn == "TextGrid") {    
      s <- input$tabbedtext 
      s <- s%>% mutate(source= gsub("(.*)/","",datapath))
      p <- map2_df(input$tabbedtext$name, input$tabbedtext$datapath, 
                                   ~textgrid_to_df(.y))
      p <- p%>%left_join(s, by="source")%>%mutate(name = gsub(".textgrid","",name),name = gsub(".TextGrid","",name))
      p <- p%>%mutate(tmin_ms = as.integer(time_start*1000),tmax_ms=as.integer(time_end*1000),annotation=content,tier=tier_name, filename=name,spk=tier_name,dur= tmax_ms-tmin_ms,corpus="corpus",spk=gsub("_.*","",spk))%>%select(tier,spk,tmin_ms,tmax_ms,dur,annotation,filename,corpus)%>%filter(annotation!="")
      setDF(p)
      
      
      }
    else if (input$radio1bttn == "EAF") {
      
      s <- input$tabbedtext 
      s <- s%>% mutate(source= gsub("(.*)/","",datapath))
      p <- map2_df(input$tabbedtext$name, input$tabbedtext$datapath,                                           ~eaf_to_df(.y))
      p <- p%>%left_join(s, by="source")%>%mutate(name = gsub(".eaf","",name))
      p <- p%>%mutate(tmin_ms = as.integer(time_start*1000),tmax_ms=as.integer(time_end*1000),dur= tmax_ms-tmin_ms,annotation=content,spk=tier_name, filename=name,corpus="corpus",spk=gsub("_.*","",spk),tier=tier_name)%>%select(tier,spk,tmin_ms,tmax_ms,dur,annotation,filename,corpus)%>%filter(annotation!="")
      setDF(p)
      
      }
  })
  
  ## Metadata data frame (optional) ------ 
  
  metadatafile <- reactive({ 
    
    metadata <- read_delim(input$metadata$datapath,"\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
    
    
  })
  
  ## Prosody data frame from pitch and intensity files -----
  
  pitchdf <- reactive({
    
    
    p <- map2_df(input$pitchfile$name, input$pitchfile$datapath, 
                 ~fread(.y)%>% mutate(filename = .x))
    p <- p%>% rownames_to_column(.,"id")%>%mutate(time = round(as.numeric(time),2), pitchst = round((12*log2(pitch/1)),2),pitch=round(pitch,2))%>%distinct(filename,time,.keep_all = TRUE)%>%mutate(time_ms = as.integer(time*1000), filename=gsub(".txt","",filename))
    
  })
  
  #  Expected structure of intensity text file. Columns must be named: first one (time), second one (intensity)
  
  intensitydf <- reactive({
    
    p <- map2_df(input$intensityfile$name, input$intensityfile$datapath, 
                 ~fread(.y)%>% mutate(filename = .x))
    p <- p%>%mutate(time = round(as.numeric(time),2), intensity=round(intensity,2))%>%distinct(filename,time,.keep_all = TRUE)%>%mutate(time_ms = as.integer(time*1000), filename=gsub(".txt","",filename))
    
    
  })
  
  pitchintensitydf <- reactive({
    if(input$selectdata=="fonocortesiadb"){fonocortesiaprosody}
    else if (input$selectdata=="picodeorodb")
    {picodeoroprosody}
    else if 
    (input$selectdata=="I will upload my data at Upload Section"){
      prosody <- readRDS(input$uploadRDSprosody$datapath)}
    else{
      p <- pitchdf()%>%dplyr::left_join(intensitydf()%>%select(filename,time,intensity),by=c("filename","time"))}
    
  })  
  
  vowelssel <- reactive({
    if(input$selectdata=="fonocortesiadb"){vowels}
    else if (input$selectdata=="picodeorodb")
    {picodeorodbvowels}
    else if (input$selectdata=="I will upload my data at Upload Section")
    {vowelsupload <- readRDS(input$uploadRDSvowels$datapath)}
    else{
      phonemesdf()}
    
  })  
  
  ## Words data frame --------------------------------------------------------
  
  wordsdf <- eventReactive(input$createwords,{
    
    if(is.null(input$pitchfile))
      
    { words <- subset(tabbeddf(), grepl(input$wordsname,tier))
    words <- words%>%arrange(filename,tmin_ms)%>%group_by(filename)%>%mutate(w_id= paste("word_",filename,"_",spk,"_",tmin_ms,sep=""))
    words <- words%>%arrange(filename,tmin_ms)}
    
    else {
    
    if(is.null(input$posfile)){
      
      if(!is.null(input$sentfile)&!is.null(input$udpipefile)){
        
        words <- subset(tabbeddf(), grepl(input$wordsname,tier))
        words <- words%>%arrange(filename,tmin_ms)%>%group_by(filename)%>%mutate(w_id= paste("word_",filename,"_",spk,"_",tmin_ms,sep=""))
        words <- words%>%arrange(filename,tmin_ms)
        orstip <- dfprosody()
        prosodydb <- pitchintensitydf()
        # 
        setDT(words) # make a data.table
        setDT(prosodydb) # make a data.table
        prosodydb<-prosodydb[, dummy := time_ms]
        prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
        setkey(words, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
        setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
        prueba <- foverlaps(words, prosodydb, nomatch=NA)[, dummy := NULL]
        
        
        
        suppressWarnings(orstwordsrn <-prueba%>% 
                           group_by(w_id) %>% summarise(
                             tier = max(tier),
                             spk = max(spk),
                             tmin = max(tmin_ms),
                             tmax = max(tmax_ms),
                             tmin_ms = max(tmin_ms),
                             tmax_ms = max(tmax_ms),
                             tmin_word = max(tmin_ms),
                             tmax_word = max(tmax_ms),
                             dur = max(dur),
                             corpus = max(corpus),
                             word_annotation = max(annotation),
                             filename = max(filename),
                             PirHz = round(max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),2),
                             PirSt = round(12*log2(max(pitch,na.rm=TRUE)/min(pitch, na.rm = TRUE)),2),
                             Pimd = round(median(pitch, na.rm = TRUE),2),
                             PimnHz = round(mean(pitch, na.rm = TRUE),2),
                             PimnSt = round(12*log2(mean(pitch,na.rm = TRUE)/1),2),
                             Imd = round(median(intensity, na.rm = TRUE),2),
                             Imn = round(mean(intensity, na.rm = TRUE),2),
                             corpus = "corpus"
                           ) %>%ungroup()%>% arrange(filename,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup())
        # col_means <- lapply(orstalofrn%>%select(-transition_post, -transition_prev), mean, na.rm = TRUE)
        # col_means <- lapply(col_means, round,2)
        # orstalofrn <- replace_na(orstalofrn, col_means)
        orstwordsrn <-
          orstwordsrn %>% mutate(
            RdprSt = PirSt - lag(PirSt),
            Idpr = round(Imn - lag(Imn, 1),2),
            spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
            spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
            ppr = ifelse(spkpr =="mismo",trpr,NA),
            ppst = ifelse(spkpst =="mismo",trpst,NA)
          )
        orstwordsrn <-
          orstwordsrn %>% group_by(filename, spk) %>% mutate(
            # Rng_mean_sp = round(mean(Pitch_range_St,na.rm = TRUE), 2),
            Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
            Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
            # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
            # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
            PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
            PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
            # Pdspkp = round(Pit_diff_sp_Hz / mean(Pitch_mean_Hz,na.rm = TRUE), 2) * 100
            # Pitch_mean_sp = round(mean(Pitch_mean_Hz,na.rm = TRUE), 2)
          ) %>% ungroup()
        
        orstip <- orstip%>%mutate(tmin_ip = tmin_ms, tmax_ip = tmax_ms)
        setDT(orstwordsrn)
        setDT(orstip)
        orstwordsrn <- orstwordsrn[orstip, on = .(tmin_word>=tmin_ip,tmax_word<=tmax_ip,spk==spk,filename==filename)]
        setDF(orstwordsrn)
        setDF(orstip)
        
        # orstwordsrn<- sqldf(
        #   "select x.*,y.ip_id,y.ip_annotation from orstwordsrn x left join orstip as y on (x.tmin >= (y.tmin)-50) and (x.tmax <= (y.tmax)+50) and (x.filename = y.filename)and (x.spk = y.spk)")
        
        udmodel_spanish <- udpipe_load_model(input$udpipefile$datapath)
        
        orstwords_tag <- orstwordsrn
        orstwords_tag$notag <- orstwords_tag$word_annotation
        orstwords_tag <-
          udpipe_annotate(
            udmodel_spanish,
            x = orstwords_tag$notag,
            tokenizer = "vertical",
            tagger = "default",
            parser = "none",
            doc_id = orstwords_tag$w_id
          )
        orstwords_tag <- as.data.frame(orstwords_tag) %>% select(doc_id,lemma,upos)
        orstwords_tag$w_id <- orstwords_tag$doc_id
        
        # Merging ending words data frame
        
        orstwordsmer <- orstwordsrn%>%left_join(orstwords_tag,by="w_id")
        orstwordsmer <- distinct(orstwordsmer%>%ungroup(),w_id,.keep_all = TRUE)
        
        sentiment <- read_csv(input$sentfile$datapath)
        orstwordsmer <- orstwordsmer %>% left_join(sentiment, by="word_annotation") 
        orstwordsmer <- distinct(orstwordsmer%>%ungroup(),w_id, .keep_all = TRUE)
        orstwordsmer <- orstwordsmer[,!grepl("i\\.", colnames(orstwordsmer))]
        
      }else if (!is.null(input$sentfile)&is.null(input$udpipefile)){
        
        words <- subset(tabbeddf(), grepl(input$wordsname,tier))
        words <- words%>%group_by(filename)%>%mutate(w_id= paste("word_",filename,"_",spk,"_",tmin_ms,sep=""))
        words <- words%>%arrange(filename,tmin_ms)
        orstip <- dfprosody()
        prosodydb <- pitchintensitydf()
        setDT(words) # make a data.table
        setDT(prosodydb) # make a data.table
        prosodydb<-prosodydb[, dummy := time_ms] 
        prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
        setkey(words, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
        setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
        prueba <- foverlaps(words, prosodydb, nomatch=NA)[, dummy := NULL]
        
        #       prueba<- sqldf("select x.*,y.pitch, y.intensity, y.time_ms from words x
        # left join prosodydb as y on (y.time_ms between x.tmin_ms and x.tmax_ms)
        # and (x.filename = y.filename)")
        
        suppressWarnings(orstwordsrn <-prueba%>% 
                           group_by(w_id) %>% summarise(
                             tier = max(tier),
                             spk = max(spk),
                             tmin = max(tmin_ms),
                             tmax = max(tmax_ms),
                             tmin_ms = max(tmin_ms),
                             tmax_ms = max(tmax_ms),
                             tmin_word = max(tmin_ms),
                             tmax_word = max(tmax_ms),
                             dur = max(dur),
                             corpus = max(corpus),
                             word_annotation = max(annotation),
                             filename = max(filename),
                             PirHz = round(max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),2),
                             PirSt = round(12*log2(max(pitch,na.rm=TRUE)/min(pitch, na.rm = TRUE)),2),
                             Pimd = round(median(pitch, na.rm = TRUE),2),
                             PimnHz = round(mean(pitch, na.rm = TRUE),2),
                             PimnSt = round(12*log2(mean(pitch,na.rm = TRUE)/1),2),
                             Imd = round(median(intensity, na.rm = TRUE),2),
                             Imn = round(mean(intensity, na.rm = TRUE),2),
                             corpus = "corpus"
                           ) %>%ungroup()%>% arrange(filename,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup())
        # col_means <- lapply(orstalofrn%>%select(-transition_post, -transition_prev), mean, na.rm = TRUE)
        # col_means <- lapply(col_means, round,2)
        # orstalofrn <- replace_na(orstalofrn, col_means)
        orstwordsrn <-
          orstwordsrn %>% mutate(
            RdprSt = PirSt - lag(PirSt),
            Idpr = round(Imn - lag(Imn, 1),2),
            spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
            spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
            ppr = ifelse(spkpr =="mismo",trpr,NA),
            ppst = ifelse(spkpst =="mismo",trpst,NA)
          )
        orstwordsrn <-
          orstwordsrn %>% group_by(filename, spk) %>% mutate(
            # Rng_mean_sp = round(mean(Pitch_range_St,na.rm = TRUE), 2),
            Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
            Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
            # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
            # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
            PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
            PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
            # Pdspkp = round(Pit_diff_sp_Hz / mean(Pitch_mean_Hz,na.rm = TRUE), 2) * 100
            # Pitch_mean_sp = round(mean(Pitch_mean_Hz,na.rm = TRUE), 2)
          ) %>% ungroup()
        orstip <- orstip%>%mutate(tmin_ip = tmin_ms, tmax_ip = tmax_ms)
        setDT(orstwordsrn)
        setDT(orstip)
        orstwordsrn <- orstwordsrn[orstip, on = .(tmin_word>=tmin_ip,tmax_word<=tmax_ip,spk==spk,filename==filename)]
        setDF(orstwordsrn)
        setDF(orstip)
        
        # orstwordsrn<- sqldf(
        #   "select x.*,y.ip_id,y.ip_annotation from orstwordsrn x left join orstip as y on (x.tmin >= (y.tmin)-50) and (x.tmax <= (y.tmax+50)) and (x.filename = y.filename)and (x.spk = y.spk)")
        
        sentiment <- read_csv(input$sentfile$datapath)
        orstwordsrn <- orstwordsrn %>% left_join(sentiment, by="word_annotation") 
        orstwordsrn <- distinct(orstwordsrn%>%ungroup(),w_id, .keep_all = TRUE)
        orstwordsrn <- orstwordsrn[,!grepl("i\\.", colnames(orstwordsrn))]
      }else if (is.null(input$sentfile)&!is.null(input$udpipefile)){
        
        words <- subset(tabbeddf(), grepl(input$wordsname,tier))
        words <- words%>%group_by(filename)%>%mutate(w_id= paste("word_",filename,"_",spk,"_",tmin_ms,"_",sep=""))
        words <- words%>%arrange(filename,tmin_ms)
        orstip <- dfprosody()
        prosodydb <- pitchintensitydf()
        
        setDT(words) # make a data.table
        setDT(prosodydb) # make a data.table
        prosodydb<-prosodydb[, dummy := time_ms] 
        prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
        setkey(words, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
        setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
        prueba <- foverlaps(words, prosodydb, nomatch=NA)[, dummy := NULL]
        
        #       prueba<- sqldf("select x.*,y.pitch, y.intensity, y.time_ms from words x
        # left join prosodydb as y on (y.time_ms between x.tmin_ms and x.tmax_ms)
        # and (x.filename = y.filename)")
        
        suppressWarnings(orstwordsrn <-prueba%>% 
                           group_by(w_id) %>% summarise(
                             tier = max(tier),
                             spk = max(spk),
                             tmin = max(tmin_ms),
                             tmax = max(tmax_ms),
                             tmin_ms = max(tmin_ms),
                             tmax_ms = max(tmax_ms),
                             tmin_word = max(tmin_ms),
                             tmax_word = max(tmax_ms),
                             dur = max(dur),
                             corpus = max(corpus),
                             word_annotation = max(annotation),
                             filename = max(filename),
                             PirHz = round(max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),2),
                             PirSt = round(12*log2(max(pitch,na.rm=TRUE)/min(pitch, na.rm = TRUE)),2),
                             Pimd = round(median(pitch, na.rm = TRUE),2),
                             PimnHz = round(mean(pitch, na.rm = TRUE),2),
                             PimnSt = round(12*log2(mean(pitch,na.rm = TRUE)/1),2),
                             Imd = round(median(intensity, na.rm = TRUE),2),
                             Imn = round(mean(intensity, na.rm = TRUE),2),
                             corpus = "corpus"
                           ) %>%ungroup()%>% arrange(filename,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup())
        
        orstwordsrn <-
          orstwordsrn %>% mutate(
            RdprSt = PirSt - lag(PirSt),
            Idpr = round(Imn - lag(Imn, 1),2),
            spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
            spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
            ppr = ifelse(spkpr =="mismo",trpr,NA),
            ppst = ifelse(spkpst =="mismo",trpst,NA)
          )
        orstwordsrn <-
          orstwordsrn %>% group_by(filename, spk) %>% mutate(
            # Rng_mean_sp = round(mean(Pitch_range_St,na.rm = TRUE), 2),
            Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
            Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
            # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
            # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
            PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
            PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
            # Pdspkp = round(Pit_diff_sp_Hz / mean(Pitch_mean_Hz,na.rm = TRUE), 2) * 100
            # Pitch_mean_sp = round(mean(Pitch_mean_Hz,na.rm = TRUE), 2)
          ) %>% ungroup()
        
        setDT(orstwordsrn)
        setDT(orstip)
        
        orstwordsrn <- orstwordsrn%>%mutate(tmin_word = tmin_ms, tmax_word =tmax_ms,w_id=paste("word_",filename,"_",spk,"_",tmin_ms,sep=""))
        orstip <- orstip%>%mutate(tmin_ip = tmin_ms, tmax_ip =tmax_ms,ip_id=paste("ip_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
        
        orstwordsrn <- orstwordsrn[orstip, on = .(tmin_word>=tmin_ip,tmax_word<=tmax_ip,spk==spk,filename==filename)]
        setDF(orstwordsrn)
        setDF(orstip)
        
        # orstwordsrn<- sqldf(
        #   "select x.*,y.ip_id,y.ip_annotation from orstwordsrn x left join orstip as y on (x.tmin >= (y.tmin)-50) and (x.tmax <= (y.tmax)+50) and (x.filename = y.filename)and (x.spk = y.spk)")
        
        udmodel_spanish <- udpipe_load_model(input$udpipefile$datapath)
        
        orstwords_tag <- orstwordsrn
        orstwords_tag$notag <- orstwords_tag$word_annotation
        orstwords_tag <-
          udpipe_annotate(
            udmodel_spanish,
            x = orstwords_tag$notag,
            tokenizer = "vertical",
            tagger = "default",
            parser = "none",
            doc_id = orstwords_tag$w_id
          )
        orstwords_tag <- as.data.frame(orstwords_tag) %>% select(doc_id,lemma,upos)
        orstwords_tag$w_id <- orstwords_tag$doc_id
        
        # Merging ending words data frame
        
        orstwordsmer <- orstwordsrn%>%left_join(orstwords_tag,by="w_id")
        orstwordsmer <- distinct(orstwordsmer%>%ungroup(),w_id,.keep_all = TRUE)
        orstwordsmer <- orstwordsmer[,!grepl("i\\.", colnames(orstwordsmer))]
        
      }else{
        
        words <- subset(tabbeddf(), grepl(input$wordsname,tier))
        words <- words%>%group_by(filename)%>%mutate(w_id= paste("word_",filename,"_",spk,"_",tmin_ms,sep=""))
        words <- words%>%arrange(filename,tmin_ms)
        orstip <- dfprosody()
        prosodydb <- pitchintensitydf()
        
        setDT(words) # make a data.table
        setDT(prosodydb) # make a data.table
        prosodydb<-prosodydb[, dummy := time_ms] 
        prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
        setkey(words, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
        setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
        prueba <- foverlaps(words, prosodydb, nomatch=NA)[, dummy := NULL]
        
        
        suppressWarnings(orstwordsrn <-prueba%>% 
                           group_by(w_id) %>% summarise(
                             tier = max(tier),
                             spk = max(spk),
                             tmin = max(tmin_ms),
                          
                             tmax = max(tmax_ms),
                             tmin_ms = max(tmin_ms),
                             tmax_ms = max(tmax_ms),
                             tmin_word = max(tmin_ms),
                             tmax_word = max(tmax_ms),
                             dur = max(dur),
                             corpus = max(corpus),
                             word_annotation = max(annotation),
                             filename = max(filename),
                             PirHz = round(max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),2),
                             PirSt = round(12*log2(max(pitch,na.rm=TRUE)/min(pitch, na.rm = TRUE)),2),
                             Pimd = round(median(pitch, na.rm = TRUE),2),
                             PimnHz = round(mean(pitch, na.rm = TRUE),2),
                             PimnSt = round(12*log2(mean(pitch,na.rm = TRUE)/1),2),
                             Imd = round(median(intensity, na.rm = TRUE),2),
                             Imn = round(mean(intensity, na.rm = TRUE),2),
                             corpus = "corpus"
                           ) %>%ungroup()%>% arrange(filename,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup())
        
        orstwordsrn <-
          orstwordsrn %>% mutate(
            RdprSt = PirSt - lag(PirSt),
            Idpr = round(Imn - lag(Imn, 1),2),
            spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
            spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
            ppr = ifelse(spkpr =="mismo",trpr,NA),
            ppst = ifelse(spkpst =="mismo",trpst,NA)
          )
        orstwordsrn <-
          orstwordsrn %>% group_by(filename, spk) %>% mutate(
            # Rng_mean_sp = round(mean(Pitch_range_St,na.rm = TRUE), 2),
            Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
            Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
            # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
            # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
            PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
            PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
            # Pdspkp = round(Pit_diff_sp_Hz / mean(Pitch_mean_Hz,na.rm = TRUE), 2) * 100
            # Pitch_mean_sp = round(mean(Pitch_mean_Hz,na.rm = TRUE), 2)
          ) %>% ungroup()
        
        orstip <- orstip%>%mutate(tmin_ip = tmin_ms, tmax_ip = tmax_ms)
        setDT(orstwordsrn)
        setDT(orstip)
        orstwordsrn <- orstwordsrn[orstip, on = .(tmin_word>=tmin_ip,tmax_word<=tmax_ip,spk==spk,filename==filename)]
        setDF(orstwordsrn)
        setDF(orstip)
        
        orstwordsrn <- orstwordsrn%>%arrange(ip_id,tmin)%>%mutate(words_ag = ifelse(lead(ip_id)!=ip_id,paste(word_annotation,"/",sep=""),word_annotation))%>%ungroup()
        orstwordsrn <- orstwordsrn[,!grepl("i\\.", colnames(orstwordsrn))]
        
      }
      
    }
    else
    {words <- read.xlsx2(input$posfile$datapath)}}
    
  })
  
## Phonemes data frame -----------------------------------------------------
  
  phonemesdf <- eventReactive(input$createphonemes,{
    
    if(!is.null(input$pitchfile)){
    
    
    orstwords <- subset(tabbeddf(), grepl(input$wordsname,tier))
    orstwords <- orstwords%>%arrange(filename,tmin_ms)%>%group_by(filename)%>%mutate(w_id= paste("word_",filename,"_",spk,"_",tmin_ms,sep=""),word=annotation,word_dur=dur,tmin_word = tmin_ms-15, tmax_word =tmax_ms+15)
    orstwords <- orstwords%>%arrange(filename,tmin_ms)
    
    orstip <- subset(tabbeddf(), grepl(input$ipsname,tier))
    orstip <- orstip%>%arrange(filename,tmin_ms)%>%group_by(filename)%>%mutate(ip_id= paste("ip_",filename,"_",spk,"_",tmin_ms,"_", dplyr::row_number(),sep=""),ip=annotation,ip_dur=dur,tmin_ip = tmin_ms-15, tmax_ip =tmax_ms+15)
    orstip <- orstip%>%arrange(filename,tmin_ms)
    
    orstalof <- subset(tabbeddf(), grepl(input$phonemesname,tier))
    orstalof <- orstalof%>%group_by(filename)%>%mutate(ph_id= paste("alof_",filename,"_",dplyr::row_number(),sep=""))
    
    orstalof <- orstalof%>%mutate(tmin_phon = tmin_ms, tmax_phon =tmax_ms,phon_id=paste("phon",row_number(),sep = "_"))
    
    setDT(orstwords)
    setDT(orstalof)
    
    join_phon_words <- orstalof[orstwords, on = .(tmin_phon>=tmin_word,tmax_phon<=tmax_word,spk==spk,filename==filename)]
    
    combined <- join_phon_words
    setDF(combined)
    combined<- combined%>%group_by(w_id)%>%mutate(order=row_number())%>%ungroup()
    setDF(combined)
    if(!is.null(input$ipafile))
    {
      lexicon <- read_delim(input$ipafile$datapath, delim = "\t", escape_double = FALSE, col_names = TRUE, trim_ws = TRUE)
    setDF(lexicon)
    combined <- combined %>%ungroup()%>%left_join(lexicon,by=c("word","order"))
    orstalof <- combined %>%mutate(annotation2=annotation, annotation=phonemes2, annotation=ifelse(is.na(annotation),annotation2,annotation))}
    else {orstalof <- combined}
    
    
    # orstwords <- wordsdf()
    # orstip <- dfprosody()
    
    prosodydb <- pitchintensitydf()
    
    setDT(orstalof) # make a data.table
    setDT(prosodydb) # make a data.table
    
    prosodydb<-prosodydb[, dummy := time_ms]
    prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
    setkey(orstalof, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
    setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
    orstalof<- orstalof%>%filter(!is.na(tmin_ms))
    alofwithpros <- foverlaps(orstalof, prosodydb, nomatch=NA)[, dummy := NULL]
    
    setDF(alofwithpros)
    alofwithpros <-
      alofwithpros %>% group_by(filename)%>%
      mutate(phon_type = ifelse(grepl("[aeiou]",annotation),"vowel",
                                ifelse(grepl("[jw]",annotation),"glide","consonant")),
             tonic = ifelse(grepl("\\*",annotation),"yes","no"))
    
    alofwithpros <- subset(alofwithpros, phon_type=="vowel")
    alofwithpros <- alofwithpros%>%mutate(
      quarter=ifelse(time_ms <= (tmin_ms+(0.25*dur)),"q1",NA),
      quarter=ifelse(time_ms >= (tmin_ms+(0.25*dur))&time_ms <= (tmin_ms+(0.5*dur)),"q2",quarter),
      quarter=ifelse(time_ms >= (tmin_ms+(0.5*dur))&time_ms <= (tmin_ms+(0.75*dur)),"q3",quarter),
      quarter=ifelse(time_ms >=(tmin_ms+(0.75*dur))&time_ms <= (tmin_ms+(dur)),"q4",quarter))
    q1alof <- alofwithpros%>%filter(quarter=="q1")%>%group_by(ph_id)%>%summarise(q1piHz=mean(pitch,na.rm=TRUE),q1pist=mean(pitchst,na.rm=TRUE))
    q2alof <- alofwithpros%>%filter(quarter=="q2")%>%group_by(ph_id)%>%summarise(q2piHz=mean(pitch,na.rm=TRUE),q2pist=mean(pitchst,na.rm=TRUE))
    q3alof <- alofwithpros%>%filter(quarter=="q3")%>%group_by(ph_id)%>%summarise(q3piHz=mean(pitch,na.rm=TRUE),q3pist=mean(pitchst,na.rm=TRUE))
    q4alof <- alofwithpros%>%filter(quarter=="q4")%>%group_by(ph_id)%>%summarise(q4piHz=mean(pitch,na.rm=TRUE),q4pist=mean(pitchst,na.rm=TRUE))
    alofwithpros <- alofwithpros%>% left_join(q1alof,by="ph_id")
    alofwithpros <- alofwithpros%>% left_join(q2alof,by="ph_id")
    alofwithpros <- alofwithpros%>% left_join(q3alof,by="ph_id")
    alofwithpros <- alofwithpros%>% left_join(q4alof,by="ph_id")
    
    orstalofrn <-
      suppressWarnings(alofwithpros%>%filter(phon_type=="vowel")%>%
                         group_by(ph_id) %>% summarise(
                           tier = max(tier),
                           tmin = max(tmin_ms),
                           tmax = max(tmax_ms),
                           tmin_ms = max(tmin_ms),
                           tmax_ms = max(tmax_ms),
                           spk = max(spk),
                           dur = max(dur),
                           phon = max(annotation),
                           phon_type = max(phon_type),
                           tonic = max(tonic),
                           filename = max(filename),
                           q1piHz = mean(q1piHz,na.rm=TRUE),
                           q1pist = mean(q1pist,na.rm=TRUE),
                           q2piHz = mean(q2piHz,na.rm=TRUE),
                           q2pist = mean(q2pist,na.rm=TRUE),
                           q3piHz = mean(q3piHz,na.rm=TRUE),
                           q3pist = mean(q3pist,na.rm=TRUE),
                           q4piHz = mean(q4piHz,na.rm=TRUE),
                           q4pist = mean(q4pist,na.rm=TRUE),
                           PirHz = max(pitch,na.rm = TRUE)-min(pitch,na.rm = TRUE),
                           PirSt = max(pitchst,na.rm = TRUE)-min(pitchst,na.rm = TRUE),
                           Pimd = median(pitch, na.rm = TRUE),
                           PimdSt = median(pitchst,na.rm = TRUE),
                           PimnHz = mean(pitch, na.rm = TRUE),
                           PimnSt = mean(pitchst,na.rm = TRUE),
                           Imd = median(intensity, na.rm = TRUE),
                           Imn = mean(intensity, na.rm = TRUE),
                           corpus = "corpus"
                         )) %>%ungroup()%>% arrange(filename,tmin) %>%
      mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>%
      group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1),
                                   trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
    orstalofrn <-
      orstalofrn %>% mutate(
        # Idpr = round(Imn - lag(Imn, 1),2),
        spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
        spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
        ppr = ifelse(spkpr =="mismo",trpr,NA),
        ppst = ifelse(spkpst =="mismo",trpst,NA),
        ftopr = ifelse(spkpr =="otro",trpr,NA),
        ftopst = ifelse(spkpst =="otro",trpst,NA)
        
      )
    
    orstalofrn <-
      orstalofrn %>% group_by(filename, spk) %>% mutate(
        Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
        PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
        PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2),
        PispkSt = mean(PimnSt, na.rm= TRUE)
      ) %>% ungroup()
    
    #Intensity and pitch from previous vowels 
    
    orstalofrn <-
      orstalofrn %>% group_by(filename, phon_type )%>% mutate(
        Pdprvow = PimnSt - lag(PimnSt),
        Idprvow = Imn - lag(Imn, 1),
        Pdprvow = ifelse(is.na(Pdprvow),0,Pdprvow),
        Idprvow = ifelse(is.na(Idprvow),0,Idprvow),
      )%>%ungroup()
    
    orstwords <- orstwords%>%mutate(tmin_word = tmin_ms-15, tmax_word =tmax_ms+15,word_dur = dur,w_id=paste("word_",filename,"_",spk,"_",tmin_ms,sep=""))
    orstip <- orstip%>%mutate(tmin_ip = tmin_ms-15, tmax_ip =tmax_ms+15,ip_dur = dur,ip_id=paste("ip_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
    orstalofrn <- orstalofrn%>%mutate(tmin_phon = tmin_ms, tmax_phon =tmax_ms,phon_id=paste("phon",row_number(),sep = "_"))
    
    setDT(orstip)
    setDT(orstwords)
    setDT(orstalofrn)
    
    join_ip_words <- orstwords[orstip, on = .(tmin_word>=tmin_ip,tmax_word<=tmax_ip,spk==spk,filename==filename)]
    join_ip_words <- join_ip_words %>%mutate(tmin_copp = tmin_ms-15, tmax_copp = tmax_ms+15 )%>%rename(tmin_ip = tmin_word, tmax_ip = tmax_word)
    join_phon_words <- orstalofrn[join_ip_words, on = .(tmin_phon>=tmin_copp,tmax_phon<=tmax_copp,spk==spk,filename==filename)]%>%rename(tmin_word = tmin_phon, tmax_word = tmax_phon)
    combined <- join_phon_words%>% mutate(tmin_phon = tmin_ms, tmax_phon = tmax_ms)%>%arrange(filename,ip_id,tmin)    
    
    # TOBI
    
    orstTOBI <- combined %>% ungroup() %>% filter(tonic=="yes")%>%mutate(
      
      toneme = ifelse(lead(ip_id)!=ip_id,"yes","no"),
      TOBI = ifelse((q4pist - q1pist)>1.5,"L+H*",NA),
      TOBI = ifelse((q4pist - q1pist)< -1.5,"H+L*",TOBI),
      TOBI = ifelse(is.na(TOBI) & ((PispkSt - PimnSt) > 1.5),"H*",TOBI),
      TOBI = ifelse(is.na(TOBI)&((PispkSt - PimnSt) < -1.5),"L*",TOBI),
      TOBI = ifelse(is.na(TOBI),"N",TOBI),
      TOBI = ifelse(is.na(q1pist)&is.na(q2pist)&is.na(q3pist)&is.na(q4pist),NA,TOBI))%>%select(ph_id,toneme, TOBI)
    
    combined <- combined%>%ungroup()%>%arrange(filename,w_id,tmin)%>% mutate(voweltype = ifelse(lead(w_id,1)!=w_id & tonic=="yes","aguda",NA),
                                                                             voweltype = ifelse(lead(w_id,1)==w_id & lead(w_id,2)!=w_id & tonic=="yes","llana",voweltype),
                                                                             voweltype = ifelse(lead(w_id,1)==w_id & lead(w_id,2)==w_id & lead(w_id,3)!=w_id & tonic=="yes","esdrujula",voweltype))
    combined <- combined %>% left_join(orstTOBI,by = "ph_id")
    combined1 <- combined %>% ungroup()%>%arrange(filename, ip_id, tmin)%>%mutate(
      MASpart = ifelse(toneme=="yes","toneme",NA),
      MASpart = ifelse(lag(ip_id)!=ip_id &toneme!="yes","anacrusis",MASpart),
      MASpart = ifelse(is.na(MASpart),"body",MASpart),
      TOBIPRPST = paste("(",TOBI,")", sep=""),
      TOBIPRPST = ifelse(ip_id ==lag(ip_id) &(na.omit(q2pist)-na.omit(lag(q3pist)) > (1.5)),paste("L+",TOBIPRPST,sep=""),TOBIPRPST),
      TOBIPRPST = ifelse(ip_id==lag(ip_id) &(na.omit(q2pist)-na.omit(lag(q3pist))) < (-1.5),paste("H+",TOBIPRPST,sep=""),TOBIPRPST),
      TOBIPRPST = ifelse(ip_id==lead(ip_id) &(na.omit(lead(q2pist))-na.omit(q4pist)) > (1.5),paste(TOBIPRPST,"+H",sep=""),TOBIPRPST),
      TOBIPRPST = ifelse(ip_id==lead(ip_id) &(na.omit(lead(q2pist))-na.omit(q4pist)) < (-1.5),paste(TOBIPRPST,"+L",sep=""),TOBIPRPST),
      TOBIPRPST = ifelse(ip_id==lead(ip_id) &(na.omit(lead(q4pist))-na.omit(lead(q3pist))) > (1.5), paste(TOBIPRPST,"H%",sep=""),TOBIPRPST),
      TOBIPRPST = ifelse(ip_id==lead(ip_id) &(na.omit(lead(q4pist))-na.omit(lead(q3pist))) < (-1.5),paste(TOBIPRPST,"L%",sep=""),TOBIPRPST),
      TOBIPRPST = gsub("NA","",TOBIPRPST),
      TOBIPRPST = gsub("NA+","",TOBIPRPST),
      TOBIPRPST = gsub("+NA","",TOBIPRPST)
    ) %>%group_by(ip_id)%>%mutate(order=row_number())
    
    body <- combined1%>% arrange(filename,ip_id,tmin) %>% filter(tonic=="yes")%>%group_by(ip_id)%>%mutate(firsttonic = row_number())%>%filter(firsttonic==1)%>%ungroup()%>%select(phon_id, firsttonic)
    
    combined2 <- combined1%>%left_join(body, by = "phon_id") %>% arrange(filename,ip_id,tmin)%>% mutate(mean= (q2piHz + q3piHz)/2, mean2 =(lead(q2piHz) + lead(q3piHz))/2,postfirsttonic = ifelse(!is.na(firsttonic), ((mean2-mean)/mean2)*100,NA), meanprev = lag(mean),meanpost =lead(mean))%>%ungroup() %>% arrange(filename,tmin)%>%mutate_if(is.numeric,round,2)
    
    tonemeMAS <- combined2 %>% filter(toneme=="yes")%>%mutate(tonemeMAS = ((mean-meanprev)/meanprev)*100 ,
                                                              tonemeMAS=ifelse(is.na(tonemeMAS),((q4piHz-q1piHz)/q4piHz)*100,tonemeMAS),
                                                              q1q2=((q2piHz-q1piHz)/q2piHz)*100,q1q3=((q3piHz-q1piHz)/q3piHz)*100,q1q4=((q4piHz-q1piHz)/q4piHz)*100,  q2q3=((q3piHz-q2piHz)/q3piHz)*100,q2q4=((q4piHz-q2piHz)/q4piHz)*100,q3q4=((q4piHz-q3piHz)/q4piHz)*100,tonemePOST = ifelse(voweltype=="llana",((meanpost-mean)/meanpost)*100,tonemeMAS),
                                                              tonemeMAStag = ifelse(between( tonemeMAS,0,20),"rising20",NA),
                                                              tonemeMAStag = ifelse(between( tonemeMAS,20,40),"rising40",  tonemeMAStag),
                                                              tonemeMAStag = ifelse(between( tonemeMAS,40,60),"rising60",  tonemeMAStag),
                                                              tonemeMAStag = ifelse( tonemeMAS>60,"risingplus",  tonemeMAStag),
                                                              tonemeMAStag = ifelse(between( tonemeMAS,-20,0),"falling20",  tonemeMAStag),
                                                              tonemeMAStag = ifelse(between( tonemeMAS,-40,-20),"falling40",  tonemeMAStag),
                                                              tonemeMAStag = ifelse(between( tonemeMAS,-60,-40),"falling60",  tonemeMAStag),
                                                              tonemeMAStag = ifelse(between( tonemeMAS,-200,-60),"fallingplus",  tonemeMAStag),
                                                              tonemePOSTtag = ifelse(between(tonemePOST,0,20),"rising20",NA),
                                                              tonemePOSTtag = ifelse(between(tonemePOST,20,40),"rising40", tonemePOSTtag),
                                                              tonemePOSTtag = ifelse(between(tonemePOST,40,60),"rising60", tonemePOSTtag),
                                                              tonemePOSTtag = ifelse(tonemePOST>60,"risingplus", tonemePOSTtag),
                                                              tonemePOSTtag = ifelse(between(tonemePOST,-20,0),"falling20", tonemePOSTtag),
                                                              tonemePOSTtag = ifelse(between(tonemePOST,-40,-20),"falling40", tonemePOSTtag),
                                                              tonemePOSTtag = ifelse(between(tonemePOST,-60,-40),"falling60", tonemePOSTtag),
                                                              tonemePOSTtag = ifelse(between(tonemePOST,-200,-60),"fallingplus", tonemePOSTtag),
                                                              tonemeMAScomplete = ifelse(tonemeMAStag==tonemePOSTtag,tonemeMAStag,paste0(tonemeMAStag,"_",tonemePOSTtag,""))
    )%>%mutate_if(is.numeric,round,2)%>%select(phon_id,tonemeMAS, tonemePOST,tonemeMAStag,tonemePOSTtag,tonemeMAScomplete)
    
    
    combined2sel <- subset(combined2, (!is.na(firsttonic)&postfirsttonic<40)| (is.na(firsttonic)&lag(postfirsttonic)>40)|toneme=="yes")
    
    declinationMAS <- combined2sel%>%filter((!is.na(firsttonic)&postfirsttonic<40)| (is.na(firsttonic)&lag(postfirsttonic)>40)|toneme=="yes")%>%mutate(declinationBODY = ifelse(lag(MASpart)!="toneme",((mean-lag(mean))/mean)*100,NA),ANACRUSIS = ifelse(lag(MASpart,2)=="toneme",((lag(mean)-lag(mean,2))/lag(mean)*100),NA))%>%mutate_if(is.numeric,round,2)%>%select(phon_id, declinationBODY,ANACRUSIS)
    
    
    
    combined3 <- combined2%>%left_join(declinationMAS, by = "phon_id")%>%left_join(tonemeMAS, by = "phon_id")
    
    setDF(combined)
    combined4 <- combined3%>%arrange(ip_id,tmin)%>%mutate(words_ag = ifelse(lead(ip_id)!=ip_id,paste(word,"/",sep=""),word))%>%ungroup()
    combined4 <- combined4[,!grepl("i\\.", colnames(combined4))]}
else{orstalof <- subset(tabbeddf(), grepl(input$phonemesname,tier))
orstalof <- orstalof%>%group_by(filename)%>%mutate(ph_id= paste("alof_",filename,"_",dplyr::row_number(),sep=""))

orstalof <- orstalof%>%mutate(tmin_phon = tmin_ms, tmax_phon =tmax_ms,phon_id=paste("phon",row_number(),sep = "_"))}    
    
  })
  
  ## Ip joined ------------------------------------------------------------
  
  ipjoined <- eventReactive(input$createbutton2,{
    
    
    vowels <- phonemesdf()
        vowels2 <- vowels %>% filter(toneme=="yes")%>% group_by(ip_id)%>%summarise(TOBI = last(TOBI), TOBIPRPST = last(TOBIPRPST),ANACRUSIS = max(ANACRUSIS),declinationBODY = max(declinationBODY),tonemeMAS= max(tonemeMAS),tonemeMASTAG = max(tonemeMAScomplete))
    vowels3 <- phonemesdf() %>% group_by(ip_id)%>%summarise(phonemes = n(),words=n_distinct(w_id))
    
    words2 <- dfprosody() %>% left_join(vowels2, by = "ip_id")
    words2 <- words2 %>% left_join(vowels3, by = "ip_id")
    
    
  })              
  
  ## Words joined ------------------------------------------------------------
  
  wordsdfjoined <- eventReactive(input$createwords2,{
    
    
    vowels <- phonemesdf()
    vowels2 <- vowels %>% filter(tonic=="yes")%>% group_by(w_id)%>%summarise(TOBI = last(TOBI),TOBIPRPST = last(TOBIPRPST),voweltype = last(voweltype),ANACRUSIS = max(ANACRUSIS),declinationBODY = max(declinationBODY),tonemeMAS= max(tonemeMAS),tonemeMASTAG = max(tonemeMAScomplete))
    vowels3 <- phonemesdf() %>% group_by(w_id)%>%summarise(phonemes = n())
    
    words2 <- wordsdf() %>% left_join(vowels2, by = "w_id")
    words2 <- words2 %>% left_join(vowels3, by = "w_id")
    
    
  })
  
  ##  Collapse 1 Data frame -----------------------------------------------------
  
  collapse1df <- eventReactive(input$createcollapse1,{
    
    if(!is.null(input$pitchfile)){
    
    orstip <- tabbeddf()
    prosodydb <- pitchintensitydf()
    searchtext <- ifelse(input$collapse1!="",input$collapse1,"novalue")
    orstip <- subset(orstip, grepl(input$collapse1,tier))
    orstip <- orstip%>%group_by(filename)%>%mutate(collap1_id= paste("collap1_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
    
    prosodydb <- pitchintensitydf()
    
    setDT(orstip) # make a data.table
    setDT(prosodydb) # make a data.table
    prosodydb<-prosodydb[, dummy := time_ms] 
    prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
    setkey(orstip, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
    setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
    ippros <- foverlaps(orstip, prosodydb, nomatch=NA)[, dummy := NULL]
    
    # orstiprn <- 
    #   
    #   ippros<- sqldf(
    #     "select x.*,y.pitch,y.pitchst, y.intensity, y.time_ms from orstip x left join prosodydb as y on(x.filename = y.filename) and (y.time_ms >= (x.tmin_ms)) and (y.time_ms <= (x.tmax_ms))")
    orstiprn <-
      suppressWarnings(ippros%>% 
                         group_by(collap1_id) %>% summarise(
                           spk = max(spk),
                           tmin = max(tmin_ms),
                           tmax = max(tmax_ms),
                           tmin_ms = max(tmin_ms),
                           tmax_ms = max(tmax_ms),
                           dur = max(dur),
                           collap1_annotation = max(annotation),
                           filename = max(filename),
                           PirHz = max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),
                           # PirSt = round(12*log2(max(pitch,na.rm=TRUE)/min(pitch, na.rm = TRUE)),2),
                           PirSt = max(pitchst, na.rm=TRUE)-min(pitchst,na.rm=TRUE),
                           Pimd = median(pitch, na.rm = TRUE),
                           PimnHz = mean(pitch, na.rm = TRUE),
                           # PimnSt = round(12*log2(mean(pitch,na.rm = TRUE)/1),2),
                           PimnSt = mean(pitchst,na.rm = TRUE),
                           Imd = median(intensity, na.rm = TRUE),
                           Imn = mean(intensity, na.rm = TRUE),
                           dcln = last(pitch)-first(pitch),
                           dcln_st = last(pitchst)-first(pitchst),
                           corpus = "corpus"
                         )) %>%ungroup()%>% arrange(filename,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
    orstiprn <-
      orstiprn %>% mutate(
        RdprSt = PirSt - lag(PirSt),
        Idpr = round(Imn - lag(Imn, 1),2),
        spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
        spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
        ppr = ifelse(spkpr =="mismo",trpr,NA),
        ppst = ifelse(spkpst =="mismo",trpst,NA)
      )
    orstiprn <-
      orstiprn %>% group_by(filename, spk) %>% mutate(
        # Rng_mean_sp = round(mean(pitch_range_St,na.rm = TRUE), 2),
        Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
        Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
        # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
        # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
        PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
        PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2)
        # Pdspkp = round(Pit_diff_sp_Hz / mean(pitch_mean_Hz,na.rm = TRUE), 2) * 100
        # pitch_mean_sp = round(mean(pitch_mean_Hz,na.rm = TRUE), 2)
      ) %>% ungroup()}
else{
  
  orstip <- tabbeddf()
  orstip <- subset(orstip, grepl(input$collapse1,tier))
  orstip <- orstip%>%group_by(filename)%>%mutate(collap1_id= paste("collap1_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
  
}    
    #   if(vowelsdf()$corpus == "corpus"){
    #     vowels <- vowelsdf()
    #     collapse1 <- sqldf(
    #       "select x.*,
    # y.collap1_annotation as collap1_text, y.dur as collap1_dur,y.tmin as collap1_tmin,y.tmax as collap1_tmax,
    # y.collap1_id as collap1_id from vowels x left join orstiprn as
    # y on (x.spk = y.spk) and (x.filename = y.filename) and (x.tmin >= (y.tmin-5)) and (x.tmax <= (y.tmax+5))")
    #     collapse1 <- collapse1 %>% group_by(collap1_id)%>%summarise(phonemes = n(), words = n_distinct(w_id), ips = n_distinct(ip_id),TOBI = last(TOBI))
    #     orstiprn <- orstiprn %>% left_join(collapse1, by = "collap1_id")
    #   }else{orstiprn}
    
  })
  
  collapse1joined <- eventReactive(input$createcollapsejoin,{
    
    
    orstiprn <- collapse1df()
    vowels <- phonemesdf()
    
    setDT(vowels)
    setDT(orstiprn)
    collapse1_j <- vowels[orstiprn, on = .(tmin >= tmin, tmax <= tmax,spk==spk,filename==filename)]
    setDF(collapse1_j)
    setDF(vowels)    
    
#     collapse1_j <- sqldf(
#       "select x.*,
# y.collap1_annotation as collap1_text, y.dur as collap1_dur,y.tmin as collap1_tmin,y.tmax as collap1_tmax,
# y.collap1_id as collap1_id from vowels x left join orstiprn as
# y on (x.spk = y.spk) and (x.filename = y.filename) and (x.tmin >= (y.tmin-5)) and (x.tmax <= (y.tmax+5))")
    collapse1 <- collapse1_j %>% group_by(collap1_id)%>%summarise(phonemes = n(), words = n_distinct(w_id), ips = n_distinct(ip_id),TOBI = last(TOBI), TOBIPRPST = last(TOBIPRPST),ANACRUSIS = last(ANACRUSIS),declinationBODY = last(declinationBODY),tonemeMAS= last(tonemeMAS),tonemeMASTAG = last(tonemeMAScomplete),ip_dur=max(ip_dur),word_dur=max(word_dur))
    collapse2 <- collapse1_j %>% group_by(collap1_id)%>%distinct(w_id,.keep_all = TRUE)%>% mutate(collapse1_text = paste0(words_ag, collapse = " "))%>%summarise(collapse1_text=max(collapse1_text))
    orstiprn <- orstiprn %>% left_join(collapse1, by = "collap1_id")
    orstiprn <- orstiprn %>% left_join(collapse2, by = "collap1_id")
    orstiprn <- orstiprn%>%mutate(collap1_dur = dur,wordip = ip_dur - word_dur, ipcollap1 = dur - ip_dur)
    # orstiprn <- orstiprn[,!grepl("i\\.", colnames(orstiprn))]
    
  })              
  
  ##  Collapse 2 Data frame -----------------------------------------------------
  
  collapse2df <- eventReactive(input$createcollapse2,{
    if(!is.null(input$pitchfile)){
    orstip <- tabbeddf()
    prosodydb <- pitchintensitydf()
    searchtext <- ifelse(input$collapse2!="",input$collapse2,"novalue")
    orstip <- subset(orstip, grepl(input$collapse2,tier))
    orstip <- orstip%>%group_by(filename)%>%mutate(collap2_id= paste("collap2_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
    
    prosodydb <- pitchintensitydf()
    
    setDT(orstip) # make a data.table
    setDT(prosodydb) # make a data.table
    prosodydb<-prosodydb[, dummy := time_ms] 
    prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
    setkey(orstip, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
    setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
    ippros <- foverlaps(orstip, prosodydb, nomatch=NA)[, dummy := NULL] 
    

      
      # ippros<- sqldf(
      #   "select x.*,y.pitch,y.pitchst, y.intensity, y.time_ms from orstip x left join prosodydb as y on (y.time_ms >= (x.tmin_ms)) and (y.time_ms <= (x.tmax_ms)) and (x.filename = y.filename)")
    orstiprn <-
      suppressWarnings(ippros%>% 
                         group_by(collap2_id) %>% summarise(
                           spk = max(spk),
                           tmin = max(tmin_ms),
                           tmax = max(tmax_ms),
                           tmin_ms = max(tmin_ms),
                           tmax_ms = max(tmax_ms),
                           dur = max(dur),
                           collap2_annotation = max(annotation),
                           filename = max(filename),
                           PirHz = round(max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),2),
                           PirSt = round(12*log2(max(pitch,na.rm=TRUE)/min(pitch, na.rm = TRUE)),2),
                           rangesemitonos = max(pitchst, na.rm=TRUE)-min(pitchst,na.rm=TRUE),
                           Pimd = round(median(pitch, na.rm = TRUE),2),
                           PimnHz = round(mean(pitch, na.rm = TRUE),2),
                           PimnSt = round(12*log2(mean(pitch,na.rm = TRUE)/1),2),
                           Pitchsemitonos = mean(pitchst,na.rm=TRUE),
                           Imd = round(median(intensity, na.rm = TRUE),2),
                           Imn = round(mean(intensity, na.rm = TRUE),2),
                           dcln = round(last(pitch)-first(pitch),2),
                           dcln_st = round(last(pitchst)-first(pitchst),2),
                           corpus = "corpus"
                         )) %>%ungroup()%>% arrange(filename,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
    orstiprn <-
      orstiprn %>% mutate(
        RdprSt = PirSt - lag(PirSt),
        Idpr = round(Imn - lag(Imn, 1),2),
        spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
        spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
        ppr = ifelse(spkpr =="mismo",trpr,NA),
        ppst = ifelse(spkpst =="mismo",trpst,NA)
      )
    orstiprn <-
      orstiprn %>% group_by(filename, spk) %>% mutate(
        # Rng_mean_sp = round(mean(pitch_range_St,na.rm = TRUE), 2),
        Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
        Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
        # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
        # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
        PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
        PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2)
        # Pdspkp = round(Pit_diff_sp_Hz / mean(pitch_mean_Hz,na.rm = TRUE), 2) * 100
        # pitch_mean_sp = round(mean(pitch_mean_Hz,na.rm = TRUE), 2)
      ) %>% ungroup()}
    
    else{
      
      
      orstip <- tabbeddf()
      orstip <- subset(orstip, grepl(input$collapse2,tier))
      orstip <- orstip%>%group_by(filename)%>%mutate(collap2_id= paste("collap2_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
      
      
    }
    
    #   if(vowelsdf()$corpus == "corpus"){
    #     vowels <- vowelsdf()
    #     collapse1 <- collapse1df()
    #     collapse <- sqldf(
    #       "select x.*,
    # y.collap1_annotation as collap1_text, y.dur as collap1_dur,y.tmin as collap1_tmin,y.tmax as collap1_tmax,
    # y.collap1_id as collap1_id from vowels x left join collapse1 as
    # y on (x.tmin >= (y.tmin-5)) and (x.tmax <= (y.tmax+5))
    # and (x.spk = y.spk) and (x.filename = y.filename)")
    #     collapse <- sqldf(
    #       "select x.*,
    # y.collap2_annotation as collap2_text, y.dur as collap2_dur,y.tmin as collap2_tmin,y.tmax as collap2_tmax,
    # y.collap2_id as collap2_id from collapse x left join orstiprn as
    # y on (x.tmin >= (y.tmin-5)) and (x.tmax <= (y.tmax+5))
    # and (x.spk = y.spk) and (x.filename = y.filename)")
    #     collapse2 <- collapse %>% group_by(collap2_id)%>%summarise(phonemes = n(), words = n_distinct(w_id), ips = n_distinct(ip_id), collap1 = n_distinct(collap1_id), TOBI = last(TOBI))
    #     orstiprn <- orstiprn %>% left_join(collapse2, by = "collap2_id")
    #   }else{orstiprn}
    
    
    
    
  })    
  
  collapse2joined <- eventReactive(input$createcollapsejoin2,{
    
    collapse1 <- collapse1joined()
    collapse2 <- collapse2df()
    
    setDT(collapse1)
    setDT(collapse2)
    collapse2_j <- collapse1[collapse2, on = .(tmin >= tmin, tmax <= tmax,spk==spk,filename==filename)]
    setDF(collapse2_j)
    setDF(collapse2)
    
#     collapse2_j <- sqldf(
#       "select x.*,
# y.collap2_annotation as collapse2_text, y.dur as collap2_dur,y.tmin as collap2_tmin,y.tmax as collap2_tmax,
# y.collap2_id from collapse1 x left join collapse2 as
# y on (x.spk = y.spk) and (x.filename = y.filename) and (x.tmin >= (y.tmin-50)) and (x.tmax <= (y.tmax+50))")
    collapse2grouped <- collapse2_j %>% group_by(collap2_id)%>%distinct(collap1_id,.keep_all = TRUE)%>% mutate(collapse2_text = paste0("{",collapse1_text,"}", collapse = " "))%>%summarise(collapse2_text=max(collapse2_text),collaps1 = n_distinct(collap1_id),collap1_dur=max(collap1_dur))
    collapse2 <- collapse2 %>% left_join(collapse2grouped, by ="collap2_id")
    vowels <- phonemesdf()
    
    setDT(vowels)
    setDT(collapse2)
    collapse2_j <- vowels[collapse2, on = .(tmin >= tmin, tmax <= tmax,spk==spk,filename==filename)]
    setDF(collapse2_j)
    setDF(collapse2)
    setDF(vowels)
    
#     collapse1_j <- sqldf(
#       "select x.*,
# y.collap2_id as collap2_id from vowels x left join collapse2 as
# y on (x.spk = y.spk) and (x.filename = y.filename) and (x.tmin >= (y.tmin-5)) and (x.tmax <= (y.tmax+5))")
    collapse4 <- collapse2_j %>% group_by(collap2_id)%>%summarise(phonemes = n(), words = n_distinct(w_id), ips = n_distinct(ip_id),TOBI = last(TOBI), TOBIPRPST = last(TOBIPRPST),ANACRUSIS = last(ANACRUSIS),declinationBODY = last(declinationBODY),tonemeMAS= last(tonemeMAS),tonemeMASTAG = last(tonemeMAScomplete),ip_dur=mean(ip_dur),word_dur=mean(word_dur))
    collapse2 <- collapse2 %>% left_join(collapse4, by = "collap2_id")
    collapse2 <- collapse2 %>% mutate(ipcollap2 =dur - ip_dur)
    
    #   vowels <- vowelsdf()
    #   collapse1_j <- sqldf(
    #     "select x.*,
    # y.collap2_annotation as collap2_text, y.dur as collap2_dur,y.tmin as collap2_tmin,y.tmax as collap2_tmax,
    # y.collap2_id as collap2_id from vowels x left join orstiprn as
    # y on (x.spk = y.spk) and (x.filename = y.filename) and (x.tmin >= (y.tmin-50)) and (x.tmax <= (y.tmax+50))")
    #   collapse1 <- collapse2_j %>% group_by(collap2_id)%>%summarise(phonemes = n(), words = n_distinct(w_id), ips = n_distinct(ip_id),TOBI = last(TOBI))
    #   collapse2 <- collapse2_j %>% group_by(collap2_id)%>%distinct(w_id,.keep_all = TRUE)%>% mutate(collapse2_text = paste0(words_ag, collapse = " "))%>%summarise(collapse2_text=max(collapse1_text))
    #   orstiprn <- orstiprn %>% left_join(collapse1, by = "collap2_id")
    #   orstiprn <- orstiprn %>% left_join(collapse2, by = "collap2_id")
    #   
    
    
    
  }) 
  
  # It is important that metadata spk column contains the same reference names as in spk tabbeddf column.
  # If names are different, the joining of both data frame won't work.
  
  ## Extended intonational phrase data frame ----
  
  dfprosody <- eventReactive(input$createbutton,
                             
                             
                             if(!is.null(input$pitchfile)&!is.null(input$intensityfile)&is.null(input$metadata)){
                               
                               orstip <- tabbeddf()
                               prosodydb <- pitchintensitydf()
                               orstip <- subset(orstip, grepl(input$ipsname,tier))
                               orstip <- orstip%>%arrange(filename,tmin_ms)%>%group_by(filename)%>%mutate(ip_id= paste("ip_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
                               
                               setDT(orstip) # make a data.table
                               setDT(prosodydb) # make a data.table
                               prosodydb<-prosodydb[, dummy := time_ms]
                               prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
                               setkey(orstip, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
                               setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
                               ippros <- foverlaps(orstip, prosodydb, nomatch=NA)[, dummy := NULL]
                            
                               orstiprn <-
                                 suppressWarnings(ippros%>% 
                                                    group_by(ip_id) %>% summarise(
                                                      spk = max(spk),
                                                      tmin = max(tmin_ms),
                                                      tmax = max(tmax_ms),
                                                      tmin_ms = max(tmin_ms),
                                                      tmax_ms = max(tmax_ms),
                                                      dur = max(dur),
                                                      ip_annotation = max(annotation),
                                                      filename = max(filename),
                                                      PirHz = max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),
                                                      PirSt = max(pitchst, na.rm=TRUE)-min(pitchst,na.rm=TRUE),
                                                      Pimd = median(pitch, na.rm = TRUE),
                                                      PimnHz = mean(pitch, na.rm = TRUE),
                                                      PimnSt = mean(pitchst,na.rm = TRUE),
                                                      Imd = median(intensity, na.rm = TRUE),
                                                      Imn = mean(intensity, na.rm = TRUE),
                                                      dcln = last(pitch)-first(pitch),
                                                      dcln_st = last(pitchst)-first(pitchst),
                                                      corpus = "corpus"
                                                      
                                                    )) %>%ungroup()%>% arrange(filename,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
                               orstiprn <-
                                 orstiprn %>% mutate(
                                   RdprSt = PirSt - lag(PirSt),
                                   Idpr = round(Imn - lag(Imn, 1),2),
                                   spkpr = ifelse(spk != lag(spk, 1),"otro","mismo"),
                                   spkpst = ifelse(spk != lead(spk, 1),"otro","mismo"),
                                   ppr = ifelse(spkpr =="mismo",trpr,NA),
                                   ppst = ifelse(spkpst =="mismo",trpst,NA)
                                 )
                               orstiprn <-
                                 orstiprn %>% group_by(filename, spk) %>% mutate(
                                   # Rng_mean_sp = round(mean(pitch_range_St,na.rm = TRUE), 2),
                                   Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
                                   Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
                                   # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
                                   # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
                                   PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
                                   PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2)
                                   # Pdspkp = round(Pit_diff_sp_Hz / mean(pitch_mean_Hz,na.rm = TRUE), 2) * 100
                                   # pitch_mean_sp = round(mean(pitch_mean_Hz,na.rm = TRUE), 2)
                                 ) %>% ungroup()
                               
                             }
                             
                             else if(!is.null(input$pitchfile)&!is.null(input$intensityfile)&!is.null(input$metadata)){
                               
                               orstip <- tabbeddf()
                               prosodydb <- pitchintensitydf()
                               orstip <- subset(orstip, grepl(input$ipsname,tier))
                               orstip <- orstip%>%arrange(filename,tmin_ms)%>%group_by(filename)%>%mutate(ip_id= paste("ip_",filename,"_",spk,"_",tmin_ms,"_",dplyr::row_number(),sep=""))
                               setDT(orstip) # make a data.table
                               setDT(prosodydb) # make a data.table
                               prosodydb<-prosodydb[, dummy := time_ms] 
                               prosodydb <- prosodydb[order(time_ms)]  # sorting by time so I can choose first match
                               setkey(orstip, filename, tmin_ms, tmax_ms)  # setting keys tells data.table what to join on
                               setkey(prosodydb, filename, time_ms, dummy) # setting keys tells data.table what to join on
                               ippros <- foverlaps(orstip, prosodydb, nomatch=NA)[, dummy := NULL]
                               
                               # orstiprn <- 
                               #   
                               # ippros<- sqldf(
                               #     "select x.*,y.pitch, y.intensity, y.time_ms from orstip x left join prosodydb as y on (y.time_ms >= (x.tmin_ms)) and (y.time_ms <= (x.tmax_ms)) and (x.filename = y.filename)")
                               orstiprn <-
                                 ippros%>% 
                                 group_by(ip_id) %>% summarise(
                                   spk = max(spk),
                                   tmin = max(tmin_ms),
                                   tmax = max(tmax_ms),
                                   tmin_ms = max(tmin_ms),
                                   tmax_ms = max(tmax_ms),
                                   dur = max(dur),
                                   phon = max(annotation),
                                   filename = max(filename),
                                   PirHz = round(max(pitch,na.rm=TRUE) - min(pitch, na.rm = TRUE),2),
                                   PirSt = round(12*log2(max(pitch,na.rm=TRUE)/min(pitch, na.rm = TRUE)),2),
                                   Pimd = round(median(pitch, na.rm = TRUE),2),
                                   PimnHz = round(mean(pitch, na.rm = TRUE),2),
                                   PimnSt = mean(pitchst,na.rm = TRUE),
                                   Imd = round(median(intensity, na.rm = TRUE),2),
                                   Imn = round(mean(intensity, na.rm = TRUE),2),
                                   dcln = round(last(pitch)-first(pitch),2),
                                   dcln_st = round(last(pitchst)-first(pitchst),2),
                                   corpus = "corpus"
                                   
                                 ) %>%ungroup()%>% arrange(filename,tmin) %>%   mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>% group_by(filename)%>% mutate(trpr = round(tmin - lag(tmax,1),1), trpst = round(lead(tmin,1)-tmax,1))%>%ungroup()
                               
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
                                 orstiprn %>% group_by(filename, spk) %>% mutate(
                                   # Rng_mean_sp = round(mean(pitch_range_St,na.rm = TRUE), 2),
                                   Rdspk = round(PirSt - mean(PirSt,na.rm = TRUE), 2),
                                   Idspk = round(Imn - mean(Imn,na.rm = TRUE), 2),
                                   # rate_diff_sp_ph = round(speech_rate_ph - mean(speech_rate_ph,na.rm = TRUE), 2),
                                   # rate_diff_sp_w = round(speech_rate_w - mean(speech_rate_w,na.rm = TRUE), 2),
                                   PdspkHz = round(PimnHz - mean(PimnHz,na.rm = TRUE), 2),
                                   PdspkSt = round(PimnSt - mean(PimnSt,na.rm = TRUE), 2)
                                   # Pdspkp = round(Pit_diff_sp_Hz / mean(pitch_mean_Hz,na.rm = TRUE), 2) * 100
                                   # pitch_mean_sp = round(mean(pitch_mean_Hz,na.rm = TRUE), 2)
                                 ) %>% ungroup()
                               orstiprn <- orstiprn%>%left_join(metadatafile(), by="spk")
                             } else if (is.null(input$pitchfile)&is.null(input$intensityfile)&!is.null(input$metadata)) 
                             {orstip <- tabbeddf()%>%left_join(metadatafile(), by="spk")
                             
                             } 
                             
                             else {
                               orstip <- tabbeddf()%>%arrange(filename,tmin_ms)
                               orstip <- subset(orstip, grepl(input$ipsname,tier))}
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
  )           
  
  fulljoined <- eventReactive(input$createdfjoined,{
    
    d <- phonemesdf()%>%filter(toneme=="yes")%>%group_by(ip_id)%>%summarise(TOBI = max(TOBI))
    e <- phonemesdf()%>%group_by(ip_id)%>%summarise(phonemes=n(),words = n_distinct(w_id))
    p <- dfprosody()%>%left_join(d, by="ip_id")
    p <- p %>%left_join(e, by="ip_id")
  })
  
  output$fulljoined <- renderDataTable({datatable(fulljoined(),extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  
  ## Data frame to appear on statistical methods -----                 
  
  dfselection <- reactive({
    if(input$selectdata=="fonocortesiadb"){fonocortesiadb}
    else if (input$selectdata=="picodeorodb") {picodeorodb}
    else if (input$selectdata=="I will use ips from create section" & input$joinedsel=="no")
    {p <- as.data.frame(dfprosody())} else if (input$selectdata=="I will use words from create section"& input$joinedsel=="no")
    {p<- as.data.frame(wordsdf())} else if (input$selectdata=="I will use vowels from create section")
    {p<- phonemesdf()
    p <- as.data.frame(p)} else if (input$selectdata=="I will use collapse1 from create section"& input$joinedsel=="no")
    {p<- collapse1df()
    p <- as.data.frame(p)} else if (input$selectdata=="I will use collapse2 from create section"& input$joinedsel=="no")
    {p<- collapse2df()
    p <- as.data.frame(p)}else if (input$selectdata=="I will use words from create section" & input$joinedsel=="yes")
    {p<- wordsdfjoined()
    p <- as.data.frame(p)}else if (input$selectdata=="I will use ips from create section"& input$joinedsel=="yes")
    {p<- ipjoined()
    p <- as.data.frame(p)}else if (input$selectdata=="I will use collapse1 from create section"& input$joinedsel=="yes")
    {p<- collapse1joined()
    p <- as.data.frame(p)} else if (input$selectdata=="I will use collapse2 from create section"& input$joinedsel=="yes")
    {p<-collapse2joined()
    p <- as.data.frame(p)} else if (input$selectdata=="I will upload my data at Upload Section")
    {df <- readRDS(input$uploadRDSip$datapath) }
    
  })
  # dffilt <- reactive({
  #   isolate(p <- dfselection())
  #    
  #   
  #   })
  
ip2var1 <- reactive({
    
    p <- dfselection()
    setDF(p)
    
    if(is.null(input$filter1)){p}
    else {dfselection()%>%filter(!!input$var1%in%!!input$filter1)}
    
  })
  
  ip2var2 <- reactive({
    p <- ip2var1()
    setDF(p)
    if(is.null(input$filter2)){p}
    else {p%>%filter(!!input$var2%in%!!input$filter2)}
    
  })
  
  ip2var3 <- reactive({
    p <- ip2var2()
    setDF(p)
    if(is.null(input$filter3)){p}
    else {p%>%filter(!!input$var3%in%!!input$filter3)}
    
  })
  
  ip2var4 <- reactive({
    p <- ip2var3()
    if(is.null(input$filter4)){p}
    else {p%>%filter(grepl(input$filter4,!!input$var4))}

  })
  
  ip2var5 <- reactive({
    p <- ip2var4()
    if(isTRUE(is.null(input$filternum1))|isTRUE(input$filternum1=="")|isTRUE(is.na(input$filternum1))){p}
    else {p%>%filter(!!input$var5>=input$filternum1)}

  })
  
  ip2var52 <- reactive({
    p <- ip2var5()
    if(isTRUE(is.null(input$filternum2))|isTRUE(input$filternum2=="")|isTRUE(is.na(input$filternum2))){p}
    else {p%>%filter(!!input$var5<=input$filternum2 )}

  })

  ip2var6 <- reactive({
    p <- ip2var52()
    if(isTRUE(is.null(input$filternum3))|isTRUE(input$filternum3=="")|isTRUE(is.na(input$filternum3))){p}
    else {p%>%filter(!!input$var6>=input$filternum3 )}

  })

  ip2var62 <- reactive({
    p <- ip2var6()
    if(isTRUE(is.null(input$filternum4))|isTRUE(input$filternum4=="")|isTRUE(is.na(input$filternum4))){p}
    else {p%>%filter(!!input$var6<=input$filternum4 )}

  })
  
  ip2 <- reactive({
    
    p <- ip2var62()
    setDF(p)
    
  })
  
  # Columns selected to be exported on Filter Section
  
  databaseexport <- reactive({
    
    ip2()
    
  }
  )
  
  # UI filtering parts ------------------------------------------------------
  

  ## UI General Filter -----  
  

  output$var1ui <- renderUI({
    varSelectInput("var1","Filter 1 (select)",multiple = FALSE,
                   data = dfselection()%>%select(where(is.character)))})
  output$filter1ui <- renderUI({
    if(input$resetbutton){
    choices = unique(dfselection()%>%select(input$var1))
    selectInput(inputId = "filter1",label ="",multiple = TRUE,choices = choices)}else{choices = unique(dfselection()%>%select(input$var1))
    selectInput(inputId = "filter1",label ="",multiple = TRUE,choices = choices)}
  })
  output$var2ui <- renderUI({
    
    div(
      varSelectInput("var2","Filter 2 (select)",multiple = FALSE, data = dfselection()%>%select(where(is.character))))})
  output$filter2ui <- renderUI({
    if(input$resetbutton){
      choices = unique(dfselection()%>%select(input$var2))
      selectInput(inputId = "filter2",label ="",multiple = TRUE,choices = choices)}else{choices = unique(dfselection()%>%select(input$var2))
      selectInput(inputId = "filter2",label ="",multiple = TRUE,choices = choices)}
    })
  output$var3ui <- renderUI({
    div(
      varSelectInput("var3","Filter 3 (select)",multiple = FALSE,data = dfselection()%>%select(where(is.character))))})
  
  output$filter3ui <- renderUI({
    
    if(input$resetbutton){
      choices = unique(dfselection()%>%select(input$var3))
      selectInput(inputId = "filter3",label ="",multiple = TRUE,choices = choices)}else{choices = unique(dfselection()%>%select(input$var3))
      selectInput(inputId = "filter3",label ="",multiple = TRUE,choices = choices)}
    
    })
  output$var4ui <- renderUI({
    
    div(
      varSelectInput("var4","Filter 4 (regex)",multiple = FALSE,data = dfselection()%>%select(where(is.character))))})
  output$filter4ui <- renderUI({
    if(input$resetbutton){
      textInput("filter4", label = "",value = NULL)}else{
        textInput("filter4", label = "",value=NULL)
        
      }
    })
  output$var5ui <- renderUI({
    div(
      varSelectInput("var5","Filter 5 (num)",multiple = FALSE,data = dfselection()%>%select(where(is.numeric)),selected = "dur"))})
  # output$filter5 <- renderUI({div(
  #     sliderInput("filter5","",min = min(dfselection()%>%select(!!input$var5),na.rm = TRUE),max = max(dfselection()%>%select(!!input$var5),na.rm = TRUE),value=c(min(dfselection()%>%select(!!input$var5),na.rm = TRUE),max(dfselection()%>%select(!!input$var5),na.rm = TRUE))))})
  output$filternum1ui <- renderUI({
    if(input$resetbutton){
      textInput(inputId = "filternum1",">=",value = "")}else{
       textInput(inputId = "filternum1",">=",value = NULL)
        
      }
    })
  output$filternum2ui <- renderUI({
    
    if(input$resetbutton){
      textInput(inputId = "filternum2","<=",value = "")}else{
        textInput(inputId = "filternum2","<=",value = NULL)
        
      }
  })
  
  output$var6ui <- renderUI({
    div(
      varSelectInput("var6","Filter 6 (num)",multiple = FALSE,data = dfselection()%>%select(where(is.numeric)),selected = "dur"))})
  output$filternum3ui <- renderUI({
    
    if(input$resetbutton){
      textInput(inputId = "filternum3",">=",value = "")}else{
        textInput(inputId = "filternum3",">=",value = NULL)
        
      }
    
  })
  output$filternum4ui <- renderUI({
    
    if(input$resetbutton){
      textInput(inputId = "filternum4","<=",value = "")}else{
       textInput(inputId = "filternum4","<=",value = NULL)
        
      }
    })
  
  output$databaseselfilter <- renderUI({
   
    div(varSelectInput("databasesel",label = "",multiple=TRUE,data = dfselection()),downloadButton("downloaddataxlsx",label = "Download complete or filtered database",icon=icon("download")))})           
  #
  ## UI Descriptive by group Filter -----  
  
  output$frequ <- renderUI({div(varSelectInput("frequ","Group by",multiple = TRUE,data = ip2()%>%select(where(is.character))))})    
  
  ## UI Descriptive by variable Filter -----   
  
  output$descriptivenum <- renderUI({div(varSelectInput("descriptivenum","Select variables",multiple = TRUE,data = ip2()%>%select(where(is.numeric))))})
  
  ## UI Ngrams Filter -----
  
  output$varngrams <- renderUI({div(varSelectInput("varngrams","Select variable to generate ngrams",multiple = FALSE,data = ip2()%>%select(where(is.character)),selected = "corpus"))})
  output$ngramsize <- renderUI({div(numericInput("ngramsize","Select ngrams size",min = 1,max = 10,step=1,value = 1))})
  
  ## UI Decision tree Filter -----
  
  output$var6tree <- renderUI({div(varSelectInput("var6tree","Select variable",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$var7 <- renderUI({div(varSelectInput("var7","Select variable",multiple = TRUE,data = ip2()))})
  output$choice6 <- renderUI(pickerInput("filter7",choices = unique(ip2()%>%select(!!input$var6tree)),multiple = TRUE))
  
  ## UI heatmap Filter ----- 
  
  output$heatmapdep <- renderUI({div(varSelectInput("heatmapdep","Select variable",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$heatmapindep <- renderUI({div(varSelectInput("heatmapindep","Select variable",multiple = TRUE,data = ip2()%>%select(where(is.numeric))))})
  output$heatmapsel <- renderUI(pickerInput("heatmappicker",choices = unique(ip2()%>%select(!!input$heatmapdep)),multiple = TRUE))
  
  ## UI PCA Filter ----- 
  
  output$var6pca <- renderUI({div(varSelectInput("var6pca","Select variable",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$var7pca <- renderUI({div(varSelectInput("var7pca","Select variable",multiple = TRUE,data = ip2()%>%select(where(is.numeric))))})
  output$choicepca <- renderUI(pickerInput("filter8",choices = unique(ip2()%>%select(!!input$var6pca)),multiple = TRUE))
  
  ## UI MFA Filter ----- 
  
  output$var6mfa <- renderUI({div(varSelectInput("var6mfa","Select variable",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$var7mfa <- renderUI({div(varSelectInput("var7mfa","Select variable",multiple = TRUE,data = ip2()))})
  output$choicemfa <- renderUI(selectInput("filter9","Select category", choices = unique(ip2()%>%select(!!input$var6mfa)),multiple = TRUE))
  
  ## UI logistic Filter ----- 
  
  output$var6log <- renderUI({div(varSelectInput("var6log","Select variable",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$var7log <- renderUI({div(varSelectInput("var7log","Select variable",multiple = TRUE,data = ip2()))})
  output$choicelog <- renderUI(selectInput("filterlog","Select categories", choices = unique(ip2()%>%select(!!input$var6log)),multiple = TRUE))
  
  ## UI Crosstabs and chisquare Filter ----- 
  
  output$crosstabsdep <- renderUI({div(varSelectInput("crosstabsdep","Variable1",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$crosstabsindep <- renderUI({div(varSelectInput("crosstabsindep","Variable2",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$crosstabssel <- renderUI(pickerInput("crosstabspicker",choices = unique(ip2()%>%select(!!input$crosstabsdep)),multiple = TRUE))
  output$crosstabssel2 <- renderUI(pickerInput("crosstabspicker2",choices = unique(ip2()%>%select(!!input$crosstabsindep)),multiple = TRUE))
  
  ## UI ANOVA Filter -----
  
  output$anovadep <- renderUI({div(varSelectInput("anovadep","Select variable",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$anovaindep <- renderUI({div(varSelectInput("anovaindep","Select variable",multiple = FALSE,data = ip2()%>%select(where(is.numeric))))})
  output$anovasel <- renderUI(pickerInput("anovapicker",choices = unique(ip2()%>%select(!!input$anovadep)),multiple = TRUE))
  
  ## UI Correlations Filter ----- 
  
  output$correlationsdep <- renderUI({div(varSelectInput("correlationsdep","Select variable",multiple = TRUE,data = ip2()%>%select(where(is.numeric))))})
  
  ## UI Raw prosody Filter -----  
  
  output$filenamerawselect <- renderUI({div(varSelectInput("filenamerawselect","Select variable",multiple = FALSE,data = ip2()%>%select(where(is.character)),selected = "filename"))})
  output$filenamerawchoices <- renderUI({div(pickerInput("filenamerawchoices",choices = unique(ip2()%>%select(!!input$filenamerawselect)),multiple = FALSE))})
  output$dtrawsel <- renderUI({varSelectInput("dtrawsel",label = "Select variables",data = ip2(),multiple = TRUE)})
  
  output$rawfilter <- renderUI({
    div(
      
      # varSelectInput("filenameraw","Select filename variable",multiple=FALSE,ip2()%>%select(where(is.character))),
      # varSelectInput("filenamerawselect","Select file", multiple=FALSE,ip2()%>%select(where(is.character))),
      # pickerInput("filenamerawchoice","Select file", multiple=FALSE,choices=unique(ip2()%>%select(!!input$filenamerawselect))),
      # varSelectInput("rawtext","Select text variable",multiple=FALSE, ip2()%>%select(where(is.character))),
      # varSelectInput("rawspk","Select speaker variable",multiple=FALSE, ip2()%>%select(where(is.character)), selected = "spk"),
      varSelectInput("rawx","Select x or time variable",multiple=FALSE, pitchintensitydf()%>%select(where(is.numeric)),selected = "time_ms"),
      varSelectInput("rawy","Select pitch or intensity variable",multiple=FALSE,pitchintensitydf()%>%select(where(is.numeric)),selected = "pitchst"),
      numericInput("sliderraw1",">=", value = 1),
      numericInput("sliderraw2","<=", value = 2000),
      actionButton("rawbutton","Create pitch or intensity plot"))
  })
  ## UI linear prosody Filter -----
  
  output$prosviewfile1 <- renderUI({div(varSelectInput("prosviewfile1","Select filename variable",multiple = FALSE,data = ip2()%>%select(where(is.character)),selected = "filename"))})
  output$prosview2 <- renderUI({div(varSelectInput("prosview2","Select category variable",multiple = FALSE,data = ip2()%>%select(where(is.character))))})
  output$prosview3 <- renderUI({div(varSelectInput("prosview3","Select numeric variable",multiple = FALSE,data = ip2()%>%select(where(is.numeric))))})
  output$dtlinearsel <- renderUI({div(varSelectInput("dtlinearsel","Select category variable",multiple = TRUE,data = ip2()))})
  
  ## UI tagged prosody Filter -----                 
  
  output$cont1 <- renderUI({div(varSelectInput("cont1","Select unit variable", vowelssel()%>%select(where(is.character)), multiple = FALSE, selected="ip_id"))})
  output$cont3 <- renderUI({div(varSelectInput("cont3","Select x variable", vowelssel()%>%select(where(is.numeric)), multiple = FALSE, selected="tmin"))})
  output$cont4 <- renderUI({div(varSelectInput("cont4","Select y variable", vowelssel()%>%select(where(is.numeric)), multiple = FALSE, selected = "PimnSt"))})
  output$cont5 <- renderUI({div(varSelectInput("cont5","Select linetype variable", vowelssel()%>%select(where(is.character),where(is.factor)), multiple = FALSE,selected="w_id"))})
  output$cont6 <- renderUI({div(varSelectInput("cont6","Select factor variable", vowelssel()%>%select(where(is.character),where(is.factor)), multiple = FALSE, selected="w_id"))})
  output$cont7 <- renderUI({div(varSelectInput("cont7","Select size variable", vowelssel()%>%select(where(is.numeric)), multiple = FALSE, selected="Imn"))})
  output$cont8 <- renderUI({div(varSelectInput("cont8","Select text variable", vowelssel()%>%select(where(is.character),where(is.factor)), multiple = FALSE, selected="phon"))})
  output$cont9 <- renderUI({div(varSelectInput("cont9","Select text variable", vowelssel()%>%select(where(is.character),where(is.factor)), multiple = FALSE, selected="ip"))})
  output$cont10 <- renderUI(div(varSelectInput("cont10","Select variables to show", vowelssel(), multiple = TRUE)))
  
  
  # UI datatables -----------------------------------------------------------
  
  output$database <- renderDataTable({
    
    datatable(ip2()%>%select(!!!input$databasesel)%>%mutate(across(where(is.numeric), round, 2),across(where(is.character),as.factor)),extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))
  }
  
  )
  
  selectedRow <- eventReactive(input$database_rows_selected,{
    if(input$selectdata=="fonocortesiadb"){
    ip2()[input$database_rows_selected,"filename"]}else{input$audiofile2}
  })
  
  tminselectedRow <- eventReactive(input$database_rows_selected,{
    ip2()[input$database_rows_selected,"tmin_ms"]
  })
  
  tmaxselectedRow <- eventReactive(input$database_rows_selected,{
    
    ip2()[input$database_rows_selected,"tmax_ms"]
  })
  
  output$selected <- renderUI({ 
    Sys.sleep(1)
    if(input$selectdata=="fonocortesiadb"){
    link <- paste("audios/",selectedRow(),".mp3#t=",tminselectedRow()/1000,",",tmaxselectedRow()/1000, sep = "")}
    else{
      
      link <- paste(selectedRow(),"#t=",tminselectedRow()/1000,",",tmaxselectedRow()/1000, sep = "")}
    tags$audio(src = link, type = "audio/mp3", autoplay = NA, controls = NA)
   
    
    
  })
  

  
  output$selectedtext <- renderText({ 
    link <- paste("audios/",selectedRow(),".mp3#t=",tminselectedRow()/1000,",",tmaxselectedRow()/1000, sep = "")
    link
  })
  
  output$frequency <- renderDataTable({
    
    p<- ip2()%>%group_by(!!!input$frequ)%>%select(!!!input$varmeans)%>%na.omit()%>%summarise(freq= n(), across(everything(),list(mean = mean, median = median, std = sd, max=max, min=min)))%>%select(-freq_mean,-freq_median,-freq_max,-freq_min,-freq_std)%>%mutate(across(where(is.numeric), round, 1))
    p <-  setDT(p)
    datatable(p,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20))) })
  
  output$descriptivestats <- renderDataTable({
    
    p <- ip2()
    d <- describe(ip2()%>%select(!!!input$descriptivenum))
    d<- setDT(d, keep.rownames = "variable")
    d<- d %>% mutate(across(where(is.numeric), round, 2))
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20))
    )}
  )
  
  
  # UI plots ----------------------------------------------------------------
  
  output$frequencyplot <- renderPlot({
    p<- ip2()%>%group_by(!!!input$frequ)%>%summarise(freq=n())%>%rename(var1=1,var2=2,freq=3)
    
    d <-    ggplot(p, aes(fill=var1, y=freq, x=var2)) +
      geom_bar(position='dodge', stat='identity')
    d
  })
  
  
  # ngrams ------------------------------------------------------------------
  
  ngrams1 <- eventReactive(input$ngramsbutton,{
    
    
    p<-   ip2() %>% unnest_tokens(ngram,!!input$varngrams,token="ngrams", n= input$ngramsize)%>%group_by(ngram)%>%summarise(freq=n())%>%arrange(desc(freq))%>%filter(ngram!="")
    
    setDT(p)
    p <- p%>%head(n=100)
    
  })
  
  output$ngrams1table <- renderDataTable({
    
    datatable(head(ngrams1(),n=100))
    
  })
  
  
  
  output$ngramswcloud <- renderWordcloud2({
    p <- ngrams1()
    p <- p %>%rename(word = ngram)
    wordcloud2(data=p)}

  )
  
  
  # Decision tree -----------------------------------------------------------
  
  
  treeselect <- eventReactive(input$treebutton,{p <- ip2()%>%mutate(varx = !!input$var6tree)%>%filter(varx%in%!!input$filter7)%>%select(varx,!!!input$var7)%>%na.omit()
  d <-rpart(varx~., data= p,cp=.02)
  d}
  )
  output$treeplot <- renderPlot(rpart.plot(treeselect(),extra = 101, box.palette="RdBu", shadow.col="gray", nn=TRUE))
  
  treeselectparty <- eventReactive(input$treebutton,{p <- ip2()%>%mutate(varx = !!input$var6tree)%>%filter(varx%in%!!input$filter7)%>%select(varx,!!!input$var7)%>%na.omit()%>%mutate(across(where(is_character),as_factor))
  d <-ctree(varx~., data= p, control = ctree_control(maxdepth = 3))
  d}
  )
  
  output$treeplotparty <- renderPlot(plot(treeselectparty()))
  output$treeparty <-renderPrint(treeselectparty())
  
  
  # Crosstabs and chi square ------------------------------------------------
  
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
  
  
  # Correlations ------------------------------------------------------------
  
  correlations <- eventReactive(input$correlationsbutton,{
    
    p <- ip2()%>%select(!!!input$correlationsdep)%>%na.omit()
    p.mat <- cor_pmat(p)
    corr <- round(cor(p),1)
    ggcorrplot(corr,hc.order = TRUE, type = "lower",
               p.mat = p.mat)
  })
  
  output$correlationsplot <- renderPlot(correlations())
  
  
  # ANOVA -------------------------------------------------------------------
  
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
  
  
  # Heatmap -----------------------------------------------------------------
  
  mapa <- eventReactive(input$heatmapbutton,{
    
    p <- ip2()%>%mutate(varx = !!input$heatmapdep)%>%filter(varx%in%!!input$heatmappicker)%>%group_by(varx)%>%select(varx,!!!input$heatmapindep)%>% summarize_if(., is.numeric, mean, na.rm = TRUE)
    p <- p %>%column_to_rownames(var="varx")
    
  }
  )
  
  output$heatmap <- renderPlotly(heatmaply(
    mapa(), 
    xlab = "Features",
    ylab = "Unit", 
    scale = "column",
    main = "Data transformation using 'scale'"
  ))
  
  
  # PCA ---------------------------------------------------------------------
  
  
  pcaresult <- eventReactive(input$pcabutton,{
    
    p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7pca)%>%na.omit()
    d <- PCA(p, quali.sup = 1, ncp = 3, graph = FALSE)
    summary(d)
    
    
  })
  
  pcaresultfacto <- eventReactive(input$pcabutton,{
    
    p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7pca)%>%na.omit()
    d <- PCA(p, quali.sup = 1, ncp = 3, graph = FALSE)
    res.var <- get_pca_var(d)
    
    
  })
  
  pcaresultfacto2 <- eventReactive(input$pcabutton,{
    
    p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7pca)%>%na.omit()
    d<- PCA(p, quali.sup = 1, ncp = 3, graph = FALSE)
    
    
  })
  
  output$pcarcontrib <- renderPrint(pcaresultfacto()$contrib)
  output$pcarcoord <- renderPrint(pcaresultfacto()$coord)
  output$pcaploteigen <- renderPlot({d<-fviz_eig(pcaresultfacto2())
  d})
  output$pcaplotind<- renderPlot(
    {p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7pca)%>%na.omit()
    
    res.pca <- pcaresultfacto2()
    groups <- as.factor(p$varx)
    d<- fviz_pca_ind(res.pca,
                     habillage = groups, # color by groups
                     # palette = c("#00AFBB",  "#FC4E07"),
                     legend.title = "Groups",
                     repel = TRUE)
    d
    }
  )
  
  pcaplotvar <-eventReactive(input$pcavarbutton,
                             {
                               
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
    
    res.hcpc <- HCPC(pcaresultfacto2(), graph = FALSE,nb.clust = -1)
    
    d<-fviz_dend(res.hcpc, 
                 cex = 0.7,                     # Label size
                 palette = "jco",               # Color palette see ?ggpubr::ggpar
                 rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                 rect_border = "jco",           # Rectangle color
                 labels_track_height = 0.8) # Augment the room for labels
    d
    
  })
  
  
  
  pcaclustable <- eventReactive(input$pcaclusterbutton,{
    
    p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7pca)%>%na.omit()
    # # Compute hierarchical clustering on principal components
    # res.pca <- PCA(p%>%select(-varx), ncp = 3, graph = FALSE)
    res.hcpc <- HCPC(pcaresultfacto2(), graph = FALSE,nb.clust = -1)
    table(res.hcpc$data.clust$clust,p$varx)
    
  })
  
  pcaclustable2 <- eventReactive(input$pcaclusterbutton,{
    
    # p <- ip2()%>%mutate(varx = !!input$var6pca)%>%filter(varx%in%!!input$filter8)%>%select(varx,!!!input$var7pca)%>%na.omit()
    # # Compute hierarchical clustering on principal components
    # res.pca <- PCA(p%>%select(-varx), ncp = 3, graph = FALSE)
    res.hcpc <- HCPC(pcaresultfacto2(), graph = FALSE)
    p<- res.hcpc$data.clust%>%group_by(clust)%>%summarise(freq=n(),across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE),2)))
    p <-  setDT(p)
    d<-  datatable(p,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))
    
    
  })
  output$pcaplotcluster <-         renderPlot({pcaplotcluster()                })
  
  output$pcaclustable <- renderTable({pcaclustable()})
  output$pcaclustable2 <- renderDataTable({pcaclustable2()})
  
  
  # MFA ---------------------------------------------------------------------
  
  mfaresult <- eventReactive(input$mfabutton,{
    
    p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%select(varx,!!!input$var7mfa)%>%na.omit()
    d <- FAMD(p%>%select(-varx), graph = FALSE) 
    
    summary(d)
    
    
  }) 
  
  mfaresultfacto <- eventReactive(input$mfabutton,{

    p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7mfa)%>%na.omit()
    d <- FAMD(p%>%select(-varx), graph = FALSE)
    
    res.var <- get_famd_var(d)
    
    
    
  })
  
  mfaresultfacto2 <- eventReactive(input$mfabutton,{
    p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7mfa)%>%na.omit()
    
    # d <- prcomp(p%>%select(-varx), center = TRUE,scale. = TRUE)
    
    d<- FAMD(p%>%select(-varx), ncp = 3, graph = FALSE)
    
    
  })
  
  output$mfarcontrib <- renderPrint(mfaresultfacto()$contrib)
  output$mfarcoord <- renderPrint(mfaresultfacto()$coord)
  output$mfaploteigen <- renderPlot({d<-fviz_eig(mfaresultfacto2())
  d})
  output$mfaplotind<- renderPlot({
    p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7mfa)%>%na.omit()
    
    res.mfa <- mfaresultfacto2()
    groups <- as.factor(p$varx)
    d<- fviz_famd_ind(res.mfa,
                      habillage = groups, # color by groups
                      # palette = c("#00AFBB",  "#FC4E07"),
                      legend.title = "Groups",
                      repel = TRUE)
    d
  }
  )
  
  mfaplotvar <-eventReactive(input$mfavarbutton,
                             {
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

    p <- ip2()%>%mutate(varx = !!input$var6mfa)%>%filter(varx%in%!!input$filter9)%>%group_by(varx)%>%slice(seq(n()*.40))%>%select(varx,!!!input$var7mfa)%>%na.omit()
    # # Compute hierarchical clustering on principal components
    # res.mfa <- mfa(p%>%select(-varx), ncp = 3, graph = FALSE)
    res.hcpc <- HCPC(mfaresultfacto2(), graph = FALSE)
    table(res.hcpc$data.clust$clust,p$varx)
    
  })
  
  mfaclustable2 <- eventReactive(input$mfaclusterbutton,{
    
    res.hcpc <- HCPC(mfaresultfacto2(), graph = FALSE)
    p<- res.hcpc$data.clust%>%group_by(clust)%>%summarise(freq=n(),across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE),2)))
    p <-  setDT(p)
    d<-  datatable(p,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))
    
    
  })
  output$mfaplotcluster <-         renderPlot({mfaplotcluster()                })
  
  output$mfaclustable <- renderTable({mfaclustable()})
  output$mfaclustable2 <- renderDataTable({mfaclustable2()})
  
  # Logistic regression -----------------------------------------------------
  
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
  
  # Linear prosody ---------------------------------------------------------------
  
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
  output$dtlinear <- renderDataTable({
    p <- ip2()%>%filter(!!input$prosviewfile1%in%!!input$prosviewfileselect)%>%select(!!!input$dtlinearsel)
    d<-  datatable(p%>%mutate(across(where(is.numeric), round, 2),across(where(is.character),as.factor)),extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  
  # Pitch contour ---------------------------------------------------------------
  
  output$filterscontour <- renderUI({
    
    p <- vowelssel()
    selectInput("cont2","Select group", choices = unique(p%>%select(input$cont1)),multiple = F, selected = NULL)
    
  })
  
  melodyplot <- eventReactive(input$contplot, {
    
    dataframe <- vowelssel()%>% filter(!!input$cont1%in%!!input$cont2)%>%arrange(!!input$cont3)%>% mutate(xaxe = !!input$cont3, yaxe = !!input$cont4, linety = !!input$cont5, factormel = !!input$cont6, sizemel = !!input$cont7, textcolap = !!input$cont8, wrapping = !!input$cont9)
    
    ggplot(data=dataframe, aes(x=xaxe, y=yaxe, linetype=linety)) +
      geom_line(color="black", size=1)+ ylim(70,120)+
      geom_point(aes(colour = factor(factormel), size=sizemel))+
      geom_text(aes(label=textcolap)) + facet_wrap(~str_wrap(wrapping)) })
  
  dataframemel <- eventReactive(input$contDT, {
    dataframe <- vowelssel()%>% filter(!!input$cont1%in%!!input$cont2)%>%arrange(!!input$cont3)%>%select(!!!input$cont10)
  })
  
  output$dataframemel <- renderDataTable({dataframemel()})
  
  output$melodyplot    <-     renderPlotly({melodyplot()})
  
  
  # Raw prosody -------------------------------------------------------------
  
  rawplot <- eventReactive(input$rawbutton,{
    
    
    p <- pitchintensitydf()%>%filter(!!input$filenamerawselect %in% !!input$filenamerawchoices)%>%mutate(varx = !!input$rawx, vary=!!input$rawy)%>%filter(varx>=!!input$sliderraw1,varx<=!!input$sliderraw2)
    d<- ggplot(data=p, aes(x=varx))+ geom_point(aes(y = vary), color = "blue")+ ylim(70,120)
    d
  })
  
  rawtable <- eventReactive(input$rawbutton,{
    
    
    p <- ip2()%>%filter(!!input$filenamerawselect %in% !!input$filenamerawchoices)%>%select(!!!input$dtrawsel)
    
  })
  
  output$rawplot <- renderPlotly({rawplot()})
  output$rawtable <- renderDataTable({
    datatable(rawtable()%>%mutate(across(where(is.numeric), round, 2),across(where(is.character),as.factor)),extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  
  audiolink <- eventReactive(input$listenbutton,{
    # inFile <- input$audiofile2
    # file.copy(inFile$datapath, file.path("www/audios/", inFile$name))
    
    p <- ip2()%>%filter(!!input$filenamerawselect %in% !!input$filenamerawchoices)%>%mutate(varx=!!input$filenamerawselect)%>%select(varx)
    p <- p %>%group_by(varx)%>%summarise(n())
    inicio <- input$sliderraw1/1000
    final <- input$sliderraw2/1000
    linkaudio <- ifelse(input$selectdata=="fonocortesiadb",paste("audios/",p$varx,".mp3#t=",inicio,",",final,sep=""),paste(input$audiofile2,"#t=",inicio,",",final,sep=""))
    # linkaudio <- paste("audios/",p$varx,".mp3#t=",inicio,",",final,sep="")
    #t=[starttime][,endtime]
    
    
    
    
    div(tags$audio(src=linkaudio,"audio/mp3",autoplay = F, controls = T))
    
    
  })
  
  output$rawprueba <- renderUI({div(tags$video(src="audios/alicia.mp3","audio/mp3",autoplay = F, controls = T))})
  
  output$rawaudio <- renderUI({ 
    
    audiolink()
    
  })
  
  output$rawaudio2 <- renderUI({ 
    req(input$oralstatify)
    p <- ip2()%>%filter(!!input$var1 %in% !!input$filter1)%>%mutate(varx=!!input$var1)%>%select(varx)
    p <- p %>%group_by(varx)%>%summarise(n())
    
    div(tags$audio(src=paste("audios/",p$varx,".mp3",sep=""),type = "audio/mp3",autoplay = NA, controls = NA))
    
  })
  
  # Modal buttons -----------------------------------------------------------
  
  
  observeEvent(input$tutorial1button, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/SoELWc8N3zI" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),

      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialngrams, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/cYQtYB7jy1c" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialfreqgroups, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/yr50xf4P36s" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialdescriptive, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/S76eL60tK4o" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialcrosstabs, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/BLCaNHQNrh0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialcorrelations, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/f12FRY8h-uk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialanova, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/Z1UkCSTyNN4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialtree, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/EfG83YzfQf4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialpca, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/0T992O4VzjE" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialmfa, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/MBnAQR-W-H8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialheatmap, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/_TFmIVQCq_E" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutoriallogistic, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/XGs72eONbWc" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialpitchraw, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/dl47glgtC68" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutoriallinear, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/FsPa4JowvDI" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$tutorialpitchcontour, {
    showModal(modalDialog(
      title = "Tutorial",
      HTML(paste0('<iframe width="100%" height="500px" src="https://www.youtube.com/embed/urTTBmb-CH8" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$oralstatify, {
    showModal(modalDialog(
      title = "Database selected/created",
      "If you have selected Phonopoliteness or Pico de Oro/Padre Zorro database, that will be the data used along the platform. If you decided to create your data, nothing will happen if you do not upload files below at create section",
      easyClose = TRUE
    ))
  })
  
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
  
  output$varmeansui <- renderUI({
    
    varSelectInput("varmeans","means",multiple = TRUE,data = ip2()%>%select(where(is.numeric)))
    
  })
  
  output$dforiginal <- renderDataTable({datatable(tabbeddf(),extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  output$dftransform <- renderDataTable({
    
    d <- dfprosody()%>%mutate_if(is.numeric, round, 2)
 
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  
  
  output$dftransform2 <- renderDataTable({
    d <- ipjoined()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  output$phonemesdt <- renderDataTable({
    d <- phonemesdf()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  output$vowelsdt <- renderDataTable({
    d <- vowelsdf()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  output$wordsdt <- renderDataTable({
    
    d <- wordsdf()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))
    
    
  })
  output$wordsdt2 <- renderDataTable({
    
    d <- wordsdfjoined()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))
    
  })
  output$collapse1dt <- renderDataTable({
    d <- collapse1df()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  output$collapse1dtjoined <- renderDataTable({
    d <- collapse1joined()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20),search=list(regex = TRUE)))})
  
  output$collapse2dt <- renderDataTable({
    d <- collapse2df()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20)))})
  output$collapse2dtjoined <- renderDataTable({
    d <- collapse2joined()%>%mutate_if(is.numeric, round, 2)
    datatable(d,extensions=c("Buttons","ColReorder"), filter="top",selection = 'single',options = list(colReorder = TRUE,scrollX = TRUE,dom = 'Blfrtip',   buttons = c('copy', 'csv', 'excel'),scrollY = "250px",     searchHighlight = TRUE,pageLength = 15,   lengthMenu = c(5, 10, 15, 20),search=list(regex = TRUE)))})
  
  # Download sections -------------------------------------------------------
  
  output$downloaddataxlsx <- downloadHandler(
    
    filename = function() {
      paste("oralstats-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      write.xlsx2(databaseexport(), file)
    }
    
  )
  
  output$downloadprosodycsv <- downloadHandler(
    
    filename = function() {
      paste("prosodydf-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv2(pitchintensitydf()%>%select(filename,time, pitch, pitchst,intensity), file)
      
    }
    
  )
  
  output$downloadipxlsx <- downloadHandler(
    
    filename = function() {
      paste("ipdf-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx2(dfprosody(), file)
      
    }
    
  )
  
  output$downloadipjoinedxlsx <- downloadHandler(
    
    filename = function() {
      paste("ipjoineddf-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx2(ipjoined(), file)
      
    }
    
  )
  
  output$downloadwordsjoinedxlsx <- downloadHandler(
    
    filename = function() {
      paste("wordsjoineddf-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx2(wordsdfjoined(), file)
      
    }
    
  )
  
  output$downloadwordsxlsx <- downloadHandler(
    
    filename = function() {
      paste("wordsdf-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx2(wordsdf(), file)
      
    }
    
  )
  
  output$downloadphonemesxlsx <- downloadHandler(
    
    filename = function() {
      paste("phonemesdf-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx2(phonemesdf(), file)
      
    }
    
  )
  
  output$downloadvowelsxlsx <- downloadHandler(
    
    filename = function() {
      paste("vowelsdf-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv2(vowelsdf(), file)
      
      
    }
    
  )
  
  output$downloadcollapse1xlsx <- downloadHandler(
    
    filename = function() {
      paste("collapse1df-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx2(collapse1df(), file)
      
      
    }
    
  )
  
  output$downloadcollapse2xlsx <- downloadHandler(
    
    filename = function() {
      paste("collapse2df-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx2(collapse2df(), file)
      
      
    }
    
  )
  
  output$downloaddatards <- downloadHandler(
    
    filename = function() {
      paste("oralstats-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      write_rds(databaseexport(), file)
      
    }
    
  )
  
  output$downloadipjoinedrds <- downloadHandler(
    
    filename = function() {
      paste("ipjoineddf-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      write_rds(ipjoineddf(), file)
      
      
    }
    
  )
  
  output$downloadwordsjoinedrds <- downloadHandler(
    
    filename = function() {
      paste("wordsjoineddf-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      write_rds(wordsdfjoined(), file)
      
      
    }
    
  )
  
  output$downloadiprds <- downloadHandler(
    
    filename = function() {
      paste("ipdf-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      write_rds(dfprosody(), file)
      
      
    }
    
  )
  
  output$downloadwordsrds <- downloadHandler(
    
    filename = function() {
      paste("wordsdf-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      write_rds(wordsdf(), file)
      
    }
    
  )
  
  output$downloadphonemesrds <- downloadHandler(
    
    filename = function() {
      paste("phonemesdf-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      write_rds(phonemesdf(), file)
      
    }
    
  )
  
  output$downloadvowelsrds <- downloadHandler(
    
    filename = function() {
      paste("vowelsdf-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      write_rds(vowelsdf(), file)
      
    }
    
  )

  output$downloadcollapse1rds <- downloadHandler(
    
    filename = function() {
      paste("collap1df-", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      write_rds(collapse1df(), file)
      
    })
    
    output$downloadcollapse1joinedlrds <- downloadHandler(
      
      filename = function() {
        paste("collap1df-", Sys.Date(), ".rds", sep="")
      },
      content = function(file) {
        write_rds(collapse1joined(), file)
        
      }
    
  )
    output$downloadcollapse1joinedxlsx <- downloadHandler(
      
      filename = function() {
        paste("collapse1joineddf-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx2(collapse1joined(), file)
        
        
      }
      
    )
    
    output$downloadcollapse2joinedrds <- downloadHandler(
      
      filename = function() {
        paste("collapse2joineddf-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx2(collapse2joined(), file)
        
        
      }
      
    )
    
    output$downloadcollapse2joinedxlsx <- downloadHandler(
      
      filename = function() {
        paste("collapse2joineddf-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        write.xlsx2(collapse2joined(), file)
        
        
      }
      
    )
  
}         
shinyApp(ui, server)