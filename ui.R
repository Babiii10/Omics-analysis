library(shiny)

THEMES = c( "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal",
            "litera", "lumen", "lux", "materia", "minty", "pulse", "sandstone", "simplex", "sketchy",
            "slate", "solar", "spacelab", "superhero", "united", "yeti",
            "quartz", "morph", "minty", #"vapor" ,
            "superhero")
shinyUI(fluidPage(
  theme = bslib::bs_theme(
     bootswatch = "cerulean",
     #primary = "#EA80FC", 
     secondary = "#48DAC6"
    ),
  # div(input_dark_mode(id = "mode",
  #                     mode = "light"),
  #      style = "font-size:15px; color:grey; text-align:right;"
  #      ),
  # 
  # titlePanel(
  #   tags$div(
  #     #"Omics analysis",
  #     tags$div(
  #       class = "float-end",
  #       style = "margin-top: -42px; min-width: 150px;", 
  #       selectInput( "theme_app", 
  #                     # label = tags$strong("Themes"),
  #                     label =  "Menu theme",
  #                     choices = setNames(THEMES, tools::toTitleCase(THEMES)), 
  #                     selected = "cerulean"
  #       )
  #     )
  #   )
  # ),
  
  # Application title
  
  titlePanel(
    div(
      img(src = 'dna-structure.png', height = "40px", width = "40px"),
      "Omics analysis"
     ) 
    ),
  hr(nrow = 2),
  sidebarLayout(
    sidebarPanel(
      wellPanel( 
        conditionalPanel(condition ="input.confirmdatabutton==0" ,
                         radioButtons("analysis","",c("new analysis","previous analysis"),inline=T),
                         conditionalPanel( condition="input.analysis=='previous analysis' ",     
                                           fileInput("modelfile",label=h4("previous analysis"),accept=".RData")
                         ), 
                         conditionalPanel(condition="input.analysis=='new analysis' ",
                                          fluidRow(
                                            column(12,br(),radioButtons("filetype", "Extention of the file",c("csv" = "csv", "xlsx" = "xlsx"),inline = TRUE))
                                          ),
                                          fluidRow(
                                            column(12,conditionalPanel(condition ="input.help",
                                                                       helpText("Learning file is obligatory to continue")
                                            ),
                                            fileInput("learningfile", 
                                                      label = h4("learning File"),
                                                      accept =  c("text/csv",
                                                                  "application/vnd.ms-excel",
                                                                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                                                  ".xls",".xlsx"))
                                            )
                                            ,
                                            column(12,
                                                   fileInput("validationfile", label = h4("validation File "),accept =  c("text/csv","application/vnd.ms-excel","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx")) 
                                            )
                                          ),
                                          fluidRow(
                                            conditionalPanel(condition ="input.filetype=='csv' ",column(6,textInput('dec', 'character for decimal point',value = "." ))),
                                            column(6,textInput("NAstring", label = "characters for missing values",value = "NA"))
                                          ),   
                                          fluidRow(
                                            conditionalPanel(condition ="input.filetype=='csv' ",
                                                             radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),inline = TRUE )),
                                            conditionalPanel(condition ="input.filetype=='xlsx' ",
                                                             column(6,numericInput("skipn",label = "number of lines to skip",value = 0)),
                                                             column(6,numericInput("sheetn",label = "sheet",value = 1)))
                                          ),hr(),
                                          checkboxInput("transpose","Transpose the table",FALSE),
                                          checkboxInput("zeroegalNA","consider 0 as NA",FALSE)
                         )
                         ,
                         actionButton("confirmdatabutton","Confirm data", 
                                      style = "background-color: #63BFBF;
                                  color: white;
                                  border-color: #63BFBF;"),
                         conditionalPanel(condition ="input.help",
                                          helpText("Data has to be confirm to continue"))
        ),
        conditionalPanel(condition ="input.confirmdatabutton!=0",
                         h4("Learning data"),
                         textOutput("namefilelearn",inline=T), tags$head(tags$style("#namefilelearn{color: grey;font-size: 15px;font-style: italic;}")),
                         br(),
                         textOutput("dim1learn",inline=T), "lines (individuals)",
                         br(),
                         textOutput("dim2learn",inline=T), "columns (variables)",
                         br()
        ),
        conditionalPanel(condition ="input.confirmdatabutton!=0 & output.fileUploadedval",
                         h4("Validation data"),
                         textOutput("namefileval",inline=T), tags$head(tags$style("#namefileval{color: grey;font-size: 15px;font-style: italic;}")),
                         br(),
                         textOutput("dim1val",inline=T), "lines (individuals)",br(),
                         textOutput("dim2val",inline=T), "columns (variables)",
                         br()
        ), 
        conditionalPanel(condition ="input.confirmdatabutton!=0",
                         hr(),
                         fluidRow(
                           column(3,checkboxInput("invers", "inverse" , value = FALSE)),
                           column(8,
                                  p(textOutput("positif",inline=T),HTML( '&#x21D2;'), "case ",br(),
                                    textOutput("negatif",inline=T),HTML( '&#x21D2;'), "control",align="center")
                           )
                         ),
                         hr(),
                         radioButtons("paramdownplot","Download images as",choices=list("png"="png","jpg"="jpg","pdf"="pdf"),selected="png"),
                         radioButtons("paramdowntable","Download datasets as",choices=list("csv"="csv","xlsx"="xlsx"),selected="csv"),
                         hr(),
                         downloadButton("savestate","Save settings RData",class = "dlButton"),
                         hr(),
                         downloadButton("savestatetable","Save settings table and main results",class = "dlButton")
                         
        ),
        hr(),
        checkboxInput("help","show help",FALSE)
      ),width=3      
    ) ,
    
    mainPanel(
      conditionalPanel(condition ="!output.fileUploaded & !output.modelUploaded",
                       h3("The purpose of this application is to provide a user-friendly tool to build a prediction model from omics datas.",
                          align="center"),
                       h4("Check the box 'show help' for any further informations."),br(),br(),br(),
                       
                       
                       fluidRow(column(6,imageOutput("image1")),column(2,imageOutput("image2"))),
                       br(),
                       h4("This application is developped in the 12th team of I2MC for internal used.",align="center")
                       
      ),           
      conditionalPanel(condition ="output.fileUploaded || output.modelUploaded",
                       tabsetPanel(id = "data",              
                                   tabPanel("Learning Data", icon = icon("clipboard-list"),
                                            br(),
                                            conditionalPanel(condition ="input.help",
                                                             fluidRow(
                                                               column(5,br(),helpText("To verify if the import parameters are correct : the first column has to be the names of the individual, 
                                       the second the groups. Others are the datas."), 
                                                                      helpText("Non attributes values have to appears empty, "),
                                                                      helpText()
                                                               ),
                                                               column(7,imageOutput("image3",width = "100%")))
                                            ),
                                            dataTableOutput("JDDlearn")%>% withSpinner(color="#0dc5c1",type = 1),
                                            p(downloadButton("downloaddataJDDlearn","Download dataset"),align="center")
                                   ),
                                   tabPanel("Validation Data", icon = icon("check"),
                                            conditionalPanel(condition ="output.fileUploadedval",
                                                             br(),
                                                             dataTableOutput("JDDval")%>% withSpinner(color="#0dc5c1",type = 1),
                                                             p(downloadButton("downloaddataJDDval","Download dataset"),align="center")
                                            )
                                   ),
                                   
                                   tabPanel("Select Data", icon = icon("filter"),
                                            conditionalPanel(condition ="input.help",
                                                             helpText(" Select variables to extract variables from the learning dataset according to the number or the structure of Non-Attribute values (missing values)")
                                            ),  
                                            fluidRow(
                                              column(7, numericInput("prctvalues","Percentage minimum of values" , 0, min = 0, max = 100, step = 5),
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("")),br(),
                                                     checkboxInput("NAstructure", "Select variables with a NA's structure " , value = FALSE),
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("The structure test is a proportion test of the Non Attributes values between the 2 groups."))
                                              ),
                                              column(5,radioButtons("selectmethod","Methods of selection ",c("selection on all samples"="nogroup","each group has more than x% of values "="bothgroups","at least one group has more than x% of more"="onegroup")),
                                                     conditionalPanel(condition ="input.help",helpText("3 ways of selection : select variables which got at least x% of values in all samples, "),
                                                                      helpText("                select variables which which have more than x% in the two groups"),
                                                                      helpText("                select variables which have at leat one group whith more than x% of values"))
                                              )
                                              
                                            ),p(downloadButton('downloaddataselect', 'Download data selected'),align="center"),
                                            hr(),
                                            
                                            fluidRow(
                                              column(7,textOutput("nvarselect",inline=T), "selected variables" ,
                                                     plotOutput("heatmapNA",width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1) ,
                                                     p(downloadButton("downloadplotheatmapNA","Download plot"),downloadButton('downloaddataheatmapNA', 'Download raw data'),align="center")
                                              ),
                                              column(5,br(),
                                                     conditionalPanel(condition ="input.help",helpText("The 3 curves present the number of variables selected according to the three possible options and the % of Na's selected"))
                                                     ,
                                                     plotOutput("plotNA",width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotNA","Download plot"),downloadButton('downloaddataplotNA', 'Download raw data'),align="center")
                                              )
                                            ),
                                            hr(),
                                            fluidRow(
                                              column(6,  
                                                     conditionalPanel(condition ="input.NAstructure==true",
                                                                      conditionalPanel(condition ="input.help",helpText("Consider the NA in the group with less values as real 0 (replace by 0) the NA in the group with more values are raplace by the solution chosen later")),
                                                                      numericInput("thresholdNAstructure","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005))
                                              ),
                                              conditionalPanel(condition ="input.NAstructure==true",
                                                               column(6,radioButtons("structdata", "search structure in : ",c("all dataset" = "alldata","selected dataset" = "selecteddata"))))
                                            ), 
                                            hr(),
                                            fluidRow(
                                              conditionalPanel(condition ="input.NAstructure==true", 
                                                               column(9,textOutput("nstructuredfeatures",inline=T),"structured features",
                                                                      plotOutput("heatmapNAstructure" ,width = "95%",height = 600)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadstructur","Download plot"),downloadButton('downloaddatastructur', 'Download raw data'),align="center")),
                                                               column(3,br(),br(),
                                                                      numericInput("maxvaluesgroupmin","The group with the minimum number of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                                                                      numericInput("minvaluesgroupmax","The group with the maximum number of values has at least y% of values",value = 75,min = 0,max = 100,step = 5))
                                              )
                                            )
                                   ),
                                   tabPanel("Transform Data", icon =  icon("magic"),
                                            conditionalPanel(condition ="input.help",
                                                             helpText("")),
                                            fluidRow(
                                              column(5,radioButtons("rempNA", "Replacing NA (Not Attributes) by:",
                                                                    c("zero" = "z","mean of the cohort" = "moy",
                                                                      "mean by group"="moygr","PCA estimation" = "pca","Random forest estimation /!\\" = "missforest")),
                                                     helpText("/!\\ process can be long"),
                                                     
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("Random Forest can "))),
                                              column(3,
                                                     checkboxInput("log","transform data in log",FALSE),
                                                     checkboxInput("standardization","standardization dataset",FALSE),
                                                     conditionalPanel(condition ="input.help",helpText("dividing the columns quadratic mean")),
                                                     checkboxInput("arcsin","arcsine transformation",FALSE),
                                                     conditionalPanel(condition ="input.help",helpText("each column is rescaled between 1 and 0, and arcsin transformation is applying"))
                                              ),
                                              column(4,
                                                     conditionalPanel(condition="input.log",
                                                                      radioButtons("logtype",label = NULL,c("ln"="logn","log 10"="log10","log2"="log2"),inline = TRUE)))
                                            ),p(downloadButton('downloaddatatransform', 'Download transform data '),align="center"),
                                            hr(),
                                            
                                            fluidRow(
                                              column(5,plotOutput("plotheatmaptransformdata" ,width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotheatmap","Download plot"),
                                                       downloadButton('downloaddataheatmap', 'Download raw data'),align="center")),  
                                              
                                              column(7,conditionalPanel(condition ="input.help",
                                                                        helpText("The mds (MultiDimensionnal Scaling) calcul the distances between the individuals (rows) and represented it on a plan as well as possible."),
                                                                        helpText("The aim of this graphic is to vizualized if the selection and transform parameters separate well the 2 groups.")),
                                                     plotOutput("plotmds",height=500,width = "100%")%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotmds","Download plot"),
                                                       downloadButton('downloaddatamds', 'Download raw data'),align="center"))),
                                            plotOutput("plothist",height=500,width = "100%")%>% withSpinner(color="#0dc5c1",type = 1),
                                            p(downloadButton("downloadplothist","Download plot"),
                                              downloadButton('downloaddatahist', 'Download raw data'),align="center")
                                   ),
                                   tabPanel("Statistics", icon =  icon("calculator"),
                                            conditionalPanel(condition ="input.help",
                                                             helpText("")),
                                            fluidRow(
                                              column(6,
                                                     radioButtons("test", "Variable Selection Methods",
                                                                  c("No test"="notest",
                                                                    "Wilcoxon Test (univariate)" = "Wtest",
                                                                    # "Clustering + ElasticNet (multivariate)" = "clustEnet",
                                                                    "Student Test (univariate)" = "Ttest",
                                                                    "Lasso (multivariate)" = "lasso",
                                                                    "ElasticNet (multivariate)" = "elasticnet"
                                                                    # ,
                                                                    # "Ridge (multivariate)" = "ridge"
                                                                    )
                                                                  ),
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("Univariate tests: Wilcoxon (non-parametric) and Student (parametric) test each variable independently."),
                                                                      helpText("Multivariate methods: Lasso, ElasticNet, and Ridge use regularization to select variables while considering their joint effects."),
                                                                      helpText("Clustering + ElasticNet: Clusters correlated variables, selects best from each cluster, then applies bootstrap ElasticNet for robust selection.")
                                                                      ),
                                                     checkboxInput("SFtest","Shapiro and Fisher Tests",F),
                                                     conditionalPanel(condition ="input.help",helpText("The shapiro test is a test of normallity. The F test is a test of equality of variance."))
                                              ),
                                              column(6,br(),
                                                     conditionalPanel(condition ="input.test=='Wtest' || input.test=='Ttest'",
                                                                      numericInput("thresholdFC","Fold change threshold" , 0, min =0, max = 5, step = 0.5),
                                                                      conditionalPanel(condition ="input.help",helpText("Fold Change is the ratio of means between groups.")),
                                                                      numericInput("thresholdpv","p-value threshold" , 0.05, min =0, max = 1, step = 0.01),
                                                                      checkboxInput("adjustpv", "adjust p-value " , value = FALSE),
                                                                      conditionalPanel(condition ="input.help", helpText("Benjamini & Hochberg correction"))
                                                     ),
                                                     conditionalPanel(condition ="input.test=='clustEnet'",
                                                                      h5("Clustering + ElasticNet Parameters"),
                                                                      numericInput("nclusters","Number of clusters" , 100, min =10, max = 500, step = 10),
                                                                      conditionalPanel(condition ="input.help",helpText("Number of clusters for hierarchical clustering of correlated variables.")),
                                                                      numericInput("nbootstrap","Bootstrap iterations" , 500, min =100, max = 2000, step = 100),
                                                                      conditionalPanel(condition ="input.help",helpText("Number of bootstrap samples for ElasticNet selection.")),
                                                                      numericInput("alphaclustenet","Alpha (ElasticNet mixing)" , 0.5, min =0, max = 1, step = 0.1),
                                                                      conditionalPanel(condition ="input.help",helpText("Alpha=1: Lasso, Alpha=0: Ridge, 0<Alpha<1: ElasticNet")),
                                                                      numericInput("minselectionfreq","Minimum selection frequency" , 0.5, min =0, max = 1, step = 0.05),
                                                                      conditionalPanel(condition ="input.help",helpText("Minimum proportion of bootstrap iterations a variable must be selected to be included in final results.")),
                                                                      checkboxInput("preprocessclustenet","Preprocess variables",TRUE),
                                                                      conditionalPanel(condition ="input.help",helpText("Filter low variance and low frequency variables before clustering."))
                                                     ),
                                                     conditionalPanel(condition ="input.test=='lasso' || input.test=='elasticnet' || input.test=='ridge'",
                                                                      h5("Regularization Parameters"),
                                                                      conditionalPanel(condition ="input.test=='elasticnet'",
                                                                                       numericInput("alphaselection","Alpha (ElasticNet mixing)" , 0.5, min =0, max = 1, step = 0.1),
                                                                                       conditionalPanel(condition ="input.help",helpText("Alpha=1: Lasso, Alpha=0: Ridge, 0<Alpha<1: ElasticNet"))
                                                                      ),
                                                                      checkboxInput("autolambda", "Automatic lambda selection (CV)" , value = TRUE),
                                                                      conditionalPanel(condition ="!input.autolambda",
                                                                                       numericInput("lambdaselection","Lambda (regularization strength)" , 0.01, min =0, max = 10, step = 0.01)
                                                                      ),
                                                                      conditionalPanel(condition ="input.help",helpText("Lambda controls the strength of regularization. Use automatic selection via cross-validation for optimal results."))
                                                     )
                                              )
                                            ),br(),
                                            p(downloadButton('downloaddatastatistics', 'Download statistics'),downloadButton('downloadddatadiff', 'Download differently expressed variables'),align="center"),
                                            hr(),
                                            conditionalPanel(condition= "input.test== 'Wtest' || input.test== 'Ttest'",
                                                             fluidRow(
                                                               column(6,
                                                                      textOutput("nvarselect2",inline=T), "selected variables",
                                                                      plotOutput("volcanoplot" ,width = 500,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadvolcanoplot","Download plot"),downloadButton('downloaddatavolcanoplot', 'Download raw data'),align="center")
                                                               ),
                                                               column(6,
                                                                      textOutput("nbdiff",inline=T), "differently expressed",
                                                                      plotOutput("barplottest" ,width = 400,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),   
                                                                      p(downloadButton("downloadbarplottest","Download plot"),downloadButton('downloaddatabarplottest', 'Download raw data'),align="center")
                                                               )
                                                             )
                                            )
                                            ,
                                            conditionalPanel(condition= "input.test=='clustEnet'",
                                                             fluidRow(
                                                               column(12,
                                                                      h4("Clustering + ElasticNet Selection Results"),
                                                                      textOutput("nbclustenetselected",inline=T), " variables selected",br(),br(),
                                                                      h5("Method Parameters"),
                                                                      fluidRow(
                                                                        column(3, strong("Clusters:"), textOutput("clustenetnclusters",inline=T)),
                                                                        column(3, strong("Bootstrap iterations:"), textOutput("clustenetnbootstrap",inline=T)),
                                                                        column(3, strong("Alpha:"), textOutput("clustenetalphaused",inline=T)),
                                                                        column(3, strong("Min. selection freq.:"), textOutput("clustenetminfreq",inline=T))
                                                                      ),br(),
                                                                      dataTableOutput("clustenetresultstable")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton('downloadclustenetresults', 'Download clustering results'),align="center")
                                                               ),
                                                               plotOutput("PcaVarsSel") %>% withSpinner(color = "#0dc5c1",type = 1),
                                                               p(downloadButton("donwloadPCAPlot", "download the plot"), align  =  'center')
                                                             )
                                            )
                                            ,
                                            conditionalPanel(condition= "input.test=='lasso' || input.test=='elasticnet' || input.test=='ridge'",
                                                             fluidRow(
                                                               column(12,
                                                                      h4("Multivariate Selection Results"),
                                                                      textOutput("nbmultivariateselected",inline=T), " variables selected",br(),br(),
                                                                      conditionalPanel(condition ="input.autolambda",
                                                                                       h5("Optimal Hyperparameters (Cross-Validation)"),
                                                                                       fluidRow(
                                                                                         column(4, strong("Lambda (optimal):"), textOutput("optimallambda",inline=T)),
                                                                                         column(4, strong("Lambda (1SE):"), textOutput("lambda1se",inline=T)),
                                                                                         column(4, conditionalPanel(condition ="input.test=='elasticnet'",
                                                                                                                    strong("Alpha:"), textOutput("alphaused",inline=T)))
                                                                                       ),br()
                                                                      ),
                                                                      dataTableOutput("multivariateresultstable")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton('downloadmultivariateresults', 'Download multivariate results'),align="center")
                                                               )
                                                             )
                                            )
                                            ,
                                            conditionalPanel(condition ="input.SFtest==true  ",
                                                             column(6,conditionalPanel(condition ="input.help",
                                                                                       helpText("Barplot presents the Results of shapiro and Fisher test")),
                                                                    plotOutput("plottestSF")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                    p(downloadButton("downloadplottestSF","Download plot"),downloadButton('downloaddatatestSF', 'Download raw data'),align="center"))
                                                             
                                            )
                                   )
                                   ,
                                   tabPanel("Model", icon = icon("cogs"),
                                            fluidRow(
                                              column(4,
                                                     radioButtons("model", "Type of model to adjust",
                                                                  c("No model" = "nomodel",
                                                                    "Random Forest"="randomforest",
                                                                    "Support Vector Machine" = "svm",
                                                                    "Penalized Logistic Regression (ElasticNet)"="elasticnet",
                                                                    "XGBoost"="xgboost",
                                                                    #"LightGBM"="lightgbm",
                                                                    "Naive Bayes"="naivebayes",
                                                                    "K-Nearest Neighbors (KNN)"="knn"))
                                                     #,
                                                     # conditionalPanel(condition ="input.help",
                                                     #                  helpText("Random Forest: ensemble method with automatic mtry tuning."),
                                                     #                  helpText("SVM: automatic hyperparameter tuning (C, gamma)."),
                                                     #                  helpText("ElasticNet: penalized logistic regression with L1/L2 regularization."),
                                                     #                  helpText("XGBoost: gradient boosting with automatic hyperparameter tuning.")
                                                     #                  )
                                              ),
                                              column(4,
                                                     numericInput("thresholdmodel","Threshold model" ,0, min = -1, max = 1, step = 0.05),
                                                     conditionalPanel(condition ="input.help", helpText("The threshold of the score is used for classification")),
                                                     fluidRow(
                                                       column(12,checkboxInput("fs","features selection by cross validation /!\\ ",F))
                                                     ),
                                                     helpText("/!\\ process can be long")
                                              ),
                                              column(4,
                                                     conditionalPanel(condition ="input.model=='elasticnet'",
                                                                      h5("ElasticNet Hyperparameters"),
                                                                      radioButtons("tuning_method_en", "Tuning method:",
                                                                                   c(
                                                                                     #"Manual parameters" = "manual",
                                                                                     "Cross-validation (cv.glmnet)" = "traditional",
                                                                                     "GridSearchCV (superml)" = "gridsearch"),
                                                                                   selected = "traditional"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Manual: set alpha/lambda manually"),
                                                                                       helpText("cv.glmnet: optimizes lambda for fixed alpha"),
                                                                                       helpText("GridSearchCV: jointly optimizes alpha and lambda")),
                                                                      conditionalPanel(condition ="input.tuning_method_en=='manual' || input.tuning_method_en=='traditional'",
                                                                                       numericInput("alphamodel","Alpha (mixing parameter)" , 0.5, min =0, max = 1, step = 0.1),
                                                                                       conditionalPanel(condition ="input.help",helpText("Alpha=1: Lasso (L1), Alpha=0: Ridge (L2), 0<Alpha<1: ElasticNet"))
                                                                      ),
                                                                      conditionalPanel(condition ="input.tuning_method_en=='manual'",
                                                                                       numericInput("lambdamodel","Lambda" , 0.01, min =0, max = 10, step = 0.01)
                                                                      )
                                                     ),
                                                     conditionalPanel(condition ="input.model=='randomforest'",
                                                                      h5("Random Forest Hyperparameters"),
                                                                      radioButtons("tuning_method_rf", "Tuning method:",
                                                                                   c("Manual parameters" = "manual",
                                                                                     "tuneRF" = "traditional",
                                                                                     "GridSearchCV (superml)" = "gridsearch"),
                                                                                   selected = "traditional"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Manual: set mtry manually"),
                                                                                       helpText("tuneRF: optimizes mtry only"),
                                                                                       helpText("GridSearchCV: optimizes ntree, mtry, nodesize")),
                                                                      numericInput("ntreerf","Number of trees" , 1000, min =100, max = 5000, step = 100),
                                                                      conditionalPanel(condition ="input.tuning_method_rf=='manual'",
                                                                                       numericInput("mtryrf","mtry (variables per split)" , 5, min =1, max = 100, step = 1),
                                                                                       conditionalPanel(condition ="input.help",
                                                                                                        helpText("Number of variables randomly sampled at each split"))
                                                                      )
                                                                      # ,
                                                                      # conditionalPanel("input.help" #, 
                                                                      #                  
                                                                      # )
                                                     ),
                                                     conditionalPanel(condition ="input.model=='svm'",
                                                                      h5("SVM Hyperparameters"),
                                                                      checkboxInput("autotunesvm", "Automatic hyperparameter tuning (tune.svm)" , value = FALSE),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Automatically find optimal cost and gamma")
                                                                                       ),
                                                                      conditionalPanel(condition ="!input.autotunesvm",
                                                                                       numericInput("costsvm","Cost (C)" , 1, min =0.001, max = 100, step = 0.1),
                                                                                       numericInput("gammasvm","Gamma" , 0.1, min =0.00001, max = 10, step = 0.01),
                                                                                       selectInput("kernelsvm", "Kernel type:",
                                                                                                  c("Radial" = "radial",
                                                                                                    "Linear" = "linear",
                                                                                                    "Polynomial" = "polynomial",
                                                                                                    "Sigmoid" = "sigmoid"),
                                                                                                  selected = "radial"),
                                                                                       conditionalPanel(condition ="input.help",
                                                                                                        helpText("Manually set SVM hyperparameters")
                                                                                                        )
                                                                      )
                                                                      # ,
                                                                      # conditionalPanel(condition ="input.help"
                                                                      #                  
                                                                      # )
                                                     ),
                                                     conditionalPanel(condition ="input.model=='xgboost'",
                                                                      h5("XGBoost Hyperparameters"),
                                                                      radioButtons("tuning_method_xgb", "Tuning method:",
                                                                                   c("Manual parameters" = "manual",
                                                                                     "Cross-validation (xgb.cv)" = "traditional",
                                                                                     "GridSearchCV (superml)" = "gridsearch"),
                                                                                   selected = "traditional"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Manual: set all parameters manually"),
                                                                                       helpText("xgb.cv: basic cross-validation"),
                                                                                       helpText("GridSearchCV: comprehensive multi-parameter tuning")),
                                                                      conditionalPanel(condition ="input.tuning_method_xgb=='manual'",
                                                                                       bslib::tooltip(
                                                                                       numericInput("nroundsxgb","Number of rounds" , 100, min =10, max = 1000, step = 10),
                                                                                       placement =  "right",
                                                                                       htmltools::span("Also known as num_boost_round; defines the number of boosting iterations.")
                                                                                       ),
                                                                                       numericInput("maxdepthxgb","Max depth" , 6, min =1, max = 20, step = 1),
                                                                                       numericInput("etaxgb","Learning rate (eta)" , 0.3, min =0.01, max = 1, step = 0.01),
                                                                                       conditionalPanel(condition ="input.help",helpText("Manually set XGBoost hyperparameters"))
                                                                      )
                                                                      # ,
                                                                      # conditionalPanel("input.help" 
                                                                      #  
                                                                      # )
                                                     ),
                                                     conditionalPanel(condition ="input.model=='lightgbm'",
                                                                      h5("LightGBM Hyperparameters"),
                                                                      checkboxInput("autotunelgb", "Automatic hyperparameter tuning (CV)" , value = TRUE),
                                                                      conditionalPanel(condition ="input.help",helpText("Automatically find optimal parameters via cross-validation")),
                                                                      conditionalPanel(condition ="!input.autotunelgb",
                                                                                       numericInput("nroundslgb","Number of rounds" , 100, min =10, max = 1000, step = 10),
                                                                                       numericInput("numleaves","Num leaves" , 31, min =10, max = 200, step = 5),
                                                                                       numericInput("learningratelgb","Learning rate" , 0.05, min =0.001, max = 0.5, step = 0.01),
                                                                                       conditionalPanel(condition ="input.help",helpText("Manually set LightGBM hyperparameters"))
                                                                      )
                                                     ),
                                                     
                                                     conditionalPanel(condition ="input.model=='naivebayes'",
                                                                      h5("Naive Bayes Hyperparameters"),
                                                                      radioButtons("tuning_method_nb", "Tuning method:",
                                                                                   c("No tuning (laplace=0)" = "manual",
                                                                                     "GridSearchCV (superml)" = "gridsearch"),
                                                                                   selected = "manual"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("No tuning: uses default laplace=0"),
                                                                                       helpText("GridSearchCV: optimizes laplace smoothing parameter"))
                                                                      #,  conditionalPanel("input.help" )
                                                     ),

                                                     conditionalPanel(condition ="input.model=='knn'",
                                                                      h5("KNN Hyperparameters"),
                                                                      radioButtons("tuning_method_knn", "Tuning method:",
                                                                                   c("Manual parameters" = "manual",
                                                                                     "Cross-validation" = "traditional"
                                                                                     # ,
                                                                                     # "GridSearchCV (superml)" = "gridsearch"
                                                                                     ),
                                                                                   selected = "manual"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Manual: set k manually"),
                                                                                       helpText("Traditional CV: basic cross-validation for k"),
                                                                                       helpText("GridSearchCV: systematic grid search")),
                                                                      conditionalPanel(condition ="input.tuning_method_knn=='manual'",
                                                                                       numericInput("kneighbors","Number of neighbors (k)" , 5, min =1, max = 50, step = 2),
                                                                                       conditionalPanel(condition ="input.help",
                                                                                                        helpText("Manually set k parameter"))
                                                                      )
                                                                      ) 
                                                                  ) 
                                                
                                                    # )$$
                                                     ),
                                                      # affcihage des help en fonction du model
                                                      fluidRow(
                                                        conditionalPanel("input.help", 
                                                                          conditionalPanel(condition ="input.model=='knn'",
                                                                                        div(
                                                                                            class =  "myDiv",
                                                                                            h4("KNN Hyperparameter Explanation"),
                                                                                            p(strong("Number of Neighbors (k):"), "Determines how many nearest neighbors are considered when classifying a new data point. 
                                                                                           A smaller k can capture local patterns but may be sensitive to noise, while a larger k provides smoother decision boundaries but may overlook local nuances."
                                                                                            )
                                                                                          )
                                                                                          
                                                                         ),
                                                                         conditionalPanel(condition = "input.model== 'naivebayes'",
                                                                                          div(class = "myDiv",
                                                                                              h4("Naive Bayes Hyperparameter Explanation"),
                                                                                              p(strong("Laplace Smoothing:"), "A technique used to handle zero probabilities in categorical data. 
                                                                                               It adds a small constant (usually 1) to each count to ensure that no probability is exactly zero, 
                                                                                               which can be particularly useful when dealing with unseen features in the training data.") 
                                                                                          )
                                                                                        ),
                                                                         conditionalPanel(condition = "input.model=='svm'", 
                                                                                          tags$head(
                                                                                            tags$style(HTML("
                                                                                                      .myDiv {
                                                                                                        border: 5px outset green;
                                                                                                        background-color: lightblue;    
                                                                                                        text-align: left;
                                                                                                      }
                                                                                                    "))
                                                                                          ),
                                                                                          helpText("Lambda controls the strength of regularization. Use automatic 
                                                                                                selection via cross-validation for optimal results.")
                                                                                          ,
                                                                                          div(
                                                                                            class = "myDiv",
                                                                                            p("In SVM with RBF (Radial Basis Function) kernel, the hyperparameters C and gamma together control the complexity and generalisation capacity of the model.
                                                                                                They are linked in the sense that modifying one influences the effect of the other on the decision boundary."),
                                                                                            p("A high C value with a low gamma can lead to overfitting, while a low C with a high gamma may result in underfitting. 
                                                                                                 Therefore, it's crucial to tune both parameters together to find the optimal balance 
                                                                                              for the specific dataset."),
                                                                                            HTML("<ul>
                                                                                                    <li>High C + high gamma → very flexible model, high risk of overfitting. </li>
                                                                                                    <li>Low C + low gamma → very smooth model, risk of underfitting. </li>
                                                                                                    <li> High C + low gamma → model that tries to classify well but with a generally smooth boundary. </li>
                                                                                                    <li> Low C + high gamma → model that accepts errors but with complex local boundaries.  </li>
                                                                                                 </ul>"
                                                                                              
                                                                                            )
                                                                                            # ,
                                                                                            #   p("High C + high gamma → very flexible model, high risk of overfitting."),
                                                                                            #   p("Low C + low gamma → very smooth model, risk of underfitting."),
                                                                                            #   p("High C + low gamma → model that tries to classify well but with a generally smooth boundary."),
                                                                                            #   p("Low C + high gamma → model that accepts errors but with complex local boundaries. ")

                                                                                          )
                                                                                          ),
                                                                         conditionalPanel(condition ="input.model=='randomforest'",
                                                                                          div(
                                                                                            class =  "myDiv",
                                                                                            h4("Random Forest Hyperparameters Explanation"),
                                                                                            p(strong("ntree:"), "Number of trees in the forest; more trees can improve performance but increase computation time."),
                                                                                            p(strong("mtry:"), "Number of variables randomly sampled at each split; controls tree diversity and model robustness.")
                                                                                          )
                                                                                          ),
                                                                         conditionalPanel(condition ="input.model=='xgboost'",
                                                                                          div(
                                                                                            class =  "myDiv",
                                                                                            h4("XGBoost Hyperparameters Explanation"),
                                                                                            p(strong("nrounds:"), "Also known as num_boost_round; defines the number of boosting iterations."),
                                                                                            p(strong("max_depth:"), "Maximum depth of a tree; controls model complexity and overfitting."),
                                                                                            p(strong("eta (learning rate):"), "Step size shrinkage used to prevent overfitting; smaller values require more rounds.")
                                                                                          )
                                                                                          ),
                                                                         conditionalPanel(condition = "input.model=='elasticnet'", 
                                                                                          conditionalPanel(condition ="input.help",
                                                                                                           div(class =  "myDiv",
                                                                                                               h4("ElasticNet Hyperparameters Explanation"),
                                                                                                               p(strong("Alpha:"), "Controls the mix between L1 (Lasso) and L2 (Ridge) regularization.
                                                                                                                        Alpha=1 corresponds to Lasso, Alpha=0 to Ridge, and values in between represent a combination of both."),
                                                                                                               p(strong("Lambda:"), "Lambda controls the strength of regularization.
                                                                                                                        Use automatic selection via cross-validation for optimal results.")
                                                                                                               
                                                                                                           )
                                                                                          )  
                                                                                        )
                                                      ) # fin du grand conditionPanel
                                                      ),
                                                     conditionalPanel(condition ="output.fileUploadedval & input.model!='nomodel'  ",
                                                                      checkboxInput("adjustval","Adjust model on validation data",F)
                                                     )
                                                     ,
                                                     hr(),
                                                     conditionalPanel(condition ="input.model!='nomodel'  ",
                                                                      fluidRow(
                                                                        column(4,
                                                                               textOutput('nbselectmodel',inline=T),'selected variables',
                                                                               h3("Model Learning")
                                                                        ),
                                                                        column(4,br(),downloadButton('downloaddatalearning', 'Download learning data')),
                                                                        column(4,radioButtons("plotscoremodel", "",c( "boxplot"="boxplot","points" = "points")))
                                                                      ),
                                                                      conditionalPanel(condition ="input.model=='elasticnet' || input.model=='svm' || input.model=='randomforest' || input.model=='xgboost'",
                                                                                       fluidRow(
                                                                                         column(12,
                                                                                                h4("Optimal Hyperparameters (Training)"),
                                                                                                conditionalPanel(condition ="input.model=='elasticnet'",
                                                                                                                 div(
                                                                                                                   class =  "well",
                                                                                                                   style = 'color :  blue;',
                                                                                                                   fluidRow(
                                                                                                                     column(3, strong("Alpha:"), textOutput("modelalpha",inline=T)),
                                                                                                                     column(3, strong("Lambda:"), textOutput("modellambda",inline=T)),
                                                                                                                     column(3, strong("Lambda (1SE):"), textOutput("modellambda1se",inline=T)),
                                                                                                                     column(3, strong("Non-zero coef:"), textOutput("modelnonzerocoef",inline=T))
                                                                                                                   )
                                                                                                                 )
                                                                                                                 
                                                                                                ),
                                                                                                conditionalPanel(condition ="input.model=='svm'",
                                                                                                                 div(
                                                                                                                   class =  "well",
                                                                                                                   style = 'color :  blue;',
                                                                                                                   fluidRow(
                                                                                                                     column(4, strong("Cost (C):"), textOutput("svmcost",inline=T)),
                                                                                                                     column(4, strong("Gamma:"), textOutput("svmgamma",inline=T)),
                                                                                                                     column(4, strong("Kernel:"), textOutput("svmkernel",inline=T)
                                                                                                                            #"Radial"
                                                                                                                            )
                                                                                                                   )
                                                                                                                 )
                                                                                                ),
                                                                                                conditionalPanel(condition ="input.model=='randomforest'",
                                                                                                                 div(
                                                                                                                   class =  "well",
                                                                                                                   style = 'color :  blue;',
                                                                                                                   fluidRow(
                                                                                                                     column(4, strong("Optimal mtry:"), textOutput("rfmtry",inline=T)),
                                                                                                                     column(4, strong("Number of trees:"), textOutput("rfntree",inline=T)),
                                                                                                                     column(4, strong("Tuning method:"), "tuneRF")
                                                                                                                   )
                                                                                                                 )
                                                                                                ),
                                                                                                conditionalPanel(condition ="input.model=='xgboost'",
                                                                                                                 div(
                                                                                                                   class =  "well",
                                                                                                                   style = 'color :  blue;',
                                                                                                                   fluidRow(
                                                                                                                     column(3, strong("Optimal nrounds:"), textOutput("xgbnrounds",inline=T)),
                                                                                                                     column(3, strong("Max depth:"), textOutput("xgbmaxdepth",inline=T)),
                                                                                                                     column(3, strong("Learning rate (eta):"), textOutput("xgbeta",inline=T)),
                                                                                                                     column(3, strong("Min child weight:"), textOutput("xgbminchild",inline=T))
                                                                                                                   )
                                                                                                                 )
                                                                                                ),
                                                                                                conditionalPanel( condition = "input.model=='knn'",
                                                                                                             fluidRow(
                                                                                                               column(3,
                                                                                                                     strong("Optimal K in KNN:"), textOutput("knnk",inline=T)
                                                                                                                     )
                                                                                                             )
                                                                                                ),
                                                                                                hr()
                                                                                         )
                                                                                       )
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               plotOutput("plotmodeldecouvroc")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                               p(downloadButton("downloadplotdecouvroc","Download plot"),
                                                                                 downloadButton('downloaddatadecouvroc', 'Download raw data'),align="center")
                                                                        ),
                                                                        column(4,
                                                                               plotOutput("plotmodeldecouvbp")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                               p(downloadButton("downloadplotmodeldecouvbp","Download plot"),
                                                                                 downloadButton('downloaddatamodeldecouvbp', 'Download raw data'),align="center")
                                                                        ),
                                                                        column(2,
                                                                               conditionalPanel(condition="input.plotscoremodel=='points'",checkboxInput("shownames1","show indivuals names",value=FALSE)),
                                                                               br(),
                                                                               tableOutput("tabmodeldecouv"),
                                                                               "Sensibility = ",textOutput("sensibilitydecouv",inline=T), 
                                                                               br(),
                                                                               "Specificity = ",textOutput("specificitydecouv",inline=T),
                                                                               br(),hr(),br(),
                                                                               tableOutput("youndendecouv")
                                                                               
                                                                        )
                                                                      ),
                                                                      hr(),
                                                                      conditionalPanel(condition ="input.adjustval==true  ",
                                                                                       fluidRow(div(
                                                                                         column(6,h3("model validation"))
                                                                                         # , 
                                                                                         # column(6,br(),downloadButton('downloaddatavalidation', 'Download validation data'))
                                                                                         )
                                                                                       ), 
                                                                                       fluidRow(
                                                                                         column(6,plotOutput("plotmodelvalroc")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                                                p(downloadButton("downloadplotvalroc","Download plot"),
                                                                                                  downloadButton('downloaddatavalroc', 'Download raw data'),align="center")
                                                                                         ),
                                                                                         column(4,plotOutput("plotmodelvalbp")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                                                p(downloadButton("downloadplotmodelvalbp","Download plot"),
                                                                                                  downloadButton('downloaddatamodelvalbp', 'Download raw data'),align="center")
                                                                                         ),
                                                                                         column(2,
                                                                                                #conditionalPanel(condition="input.plotscoremodel=='points'",checkboxInput("shownames2","show indivuals names",value=FALSE)),
                                                                                                tableOutput("tabmodelval"),
                                                                                                "Sensibility = ",textOutput("sensibilityval",inline=T), 
                                                                                                br(),
                                                                                                "Specificity = ",textOutput("specificityval",inline=T),
                                                                                                br(),hr(),br(),
                                                                                                tableOutput("youndenval")
                                                                                                
                                                                                         )
                                                                                       )
                                                                      )
                                                     )
                                                     ),
                                              tabPanel("Details of the model", icon =  icon("file-alt"),
                                                       h3("Summary of the model"),
                                                       verbatimTextOutput("summarymodel"),
                                                       plotOutput("plotimportance"),
                                                       p(downloadButton("downloadplotimportance","Download plot"),
                                                         downloadButton('downloaddataplotimportance', 'Download raw data'),align="center")
                                              ),
                                              tabPanel("Test parameters", icon  =  icon("cog"),
                                                       fluidRow(
                                                         column(6,
                                                                h4("Selection Parameters"),
                                                                sliderInput("prctvaluestest", "Percent of values accepted",min = 0, max = 100, value = c(50,50),width="60%"),
                                                                checkboxGroupInput("selectmethodtest","Methods of selection ",c("selection on all samples"="nogroup","each group has more than x% of values "="bothgroups",
                                                                                                                                "at least one group has more than x% of more"="onegroup"),selected ="bothgroups" )
                                                         ),
                                                         column(6,
                                                                checkboxGroupInput("NAstructuretest", "Select variables with a NA's structure " , choices = list("TRUE /!\\"=TRUE,"FALSE"=FALSE),selected ="FALSE"),
                                                                helpText("/!\\ process can be long"),
                                                                conditionalPanel(condition ="output.testNAstructure ",
                                                                                 fluidRow(
                                                                                   column(6,
                                                                                          numericInput("thresholdNAstructuretest","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005),
                                                                                          radioButtons("structdatatest", "search structure in",c("all dataset" = "alldata","selected dataset" = "selecteddata"))
                                                                                   ),
                                                                                   column(6,
                                                                                          numericInput("maxvaluesgroupmintest","The group with the minimum number of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                                                                                          numericInput("minvaluesgroupmaxtest","The group with the maximum number of values has at least y% of values",value = 75,min = 0,max = 100,step = 5)
                                                                                   )
                                                                                 )
                                                                )
                                                         )
                                                       ),
                                                       #textOutput("testNAstructure"),
                                                       #hr(),
                                                       fluidRow(
                                                         column(6,h3("Transform Parameters")),
                                                         column(6,h3("Statistics Parameters"))
                                                       ),
                                                       fluidRow(
                                                         column(3,
                                                                checkboxGroupInput("rempNAtest", "Replacing NA (Not Attributes) by",
                                                                                   c("zero" = "z",
                                                                                     "mean of the cohort" = "moy",
                                                                                     "mean by group"="moygr",
                                                                                     "PCA estimation" = "pca",
                                                                                     "Random forest estimation /!\\" = "missforest"),
                                                                                   selected = "moygr")
                                                         ),
                                                         column(3,
                                                                #br(),br(),
                                                                checkboxGroupInput("logtest","transform data in log",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected = "FALSE"),
                                                                radioButtons("logtypetest",label = NULL,c("ln"="logn","log 10"="log10","log2"="log2"),inline = TRUE),
                                                                checkboxGroupInput("standardizationtest","standardization dataset",
                                                                                   choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,
                                                                                   selected = "FALSE"),
                                                                checkboxGroupInput("arcsintest","arcsine transformation",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected ="FALSE")
                                                         ),
                                                         #),
                                                         #hr(),
                                                         #fluidRow(
                                                         column(3,
                                                                checkboxGroupInput("testtest", "Tests",
                                                                                   #c( "No test"="notest","Wilcoxon Test" = "Wtest","Student Test" = "Ttest"),
                                                                                   c( "No test"="notest",
                                                                                      "Wilcoxon Test" = "Wtest",
                                                                                      "Student Test" = "Ttest",
                                                                                      # "Clustering + ElasticNet" = "clustEnet",
                                                                                      "Lasso" = "lasso", 
                                                                                      "ElasticNet" = "elasticnet"
                                                                                      # ,
                                                                                      # "Ridge" = "ridge"
                                                                                      ),
                                                                                   selected = "Wtest"),
                                                                checkboxGroupInput("adjustpvtest", "adjust p-value " , 
                                                                                   choices = list("TRUE"=TRUE,"FALSE"=FALSE),
                                                                                   inline = TRUE,
                                                                                   selected = "FALSE")
                                                         ),
                                                         column(3,
                                                                numericInput("thresholdFCtest","choise of the Fold change threshold" , 0, min =0, max = 5, step = 0.5),
                                                                numericInput("thresholdpvtest","choise of the p-value threshold %" , 0.05, min =0, max = 1, step = 0.01)
                                                         )
                                                       ),
                                                       #hr(),
                                                       h3("Model Parameters"),
                                                       fluidRow(
                                                         column(3,
                                                                # checkboxGroupInput("modeltest", "Type of model to adjust", 
                                                                #                    c("No model" = "nomodel",
                                                                #                      "Random Forest"="randomforest",
                                                                #                      "Support Vector Machine" = "svm"),
                                                                #                    selected = "svm")
                                                                checkboxGroupInput("modeltest", "Type of model to adjust",
                                                                                   c("No model" = "nomodel",
                                                                                     "Random Forest"="randomforest",
                                                                                     "Support Vector Machine" = "svm",
                                                                                     "ElasticNet"="elasticnet",
                                                                                     "XGBoost"="xgboost",
                                                                                     # "LightGBM"="lightgbm",  
                                                                                     "K-Nearest Neighbors"="knn",
                                                                                     "Naive Bayes"="naivebayes"),
                                                                                   selected = "svm")
                                                         ),
                                                         column(4,
                                                                #numericInput("thresholdmodeltest","threshold model" ,0, min = -1, max = 1, step = 0.05),
                                                                radioButtons("tuning_method_test", "Hyperparameter tuning:",
                                                                             c("Default parameters" = "default",
                                                                               "Automatic tuning" = "automatic"),
                                                                             selected = "default"),
                                                                helpText("Automatic tuning uses model-specific optimization (tune.svm, tuneRF, cv.glmnet, xgb.cv, etc.)"),
                                                                checkboxGroupInput("fstest","features selection by cross validation",choices = list("TRUE /!\\"=TRUE,"FALSE"=FALSE),inline = TRUE,selected ="FALSE"),
                                                                helpText("/!\\ process can be long"),
                                                                radioButtons("threshold_method_test", "Threshold optimization:",
                                                                             c("Fixed (0.5 for probabilistic models)" = "fixed",
                                                                               "Youden (maximize sensitivity + specificity)" = "youden",
                                                                               "Equiprobability (equal error rate)" = "equiprob"),
                                                                             selected = "fixed"),
                                                                helpText("Threshold calculated on TRAIN data, applied to validation. 
                                                                         Youden: optimal balance sens/spec. 
                                                                         Equiprobability: minimizes FP=FN. 
                                                                         Note: Multiple testing may slightly inflate validation metrics.")
                                                         ),
                                                         column(5,
                                                                p(actionButton("tunetest",h4("Test all models"),width=200),align="center")
                                                         )
                                                       ),
                                                       dataTableOutput("tabtestparameters")%>% withSpinner(color="#0dc5c1",type = 1),
                                                       p(downloadButton("downloadtabtestparameters","Download dataset"),align="center"),
                                                       # les graphiques pour les différents paramètres
                                                       fluidRow(
                                                         column(6,
                                                                plotOutput("plottestparameterslearning")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("downloadplottestparameterslearning","Download plot"), align = 'center')
                                                         ),
                                                         column(6,
                                                                plotOutput("plottestparametersvalidation")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("downloadplottestparametersvalidation","Download plot"), align = 'center')
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(
                                                           12,
                                                           plotOutput("plottestparametersboth")%>% withSpinner(color="#0dc5c1",type = 1),
                                                           p(downloadButton("downloadplottestparametersboth","Download plot"), align = 'center')
                                                         )
                                                       ),
                                                       # fluidRow(
                                                       #   column(
                                                       #     12,
                                                       #     plotOutput("plottestparametersboth")%>% withSpinner(color="#0dc5c1",type = 1),
                                                       #     p(downloadButton("downloadplottestparametersboth","Download plot"), align = 'center')
                                                       #   )
                                                       # ),
                                                       # h4("Additional Analysis", style = "margin-top: 30px; margin-bottom: 20px;"),
                                                       # fluidRow(
                                                       #   column(
                                                       #     12,
                                                       #     h5("Threshold vs Performance", style = "margin-bottom: 10px;"),
                                                       #     plotOutput("plottestparametersthreshold")%>% withSpinner(color="#0dc5c1",type = 1),
                                                       #     p(downloadButton("downloadplottestparametersthreshold","Download plot"), align = 'center'),
                                                       #     helpText("Relationship between optimal threshold and validation performance. Each point represents a parameter combination. The curve shows the trend.")
                                                       #   )
                                                       # ),
                                                       fluidRow(
                                                         column(
                                                           12,
                                                           h5("Overfitting Analysis", style = "margin-top: 20px; margin-bottom: 10px;"),
                                                           plotOutput("plottestparametersoverfitting")%>% withSpinner(color="#0dc5c1",type = 1),
                                                           p(downloadButton("downloadplottestparametersoverfitting","Download plot"), align = 'center'),
                                                           helpText("Analysis of model overfitting. Positive values indicate overfitting (learning performance > validation performance). Values close to zero indicate good generalization.")
                                                         )
                                                       )
                                              )
                                            )
                                   )
                       )
      )
    )
    )
