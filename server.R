options(shiny.maxRequestSize=60*1024^2) 
source("global.R")
#options(xtable.include.colnames=T)
#options(xtable.include.rownames=T)

shinyServer(function(input, output,session) {
  #if(requireNamespace("superml", quietly = TRUE)) {
    #attachNamespace("superml")
  #}
  # output$theme_value = reactive({
  #   if (is.null(input$theme) || input$theme == "default") {
  #     return ("quartz")
  #   } else {
  #     return(input$theme)
  #   }
  # }
  
  # observe le switch / bouton
  # observeEvent(input$mode, {
  #   # si mode = "dark", on applique un thème sombre
  #   # sinon thème clair de base
  #   new_th <- if (input$mode == "dark") {
  #     bs_theme(bootswatch = "darkly")
  #   } else {
  #     bs_theme()  # thème par défaut (clair)
  #   }
  #   session$setCurrentTheme(new_th)
  # })
  
  #changer le theme 
  # observeEvent(input$theme_app, {
  #           session$setCurrentTheme(bs_theme(bootswatch = input$theme_app))
  # }, ignoreInit = TRUE)
  
  output$modelUploaded <- reactive({
    return(!is.null(input$modelfile))
  })
  outputOptions(output, 'modelUploaded', suspendWhenHidden=FALSE)
  
  output$fileUploaded <- reactive({
    return(!is.null(input$learningfile))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$image1<-renderImage({return (list(src="pictures/Logo I2MC.jpg", 
                                           contentType="image/jpeg",
                                           width=300,
                                           height=200,
                                           alt="I2MC logo"))},deleteFile = F)
  output$image2<-renderImage({return (list(src="pictures/rflabxx.png", 
                                           contentType="image/png",
                                           width=600,
                                           height=200,
                                           alt="RFlab logo"))},deleteFile = F)
  output$image3<-renderImage({return (list(src="pictures/structurdata2.jpg", 
                                           contentType="image/jpeg",
                                           width=600,
                                           height=300,
                                           alt="structure data"))},deleteFile = F)

  output$fileUploadedval <- reactive({
    return( !is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'fileUploadedval', suspendWhenHidden=FALSE)
  
  output$modelUploadedval <- reactive({
    return(!is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'modelUploadedval', suspendWhenHidden=FALSE)
  
#Save state#############  
  state <- reactiveValues()
  observe({
    importparameters<<-list("learningfile"=input$learningfile,
                            "validationfile"=input$validationfile,
                            "modelfile"=input$modelfile,
                            "extension" = input$filetype,
                            "NAstring"=input$NAstring,
                            "sheetn"=input$sheetn,
                            "skipn"=input$skipn,
                            "dec"=input$dec,
                            "sep"=input$sep,
                            "transpose"=input$transpose,
                            "zeroegalNA"=input$zeroegalNA,
                            confirmdatabutton=input$confirmdatabutton)
    
    selectdataparameters<<-list("prctvalues"=input$prctvalues,
                                "selectmethod"=input$selectmethod,
                                "NAstructure"=input$NAstructure,
                                "structdata"=input$structdata,
                                "thresholdNAstructure"=input$thresholdNAstructure,
                                "maxvaluesgroupmin"=input$maxvaluesgroupmin,
                                "minvaluesgroupmax"=input$minvaluesgroupmax)
    
    transformdataparameters<<-list("log"=input$log,
                                   "logtype"=input$logtype,
                                   "standardization"=input$standardization,
                                   "arcsin"=input$arcsin,
                                   "rempNA"=input$rempNA)
    
    
    testparameters<<-list("SFtest"=input$SFtest,"test"=input$test,"adjustpv"=input$adjustpv,"thresholdpv"=input$thresholdpv,"thresholdFC"=input$thresholdFC)
    
    modelparameters<<-list("modeltype"=input$model,"invers"=input$invers,"thresholdmodel"=input$thresholdmodel,
                           "fs"=input$fs,"adjustval"=input$adjustval)
    parameters<-list("importparameters"=importparameters,"selectdataparameters"=selectdataparameters,
                     "transformdataparameters"=transformdataparameters,"testparameters"=testparameters,"modelparameters"=modelparameters)
    data<-DATA()
    selectdata<-SELECTDATA()
    transformdata<-TRANSFORMDATA()
    test<-TEST()
    model<-MODEL()
    settingstable<-statetable()
    isolate(state<<-list("parameters"=parameters,"data"=data,"selectdata"=selectdata,"transformdata"=transformdata,"test"=test,"model"=model,"settingstable"=settingstable)) 
  })
  
  output$savestate <- downloadHandler(
    filename <- function(){
      paste("model.RData")
    },
    content = function(file) { 
      save(state, file = file)
    }
  )
  observe({
    if(input$confirmdatabutton!=0 & !is.null(input$modelfile)){
      print("update")
      dataaaa<<-DATA()
      updateNumericInput(session, "prctvalues", value = DATA()$previousparameters$selectdataparameters$prctvalues)
      updateRadioButtons(session,"selectmethod",selected =  DATA()$previousparameters$selectdataparameters$selectmethod)
      updateCheckboxInput(session ,"NAstructure",value=DATA()$previousparameters$selectdataparameters$NAstructure)
      updateRadioButtons(session,"structdata",selected=DATA()$previousparametersselectdataparameters$parameters$structdata)
      updateNumericInput(session, "maxvaluesgroupmin", value = DATA()$previousparametersselectdataparameters$parameters$maxvaluesgroupmin)
      updateNumericInput(session, "minvaluesgroupmax", value = DATA()$previousparametersselectdataparameters$parameters$minvaluesgroupmax)
      updateNumericInput(session, "thresholdNAstructure", value = DATA()$previousparameters$selectdataparameters$thresholdNAstructure)
      
      updateRadioButtons(session,"rempNA",selected=DATA()$previousparameters$transformdataparameters$rempNA)
      updateCheckboxInput(session ,"log",value=DATA()$previousparameters$transformdataparameters$log)
      updateRadioButtons(session ,"logtype",selected=DATA()$previousparameters$transformdataparameters$logtype)
      updateCheckboxInput(session ,"standardization",value=DATA()$previousparameters$transformdataparameters$standardization)
      updateCheckboxInput(session ,"arcsin",value=DATA()$previousparameters$transformdataparameters$arcsin)
      
      #updateRadioButtons(session,"test",selected=DATA()$previousparameters$testparameters$test)
      #updateNumericInput(session, "thresholdFC", value = DATA()$previousparameters$testparameters$parameters$thresholdFC)
      #updateNumericInput(session, "thresholdpv", value = DATA()$previousparameters$testparameters$parameters$thresholdpv)
      #updateCheckboxInput(session ,"adjustpval",value=DATA()$previousparameters$testparameters$parameters$adjustpval)
      #updateCheckboxInput(session ,"SFtest",value=DATA()$previousparameters$testparameters$parameters$SFtest)

      updateRadioButtons(session,"test",selected=DATA()$previousparameters$testparameters$test)
      updateNumericInput(session, "thresholdFC", value = DATA()$previousparameters$testparameters$thresholdFC)
      updateNumericInput(session, "thresholdpv", value = DATA()$previousparameters$testparameters$thresholdpv)
      updateCheckboxInput(session ,"adjustpv",value=DATA()$previousparameters$testparameters$adjustpv)
      updateCheckboxInput(session ,"SFtest",value=DATA()$previousparameters$testparameters$SFtest)
      
      updateRadioButtons(session,"model",selected=DATA()$previousparameters$modelparameters$modeltype)
      updateNumericInput(session, "thresholdmodel", value = DATA()$previousparameters$modelparameters$thresholdmodel)
      updateCheckboxInput(session ,"fs",value=DATA()$previousparameters$modelparameters$fs)

      updateCheckboxInput(session ,"adjustval",value=DATA()$previousparameters$modelparameters$adjustval)
      updateCheckboxInput(session ,"invers",value=DATA()$previousparameters$modelparameters$invers)
      
    }
  })
  
  statetable<-reactive({
    table <- matrix(data = "",nrow = 20,ncol=11)
    if((input$confirmdatabutton!=0 & !is.null(input$modelfile))){
      learningfile <- DATA()$previousparameters$importparameters$learningfile
    }
    else{learningfile<-input$learningfile}

    table[1,1:9]<-c("#","Extensionfile","decimal character","separator character","NA string","sheet number","skip lines","consider NA as 0","transpose")
    table[2,1:9]<-c("import parameters",learningfile$type,input$dec,input$sep,input$NAstring,
                         input$sheetn,input$skipn,input$zeroegalNA,input$transpose)

    table[3,]<-c("#","name learning file", "number of rows", "number of columns", paste("number of ",levels(DATA()$LEARNING[,1])[1]),
             paste("number of ",levels(DATA()$LEARNING[,1])[2]),"name validation file", "number of rows", "number of columns", paste("number of ",levels(DATA()$VALIDATION[,1])[1]),
             paste("number of ",levels(DATA()$VALIDATION[,1])[2]))
    table[4,]<-c("main results",learningfile$name,dim(DATA()$LEARNING)[1],dim(DATA()$LEARNING)[2],nll(sum(DATA()$LEARNING[,1]==levels(DATA()$LEARNING[,1])[1])),
                 nll(sum(DATA()$LEARNING[,1]==levels(DATA()$LEARNING[,1])[2])),nll(input$validationfile$name),nll(dim(DATA()$VALIDATION)[1]),
                 nll(dim(DATA()$VALIDATION)[2]),nll(sum(DATA()$VALIDATION[,1]==levels(DATA()$VALIDATION[,1])[1])),
                 nll(sum(DATA()$VALIDATION[,1]==levels(DATA()$VALIDATION[,1])[2])))
    table[5,1:8]<-c("#","percentage of values minimum","method of selection","select features structured","search structur in",
                     "threshold p-value of proportion test", "maximum % values of the min group","minimum % values of the max group")
    table[6,1:8]<-c("select parameters",selectdataparameters[[1]],selectdataparameters[[2]],selectdataparameters[[3]],
                    selectdataparameters[[4]],selectdataparameters[[5]],selectdataparameters[[6]],selectdataparameters[[7]])
    table[7,1:3]<-c("#","number of feature selected","number of feature structured")
    table[8,1:3]<-c("main results",dim(SELECTDATA()$LEARNINGSELECT)[2]-1,nll(dim(SELECTDATA()$STRUCTUREDFEATURES)[2]))
    table[9,1:5]<-c("#","remplace NA by","transformation log","strandardisation","arcsin transformation")
    if(transformdataparameters[[1]]=="FALSE"){logprint<-"FALSE"}
    else{logprint<-transformdataparameters[[2]]}
    table[10,1:5]<-c("transform parameters",transformdataparameters[[5]],logprint,transformdataparameters[[3]],transformdataparameters[[4]])
    table[11,1]<-c("#")
    table[12,1]<-c("main results")
    table[13,1:5]<-c("#","test","use Bonferroni adjustment","threshold of significativity","Fold change threshold")
    table[14,1:5]<-c("test parameters",input$test,input$adjustpv,input$thresholdpv,input$thresholdFC)
    table[15,1:2]<-c("#","number of differently expressed features")
    table[16,1:2]<-c("main results",dim(TEST()$LEARNINGDIFF)[2]-1)

    if(input$model!="nomodel"){
      table[17,1:6]<-c("#","model type","cut-off of the model","feature selection","apply model on validation","invers groups")
      table[18,1:6]<-c("model parameters",input$model,input$thresholdmodel,
                       input$fs,input$adjustval,input$invers)
      
      # table[17,1:6]<-c("#","model type","cut-off of the model (Youden)","feature selection",
      #                  "apply model on validation","invers groups")
      
      cat('MODEL()$modelparameters$thresholdmodel \n')
      print(MODEL()$modelparameters$thresholdmodel)
      # table[18,1:6]<-c("model parameters",input$model,
      #                  round(MODEL()$modelparameters$thresholdmodel, 3),
      #                  input$fs,input$adjustval,input$invers)
      
      table[19,1:8]<-c("#","number of features","AUC learning","sensibility learning","specificity learning","AUC validation","sensibility validation","specificity validation")
#       line20<<-c("main results",dim(MODEL()$DATALEARNINGMODEL$learningmodel)[2]-1,
#                  as.numeric(auc(roc(MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$scorelearning))),
#                  sensibility(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
#                  specificity(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
#                  as.numeric(auc(roc(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval))),
#                  sensibility(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval),
#                  specificity(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval)
#       )
      table[20,1:5]<-c("main results",dim(MODEL()$DATALEARNINGMODEL$learningmodel)[2]-1,
                  round(as.numeric(auc(roc(MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$scorelearning))),digits = 3),
                  sensibility(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
                  specificity(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning)
                  )
      if(input$adjustval){
      table[20,6:8]<-c(round(as.numeric(auc(roc(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval))),digits = 3),
                  sensibility(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval),
                  specificity(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval)
                  )
      }
    }
    return(table)
    
  }) 
  
  output$savestatetable<- downloadHandler(
    filename = function() { paste('settingstable', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   statetable(), file,cnames=F,rnames=F) })
  
############
  output$namefilelearn<-renderText({
    namelearn<-input$learningfile$name
  })
  output$dim1learn<-renderText({
    di1<-dim(x = DATA()$LEARNING)[1]  
  })
  output$dim2learn<-renderText({
    di2<-dim(x = DATA()$LEARNING)[2]-1  
  })
  output$namefileval<-renderText({
    nameval<-input$validationfile$name
  })  
  output$dim1val<-renderText({
    di1<-dim(x = DATA()$VALIDATION)[1]  
  })
  output$dim2val<-renderText({
    di2<-dim(x = DATA()$VALIDATION)[2]  
  })  

  #si erreur envoyÃÂÃÂ© pb import
  DATA<-reactive({
     # Require that either a learning file or a model file is uploaded before proceeding
     

     importparameters<<-list("learningfile"=input$learningfile,"validationfile"=input$validationfile,"modelfile"=input$modelfile,"extension" = input$filetype,
                            "NAstring"=input$NAstring,"sheetn"=input$sheetn,"skipn"=input$skipn,"dec"=input$dec,"sep"=input$sep,
                            "transpose"=input$transpose,"zeroegalNA"=input$zeroegalNA,confirmdatabutton=input$confirmdatabutton,invers=input$invers)

     out<-tryCatch(importfunction(importparameters),error=function(e) e )
#      if(any(class(out)=="error"))print("error")
#      else{resimport<-out}
     validate(need(any(class(out)!="error"),"error import"))
     resimport<<-out
      #resimport<-importfunction(importparameters)
    list(LEARNING=resimport$learning, 
         VALIDATION=resimport$validation,
        previousparameters=resimport$previousparameters  
#          LEVELS=resimport$lev
         )
  })
  
  output$JDDlearn=renderDataTable({
    learning<-DATA()$LEARNING
    validate(need(!is.null(learning),"problem import"))
    colmin<-min(ncol(learning),100)
    rowmin<-min(nrow(learning),100)
    cbind(Names=rownames(learning[1:rowmin,1:colmin]),learning[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10))
  
  output$downloaddataJDDlearn <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$LEARNING, file) })
  
  
  output$JDDval=renderDataTable({
    validation<-DATA()$VALIDATION
    validate(need(!is.null(validation),"problem import"))
    colmin<-min(ncol(validation),100)
    rowmin<-min(nrow(validation),100)
    cbind(Names=rownames(validation[1:rowmin,1:colmin]),validation[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10)) 
  
  output$downloaddataJDDval <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$VALIDATION, file) })


#################
SELECTDATA<-reactive({
  selectdataparameters<<-list("prctvalues"=input$prctvalues,"selectmethod"=input$selectmethod,"NAstructure"=input$NAstructure,"structdata"=input$structdata,
                              "thresholdNAstructure"=input$thresholdNAstructure,"maxvaluesgroupmin"=input$maxvaluesgroupmin,"minvaluesgroupmax"=input$minvaluesgroupmax)
  validate(need(selectdataparameters$prctvalues>=0 &selectdataparameters$prctvalues<=100,"%  NA has to be between 0 and 100"))
  validate(need(input$minvaluesgroupmax>=0 &input$minvaluesgroupmax<=100 & input$maxvaluesgroupmin>=0 &input$maxvaluesgroupmin<=100,"% threshold has to be between 0 and 100"),
           need(input$thresholdNAstructure>0,input$thresholdNAstructure<1,"threshold of the pvalue has to be between 0 and 1"))
  learning<<-DATA()$LEARNING
  validate(need(input$confirmdatabutton!=0,"Importation of datas has to be confirmed"))
  
  validate(need(length(levels(learning[,1]))==2,"number of groups is not equal to 2"))
  resselectdata<<-selectdatafunction(learning = learning,selectdataparameters = selectdataparameters)
  list(LEARNINGSELECT=resselectdata$learningselect,STRUCTUREDFEATURES=resselectdata$structuredfeatures,DATASTRUCTUREDFEATURES=resselectdata$datastructuredfeatures,selectdataparameters)
})
#####
#Selection Output
#####
  output$downloaddataselect<- downloadHandler(
    filename = function() { paste('Dataselect', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(SELECTDATA()$LEARNINGSELECT, file)
    }
  )
  
output$nvarselect=renderText({
    di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
  })
  
output$heatmapNA<-renderPlot({
  learningselect<-SELECTDATA()$LEARNINGSELECT
  heatmapNA(toto =learningselect)
})
output$downloadplotheatmapNA = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =    heatmapNA(toto =SELECTDATA()$LEARNINGSELECT), 
           device = input$paramdownplot)
  },
  contentType=NA)

output$downloaddataheatmapNA <- downloadHandler(
  filename = function() { paste('dataset distribution of NA', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(as.data.frame(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F)), file)
  }
)

# observe({
#   req(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F))
#   print(class(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F)))
# })

output$plotNA<-renderPlot({
  learningselect<-SELECTDATA()$LEARNINGSELECT
  learning<-DATA()$LEARNING
  distributionvalues(toto = learning,prctvaluesselect =input$prctvalues/100,nvar = ncol(learningselect) ,ggplot =  T)  
})


output$downloadplotNA = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =         distributionvalues(toto = DATA()$LEARNING,prctvaluesselect =input$prctvalues/100,nvar = ncol(SELECTDATA()$LEARNINGSELECT) ,ggplot =  T), 
           device = input$paramdownplot)},contentType=NA)

output$downloaddataplotNA <- downloadHandler( 
  filename = function() {
    paste('dataset', '.',input$paramdowntable, sep='') 
    },
  content = function(file) {
    downloaddataset(distributionvalues(toto = DATA()$LEARNING,prctvaluesselect =input$prctvalues/100,nvar = ncol(SELECTDATA()$LEARNINGSELECT) ,ggplot =  T,graph = F)  , file)
  }
)

output$nstructuredfeatures<-renderText({
  ncol(SELECTDATA()$STRUCTUREDFEATURES)
})
output$heatmapNAstructure<-renderPlot({
  group<<-DATA()$LEARNING[,1]
  structuredfeatures<<-SELECTDATA()$STRUCTUREDFEATURES
  heatmapNA(toto=cbind(group,structuredfeatures))            
  #else{errorplot(text = " No NA's structure")}
})
  
output$downloadstructur = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot = heatmapNA(cbind(DATA()$LEARNING[,1],SELECTDATA()$STRUCTUREDFEATURES)), 
           device = input$paramdownplot)
  },
  contentType=NA)

output$downloaddatastructur <- downloadHandler( 
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(SELECTDATA()$STRUCTUREDFEATURES, file)
  }
) 
  
#####  
TRANSFORMDATA<-reactive({
  learningselect<<-SELECTDATA()$LEARNINGSELECT
  structuredfeatures<<-SELECTDATA()$STRUCTUREDFEATURES
  datastructuresfeatures<<-SELECTDATA()$DATASTRUCTUREDFEATURES
  transformdataparameters<<-list("log"=input$log,"logtype"=input$logtype,"standardization"=input$standardization,"arcsin"=input$arcsin,"rempNA"=input$rempNA)
  validate(need(ncol(learningselect)>0,"No select dataset"))
  if(transformdataparameters$rempNA%in%c("pca","missforest")){
    validate(need(min(apply(X = learningselect,MARGIN = 2,FUN = function(x){sum(!is.na(x))}))>1,"not enough data for pca estimation"))
  } 
  learningtransform<-transformdatafunction(learningselect = learningselect,structuredfeatures = structuredfeatures,
                                      datastructuresfeatures =   datastructuresfeatures,transformdataparameters = transformdataparameters)

  list(LEARNINGTRANSFORM=learningtransform,transformdataparameters=transformdataparameters)
})

##
output$downloaddatatransform<- downloadHandler(
  filename = function() { paste('Transformdata', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TRANSFORMDATA()$LEARNINGTRANSFORM, file)
  }
)

output$plotheatmaptransformdata<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  heatmapplot(toto =learningtransform,ggplot = T,scale=F)
})

output$downloadplotheatmap = downloadHandler(
  filename = function() { 
    paste0('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =    heatmapplot(toto =TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot = T,scale=F), 
           device = input$paramdownplot)},
  contentType=NA)

output$downloaddataheatmap <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(as.data.frame(heatmapplot(toto =TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot = T,scale=F,graph=F)), file)
  })

output$plotmds<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  mdsplot(toto = learningtransform,ggplot=T)
})
output$downloadplotmds = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =        mdsplot(toto = TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot=T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamds <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(    mdsplot(toto = TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot=T,graph=F), file)
  })


output$plothist<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  histplot(toto=learningtransform)
})
output$downloadplothist = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =         histplot(toto=TRANSFORMDATA()$LEARNINGTRANSFORM),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatahist <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(histplot(toto=TRANSFORMDATA()$LEARNINGTRANSFORM,graph=F), file)
  })

#########
TEST<-reactive({
  # Get lambda and alpha parameters for multivariate methods
  lambda_param <- NULL
  alpha_param <- 0.5
  if(input$test %in% c("lasso","elasticnet","ridge")){
    if(!input$autolambda){
      lambda_param <- input$lambdaselection
    }
    if(input$test == "elasticnet"){
      alpha_param <- input$alphaselection
    }
  }
  
  # Get clustering + elasticnet parameters
  n_clusters_param <- NULL
  n_bootstrap_param <- NULL
  min_selection_freq_param <- NULL
  preprocess_param <- NULL
  min_patients_param <- NULL
  
  if(input$test == "clustEnet"){
    n_clusters_param <- input$nclusters
    n_bootstrap_param <- input$nbootstrap
    alpha_param <- input$alphaclustenet
    min_selection_freq_param <- input$minselectionfreq
    preprocess_param <- input$preprocessclustenet
    min_patients_param <- 20
  }

  testparameters<<-list("SFtest"=input$SFtest,"test"=input$test,"adjustpval"=input$adjustpv,"thresholdpv"=input$thresholdpv,
                        "thresholdFC"=input$thresholdFC,"invers"=input$invers,
                        "lambda"=lambda_param,"alpha"=alpha_param,
                        "n_clusters"=n_clusters_param,"n_bootstrap"=n_bootstrap_param,
                        "min_selection_freq"=min_selection_freq_param,
                        "preprocess"=preprocess_param,"min_patients"=min_patients_param  )
  learningtransform<<-TRANSFORMDATA()$LEARNINGTRANSFORM
  restest<<-testfunction(tabtransform = learningtransform,testparameters = testparameters )
  validate(need(testparameters$thresholdFC>=0,"threshold Foldchange has to be positive"))
  validate(need(testparameters$thresholdpv>=0 &testparameters$thresholdpv<=1,"p-value has to be between 0 and 1"))

  list(LEARNINGDIFF=restest$tabdiff,DATATEST=restest$datatest,HYPOTHESISTEST=restest$hypothesistest,
       USEDDATA=restest$useddata,testparameters=restest$testparameters,
       MULTIVARIATERESULTS=restest$multivariateresults)

})
##
output$downloadddatadiff<- downloadHandler(
  filename = function() { paste('Datadiff', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TEST()$LEARNINGDIFF, file)
  }
)
output$downloaddatastatistics<- downloadHandler(
  filename = function() { paste('Datastatistics', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TEST()$DATATEST, file)
  }
)
output$positif<-renderText({
  res<-levels(DATA()$LEARNING[,1])[1]
})
output$negatif<-renderText({
  res<-levels(DATA()$LEARNING[,1])[2]
})
output$volcanoplot <- renderPlot({
  datatest<<-TEST()$DATATEST
  useddata<<-TEST()$USEDDATA
  colnames(useddata)[3]<-colnames(datatest)[5]
  volcanoplot(logFC =useddata[,3],pval = useddata$pval,thresholdFC = input$thresholdFC,thresholdpv = (input$thresholdpv ),completedata=useddata[,1:3] )
})
output$downloadvolcanoplot = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = volcanoplot(logFC =TEST()$USEDDATA$logFC,pval = TEST()$USEDDATA$pval,thresholdFC = input$thresholdFC,
                                    thresholdpv = input$thresholdpv ,completedata=TEST()$DATATEST ) ,  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatavolcanoplot<- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(volcanoplot(logFC =TEST()$USEDDATA$logFC,pval = TEST()$USEDDATA$pval,thresholdFC = input$thresholdFC,
                                    thresholdpv = (input$thresholdpv ),completedata=TEST()$DATATEST,graph=F ), file) })
output$nvarselect2<-renderText({
  di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
})  
output$nbdiff<-renderText({
  nbdiff = positive(ncol(TEST()$LEARNINGDIFF)-1)
})


output$barplottest <- renderPlot({
  learningdiff<<-TEST()$LEARNINGDIFF
  useddata<<-TEST()$USEDDATA
  if(nrow(learningdiff)!=0){barplottest(feature=useddata$names,logFC=useddata$logFC,levels=levels(learningdiff[,1]),pval=useddata$pval,mean1=useddata$mean1,mean2=useddata$mean2,thresholdpv=input$thresholdpv,
                                             thresholdFC=input$thresholdFC,graph=T,maintitle="Mean by group for differentially expressed variables")
}
  else{errorplot(text = " No differently expressed ")}
  
})
output$downloadbarplottest = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=input$thresholdpv,
                                    thresholdFC=input$thresholdFC,graph=T,maintitle="Mean by group for differentially expressed variables"),  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatabarplottest <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=input$thresholdpv,
                                thresholdFC=input$thresholdFC,maintitle="Mean by group for differentially expressed variables",graph=F), file) })

# output$dataconditiontest=renderDataTable({
#   hypothesistest<-TEST()$hypothesistest},options = list("orderClasses" = F,
#                                                         "responsive" = F,
#                                                         "pageLength" = 10))
output$plottestSF=renderPlot({
  hypothesistest<-TEST()$HYPOTHESISTEST   
  barplottestSF(hypothesistest)
})
output$downloadplottestSF = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   barplottestSF(TEST()$HYPOTHESISTEST  ),  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatatestSF <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(  barplottestSF(TEST()$HYPOTHESISTEST ,graph=F), file) })

# Multivariate selection results outputs
output$nbmultivariateselected<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults)){
    length(multivariateresults$selected_vars)
  } else {
    0
  }
})

output$optimallambda<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$lambda)){
    format(multivariateresults$lambda, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$lambda1se<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$lambda_1se)){
    format(multivariateresults$lambda_1se, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$alphaused<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$alpha)){
    format(multivariateresults$alpha, digits = 3)
  } else {
    "N/A"
  }
})

output$multivariateresultstable<-renderDataTable({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && nrow(multivariateresults$results) > 0){
    results <- multivariateresults$results
    results$coefficient <- round(results$coefficient, 4)
    results$AUC <- round(results$AUC, 3)
    results$FoldChange <- round(results$FoldChange, 3)
    results$logFoldChange <- round(results$logFoldChange, 3)
    results$mean_group1 <- round(results$mean_group1, 3)
    results$mean_group2 <- round(results$mean_group2, 3)
    results
  } else {
    data.frame()
  }
},options = list("orderClasses" = F, "responsive" = F, "pageLength" = 10))

output$downloadmultivariateresults <- downloadHandler(
  filename = function() { paste('multivariate_results', '.',input$paramdowntable, sep='') },
  content = function(file) {
    multivariateresults <- TEST()$MULTIVARIATERESULTS
    if(!is.null(multivariateresults)){
      downloaddataset(multivariateresults$results, file)
    }
  }
)

#Clustering + ElasticNet results outputs
output$nbclustenetselected<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$method) && multivariateresults$method == "clustEnet"){
    length(multivariateresults$selected_vars)
  } else {
    0
  }
})

output$clustenetnclusters<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$clust_result)){
    multivariateresults$clust_result$n_clusters
  } else {
    "N/A"
  }
})

output$clustenetnbootstrap<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$clust_result)){
    multivariateresults$clust_result$n_bootstrap
  } else {
    "N/A"
  }
})

output$clustenetalphaused<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$clust_result)){
    format(multivariateresults$clust_result$alpha, digits = 3)
  } else {
    "N/A"
  }
})

output$clustenetminfreq<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$clust_result)){
    format(multivariateresults$clust_result$min_selection_freq, digits = 2)
  } else {
    "N/A"
  }
})

output$clustenetresultstable<-renderDataTable({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$method) &&
     multivariateresults$method == "clustEnet" && nrow(multivariateresults$results) > 0){
    results <- multivariateresults$results
    results$SelectionFrequency <- round(results$SelectionFrequency, 3)
    results$AUC <- round(results$AUC, 3)
    results$FoldChange <- round(results$FoldChange, 3)
    results$logFoldChange <- round(results$logFoldChange, 3)
    results$mean_group1 <- round(results$mean_group1, 3)
    results$mean_group2 <- round(results$mean_group2, 3)
    results
  } else {
    data.frame()
  }
},options = list("orderClasses" = F, "responsive" = F, "pageLength" = 10))

output$downloadclustenetresults <- downloadHandler(
  filename = function() { paste('clustenet_results', '.',input$paramdowntable, sep='') },
  content = function(file) {
    multivariateresults <- TEST()$MULTIVARIATERESULTS
    if(!is.null(multivariateresults) && multivariateresults$method == "clustEnet"){
      downloaddataset(multivariateresults$results, file)
    }
  }
)

# Visualisation PCA des variables sélectionnées par clustering + ElasticNet
output$PcaVarsSel = renderPlot({
  req(TEST()$MULTIVARIATERESULTS)
  
  req(TEST()$MULTIVARIATERESULTS)
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  
  # Vérifier que c'est bien la méthode clustEnet et qu'il y a des variables sélectionnées
  if(!is.null(multivariateresults$method) && 
     multivariateresults$method == "clustEnet" &&
     length(multivariateresults$selected_vars) > 0){
    
    learningtransform <- TRANSFORMDATA()$LEARNINGTRANSFORM
    
    selected_vars <- multivariateresults$selected_vars
    data_selected <- learningtransform[, selected_vars, drop=FALSE]
    
    y <- learningtransform[, 1]
    
    PlotPca(data = data_selected, 
            y = y, 
            title = paste("PCA of", length(selected_vars), "selected variables (Clustering + ElasticNet)"))
    
  } else {
    # Si pas de variables sélectionnées, afficher un message
    plot(1, type="n", axes=FALSE, xlab="", ylab="")
    text(1, 1, "No variables selected by Clustering + ElasticNet", cex=1.5)
  }
  
})

# PCa_react =  reactive({
#   req(TEST()$MULTIVARIATERESULTS)
#   multivariateresults <- TEST()$MULTIVARIATERESULTS
#   
#   # Vérifier que c'est bien la méthode clustEnet et qu'il y a des variables sélectionnées
#   if(!is.null(multivariateresults$method) && 
#      multivariateresults$method == "clustEnet" &&
#      length(multivariateresults$selected_vars) > 0){
#     
#     learningtransform <- TRANSFORMDATA()$LEARNINGTRANSFORM
#     
#     selected_vars <- multivariateresults$selected_vars
#     data_selected <- learningtransform[, selected_vars, drop=FALSE]
#     
#     y <- learningtransform[, 1]
#     
#     PlotPca(data = data_selected, 
#             y = y, 
#             title = paste("PCA of", length(selected_vars), "selected variables (Clustering + ElasticNet)"))
#     
#   } else {
#     # Si pas de variables sélectionnées, afficher un message
#     plot(1, type="n", axes=FALSE, xlab="", ylab="")
#     text(1, 1, "No variables selected by Clustering + ElasticNet", cex=1.5)
#   }
# })

output$donwloadPCAPlot =  downloadHandler(
  file =  function(){
    paste("image_PCA.png", '.', input$paramdownplot, sep = '')
  }, content = function(file){
    multivariateresults <- TEST()$MULTIVARIATERESULTS
    if(!is.null(multivariateresults$method) && 
       multivariateresults$method == "clustEnet" &&
       length(multivariateresults$selected_vars) > 0){
      learningtransform <- TRANSFORMDATA()$LEARNINGTRANSFORM
      selected_vars <- multivariateresults$selected_vars
      data_selected <- learningtransform[, selected_vars, drop=FALSE]
      y <- learningtransform[, 1]
      
      ggsave(file, 
             plot = PlotPca(data = data_selected, y = y, 
                            title = paste("PCA of", length(selected_vars), "selected variables")),
             device = input$paramdownplot)
    }
  }, contentType = NA
)


######
MODEL<-reactive({
  if(input$test=="notest"){learningmodel<<-TRANSFORMDATA()$LEARNINGTRANSFORM}
  else{learningmodel<<-TEST()$LEARNINGDIFF}
  validation<<-DATA()$VALIDATION
  datastructuresfeatures<<-SELECTDATA()$DATASTRUCTUREDFEATURES
  transformdataparameters<<-TRANSFORMDATA()$transformdataparameters
  learningselect<-SELECTDATA()$LEARNINGSELECT
  # Get hyperparameters for all models
  alpha_model <- NULL
  lambda_model <- NULL
  ntree_model <- 1000
  autotunerf_param <- TRUE
  mtry_model <- NULL
  autotunesvm_param <- TRUE
  cost_model <- NULL
  gamma_model <- NULL
  kernel_model <- NULL
  autotunexgb_param <- TRUE
  nrounds_model <- NULL
  maxdepth_model <- NULL
  eta_model <- NULL

  # ElasticNet parameters - based on tuning method
  if(input$model == "elasticnet"){
    tuning_method_en <- if(!is.null(input$tuning_method_en)) input$tuning_method_en else "traditional"
    if(tuning_method_en == "manual" || tuning_method_en == "traditional"){
      alpha_model <- input$alphamodel
    }
    if(tuning_method_en == "manual"){
      lambda_model <- input$lambdamodel
    }
  }

  # Random Forest parameters - based on tuning method
  if(input$model == "randomforest"){
    tuning_method_rf <- if(!is.null(input$tuning_method_rf)) input$tuning_method_rf else "traditional"
    ntree_model <- input$ntreerf
    autotunerf_param <- (tuning_method_rf != "manual")
    if(tuning_method_rf == "manual"){
      mtry_model <- input$mtryrf
    }
  }

  # SVM parameters - no change, still using checkbox
  if(input$model == "svm"){
    autotunesvm_param <- input$autotunesvm
    if(!input$autotunesvm){
      cost_model <- input$costsvm
      gamma_model <- input$gammasvm
      kernel_model <- input$kernelsvm
    }
  }

  # XGBoost parameters - based on tuning method
  if(input$model == "xgboost"){
    tuning_method_xgb <- if(!is.null(input$tuning_method_xgb)) input$tuning_method_xgb else "traditional"
    autotunexgb_param <- (tuning_method_xgb != "manual")
    if(tuning_method_xgb == "manual"){
      nrounds_model <- input$nroundsxgb
      maxdepth_model <- input$maxdepthxgb
      eta_model <- input$etaxgb
    }
  }

  # LightGBM parameters - no change
  autotunelgb_param <- TRUE
  nrounds_lgb_model <- NULL
  num_leaves_model <- NULL
  learning_rate_lgb_model <- NULL

  if(input$model == "lightgbm"){
    autotunelgb_param <- input$autotunelgb
    if(!input$autotunelgb){
      nrounds_lgb_model <- input$nroundslgb
      num_leaves_model <- input$numleaves
      learning_rate_lgb_model <- input$learningratelgb
    }
  }

  # KNN parameters - based on tuning method
  autotuneknn_param <- TRUE
  k_neighbors_model <- NULL

  if(input$model == "knn"){
    tuning_method_knn <- if(!is.null(input$tuning_method_knn)) input$tuning_method_knn else "traditional"
    autotuneknn_param <- (tuning_method_knn != "manual")
    if(tuning_method_knn == "manual"){
      k_neighbors_model <- input$kneighbors
    }
  }

  # Determine if GridSearchCV should be used based on tuning method
  use_gridsearch_param <- FALSE
  if(input$model == "randomforest" && !is.null(input$tuning_method_rf) && input$tuning_method_rf == "gridsearch"){
    use_gridsearch_param <- TRUE
  } else if(input$model == "xgboost" && !is.null(input$tuning_method_xgb) && input$tuning_method_xgb == "gridsearch"){
    use_gridsearch_param <- TRUE
  } else if(input$model == "elasticnet" && !is.null(input$tuning_method_en) && input$tuning_method_en == "gridsearch"){
    use_gridsearch_param <- TRUE
  } else if(input$model == "naivebayes" && !is.null(input$tuning_method_nb) && input$tuning_method_nb == "gridsearch"){
    use_gridsearch_param <- TRUE
  } else if(input$model == "knn" && !is.null(input$tuning_method_knn) && input$tuning_method_knn == "gridsearch"){
    use_gridsearch_param <- TRUE
  }

  modelparameters<<-list("modeltype"=input$model,"invers"=F,"thresholdmodel"=input$thresholdmodel,
                         "fs"=input$fs,"adjustval"=input$adjustval,
                         "use_gridsearch"=use_gridsearch_param,
                         "alpha"=alpha_model,"lambda"=lambda_model,
                         "ntree"=ntree_model,"autotunerf"=autotunerf_param,"mtry"=mtry_model,
                         "autotunesvm"=autotunesvm_param,"cost"=cost_model,"gamma"=gamma_model,
                         "kernel"= kernel_model , #ifelse(is.null(kernel_model),"radial",kernel_model),
                         "autotunexgb"=autotunexgb_param,"nrounds"=nrounds_model,
                         "max_depth"=maxdepth_model,"eta"=eta_model,
                         "autotunelgb"=autotunelgb_param,"nrounds_lgb"=nrounds_lgb_model,
                         "num_leaves"=num_leaves_model,"learning_rate_lgb"=learning_rate_lgb_model,
                         "autotuneknn"=autotuneknn_param,"k_neighbors"=k_neighbors_model)
  print(ncol(learningmodel))
  validate(need(ncol(learningmodel)>1,"Not enough features"))


  resmodel<<-modelfunction(learningmodel = learningmodel,validation = validation,
                           modelparameters = modelparameters,
                           transformdataparameters = transformdataparameters,
                           datastructuresfeatures =  datastructuresfeatures,
                           learningselect = learningselect)
  
 list("DATALEARNINGMODEL"=resmodel$datalearningmodel,"MODEL"=resmodel$model,
      "DATAVALIDATIONMODEL"=resmodel$datavalidationmodel,
      "GROUPS"=resmodel$groups,"modelparameters"=resmodel$modelparameters)
  
  })


observe({
  if (input$model=="svm") { updateNumericInput(session, "thresholdmodel", value = 0)}
  else if (input$model=="randomforest"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
  else if (input$model=="elasticnet"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
  else if (input$model=="xgboost"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
  else if (input$model=="lightgbm"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
  else if (input$model=="naivebayes"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
  else if (input$model=="knn"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
})

# Display optimal hyperparameters for models
output$modelalpha<-renderText({
  if(input$model=="elasticnet" && !is.null(MODEL()$MODEL)){
    format(MODEL()$MODEL$alpha, digits = 3)
  } else {
    "N/A"
  }
})

output$modellambda<-renderText({
  if(input$model=="elasticnet" && !is.null(MODEL()$MODEL)){
    format(MODEL()$MODEL$optimal_lambda, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$modellambda1se<-renderText({
  if(input$model=="elasticnet" && !is.null(MODEL()$MODEL) && !is.null(MODEL()$MODEL$lambda_1se)){
    format(MODEL()$MODEL$lambda_1se, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$modelnonzerocoef<-renderText({
  if(input$model=="elasticnet" && !is.null(MODEL()$MODEL)){
    coef_matrix <- as.matrix(coef(MODEL()$MODEL$glmnet_model, s=MODEL()$MODEL$lambda))
    sum(coef_matrix[-1,1] != 0)
  } else {
    "N/A"
  }
})

output$svmcost<-renderText({
  if(input$model=="svm" && !is.null(MODEL()$MODEL)){
    format(MODEL()$MODEL$cost, digits = 4)
  } else {
    "N/A"
  }
})

output$svmgamma<-renderText({
  if(input$model=="svm" && !is.null(MODEL()$MODEL)){
    format(MODEL()$MODEL$gamma, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$svmkernel<-renderText({
  if(input$model=="svm" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$kernel
    cat("Kernel  type  :  ", MODEL()$MODEL$kernel, "\n")
    cat("kernel :", MODEL()$modelparameters$kernel, " \n")
    cat("Kernel  :  ", input$kernelsvm, "\n")
    input$kernelsvm
  } else {
    "N/A"
  }
  })

output$rfmtry<-renderText({
  if(input$model=="randomforest" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$optimal_mtry
  } else {
    "N/A"
  }
})

output$rfntree<-renderText({
  if(input$model=="randomforest" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$ntree_used
  } else {
    "N/A"
  }
})

output$optiTuning_K = renderText({
  if(input$model=="knn" && !is.null(MODEL()$MODEL)){
    cat("the optimal k is :", MODEL()$MODEL$optimal_k, " \n")
     MODEL()$MODEL$optimal_k
  } else {
    "N/A"
  }
})

output$xgbnrounds<-renderText({
  if(input$model=="xgboost" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$optimal_nrounds
  } else {
    "N/A"
  }
})

output$xgbmaxdepth<-renderText({
  if(input$model=="xgboost" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$optimal_max_depth
  } else {
    "N/A"
  }
})

output$xgbeta<-renderText({
  if(input$model=="xgboost" && !is.null(MODEL()$MODEL)){
    format(MODEL()$MODEL$optimal_eta, digits = 3)
  } else {
    "N/A"
  }
})

output$xgbminchild<-renderText({
  if(input$model=="xgboost" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$optimal_min_child_weight
  } else {
    "N/A"
  }
})

output$lgbnrounds<-renderText({
  if(input$model=="lightgbm" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$optimal_nrounds
  } else {
    "N/A"
  }
})

output$lgbnumleaves<-renderText({
  if(input$model=="lightgbm" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$optimal_num_leaves
  } else {
    "N/A"
  }

})

output$lgblearningrate<-renderText({
  if(input$model=="lightgbm" && !is.null(MODEL()$MODEL)){
    format(MODEL()$MODEL$optimal_learning_rate, digits = 3)
  } else {
    "N/A"
  }
})

 
output$knnk<-renderText({
  if(input$model=="knn" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$optimal_k
  } else {
    "N/A"
  }
}) 


####
output$downloaddatalearning <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   MODEL()$DATALEARNINGMODEL$learningmodel, file) })


output$plotmodeldecouvroc <- renderPlot({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL
  ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,decisionvalues =  datalearningmodel$reslearningmodel$scorelearning)
})
output$youndendecouv<-renderTable({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL
  resyounden<-younden(datalearningmodel$reslearningmodel$classlearning, datalearningmodel$reslearningmodel$scorelearning)
  resyounden<-data.frame(resyounden)
  colnames(resyounden)<-c("")
  rownames(resyounden)<-c("younden","sensibility younden","specificity younden","threshold younden")
  
  resyounden
},include.rownames=TRUE)
 
output$downloadplotdecouvroc = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =  ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,
                                  decisionvalues =  datalearningmodel$reslearningmodel$scorelearning),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatadecouvroc <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,decisionvalues =  datalearningmodel$reslearningmodel$scorelearning,graph=F), file) })

output$plotmodeldecouvbp <- renderPlot({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL
  scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                 threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T,printnames=input$shownames1)
})
output$downloadplotmodeldecouvbp = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = scoremodelplot(class = datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamodeldecouvbp <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })
output$nbselectmodel<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  ncol(datalearningmodel$learningmodel)-1
})

output$tabmodeldecouv<-renderTable({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  as.data.frame.matrix(table(datalearningmodel$reslearningmodel$predictclasslearning,datalearningmodel$reslearningmodel$classlearning ))
},include.rownames=TRUE)

output$sensibilitydecouv<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  sensibility(predict = datalearningmodel$reslearningmodel$predictclasslearning,class = datalearningmodel$reslearningmodel$classlearning)
})

output$specificitydecouv<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  specificity(predict = datalearningmodel$reslearningmodel$predictclasslearning,class = datalearningmodel$reslearningmodel$classlearning )
})


output$downloaddatavalidation <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset( data.frame("Class"=MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$validationmodel,check.names = F), file) })


output$plotmodelvalroc <- renderPlot({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  ROCcurve(validation =  datavalidationmodel$resvalidationmodel$classval,decisionvalues =  datavalidationmodel$resvalidationmodel$scoreval)
})

output$downloadplotvalroc = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =ROCcurve(validation =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,decisionvalues =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatavalroc <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   ROCcurve(validation =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,decisionvalues =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,graph=F ), file) 
    })

output$plotmodelvalbp <- renderPlot({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  scoremodelplot(class = datavalidationmodel$resvalidationmodel$classval ,score =datavalidationmodel$resvalidationmodel$scoreval,names=rownames(datavalidationmodel$resvalidationmodel),
                 threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T,printnames=input$shownames1)
})

output$downloadplotmodelvalbp = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =scoremodelplot(class = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval ,score =MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,names=rownames(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamodelvalbp <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   scoremodelplot(class = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval ,score =MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,names=rownames(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })

output$youndenval<-renderTable({
  datavalidationmodel<<-MODEL()$DATAVALIDATIONMODEL
  resyounden<-younden(datavalidationmodel$resvalidationmodel$classval,datavalidationmodel$resvalidationmodel$scoreval )
  resyounden<-data.frame(resyounden)
  colnames(resyounden)<-c("")
  rownames(resyounden)<-c("younden","sensibility younden","specificity younden","threshold younden")
  resyounden
},include.rownames=TRUE)

output$tabmodelval<-renderTable({ 
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  as.data.frame.matrix(table(datavalidationmodel$resvalidationmodel$predictclassval, datavalidationmodel$resvalidationmodel$classval))
},include.rownames=TRUE)
output$sensibilityval<-renderText({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  sensibility(predict = datavalidationmodel$resvalidationmodel$predictclassval,class = datavalidationmodel$resvalidationmodel$classval)
})
output$specificityval<-renderText({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  specificity(predict = datavalidationmodel$resvalidationmodel$predictclassval,class =  datavalidationmodel$resvalidationmodel$classval)
})
####Detail of the model
output$summarymodel<-renderPrint({
  model<-print(MODEL()$MODEL)
})
output$plotimportance<-renderPlot({
  model<<-MODEL()$MODEL
  learningmodel<<-MODEL()$DATALEARNINGMODEL$learningmodel
  modeltype<<-input$model
  importanceplot(model = model,learningmodel = learningmodel,modeltype =modeltype,graph=T )
})
output$downloadplotimportance = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =  importanceplot(model = MODEL()$MODEL,learningmodel = MODEL()$DATALEARNINGMODEL$learningmodel,modeltype =input$model,graph=T ),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddataplotimportance <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(     importanceplot(model = MODEL()$MODEL,learningmodel = MODEL()$DATALEARNINGMODEL$learningmodel,modeltype =input$model,graph=F ), file) })

####Test prameters
output$testNAstructure<- reactive({
  if("TRUE"%in%input$NAstructuretest ){test<-as.logical(TRUE)}
  else{test<-as.logical(FALSE)}
  return(test)
})
outputOptions(output, 'testNAstructure', suspendWhenHidden=FALSE)

TESTPARAMETERS <- eventReactive(input$tunetest, { 
  prctvaluestest<-seq(input$prctvaluestest[1],input$prctvaluestest[2],by = 5)
  listparameters<<-list("prctvalues"=prctvaluestest,
                        "selectmethod"=input$selectmethodtest,
                        "NAstructure"=as.logical(input$NAstructuretest),
                        "thresholdNAstructure"=input$thresholdNAstructuretest,
                        "structdata"=input$structdatatest,
                        "maxvaluesgroupmin"=input$maxvaluesgroupmintest,
                        "minvaluesgroupmax"=input$minvaluesgroupmaxtest,
                        "rempNA"=input$rempNAtest,
                        "log"=as.logical(input$logtest),
                        "logtype"=input$logtypetest,
                        "standardization"=as.logical(input$standardizationtest),
                        "arcsin"=as.logical(input$arcsintest),"test"=input$testtest,
                        "adjustpv"=as.logical(input$adjustpvtest),
                        "thresholdpv"=input$thresholdpvtest,
                        "thresholdFC"=input$thresholdFCtest,
                        "model"=input$modeltest,
                        "thresholdmodel"=0,"fs"=as.logical(input$fstest),
                        "threshold_method"=input$threshold_method_test, 
                        "tuning_method"=input$tuning_method_test)
    length(listparameters$prctvalues)
    validate(need( sum(do.call(rbind, lapply(listparameters, FUN=function(x){length(x)==0})))==0,"One of the parameters is empty"))
    tabparameters<<-constructparameters(listparameters)
    # Set initial thresholds for probabilistic models
    # Note: If threshold_method != "fixed", these values will be recalculated
    # in testparametersfunction(). The 0.5 here serves as:
    # - Final threshold if threshold_method = "fixed" (default for probabilistic models)
    # - Initial placeholder if threshold_method = "youden" or "equiprob" (will be optimized)
    
    if(input$threshold_method_test == "fixed"){
      # No optimization: 0.5 is the final threshold for probabilistic models
      cat("✓ Using fixed thresholds: 0.5 for probabilistic models, 0 for SVM\n")
    } else if(input$threshold_method_test == "youden"){
      # Youden optimization enabled: 0.5 is a placeholder, will be recalculated
      cat("✓ Threshold optimization enabled: Youden method (maximize sensitivity + specificity)\n")
      cat("  Initial threshold: 0.5 (placeholder, will be optimized)\n")
      cat("\n")
      cat("⚠️  IMPORTANT NOTE about threshold optimization in Test Parameters:\n")
      cat("   - The threshold is optimized on TRAINING data for each parameter combination\n")
      cat("   - This is CORRECT methodology: fit threshold on train, apply to validation\n")
      cat("   - However, when comparing many combinations, the best validation result may be\n")
      cat("     slightly optimistic due to multiple testing (similar to hyperparameter tuning)\n")
      cat("   - Recommendation: Use these results to SELECT the best configuration,\n")
      cat("     then RE-VALIDATE on independent test data if available\n")
      cat("\n")
    } else if(input$threshold_method_test == "equiprob"){
      # Equiprobability optimization enabled: 0.5 is a placeholder, will be recalculated
      cat("✓ Threshold optimization enabled: Equiprobability method (minimize |FP-FN|)\n")
      cat("  Initial threshold: 0.5 (placeholder, will be optimized)\n")
      cat("\n")
      cat("⚠️  IMPORTANT NOTE about threshold optimization in Test Parameters:\n")
      cat("   - The threshold is optimized on TRAINING data for each parameter combination\n")
      cat("   - This is CORRECT methodology: fit threshold on train, apply to validation\n")
      cat("   - However, when comparing many combinations, the best validation result may be\n")
      cat("     slightly optimistic due to multiple testing (similar to hyperparameter tuning)\n")
      cat("   - Recommendation: Use these results to SELECT the best configuration,\n")
      cat("     then RE-VALIDATE on independent test data if available\n")
      cat("\n")
    }
    
    tabparameters$thresholdmodel[which(tabparameters$model=="randomforest")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="elasticnet")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="xgboost")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="lightgbm")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="knn")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="naivebayes")]<-0.5
    
    validation<<-DATA()$VALIDATION
    learning<<-DATA()$LEARNING
    tabparametersresults<<-testparametersfunction(learning,validation,tabparameters)
    #clean useless columns
    if(length(which(apply(X = tabparametersresults,MARGIN=2,function(x){sum(is.na(x))})==nrow(tabparametersresults)))!=0){
      tabparametersresults<-tabparametersresults[,-which(apply(X = tabparametersresults,MARGIN=2,function(x){sum(is.na(x))})==nrow(tabparametersresults))]}
    return(tabparametersresults)

#     if(sum(listparameters$NAstructure)==0){tabparametersresults<-
#       tabparametersresults[,-c("thresholdNAstructure","structdata")]
#     }
    
                       
  })
# output$testtabparameters<- reactive({
#   if(!tabparameters ){test<-as.logical(FALSE)}
#   else{test<-as.logical(TRUE)}
#   return(test)
# })
# outputOptions(output, 'testNAstructure', suspendWhenHidden=FALSE)

output$tabtestparameters<-renderDataTable({
  resparameters<<-TESTPARAMETERS()
  cbind(Names=rownames(resparameters),resparameters)},
  options = list(    "orderClasses" = F,
                     "responsive" = F,
                     "pageLength" = 100
            #          ,rowCallback = I('
            # function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {$("td:eq(1)", nRow).css("color", "red");}'
                                                        # )
            )
            )


output$downloadtabtestparameters <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   TESTPARAMETERS(), file) })




# Nouveaux graphiques améliorés
output$plottestparametersthreshold = renderPlot({
  resparameters<<-TESTPARAMETERS()
  plot_threshold_performance(dataset_test_params = resparameters)
})

output$downloadplottestparametersthreshold = downloadHandler(
  filename = function() {paste('graph_threshold_performance','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = plot_threshold_performance(dataset_test_params = TESTPARAMETERS()),  
           device = input$paramdownplot)},
  contentType=NA)

output$plottestparametersoverfitting = renderPlot({
  resparameters<<-TESTPARAMETERS()
  plot_overfitting(dataset_test_params = resparameters)
})

output$downloadplottestparametersoverfitting = downloadHandler(
  filename = function() {paste('graph_overfitting','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = plot_overfitting(dataset_test_params = TESTPARAMETERS()),  
           device = input$paramdownplot)},
  contentType=NA)


# Fonction améliorée avec filtrage, tri et intervalles de confiance
plotbarstest =  function(dataset_test_params, type, filter_invalid = FALSE, show_ci = FALSE){
  # Filtrer les résultats non valides (AUC < 0.5 ou NA)
  if(filter_invalid){
    dataset_test_params <- dataset_test_params %>%
      filter(
        (`auc learning` > 0.5 | is.na(`auc learning`)),
        (`auc validation` > 0.5 | is.na(`auc validation`)),
        !is.na(`threshold used`) | is.na(`threshold used`)
      )
  }
  
  if(type ==  'learning'){
    new_dataset  =  dataset_test_params %>% 
      group_by(model, test) %>%
      summarise(
        mean_auc_learning = mean(`auc learning`, na.rm = TRUE),
        se_auc_learning = sd(`auc learning`, na.rm = TRUE) / sqrt(n()),
        mean_sensibility_learning = mean(`sensibility learning`, na.rm = TRUE),
        se_sensibility_learning = sd(`sensibility learning`, na.rm = TRUE) / sqrt(n()),
        mean_specificity_learning = mean(`specificity learning`, na.rm = TRUE),
        se_specificity_learning = sd(`specificity learning`, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop',
        count = n()
      )
    
    # Trier les modèles par performance moyenne (AUC)
    model_order <- new_dataset %>%
      group_by(model) %>%
      summarise(mean_perf = mean(mean_auc_learning, na.rm = TRUE)) %>%
      arrange(desc(mean_perf)) %>%
      pull(model)
    
    # Convertir le jeu de données en format long
    data_long <- pivot_longer(new_dataset, 
                              cols = starts_with("mean_"), 
                              names_to = "metric", 
                              values_to = "value")
    
    se_long <- pivot_longer(new_dataset,
                            cols = starts_with("se_"),
                            names_to = "metric_se",
                            values_to = "se")
    
    # Joindre les erreurs standard
    data_long$se <- se_long$se[match(
      paste(data_long$model, data_long$test, gsub("mean_", "", data_long$metric)),
      paste(se_long$model, se_long$test, gsub("se_", "", se_long$metric_se))
    )]
    
    data_long = data_long  %>% mutate(metric = recode(metric,
                                                      "mean_auc_learning" = "AUC Learning",
                                                      "mean_sensibility_learning" = "Sensitivity Learning",
                                                      "mean_specificity_learning" = "Specificity Learning")
    )
    
    # Trier les modèles
    data_long$model <- factor(data_long$model, levels = model_order)
    
  } else if(type == 'validation'){
    new_dataset  =  dataset_test_params %>% 
      group_by(model, test) %>%
      summarise(
        mean_auc_validation = mean(`auc validation`, na.rm = TRUE),
        se_auc_validation = sd(`auc validation`, na.rm = TRUE) / sqrt(n()),
        mean_sensibility_validation = mean(`sensibility validation`, na.rm = TRUE),
        se_sensibility_validation = sd(`sensibility validation`, na.rm = TRUE) / sqrt(n()),
        mean_specificity_validation = mean(`specificity validation`, na.rm = TRUE),
        se_specificity_validation = sd(`specificity validation`, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop',
        count = n()
      )
    
    # Trier les modèles par performance moyenne (AUC)
    model_order <- new_dataset %>%
      group_by(model) %>%
      summarise(mean_perf = mean(mean_auc_validation, na.rm = TRUE)) %>%
      arrange(desc(mean_perf)) %>%
      pull(model)
    
    # Convertir le jeu de données en format long
    data_long <- pivot_longer(new_dataset, 
                              cols = starts_with("mean_"), 
                              names_to = "metric", 
                              values_to = "value")
    
    se_long <- pivot_longer(new_dataset,
                            cols = starts_with("se_"),
                            names_to = "metric_se",
                            values_to = "se")
    
    # Joindre les erreurs standard
    data_long$se <- se_long$se[match(
      paste(data_long$model, data_long$test, gsub("mean_", "", data_long$metric)),
      paste(se_long$model, se_long$test, gsub("se_", "", se_long$metric_se))
    )]
    
    data_long = data_long  %>% mutate(metric = recode(metric,
                                                      "mean_auc_validation" = "AUC Validation",
                                                      "mean_sensibility_validation" = "Sensitivity Validation",
                                                      "mean_specificity_validation" = "Specificity Validation"
    )
    )
    
    # Trier les modèles
    data_long$model <- factor(data_long$model, levels = model_order)
    
  }else if (type == 'both'){
    new_dataset  =  dataset_test_params %>% 
      group_by(model, test) %>%
      summarise(
        mean_auc_validation = mean(`auc validation`, na.rm = TRUE),
        se_auc_validation = sd(`auc validation`, na.rm = TRUE) / sqrt(n()),
        mean_sensibility_validation = mean(`sensibility validation`, na.rm = TRUE),
        se_sensibility_validation = sd(`sensibility validation`, na.rm = TRUE) / sqrt(n()),
        mean_specificity_validation = mean(`specificity validation`, na.rm = TRUE),
        se_specificity_validation = sd(`specificity validation`, na.rm = TRUE) / sqrt(n()),
        mean_auc_learning = mean(`auc learning`, na.rm = TRUE),
        se_auc_learning = sd(`auc learning`, na.rm = TRUE) / sqrt(n()),
        mean_sensibility_learning = mean(`sensibility learning`, na.rm = TRUE),
        se_sensibility_learning = sd(`sensibility learning`, na.rm = TRUE) / sqrt(n()),
        mean_specificity_learning = mean(`specificity learning`, na.rm = TRUE),
        se_specificity_learning = sd(`specificity learning`, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop',
        count = n()
      )
    
    # Trier les modèles par performance moyenne (AUC validation, ou learning si validation NA)
    model_order <- new_dataset %>%
      group_by(model) %>%
      summarise(mean_perf = mean(ifelse(is.na(mean_auc_validation), mean_auc_learning, mean_auc_validation), na.rm = TRUE)) %>%
      arrange(desc(mean_perf)) %>%
      pull(model)
    
    # Convertir le jeu de données en format long
    data_long <- pivot_longer(new_dataset, 
                              cols = starts_with("mean_"), 
                              names_to = "metric", 
                              values_to = "value")
    
    se_long <- pivot_longer(new_dataset,
                            cols = starts_with("se_"),
                            names_to = "metric_se",
                            values_to = "se")
    
    # Joindre les erreurs standard
    data_long$se <- se_long$se[match(
      paste(data_long$model, data_long$test, gsub("mean_", "", data_long$metric)),
      paste(se_long$model, se_long$test, gsub("se_", "", se_long$metric_se))
    )]
    
    data_long = data_long  %>% mutate(metric = recode(metric,
                                                      "mean_auc_validation" = "AUC Validation",
                                                      "mean_sensibility_validation" = "Sensitivity Validation",
                                                      "mean_specificity_validation" = "Specificity Validation",
                                                      "mean_auc_learning" = "AUC Learning",
                                                      "mean_sensibility_learning" = "Sensitivity Learning",
                                                      "mean_specificity_learning" = "Specificity Learning")
    )
    
    # Trier les modèles
    data_long$model <- factor(data_long$model, levels = model_order)
  }
  
  # if(type == 'both'){
  #   scale_fill_manual(fill = c("AUC Learning" = "#E41A1C",
  #                              "Sensitivity Learning" = "#377EB8",
  #                              "Specificity Learning" = "#4DAF4A",
  #                              "AUC Validation" = "#E41A1C",
  #                              "Sensitivity Validation" = "#377EB8",
  #                              "Specificity Validation" = "#4DAF4A"))  
  # }else if(type == 'learning'){
  #   scale_fill_manual(fill = c("AUC Learning" = "#E41A1C",
  #                              "Sensitivity Learning" = "#377EB8",
  #                              "Specificity Learning" = "#4DAF4A")) 
  # }else if(type == 'validation'){
  #   scale_fill_manual(fill = c("AUC Validation" = "#E41A1C",
  #                              "Sensitivity Validation" = "#377EB8",
  #                              "Specificity Validation" = "#4DAF4A")) 
  # }
  
  # Créer le graphique à barres avec intervalles de confiance
  p <- ggplot(data_long, aes(x = model, y = value, fill = metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = round(value*100, 1)), 
              position = position_dodge(width = 0.8), 
              vjust = -0.5, size = 4) 
  
  # Ajouter les intervalles de confiance si demandé
  if(show_ci && !all(is.na(data_long$se))){
    p <- p + geom_errorbar(aes(ymin = value - 1.96*se, ymax = value + 1.96*se),
                           position = position_dodge(width = 0.9), 
                           width = 0.2, alpha = 0.7)
  }
  
  p <- p +
    facet_wrap(~ test, ncol = 2) +
    labs(x = "Models (sorted by performance)", 
         y = "Scores", 
         title = "Comparison of metrics by model and by test") +
    theme_minimal() +
    theme(axis.text.x = element_text(size =  12,face = 'bold', angle = 45, hjust = 1),
          axis.text.y =  element_text(size =  12,face = 'bold'),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"),
          legend.text = element_text(size =10, face = 'bold'),
          legend.title = element_text(size =12, face = 'bold')
    ) + 
    scale_fill_brewer(palette = "Set1")
  
  return(p)
}

# Nouvelle fonction : Graphique seuil vs performance
plot_threshold_performance = function(dataset_test_params, filter_invalid = TRUE){
  # Filtrer les résultats non valides
  if(filter_invalid){
    dataset_test_params <- dataset_test_params %>%
      filter(
        (`auc learning` > 0.5 | is.na(`auc learning`)),
        (`auc validation` > 0.5 | is.na(`auc validation`)),
        !is.na(`threshold used`)
      )
  }
  
  # Filtrer les valeurs NA
  dataset_clean <- dataset_test_params %>%
    filter(!is.na(`threshold used`), 
           !is.na(`auc validation`) | !is.na(`auc learning`))
  
  if(nrow(dataset_clean) == 0){
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot", size = 6) +
             theme_void())
  }
  
  # Créer le graphique
  p <- ggplot(dataset_clean, aes(x = `threshold used`, y = `auc validation`, color = model)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    facet_wrap(~ test, ncol = 2) +
    labs(x = "Optimal Threshold", 
         y = "AUC Validation",
         title = "Relationship between optimal threshold and validation performance",
         color = "Model") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 11, face = "bold"))
  
  return(p)
}

# Nouvelle fonction : Graphique overfitting
plot_overfitting = function(dataset_test_params, filter_invalid = TRUE){
  # Filtrer les résultats non valides
  if(filter_invalid){
    dataset_test_params <- dataset_test_params %>%
      filter(
        (`auc learning` > 0.5 | is.na(`auc learning`)),
        (`auc validation` > 0.5 | is.na(`auc validation`))
      )
  }
  
  # Calculer les différences (overfitting)
  dataset_overfit <- dataset_test_params %>%
    filter(!is.na(`auc learning`), !is.na(`auc validation`)) %>%
    mutate(
      overfitting_auc = `auc learning` - `auc validation`,
      overfitting_sens = `sensibility learning` - `sensibility validation`,
      overfitting_spec = `specificity learning` - `specificity validation`
    ) %>%
    select(model, test, overfitting_auc, overfitting_sens, overfitting_spec)
  
  if(nrow(dataset_overfit) == 0){
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot", size = 6) +
             theme_void())
  }
  
  # Calculer moyennes et erreurs standard par groupe
  dataset_summary <- dataset_overfit %>%
    pivot_longer(cols = starts_with("overfitting_"), 
                 names_to = "metric", 
                 values_to = "overfitting") %>%
    group_by(model, test, metric) %>%
    summarise(
      mean_overfitting = mean(overfitting, na.rm = TRUE),
      se_overfitting = sd(overfitting, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    ) %>%
    mutate(metric = recode(metric,
                           "overfitting_auc" = "AUC Overfitting",
                           "overfitting_sens" = "Sensitivity Overfitting",
                           "overfitting_spec" = "Specificity Overfitting"))
  
  # Trier les modèles par overfitting moyen (AUC)
  model_order <- dataset_summary %>%
    filter(metric == "AUC Overfitting") %>%
    group_by(model) %>%
    summarise(mean_overfit = mean(mean_overfitting, na.rm = TRUE)) %>%
    arrange(desc(mean_overfit)) %>%
    pull(model)
  
  dataset_summary$model <- factor(dataset_summary$model, levels = model_order)
  
  # Créer le graphique
  p <- ggplot(dataset_summary, aes(x = model, y = mean_overfitting, fill = model)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = mean_overfitting - 1.96*se_overfitting,
                      ymax = mean_overfitting + 1.96*se_overfitting),
                  position = position_dodge(width = 0.9),
                  width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    facet_grid(metric ~ test) +
    labs(x = "Models (sorted by AUC overfitting)", 
         y = "Mean Overfitting (Learning - Validation)",
         title = "Model overfitting analysis (positive = overfitting)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, face = 'bold', angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10, face = 'bold'),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 10, face = "bold"),
          legend.position = "none")
  
  return(p)
}


# # PARTIE LEARNING
output$plottestparameterslearning = renderPlot({
  resparameters<<-TESTPARAMETERS()
  plotbarstest(dataset_test_params = resparameters, type ='learning')
})

output$downloadplottestparametersvalidation = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   plotbarstest(dataset_test_params = TESTPARAMETERS() , type ='learning' ),
           device = input$paramdownplot)},
  contentType=NA)


# PARTIE VALIDATION
output$plottestparametersvalidation =  renderPlot({
  resparameters<<-TESTPARAMETERS()
  plotbarstest(dataset_test_params = resparameters, type ='validation')
})

output$downloadplottestparameterslearning = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   plotbarstest(dataset_test_params = TESTPARAMETERS() , type ='validation' ),
           device = input$paramdownplot)},
  contentType=NA)

# PARTIE GLOBAL
output$plottestparametersboth =  renderPlot({
  resparameters<<-TESTPARAMETERS()
  plotbarstest(dataset_test_params = resparameters, type ='both')
})


output$downloadplottestparametersboth = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   plotbarstest(dataset_test_params = TESTPARAMETERS() , type ='both' ),
           device = input$paramdownplot)},
  contentType=NA)

# 
# plotbarstest =  function(dataset_test_params, type){
#   # library(dplyr)
#   if(type ==  'learning'){
#     new_dataset  =  dataset_test_params %>% 
#       group_by(model, test) %>%
#       summarise(mean_auc_learning = mean(`auc learning`, na.rm = TRUE),
#                 mean_sensibility_learning = mean(`sensibility learning`, na.rm = TRUE),
#                 mean_specificity_learning = mean(`specificity learning`, na.rm = TRUE),
#                 .groups = 'drop',
#                 count = n())
#     
#     # Convertir le jeu de données en format long
#     data_long <- pivot_longer(new_dataset, 
#                               cols = starts_with("mean_"), 
#                               names_to = "metric", 
#                               values_to = "value")
#     
#     data_long = data_long  %>% mutate(metric = recode(metric,
#                                                       "mean_auc_learning" = "AUC Learning",
#                                                       "mean_sensibility_learning" = "Sensitivity Learning",
#                                                       "mean_specificity_learning" = "Specificity Learning")
#     )
#     
#   } else if(type == 'validation'){
#     new_dataset  =  dataset_test_params %>% 
#       group_by(model, test) %>%
#       summarise(mean_auc_validation = mean(`auc validation`, na.rm = TRUE),
#                 mean_sensibility_validation = mean(`sensibility validation`, na.rm = TRUE),
#                 mean_specificity_validation = mean(`specificityvalidation`, na.rm = TRUE),
#                 .groups = 'drop',
#                 count = n())
#     
#     # Convertir le jeu de données en format long
#     data_long <- pivot_longer(new_dataset, 
#                               cols = starts_with("mean_"), 
#                               names_to = "metric", 
#                               values_to = "value")
#     
#     data_long = data_long  %>% mutate(metric = recode(metric,
#                                                       "mean_auc_validation" = "AUC Validation",
#                                                       "mean_sensibility_validation" = "Sensitivity Validation",
#                                                       "mean_specificity_validation" = "Specificity Validation"
#                                                       )
#     )
#     
#   }else if (type == 'both'){
#     new_dataset  =  dataset_test_params %>% 
#       group_by(model, test) %>%
#       summarise(mean_auc_validation = mean(`auc validation`, na.rm = TRUE),
#                 mean_sensibility_validation = mean(`sensibility validation`, na.rm = TRUE),
#                 mean_specificity_validation = mean(`specificityvalidation`, na.rm = TRUE),
#                 mean_auc_learning = mean(`auc learning`, na.rm = TRUE),
#                 mean_sensibility_learning = mean(`sensibility learning`, na.rm = TRUE),
#                 mean_specificity_learning = mean(`specificity learning`, na.rm = TRUE),
#                 .groups = 'drop',
#                 count = n())
#     
#     # Convertir le jeu de données en format long
#     data_long <- pivot_longer(new_dataset, 
#                               cols = starts_with("mean_"), 
#                               names_to = "metric", 
#                               values_to = "value")
#     
#     data_long = data_long  %>% mutate(metric = recode(metric,
#                                                       "mean_auc_validation" = "AUC Validation",
#                                                       "mean_sensibility_validation" = "Sensitivity Validation",
#                                                       "mean_specificity_validation" = "Specificity Validation",
#                                                       "mean_auc_learning" = "AUC Learning",
#                                                       "mean_sensibility_learning" = "Sensitivity Learning",
#                                                       "mean_specificity_learning" = "Specificity Learning")
#     )
#     
#   }
#   
#   # Créer le graphique à barres
#   # ggplot(data_long, aes(x = interaction(model, test), y = value, fill = metric)) +
#   #   geom_bar(stat = "identity", position = "dodge") +
#   #   labs(x = "Modèle et Test", y = "Valeur", title = "Comparaison des métriques par modèle et test") +
#   #   theme_minimal() +
#   #   theme(axis.text.x = element_text(size =  12,face = 'bold'),
#   #         axis.text.y =  element_text(size =  12,face = 'bold'),
#   #         plot.title = element_text(size = 14, face = "bold"),
#   #         axis.title.x = element_text(size = 13, face = "bold"),
#   #         axis.title.y = element_text(size = 13, face = "bold")
#   #   ) +
#   #   scale_fill_brewer(palette = "Set1") +
#   #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   
#   # Créer le graphique à barres avec des facettes pour chaque test
#   ggplot(data_long, aes(x = model, y = value, fill = metric)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#     geom_text(aes(label = round(value*100, 1)), 
#               position = position_dodge(width = 0.8), 
#               vjust = -0.5) + 
#     facet_wrap(~ test, ncol = 2) +
#     labs(x = "Models", y = "Scores", title = "Comparaison des métriques par modèle et test") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(size =  12,face = 'bold'),
#           axis.text.y =  element_text(size =  12,face = 'bold'),
#           plot.title = element_text(size = 14, face = "bold"),
#           axis.title.x = element_text(size = 13, face = "bold"),
#           axis.title.y = element_text(size = 13, face = "bold"),
#           strip.text = element_text(size = 12, face = "bold"),
#           legend.text = element_text(size =10, face = 'bold'),
#           legend.title = element_text(size =12, face = 'bold')
#     ) + 
#     scale_fill_brewer(palette = "Set1") +
#     # scale_fill_manual(values = custom_colors) +
#     theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
#   
# }

}) 

# 
