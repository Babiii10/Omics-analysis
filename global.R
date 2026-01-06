options(xtable.include.colnames=T)
options(xtable.include.rownames=T)
#Packages
#rm(list=ls())
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("zoo")
usePackage("missMDA")#imputepca
usePackage("ggplot2")#Graphs
usePackage("stats")
usePackage("tidyr")
usePackage("e1071")#svm
usePackage("pROC")#roccurve
usePackage("devtools")
usePackage("readxl")
usePackage("superml")
usePackage("shiny")
usePackage("shinythemes")
usePackage("bslib")
# if (!is.element("factoextra", installed.packages()[,1]))
#   install_github("kassambara/factoextra")
#usePackage("factoextra")#PCA graphs
usePackage("reshape2")#melt function
usePackage("xlsx")#import fichier xls#Fonctions
usePackage("randomForest")
usePackage("missForest")
usePackage("Hmisc")
usePackage("corrplot")
usePackage("penalizedSVM")
usePackage("DT")
usePackage("shinycssloaders")
usePackage("writexl")
usePackage("glmnet")#for lasso, elasticnet, ridge regression
usePackage("survival")#for cox regression
usePackage("xgboost")#for xgboost gradient boosting
usePackage("lightgbm")#for lightgbm gradient boosting
usePackage("class")#for k-nearest neighbors


##########################
importfile<-function (datapath,extension,NAstring="NA",sheet=1,skiplines=0,dec=".",sep=","){
  # datapath: path of the file
  #extention: extention of the file : csv, xls, ou xlsx
  if(extension=="csv"){
    toto <<- read.csv2(datapath,header = F,sep =sep,dec=dec,na.strings = NAstring,stringsAsFactors = F,row.names=NULL,check.names = F )
  }
  if(extension=="xlsx"){
    options(warn=-1)
    filerm<<-file.rename(datapath,paste(datapath, ".xlsx", sep=""))
    options(warn=0)
    toto <<- read_excel(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet) %>% as.data.frame()
    #toto <<- read_xlsx(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
    #toto <-read.xlsx2(file = datapath,sheetIndex = sheet)
    #toto <-read_excel(datapath,na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
  }
  #remove empty column
  if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
    toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
  #remove empty row
  if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
    toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
  print(class(toto))
  
  rnames<-as.character(as.matrix(toto[,1]))
  cnames<-as.character(as.matrix(toto[1,]))
  toto<-toto[,-1]
  toto<-toto[-1,]
  row.names(toto)<-rnames[-1]
  colnames(toto)<-cnames[-1]

  toto<-as.data.frame(toto)
  rownames(toto)<-rnames[-1]
  colnames(toto)<-cnames[-1]
  return(toto)
}

# downloaddataset <- function(x,file,cnames=T,rnames=T){
#   ext<-strsplit(x = file,split = "[.]")[[1]][2]
#   if(ext=="csv"){
#     if(sum(cnames,rnames)==2){
#       write.csv(x,file)
#       }
#     else{
#       write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
#       }
#   }
#   if(ext=="xlsx"){
#     write.xlsx(x,file,col.names = cnames,row.names =rnames )
#   }
#   
# }

# df <- reactive({
#   req(input$learningfile)
#   file <- input$learningfile
#   ext <- tools::file_ext(file$datapath)
#   
#   req(file)
#   validate(need(ext == "xlsx", "Veuillez télécharger un fichier CSV"))
#   
#   df = read_excel(file$datapath)
#   print(head(df))
#   return( df)
# })


downloaddataset <- function(x,file,cnames=T,rnames=T){
  ext = tools::file_ext(file)
  if(ext=="csv"){
    if(sum(cnames,rnames)==2){
      write.csv(x,file)
    }
    else{
      write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
    }
  }
  if(ext=="xlsx"){
    #write.xlsx(x,file,col.names = cnames,row.names =rnames )
    writexl::write_xlsx(x,file, col_names = cnames)
  }
  
}

downloadplot <- function(file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  
  if(ext=="png"){
    png(file)
  }
  if(ext=="jpg"){
    jpeg(file)
  }  
  if(ext=="pdf"){
    pdf(file) 
  }     
}
# renamvar<-function(names){
#   #rename the duplicate name by adding ".1, .2 ....
#   #toto is a vector of the col names of the tab
#   names[is.na(names)]<-"NA"
#   for(i in 1:length(names)){
#     ind <- which(names%in%names[i])
#     if(length(ind)>1){
#       nb<-c(1:length(ind))
#       newnames<-paste(names[ind],".",nb,sep="")
#       
#       names[ind]<-newnames
#     }
#   }
#   return(names)
# }
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
transformdata<-function(toto,transpose,zeroegalNA){
#   if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
#     toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
#   #remove empty rows
#   if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
#     toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
#   #remove empty columns
  
  # transpose du data frame
  if(transpose){
    toto<-t(toto)
    }
  
  if(zeroegalNA){
    toto[which(toto==0,arr.ind = T)]<-NA
    }
  
toto<-as.data.frame(toto[,c(colnames(toto)[1],sort(colnames(toto)[-1]))])
}
confirmdata<-function(toto){
  toto<-as.data.frame(toto)
  toto[,1]<-as.factor(as.character(toto[,1]))
  for (i in 2:ncol(toto)){
    toto[,i]<-as.numeric(as.character(toto[,i]))
  }
  return(toto)
}


importfunction<-function(importparameters){
  previousparameters<-NULL
  validation<-NULL
  learning<-NULL
  
  if(is.null(importparameters$learningfile)&is.null(importparameters$modelfile)){return()}
  
  if(!is.null(importparameters$modelfile) ){
    load(file = importparameters$modelfile$datapath)
    previous<-state
    learning<-previous$data$LEARNING
    validation<-previous$data$VALIDATION
    #lev<-previous$data$LEVELS
    previousparameters<-previous$parameters
  }

  if(!is.null(importparameters$learningfile)  ){
    #if(importparameters$confirmdatabutton==0){
      datapath<- importparameters$learningfile$datapath
      #datapath <- input$learningfile$datapath
      #print(datapath)
      #print(paste(datapath, ".xlsx", sep=""))
      #out<<-tryCatch(
      learning<-importfile(datapath = datapath,extension = importparameters$extension,NAstring=importparameters$NAstring,
                           sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
      #              ,error=function(e) e )
      #            if(any(class(out)=="error")){tablearn<-data.frame()}
      #            else{tablearn<<-out}
      #            validate(need(ncol(tablearn)>1 & nrow(tablearn)>1,"problem import"))
      
      learning<-transformdata(toto = learning,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
      
    #}
    if(importparameters$confirmdatabutton!=0){
      learning<-confirmdata(toto = learning)
      if(importparameters$invers){learning[,1]<-factor(learning[,1],levels = rev(levels(learning[,1])))}
      
      #learning<-learning[-which(apply(X = learning,MARGIN=1,function(x){sum(is.na(x))})==ncol(learning)),]
      
#       lev<-levels(x = tablearn[,1])
#       print(lev)
#       names(lev)<-c("positif","negatif")
    }
    # else{lev<-NULL}
  }

  
  if(!is.null(importparameters$validationfile)  ){
    
    # if(importparameters$confirmdatabutton==0){
      datapathV<- importparameters$validationfile$datapath
      # out<<-tryCatch(
      validation<-importfile(datapath = datapathV,extension = importparameters$extension,
                 NAstring=importparameters$NAstring,sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
      #             ,error=function(e) e)
      #             if(any(class(out)=="error")){tabval<-NULL}
      #            else{tabval<<-out}
      #            validate(need(ncol(tabval)>1 & nrow(tabval)>1,"problem import"))
        validation<-transformdata(toto = validation,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
      
      
    # }
    if(importparameters$confirmdatabutton!=0){
      validation<-confirmdata(toto = validation)
      if(importparameters$invers){validation[,1]<-factor(validation[,1],levels = rev(levels(validation[,1])))}
      
      #validation<-validation[-which(apply(X = validation,MARGIN=1,function(x){sum(is.na(x))})==ncol(validation)),]
        
    }
    
  }

  res<-list("learning"=learning,"validation"=validation,previousparameters=previousparameters)#,"lev"=lev)
  return(res)
}


# selectvar<-function(resPCA,toto){
#   #select variables which are correlate to the axes correlate to the cotegorial variable of the first column
#   restri<-dimdesc(resPCA,axes = c(1:(min(ncol(toto),10)-1)) )
#   varquali<-vector()
#   score<-0
#   #restri is a dimdesc data
#   for (i in 1:length(restri)){
#     if ( !is.null(restri[[i]]$quali ) ) {
#       score<-score+restri[[i]]$quali[[1]]
#       varquali<-c(varquali,row.names(restri[[i]]$quanti))
#     }
#   }
#   #score<-1- ( ( (1+score)*(nrow(toto)-1) )/(nrow(toto)-ncol(toto)-1) )
#   return(list("varquali"=varquali,"score"=score))
# }

# selectdata<-function(toto){
#   #remove variable  with less than 2 value and replace 0 by NA
#   n<-ncol(toto)
#   toto[which(toto==0 ,arr.ind = T )]<-NA
#   vec<-rep(T,length=n)
#   for(i in 2:n){
#     vec[i]<-( (length(unique(toto[,i]))>2) )
#   }
#   #rm var with less than 3 values (0 or NA , and 2 other (important for the rempNA PCA))
#   toto<-toto[,as.logical(vec)]
#   return(toto)
# }

selectdatafunction<-function(learning,selectdataparameters){
  learningselect<-selectprctvalues(toto = learning,prctvalues = selectdataparameters$prctvalues,selectmethod =selectdataparameters$selectmethod)
  if(selectdataparameters$NAstructure==T){
    if(selectdataparameters$structdata=="selecteddata"){learning<-learningselect}
    restestNAstructure<-testNAstructure(toto = learning,threshold = selectdataparameters$thresholdNAstructure,maxvaluesgroupmin=selectdataparameters$maxvaluesgroupmin,
                                        minvaluesgroupmax=selectdataparameters$minvaluesgroupmax)
    if(!is.null(restestNAstructure)){
      learningselect<-cbind(learningselect[,!colnames(learningselect)%in%restestNAstructure$restestNAstructure$names],restestNAstructure$varNAstructure)}
  }
  else{restestNAstructure<-NULL}
  
  return(list(learningselect=learningselect,structuredfeatures=restestNAstructure$varNAstructure,datastructuredfeatures=restestNAstructure$restestNAstructure))
}

testObject <- function(object){
  #test if the object is in the global environnement
  exists(as.character(substitute(object)))
}

selectprctvalues<-function(toto,prctvalues=100,selectmethod="nogroup"){ 
  n<-ncol(toto)
  if (selectmethod=="nogroup"){
    NAvec<-vector(length =max(n,0) )
    for(i in 1:n){
      NAvec[i]<-  (sum(!is.na(toto[,i]))/nrow(toto)  ) 
    }
    vec<-(NAvec>=(prctvalues/100))
    
  } 
  
  if(selectmethod!="nogroup"){
    nbcat<-length(levels(toto[,1]))
    tabgroup<-matrix(nrow = nbcat, ncol=n )
    for(i in 1:nbcat){
      tab<-toto[which(toto[,1]==levels(toto[,1])[i]),]
      for(j in 1:(n) ){
        tabgroup[i,j]<-(sum(!is.na(tab[,j]))/nrow(tab))  
      }  
    }
    if(selectmethod=="onegroup"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(max (x) >= (prctvalues/100)) }) 
    }
    if(selectmethod=="bothgroups"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(min (x) >= (prctvalues/100)) }) 
    }
  }
  totoselect<-toto[,as.logical(vec)]
}

heatmapNA<-function(toto,maintitle="Distribution of NA",graph=T){
 
    if(ncol(toto)==1){errorplot(text = " No structured variables")}
    else{
      names<- paste(toto[,1],1:length(toto[,1]))
      tab<-as.data.frame(toto[,-1])
      tab[which(!is.na(tab) ,arr.ind = T )]<-"Value"
      tab[which(is.na(tab) ,arr.ind = T )]<-"NA"
      #tab<-cbind(paste(toto[,1],1:length(toto[,1])),tab)
      tab<-apply(tab,2,as.factor)
      rownames(tab)<-names
      if(!graph){ return(cbind(rownames(toto),tab))}
      if(graph){
      tabm <- melt(tab)
      #tabm<-tabm[-c(1:nrow(toto)),]
      colnames(tabm)<-c("individuals","variables","value")
      tabm$variables<-as.character(tabm$variables)
      tabm$individuals<-as.character(tabm$individuals)
      if(ncol(toto)>60){
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value)) + scale_fill_manual(values=c("lightgrey","steelblue"),name="")+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
      else{
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_manual(values=c("lightgrey","steelblue"))+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
    }
  }
}

distributionvalues<-function(toto,prctvaluesselect,nvar,maintitle="Number of variables according to\nthe % of values's selected",graph=T,ggplot=T){
  percentagevalues<-seq(0,1,by = 0.01)
  prctall<-apply(X = toto,MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto)
  prctvalueswhithoutgroup<-sapply(X = percentagevalues,FUN = function(x,prct=prctall){sum(x<=prct)})
  prctlev1<-apply(X = toto[which(toto[,1]==levels(toto[,1])[1]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[1]),])
  prctlev2<-apply(X = toto[which(toto[,1]==levels(toto[,1])[2]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[2]),])
  
  nvareachgroups<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,min))})  
  nvaronegroup<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,max))})  
  
  distribvalues<-data.frame("percentagevalues"=percentagevalues,"all samples"=prctvalueswhithoutgroup,"each groups"= nvareachgroups,"at least one group"=nvaronegroup)
  if(!graph)(return(distribvalues))
  col<-gg_color_hue(ncol(distribvalues)-1)
  if(!ggplot){
    matplot(x=distribvalues$percentagevalues,distribvalues[,-1],type=c("l","l"),lty = c(1,1,1),
            col=c("red","green","blue"), xlab="percentage of values selected",ylab="Number of variables",main=maintitle)
    legend("bottomright",colnames(distribvalues[,-1]),col=c("red","green","blue"),lty=1)
    abline(v = prctvaluesselect,lty=3,col="grey")
    abline(h = nvar,lty=3,col="grey")
  }
  if (ggplot){
    distribvalueslong<- melt(distribvalues,id.vars = "percentagevalues",variable.name = "select_method",value.name = "number_of_variables")  # convert to long format
    p<-ggplot(data=distribvalueslong,
              aes(x=percentagevalues, y=number_of_variables, colour=select_method)) +geom_line()+
      ggtitle(maintitle)
    p+theme(plot.title=element_text( size=15),legend.text=element_text(size=10),legend.title=element_text(color = 0),legend.position=c(0.20,0.15))+
      geom_vline(xintercept=prctvaluesselect,linetype=3)+
      geom_hline(yintercept=nvar,linetype=3)
  }
}

proptestNA<-function(toto){
  group<-toto[,1]
  toto[,1]<-as.character(toto[,1])
  toto[which(!is.na(toto),arr.ind=T)]<-"value"
  toto[which(is.na(toto),arr.ind=T)]<-"NA"
  pval<-vector("numeric",length = ncol(toto))
  lessgroup<-vector("character",length = ncol(toto))
  prctmore<-vector("numeric",length = ncol(toto))
  prctless<-vector("numeric",length = ncol(toto))
  for (i in 1:ncol(toto)){
    conting<-table(group,factor(toto[,i],levels=c("value","NA")))
    options(warn=-1)
    res<-prop.test(conting)
    options(warn=0)
    pval[i]<-res$p.value
    prctmore[i]<-max(res$estimate)
    prctless[i]<-min(res$estimate)
    if(res$estimate[1]==res$estimate[2]){ lessgroup[i]<-"NA"}
    else{lessgroup[i]<-rownames(conting)[which(res$estimate==min(res$estimate))]}
  }
  pval[is.na(pval)]<-1
  return(data.frame("pval"=pval,"lessgroup"=lessgroup,"prctless"=prctless,"prctmore"=prctmore,"names"=colnames(toto)))
}

testNAstructure<-function(toto,threshold=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){
  class<-toto[,1]
  resproptest<-proptestNA(toto=toto)
  vecond<-c(resproptest$pval<=threshold & resproptest$prctless<=(maxvaluesgroupmin/100) & resproptest$prctmore>=(minvaluesgroupmax/100))
  if(sum(vecond)>0){
    resp<-resproptest[vecond,]
    totopropselect<-data.frame(toto[,vecond])
    colnames(totopropselect)<-resp$names
    totopropselect<-as.data.frame(totopropselect[, order(resp[,2])])
    colnames(totopropselect)<-resp$names[order(resp[,2])]
  }
  else{return(NULL)}

  return(list("varNAstructure"=totopropselect,"restestNAstructure"=resp))
}

transformdatafunction<-function(learningselect,structuredfeatures,datastructuresfeatures,transformdataparameters){
  learningtransform<-learningselect
  if(!is.null(structuredfeatures)){
    for(i in 1:ncol(structuredfeatures)){
      learningtransform[which(is.na(structuredfeatures[,i])&learningselect[,1]==as.character(datastructuresfeatures[i,"lessgroup"])),as.character(datastructuresfeatures[i,"names"])]<-0
    }
  }
  if(transformdataparameters$log){ 
    learningtransform[,-1]<-transformationlog(x = learningtransform[,-1]+1,logtype=transformdataparameters$logtype)}
  if(transformdataparameters$arcsin){
    learningtransform[,-1]<-apply(X = learningtransform[,-1],MARGIN = 2,FUN = function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))})
    learningtransform[,-1]<-asin(sqrt(learningtransform[,-1]))
  }
  if(transformdataparameters$standardization){
    learningtransformsd<<-learningtransform
    sdlearningtransform<-apply(X = learningtransform[-1],MARGIN = 2,FUN = sd,na.rm=T)
    #print('sdlearningtransform')
    #print(sdlearningtransform)
    learningtransform[,-1]<-scale(learningtransform[,-1],center = F,scale=sdlearningtransform)
    #learningtransform[,-1]<-scale(learningtransform[,-1], center = F, scale = TRUE)
  }
  learningtransform<-replaceNA(toto=learningtransform,rempNA=transformdataparameters$rempNA,pos=T,NAstructure = F)
  
  return(learningtransform)
}

transformationlog<-function(x,logtype){
  if(logtype=="log10"){x<-log10(x)}
  if(logtype=="log2"){x<-log2(x)}
  if(logtype=="logn"){x<-log(x)}
  return(x)
}

histplot<-function(toto,graph=T){

    data<-data.frame("values"=as.vector(as.matrix(toto[,-1])))
    if(graph==F){ return(datahistogram(data = data,nbclass = 20))}
    if(graph==T){
    ggplot(data=data,aes(x=values) )+ 
      geom_histogram(col="lightgrey",fill="steelblue",bins=20)+ggtitle("Distribution of values")+
      theme(plot.title = element_text(size=15))+
       annotate("text",x=Inf,y=Inf,label=paste(nrow(data),"values"),size=6,vjust=2,hjust=1.5)
  }
}
datahistogram<-function(data,nbclass){
  dh<-hist(data[,1],nclass=nbclass,plot=F)
  minclass<-dh$breaks[-(length(dh$breaks))]
  maxclass<-dh$breaks[2:(length(dh$breaks))]
  count<-dh$counts
  res<-data.frame("count"=count,"minclass"=minclass,"maxclass"=maxclass)
}

replaceNA<-function(toto,rempNA="z",pos=F,NAstructure=F,thresholdstruct=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){ 
  #rempNA: remplace Non ATtributes values by zero("z"), the mean of the colum (moy), 
  # the mean in each group define by the factor of the first column(moygr), itarative pca (pca), or keep th NA
  if(NAstructure){
    totoNAstruct<-replaceproptestNA(toto = toto,threshold = thresholdstruct ,rempNA =rempNA,maxvaluesgroupmin,minvaluesgroupmax)
    toto[,colnames(totoNAstruct)]<-totoNAstruct
  }
  
  if (rempNA == "none" | sum(is.na(toto))==0 ) {return(toto)}
  cnames<-colnames(toto)
  class<-(toto[,1])
  cat<-levels(class)
  toto<-as.data.frame(toto[,-1],optional = T)
  #toto<-apply(toto,MARGIN = 2,function(x)as.numeric(x))
  n<-ncol(toto) 
  #par default je remplace les NA par 0
  if (rempNA == "z") {
    toto[which(is.na(toto),arr.ind = T)]<-0
  }
  if (rempNA== "moy") {
    toto<-na.aggregate(toto)}
  if(rempNA=="moygr"){
    
    for (i in 1:length(cat)){
      tab<-toto[which(class==cat[i]),]
      tab<-na.aggregate(tab)
      toto[which(class==cat[i]),]<-tab
    }
    toto[which(is.na(toto) ,arr.ind = T )]<-0
  }
  if (rempNA == "pca"){
    
    #prise en compte des liaisons entre variable et de la ressemblance entre individus    
    #nb<-estim_ncpPCA(toto[,(nbqualisup+1):n],ncp.min = 0,ncp.max = 10,method.cv = "Kfold")    #take a lot time
    nindiv<-nrow(toto)
    prctnacol<-apply(X = toto,MARGIN = 2,FUN=function(x){ if(sum(!is.na(x))<=0){x<-rep(0,length=nindiv)}
      else{x}})
    toto<-imputePCA(prctnacol,ncp = min(n-1,5),method.cv="Kfold")$completeObs
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
    toto<-as.data.frame(toto)
    
  }
  if(rempNA=="missforest"){
    toto<-missForest(toto,maxiter = 5)$ximp
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
  }
  
  toto<-cbind(class,toto)
  toto[which(is.na(toto),arr.ind = T)]<-0
  
  colnames(toto)<-cnames
  
  return(toto)
}
mdsplot<-function(toto,ggplot=T,maintitle="MDS representation of the individuals",graph=T){
  class<-toto[,1]
  toto<-toto[-1]
  d <- dist(toto) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  x <- fit$points[,1]
  y <- fit$points[,2] 
  coord<-(data.frame("class"=class,x,y))
  if(!graph){return(coord)}
  if(!ggplot){
    colr<-c("red","blue")
    
    plot(x, y, xlab="", ylab="",pch=20,main=maintitle, type="p",col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    text(x, y, labels = row.names(toto), cex=.7,col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    legend("topleft",legend=levels(class),text.col = colr)
  }
  #MDS ggplot
  if(ggplot){
    p <- ggplot(coord, aes(x, y,label=rownames(toto)))
    p + geom_text(aes(colour = class))+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

heatmapplot<-function(toto,ggplot=T,maintitle="Heatmap of the transform data ",scale=F,graph=T){
  row.names(toto)<-paste(toto[,1],1:length(toto[,1]))
  toto<-as.matrix(toto[,-1])
  if(!graph){return(toto)}
  #colnames(toto)<-seq(1:ncol(toto))
  if(scale)toto<-scale(toto, center = F, scale = TRUE)
  if(!ggplot){
      heatmap.2(toto,Rowv = NA,Colv=F,trace="none",dendrogram = "none",key=T,margins=c(2,4),keysize=1.30,main=maintitle)
    }
  if(ggplot){
    titi<-melt(toto,value.name = "Intensity")
    colnames(titi)<-c("Individuals","Variables","Intensity")
    titi[,2]<-as.character(titi[,2])
    ggplot(titi, aes( Variables, Individuals,fill = Intensity),colour=NA) + geom_raster()+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

#############
testfunction<-function(tabtransform,testparameters){
  #condition tests
  if (testparameters$SFtest){
    datatesthypothesis<-SFtest(tabtransform,shaptest=T,Ftest=T,threshold=0.05)
  }
  else{datatesthypothesis<-data.frame()}

  #diff test
  if(testparameters$test=="notest"){
    tabdiff<-tabtransform
    datatest<-NULL
    testparameters<-NULL
    useddata<-NULL
    multivariateresults<-NULL
  }
  else if(testparameters$test%in%c("lasso","elasticnet","ridge","cox")){
    # Multivariate selection methods
    multivariateresults<-multivariateselection(toto = tabtransform,
                                               method = testparameters$test,
                                               lambda = testparameters$lambda,
                                               alpha = testparameters$alpha,
                                               nlambda = 100)
    datatest<-multivariateresults$results

    if(nrow(datatest)==0){
      print("no variables selected by multivariate method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      selected_vars<-multivariateresults$selected_vars
      indvar<-(colnames(tabtransform)%in%selected_vars)
      indvar[1]<-T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
      useddata<-data.frame("names"=datatest$name,
                           "coefficient"=datatest$coefficient,
                          "logFC"=datatest$logFoldChange,
                          "mean1"=datatest$mean_group1,
                          "mean2"=datatest$mean_group2)
    }
  }else if (testparameters$test=="clustEnet"){
    # Clustering + Elastic Net selection method
    cat("Running Clustering + ElasticNet variable selection...\n")
    
    # Get parameters with defaults
    n_clusters <- if(!is.null(testparameters$n_clusters)) testparameters$n_clusters else 100
    n_bootstrap <- if(!is.null(testparameters$n_bootstrap)) testparameters$n_bootstrap else 500
    alpha_enet <- if(!is.null(testparameters$alpha)) testparameters$alpha else 0.5
    min_selection_freq <- if(!is.null(testparameters$min_selection_freq)) testparameters$min_selection_freq else 0.5
    preprocess <- if(!is.null(testparameters$preprocess)) testparameters$preprocess else TRUE
    min_patients <- if(!is.null(testparameters$min_patients)) testparameters$min_patients else 20
    
    multivariateresults <- clustEnetSelection(toto = tabtransform,
                                              n_clusters = n_clusters,
                                              n_bootstrap = n_bootstrap,
                                              alpha_enet = alpha_enet,
                                              min_selection_freq = min_selection_freq,
                                              preprocess = preprocess,
                                              min_patients = min_patients)
    datatest <- multivariateresults$results
    
    if(nrow(datatest)==0){
      print("no variables selected by clustering + elasticnet method")
      tabdiff<<-data.frame()
      useddata<-NULL
    }
    else{
      selected_vars <- multivariateresults$selected_vars
      indvar <- (colnames(tabtransform) %in% selected_vars)
      indvar[1] <- T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
      useddata <- data.frame("names"=datatest$name,
                             "SelectionFrequency"=datatest$SelectionFrequency,
                             "logFC"=datatest$logFoldChange,
                             "mean1"=datatest$mean_group1,
                             "mean2"=datatest$mean_group2)
    }
  }
  else{
    # Univariate tests (Wtest, Ttest)
    multivariateresults<-NULL
    datatest<-diffexptest(toto = tabtransform,test = testparameters$test )
    #differential expressed
    logFC<-datatest[,5]
    if(testparameters$adjustpval){pval<-datatest[,3]}
    if(!testparameters$adjustpval){pval<-datatest[,2]}
    datatestdiff<-datatest[which( (pval<testparameters$thresholdpv)&abs(logFC)>testparameters$thresholdFC ),]
    if(dim(datatestdiff)[1]==0){
      print("no differentially expressed variables")
      tabdiff<<-data.frame()
    }
    else{
      indvar<-(colnames(tabtransform)%in%datatestdiff$name)
      indvar[1]<-T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
    }
    useddata<-data.frame("names"=datatest[,1],
                         "pval"=pval,
                         "logFC"=datatest[,5],
                         "mean1"=datatest[,9],
                         "mean2"=datatest[,10])
  }
  return(list("tabdiff"=tabdiff,
              "datatest"=datatest,
              "hypothesistest"=datatesthypothesis,
              "useddata"=useddata,
              "testparameters"=testparameters,
              "multivariateresults"=multivariateresults))
}
  

diffexptest<-function(toto,test="Wtest"){ 
  #fonction test if the variables (in column) of toto (dataframe) are differently 
  #expressed according to the first variable (first column) (two groups : OP Tem)
  #test= Ttes: porsuit a sTudent test for each column (parmetric test), the sample have to be normal and with the same variance
  #Wtest : willcoxon test (nonparametric), assume that dispersion a on the same scale
  group<-toto[,1]
  toto<-toto[,-1]
  pval<-vector()
  adjustpval<-vector()
  mlev1<-vector()
  namelev1<-levels(group)[1]
  mlev2<-vector()
  namelev2<-levels(group)[2]
  FC1o2<-vector()
  FC2o1<-vector()
  auc<-vector()
  resyounden<-matrix(ncol = 4,nrow = ncol(toto))
  for (i in 1:max(1,ncol(toto)) ){
    lev1<-toto[which(group==namelev1),i]
    lev2<-toto[which(group==namelev2),i]
    mlev1[i]<-mean(lev1,na.rm = T)+0.0001
    mlev2[i]<-mean(lev2,na.rm = T)+0.0001
    
    FC1o2[i]<-mlev1[i]/mlev2[i]
    FC2o1[i]<-mlev2[i]/mlev1[i]
    auc[i]<-auc(roc(group,toto[,i],quiet=TRUE))
    resyounden[i,]<-younden(response = group,predictor = toto[,i])
    if( test=="Ttest"){pval[i]<-t.test(x = lev1,y = lev2)$p.value}
    else if( test=="Wtest"){pval[i]<-wilcox.test(lev1 ,lev2,exact = F)$p.value } 
  } 
  pval[which(is.na(pval))]<-1
  adjustpval<-p.adjust(pval, method = "BH")
  logFC1o2<-log2(abs(FC1o2))
  logFC2o1<-log2(abs(FC2o1))
  
  
  listgen<-data.frame(colnames(toto),pval,adjustpval,auc,FC1o2,logFC1o2,FC2o1,logFC2o1,mlev1,mlev2,resyounden) 
  colnames(listgen)<-c("name",paste("pval",test,sep = ""),paste("BHadjustpval",test,sep = ""),"AUC",paste("FoldChange ",namelev1,"/",namelev2,sep = ""),paste("logFoldChange ",namelev1,"/",namelev2,sep = ""),
                       paste("FoldChange ",namelev2,"/",namelev1,sep = ""),paste("logFoldChange ",namelev2,"/",namelev1,sep = ""),paste("mean",namelev1,sep = ""),paste("mean",namelev2,sep = ""),
                       "younden criterion","sensibility younden","specificity younden","threshold younden") 
  return(listgen)
}

younden<-function(response,predictor){
  res<-roc(response,predictor,quiet=T)
  youndenscore<-res$sensitivities+res$specificities-1
  best<-which(youndenscore==max(youndenscore))[1] # Only the first best is kept
  youndenbest<-youndenscore[best]
  sensiyounden<-res$specificities[best]
  speciyounden<-res$sensitivities[best]
  thresholdyounden<-res$thresholds[best]
  return(c(youndenbest,sensiyounden,speciyounden,thresholdyounden))
}

##########################
# Multivariate variable selection functions
##########################

multivariateselection<-function(toto, method="lasso", lambda=NULL, alpha=0.5, nlambda=100){
  # Function for multivariate variable selection using regularization methods
  # toto: dataframe with first column as group (factor) and other columns as features
  # method: "lasso" (alpha=1), "elasticnet" (0<alpha<1), "ridge" (alpha=0)
  # lambda: regularization parameter (NULL for automatic selection via CV)
  # alpha: elastic net mixing parameter (0=ridge, 1=lasso)
  # nlambda: number of lambda values to test

  # IMPORTANT: Encode group so that 1 = first level (positif), 0 = second level (negatif)
  lev <- levels(toto[,1])
  group <- ifelse(toto[,1] == lev[1], 1, 0)
  x <- as.matrix(toto[,-1])

  # Set alpha based on method
  if(method == "lasso"){
    alpha <- 1
  } else if(method == "ridge" | method == "cox"){
    alpha <- 0
  } else if(method == "elasticnet"){
    # alpha is provided by user, default 0.5
  }

  # Perform cross-validation to find optimal lambda if not provided
  if(is.null(lambda)){
    set.seed(20011203)
    cvfit <- cv.glmnet(x, group, family="binomial", alpha=alpha, nlambda=nlambda,
                       type.measure="auc", nfolds=min(5, nrow(toto)-1)
                       )
    lambda <- cvfit$lambda.min  # lambda that gives minimum CV error
    lambda_1se <- cvfit$lambda.1se  # lambda within 1 SE of minimum
  } else {
    cvfit <- NULL
    lambda_1se <- lambda
  }

  # Fit model with optimal lambda
  fit <- glmnet(x, group, family="binomial", alpha=alpha, lambda=lambda)

  # Extract coefficients
  coef_matrix <- as.matrix(coef(fit))
  coef_values <- coef_matrix[-1, 1]  # Remove intercept
  names(coef_values) <- colnames(x)

  # Select non-zero coefficients
  selected_vars <- names(coef_values[coef_values != 0])

  # Calculate additional statistics for selected variables
  if(length(selected_vars) > 0){
    # AUC for each selected variable
    auc_values <- sapply(selected_vars, function(var){
      auc(roc(group, x[, var], quiet=TRUE))
    })

    # Mean values by group
    mlev1 <- colMeans(x[which(group==0), selected_vars, drop=FALSE], na.rm=TRUE)
    mlev2 <- colMeans(x[which(group==1), selected_vars, drop=FALSE], na.rm=TRUE)

    # Fold change : class 1 sur class 2:  case versus control
    # class 1 : first level (positif)
    # class 2 : second level (negatif)
    FC1o2 <- mlev1 / (mlev2 + 0.0001)
    logFC1o2 <- log2(abs(FC1o2))

    # Create results dataframe
    results <- data.frame(
      name = selected_vars,
      coefficient = coef_values[selected_vars],
      AUC = auc_values,
      FoldChange = FC1o2,
      logFoldChange = logFC1o2,
      mean_group1 = mlev1,
      mean_group2 = mlev2,
      stringsAsFactors = FALSE
    )

    # Sort by absolute coefficient value
    results <- results[order(abs(results$coefficient), decreasing=TRUE), ]
  } else {
    results <- data.frame()
  }

  # Return results with model information
  return(list(
    results = results,
    selected_vars = selected_vars,
    all_coefficients = coef_values,
    lambda = lambda,
    lambda_1se = lambda_1se,
    alpha = alpha,
    cvfit = cvfit,
    fit = fit,
    method = method
  ))
}

##########################
# Clustering + Elastic Net selection function
##########################

# Preprocess peptides: filter low variance and low frequency variables
preprocess_peptides <- function(peptide_data, min_patients = 20) {
  # Filter variables with too few non-zero patients
  n_nonzero <- colSums(peptide_data != 0, na.rm = TRUE)
  keep_peptides <- n_nonzero >= min_patients
  
  # Filter variables with near-zero variance
  variances <- apply(peptide_data, 2, var, na.rm = TRUE)
  keep_var <- variances > 1e-10
  
  return(peptide_data[, keep_peptides & keep_var, drop=FALSE])
}

# Variable selection using clustering and elastic net with bootstrap
varselClust <- function(toto, n_clusters = 100, n_bootstrap = 500, alpha_enet = 0.5,
                        min_selection_freq = 0.5, preprocess = TRUE, min_patients = 20){
  
  withProgress(message = 'Selecting variables in progress...', value = 0, {
    
    # Extract group and data
    lev <- levels(toto[,1])
    group <- ifelse(toto[,1] == lev[1], 1, 0)
    y <- group
    data <- as.matrix(toto[,-1])
    
    # Optional preprocessing
    if(preprocess && ncol(data) > min_patients){
      incProgress(0.05, detail = "Data pre-processing...")
      cat("Preprocessing data: filtering low variance and low frequency variables...\n")
      data_preprocessed <- preprocess_peptides(data, min_patients = min_patients)
      if(ncol(data_preprocessed) < ncol(data)){
        cat(sprintf("  Preprocessing: %d → %d variables (removed %d)\n",
                    ncol(data), ncol(data_preprocessed), ncol(data) - ncol(data_preprocessed)))
        data <- data_preprocessed
      }
    }
    
    if(ncol(data) == 0){
      warning("No variables remaining after preprocessing")
      return(list(
        selected_peptides_per_cluster = character(0),
        final_selected_peptides = character(0),
        selection_frequencies = data.frame()
      ))
    }
    
    # Step 1: Clustering based on Spearman correlation
    incProgress(0.1, detail = sprintf("Clustering (%d variables)...", ncol(data)))
    cat(sprintf("Step 1: Clustering %d variables into %d clusters...\n", ncol(data), n_clusters))
    correlation_matrix <- cor(data, use = "pairwise.complete.obs", method = "spearman")
    #distance_matrix <- 1 - abs(correlation_matrix)
    distance_matrix <- sqrt(2 - 2*correlation_matrix)
    distance_matrix[is.na(distance_matrix)] <- 1
    hc <- hclust(as.dist(distance_matrix), method = "ward.D2")
    k <- min(n_clusters, ncol(data))
    clusters <- cutree(hc, k = k)
    
    # Step 2: Select one variable per cluster using Wilcoxon test
    incProgress(0.05, detail = "Cluster selection...")
    cat(sprintf("Step 2: Selecting one variable per cluster (Wilcoxon test)...\n"))
    selected_peptides <- c()
    
    for (i in 1:k){
      cluster_peptides <- names(clusters[clusters == i])
      
      if (length(cluster_peptides) > 1){
        p_values <- c()
        for (peptide in cluster_peptides){
          test_result <- tryCatch({
            wilcox.test(data[, peptide] ~ y, exact = FALSE)
          }, error = function(e){
            list(p.value = 1)
          })
          p_values <- c(p_values, test_result$p.value)
        }
        min_p_value_index <- which.min(p_values)
        selected_peptide <- cluster_peptides[min_p_value_index]
      } else {
        selected_peptide <- cluster_peptides[1]
      }
      selected_peptides <- c(selected_peptides, selected_peptide)
    }
    
    data_clust <- data[, selected_peptides, drop=FALSE]
    cat(sprintf("  Selected %d variables (one per cluster)\n", ncol(data_clust)))
    
    # Step 3: Bootstrap + Elastic Net selection (70% de la progression)
    incProgress(0, detail = sprintf("Bootstrap + Elastic Net (0/%d)...", n_bootstrap))
    cat(sprintf("Step 3: Bootstrap + Elastic Net selection (%d iterations)...\n", n_bootstrap))
    set.seed(123)
    selected_peptides_list <- list()
    
    progress_step <- 0.7 / n_bootstrap  # 70% du total pour le bootstrap
    
    for (b in 1:n_bootstrap) {
      if(b %% 50 == 0) {
        incProgress(progress_step * 50, 
                    detail = sprintf("Bootstrap: %d/%d (%.1f%%)", b, n_bootstrap, (b/n_bootstrap)*100))
        cat(sprintf("  Bootstrap iteration: %d/%d\n", b, n_bootstrap))
      }
      
      bootstrap_indices <- sample(1:nrow(data_clust), replace = TRUE)
      X_bootstrap <- data_clust[bootstrap_indices, , drop=FALSE]
      y_bootstrap <- y[bootstrap_indices]
      
      lasso_model <- tryCatch({
        cv.glmnet(as.matrix(X_bootstrap),
                  y_bootstrap,
                  family = "binomial",
                  alpha = alpha_enet)
      }, error = function(e){
        NULL
      })
      
      if(!is.null(lasso_model)){
        coef_lasso <- coef(lasso_model, s = "lambda.min")
        selected_peptides_iter <- rownames(coef_lasso)[which(coef_lasso != 0)][-1]
        selected_peptides_list[[b]] <- selected_peptides_iter
      }
    }
    
    # Step 4: Count selection frequencies
    incProgress(0.05, detail = "Frequency calculation...")
    peptide_selection_counts <- table(unlist(selected_peptides_list))
    data_of_frequencies <- sort(peptide_selection_counts, decreasing = TRUE)
    data_of_frequencies_df <- as.data.frame(data_of_frequencies)
    colnames(data_of_frequencies_df) <- c("Variable", "SelectionCount")
    data_of_frequencies_df$SelectionFrequency <- data_of_frequencies_df$SelectionCount / n_bootstrap
    
    # Step 5: Select final variables
    incProgress(0.05, detail = "Final selection...")
    threshold_count <- ceiling(n_bootstrap * min_selection_freq)
    final_selected_peptides <- names(peptide_selection_counts[peptide_selection_counts >= threshold_count])
    
    cat(sprintf("  Final selection: %d variables selected in >= %.0f%% of bootstraps (threshold: %d/%d)\n",
                length(final_selected_peptides), min_selection_freq * 100, threshold_count, n_bootstrap))
    
    incProgress(0, detail = "Done!")
    
    return(list(
      selected_peptides_per_cluster = selected_peptides,
      final_selected_peptides = final_selected_peptides,
      selection_frequencies = data_of_frequencies_df,
      n_clusters = k,
      n_bootstrap = n_bootstrap,
      alpha = alpha_enet,
      min_selection_freq = min_selection_freq
    ))
    
  }) # Fin withProgress
}

##########################
# Wrapper function for clustering + elasticnet to match other test methods
##########################

clustEnetSelection <- function(toto, n_clusters = 100, n_bootstrap = 500,
                               alpha_enet = 0.5, min_selection_freq = 0.5,
                               preprocess = TRUE, min_patients = 20){
  # Run varselClust
  clust_result <- varselClust(toto,
                              n_clusters = n_clusters,
                              n_bootstrap = n_bootstrap,
                              alpha_enet = alpha_enet,
                              min_selection_freq = min_selection_freq,
                              preprocess = preprocess,
                              min_patients = min_patients)
  
  selected_vars <- clust_result$final_selected_peptides
  
  # If no variables selected, return empty results
  if(length(selected_vars) == 0){
    return(list(
      results = data.frame(),
      selected_vars = character(0),
      all_coefficients = numeric(0),
      clust_result = clust_result,
      method = "clustEnet"
    ))
  }
  
  # Calculate statistics for selected variables (similar to multivariateselection)
  lev <- levels(toto[,1])
  group <- ifelse(toto[,1] == lev[1], 1, 0)
  x <- as.matrix(toto[,-1])
  
  # Get selection frequencies for selected variables
  freq_df <- clust_result$selection_frequencies
  freq_values <- freq_df$SelectionFrequency[match(selected_vars, freq_df$Variable)]
  
  # AUC for each selected variable
  auc_values <- sapply(selected_vars, function(var){
    auc(roc(group, x[, var], quiet=TRUE))
  })
  
  # Mean values by group
  mlev1 <- colMeans(x[which(group==0), selected_vars, drop=FALSE], na.rm=TRUE)
  mlev2 <- colMeans(x[which(group==1), selected_vars, drop=FALSE], na.rm=TRUE)
  
  # Fold change
  FC1o2 <- mlev1 / (mlev2 + 0.0001)
  logFC1o2 <- log2(abs(FC1o2))
  
  # Create results dataframe
  results <- data.frame(
    name = selected_vars,
    SelectionFrequency = freq_values,
    AUC = auc_values,
    FoldChange = FC1o2,
    logFoldChange = logFC1o2,
    mean_group1 = mlev1,
    mean_group2 = mlev2,
    stringsAsFactors = FALSE
  )
  
  # Sort by selection frequency
  results <- results[order(results$SelectionFrequency, decreasing=TRUE), ]
  
  # Return results
  return(list(
    results = results,
    selected_vars = selected_vars,
    all_frequencies = clust_result$selection_frequencies,
    clust_result = clust_result,
    method = "clustEnet"
  ))
}

PlotPca = function(data, y, title = "PCA of selected peptides") {
  pca_result = prcomp(data, center = TRUE, scale. = TRUE)
  
  # Calculer la variance expliquée
  var_explained <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)
  
  pca_data = data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Group = as.factor(y)
  )
  
  ggplot(pca_data, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 3, alpha = 0.7) +
    #stat_ellipse(aes(fill = Group), geom = "polygon", alpha = 0.1, show.legend = FALSE) +
    labs(
      title = title, 
      x = paste0("PC1 (", var_explained[1], "% variance)"),
      y = paste0("PC2 (", var_explained[2], "% variance)")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, face = 'bold'),
      axis.text.y = element_text(size = 10, face = 'bold'),
      plot.title = element_text(size = 15, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10)
    )
}

volcanoplot<-function(logFC,pval,thresholdFC=0,thresholdpv=0.05,graph=T,maintitle="Volcano plot",completedata){
  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
  
  threshold <- (as.numeric(abs(logFC) > thresholdFC &pval< thresholdpv ) +1)*2
  listgen<-data.frame("logFC"=logFC,"pval"=pval,"threshold"=threshold)
  if(!graph){return(completedata)}
  ##Construct the plot object
  g = ggplot(data=listgen, aes(x=logFC, y=-log10(pval))) +
    geom_point(alpha=0.4, size=1.75, colour=threshold) +
    theme(legend.position = "none") +
    #xlim(c(-(max(listgen$logFC)+0.2), max(listgen$logFC)+0.2)) + ylim(c(0, max(-log10(listgen$pval))+0.2)) +
    xlab("log2 fold change") + ylab("-log10 p-value")+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))+
    annotate("text",x=Inf,y=Inf,label=paste(substring(colnames(completedata)[3],first=4)),size=6,vjust=2,hjust=1.5)
  
  g
} 

barplottest<-function(feature,logFC,levels,pval,mean1,mean2,thresholdpv=0.05,thresholdFC=1,graph=T,maintitle="Mean by group for differentially expressed variables"){
  feature<-rep(feature,each=2)
  group<-rep(c(levels[1],levels[2]),times=(length(feature)/2))
  group<-factor(group,levels =c(levels[1],levels[2]))
  pval2<-rep((pval< thresholdpv),each=2)
  logFC2<-rep((abs(logFC)> thresholdFC),each=2) 
  mean<-vector() 
  mean[seq(from=1,to=length(feature),by = 2)]<-mean1
  mean[seq(from=2,to=length(feature),by = 2)]<-mean2
  data<-data.frame(feature,group,pval,logFC,mean,logFC2,pval2)
  data<-data[order(data$pval),]
  if(!graph){
    data<-data[order(data[,1]),]
    return(data[which((data$pval2==TRUE)& (data$logFC2==TRUE)),c(1,2,5)])}
  else{
    ggplot(data[which( ( data$pval2) & (data$logFC2) ),], aes(feature, mean,fill=group))+geom_bar(stat="identity", position="dodge")+ 
      ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}
errorplot<-function(text=paste("error /n","text error")){
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text,cex = 1.6, col = "black")}

barplottestSF<-function(toto,graph=T){
  #toto: dataframe res from conditiontest function
  if(!graph){return(toto)}
  rescond<-vector()
  for (i in (1:nrow(toto))){
    if(toto$samplenorm[i]=="norm" & toto$varequal[i]!="varequal"){rescond[i]<-"norm"}
    else if(toto$samplenorm[i]=="norm" & toto$varequal[i]=="varequal"){rescond[i]<-"both"}
    else if( toto$samplenorm[i]!="norm" &toto$varequal[i]=="varequal"){rescond[i]<-"varequal"}
    else{rescond[i]<-"none"}
    
  }
  data<-as.factor(rescond)
  p<-qplot(factor(data), geom="bar", fill=factor(data))
  p+ggtitle("Repartition of the variables according to the test results")+
    theme(plot.title=element_text(size=15))
}

SFtest<-function(toto,shaptest=T,Ftest=T,threshold=0.05){
  x<-toto[,1]
  toto<-toto[,-1]
  pvalF<-vector()
  pvalnormlev1<-vector()
  pvalnormlev2<-vector()
  vlev1<-vector()
  vlev2<-vector()
  samplenorm<-vector()
  varequal<-vector()
  conditiontest<-data.frame("name"=colnames(toto))
  for (i in 1:ncol(toto) ){
    lev1<-toto[which(x==levels(x)[1]),i]
    lev2<-toto[which(x==levels(x)[2]),i]
    if(shaptest){
      #pvalnormTem[i]<-shapiro.test(Tem)$p.value
      
      out<- tryCatch(shapiro.test(lev1)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev1[i]<-1
      else{pvalnormlev1[i]<-out}
      
      out<- tryCatch(shapiro.test(lev2)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev2[i]<-1
      else{pvalnormlev2[i]<-out}
      
      if((pvalnormlev2[i]>=threshold) & (pvalnormlev1[i]>=threshold)){samplenorm[i]<-"norm"}
      else{samplenorm[i]<-"notnorm"}
    }
    if(Ftest){
      #to perform a fisher test the value have to be normal
      pvalF[i]<-var.test(lev1,lev2)$p.value
      if(is.na(pvalF[i]))pvalF[i]<-1
      vlev1[i]<-var(lev1)
      vlev2[i]<-var(lev2)
      if(pvalF[i]>=threshold){varequal[i]<-"varequal"}
      else{varequal[i]<-"varnotequal"}
    }
  }
  if(shaptest){ conditiontest<-data.frame(conditiontest,pvalnormlev1,pvalnormlev2,"samplenorm"=samplenorm)
                colnames(conditiontest)<-c("names",paste("pvalshapiro",levels(x)[1],sep=""),paste("pvalshapiro",levels(x)[2],sep = ""),"samplenorm")
  }
  if(Ftest){conditiontest<-data.frame(conditiontest,"pvalF"=pvalF,"variancelev1"=vlev1,"variancelev2"=vlev2,"varequal"=varequal)}
  return(conditiontest) 
}

####

#' GridSearchCV wrapper for Random Forest using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune (ntree, mtry, nodesize, maxnodes)
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_rf_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)
  # library(randomForest)
  if(!requireNamespace("superml", quietly = TRUE)) {
    stop("Package 'superml' is required but not installed")
  }

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      n_estimators = c(100, 500, 1000),  # ntree in randomForest
      max_depth = c(5, 10, 15, 20, NULL),  # maxnodes (NULL = unlimited)
      min_samples_split = c(2, 5, 10),  # nodesize
      max_features = c("sqrt", "log2", floor(ncol(X)/3), floor(ncol(X)/2))  # mtry
    )
  }

  # Create trainer object
  rf_trainer <- superml::RFTrainer$new()

  # Create GridSearchCV object
  gst <-  superml::GridSearchCV$new(
    trainer = rf_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for XGBoost using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_xgb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      n_estimators = c(50, 100, 200),  # nrounds
      max_depth = c(3, 6, 9, 12),
      learning_rate = c(0.01, 0.05, 0.1, 0.3),  # eta
      gamma = c(0, 0.1, 0.5),
      subsample = c(0.6, 0.8, 1.0),
      colsample_bytree = c(0.6, 0.8, 1.0),
      min_child_weight = c(1, 3, 5)
    )
  }

  # Create trainer object
  xgb_trainer <- XGBTrainer$new()

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = xgb_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for Naive Bayes using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_nb_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      laplace = c(0, 0.5, 1, 2, 5)  # Smoothing parameter
    )
  }

  # Create trainer object
  nb_trainer <- NBTrainer$new()

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = nb_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for KNN using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
#' 
# tune_knn_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#   # KNN n'est PAS supporté par superml::GridSearchCV
#   # Utiliser uniquement la cross-validation manuelle
#   
#   # Default parameter grid if not provided
#   if(is.null(param_grid)) {
#     max_k <- min(floor(sqrt(length(y))), 30)
#     param_grid <- list(
#       n_neighbors = seq(3, max_k, by = 2)  # k parameter, odd numbers only
#     )
#   }
#   
#   # Utiliser la cross-validation manuelle traditionnelle
#   k_values <- param_grid$n_neighbors
#   best_k <- 3
#   best_acc <- 0
#   
#   set.seed(20011203)
#   for(k_test in k_values){
#     n_folds_cv <- min(5, length(y))
#     fold_size <- floor(length(y) / n_folds_cv)
#     accuracies <- numeric(n_folds_cv)
#     
#     for(fold in 1:n_folds_cv){
#       test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, length(y))
#       train_idx <- setdiff(1:length(y), test_idx)
#       
#       pred <- class::knn(train = X[train_idx, ],
#                          test = X[test_idx, ],
#                          cl = y[train_idx],
#                          k = k_test)
#       accuracies[fold] <- mean(pred == y[test_idx])
#     }
#     
#     avg_acc <- mean(accuracies)
#     if(avg_acc > best_acc){
#       best_acc <- avg_acc
#       best_k <- k_test
#     }
#   }
#   
#   return(list(
#     best_params = list(n_neighbors = best_k),
#     grid_search = NULL,
#     best_score = best_acc
#   ))
# }
tune_knn_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    max_k <- min(floor(sqrt(length(y))), 30)
    param_grid <- list(
      n_neighbors = seq(3, max_k, by = 2),  # k parameter, odd numbers only
      weights = c("uniform", "distance"),
      algorithm = c("brute", "kd_tree")
    )
  }

  # Create trainer object
  knn_trainer <- superml::KNNTrainer$new(type = "class")

  # Create GridSearchCV object
  gst <- superml::GridSearchCV$new(
    trainer = knn_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}

#' GridSearchCV wrapper for Logistic Regression (ElasticNet) using superml
#' @param X Feature matrix (data.frame or matrix)
#' @param y Target vector
#' @param param_grid List of parameters to tune
#' @param n_folds Number of cross-validation folds
#' @param scoring Scoring metric(s)
#' @return List with best parameters and best score
tune_elasticnet_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
  # library(superml)

  # Default parameter grid if not provided
  if(is.null(param_grid)) {
    param_grid <- list(
      alpha = c(0, 0.25, 0.5, 0.75, 1.0),  # 0=Ridge, 1=Lasso, 0.5=ElasticNet
      lambda = c(0.001, 0.01, 0.1, 1.0, 10),
      penalty = c("elasticnet")
    )
  }

  # Create trainer object
  lm_trainer <- LMTrainer$new(family = "binomial")

  # Create GridSearchCV object
  gst <- GridSearchCV$new(
    trainer = lm_trainer,
    parameters = param_grid,
    n_folds = n_folds,
    scoring = scoring
  )

  # Fit the grid search
  gst$fit(cbind(y = y, X), "y")

  # Get best iteration
  best_result <- gst$best_iteration(metric = scoring[1])

  return(list(
    best_params = best_result,
    grid_search = gst,
    best_score = best_result$score
  ))
}


# tune_elasticnet_gridsearch <- function(X, y, param_grid = NULL, n_folds = 5, scoring = c("accuracy", "auc")) {
#   
#   # Default parameter grid if not provided
#   if(is.null(param_grid)) {
#     param_grid <- list(
#       alpha = c(0, 0.25, 0.5, 0.75, 1.0),
#       lambda = NULL  # cv.glmnet trouvera le meilleur lambda
#     )
#   }
#   
#   # Encoder y comme 0/1 si c'est un facteur
#   if(is.factor(y)) {
#     y_numeric <- as.numeric(y) - 1
#   } else {
#     y_numeric <- y
#   }
#   
#   best_alpha <- param_grid$alpha[1]
#   best_lambda <- NULL
#   best_auc <- 0
#   
#   set.seed(20011203)
#   for(alpha_test in param_grid$alpha){
#     cvfit <- glmnet::cv.glmnet(as.matrix(X), y_numeric, 
#                                family="binomial", 
#                                alpha=alpha_test,
#                                type.measure="auc", 
#                                nfolds=min(10, length(y)-1))
#     
#     # Obtenir le meilleur AUC pour cet alpha
#     auc_max <- max(cvfit$cvm)
#     
#     if(auc_max > best_auc){
#       best_auc <- auc_max
#       best_alpha <- alpha_test
#       best_lambda <- cvfit$lambda.min
#     }
#   }
#   
#   return(list(
#     best_params = list(alpha = best_alpha, lambda = best_lambda),
#     grid_search = NULL,
#     best_score = best_auc
#   ))
# }

####

modelfunction <- function(learningmodel,
                          validation=NULL,
                          modelparameters,
                          transformdataparameters,
                          datastructuresfeatures=NULL,
                          learningselect){
  if(modelparameters$modeltype!="nomodel"){
    colnames(learningmodel)[1]<-"group"
    
    if(modelparameters$invers){
      learningmodel[,1]<-factor(learningmodel[,1],levels = rev(levels(learningmodel[,1])),ordered = TRUE)
    }
    lev<-levels(x = learningmodel[,1])
    names(lev)<-c("positif","negatif")
    
    #Build model
    if (modelparameters$modeltype=="randomforest"){
      learningmodel<-as.data.frame(learningmodel[sort(rownames(learningmodel)),])

      x<-as.data.frame(learningmodel[,-1])
      colnames(x)<-colnames(learningmodel)[-1]
      x<-as.data.frame(x[,sort(colnames(x))])
      set.seed(20011203)
      ntree_param <- ifelse(is.null(modelparameters$ntree), 1000, modelparameters$ntree)

      # Determine mtry parameter
      if(is.null(modelparameters$autotunerf) || modelparameters$autotunerf){
        # Check if GridSearchCV should be used
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # Use GridSearchCV from superml for comprehensive hyperparameter tuning
          cat("Using GridSearchCV for Random Forest hyperparameter tuning...\n")

          # Prepare parameter grid
          param_grid <- list(
            n_estimators = if(!is.null(modelparameters$rf_grid_ntree)) modelparameters$rf_grid_ntree else c(100, 500, 1000),
            max_features = if(!is.null(modelparameters$rf_grid_mtry)) modelparameters$rf_grid_mtry else c("sqrt", "log2"),
            min_samples_split = if(!is.null(modelparameters$rf_grid_nodesize)) modelparameters$rf_grid_nodesize else c(2, 5, 10)
          )

          # Run GridSearchCV
          grid_result <- tryCatch({
            tune_rf_gridsearch(X = x, y = learningmodel[,1],
                              param_grid = param_grid,
                              n_folds = 5,
                              scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to tuneRF:", e$message, "\n")
            NULL
          })

          if(!is.null(grid_result)) {
            # Extract best parameters from GridSearchCV
            best_params <- grid_result$best_params

            # Convert superml parameters to randomForest parameters
            optimal_mtry <- if(!is.null(best_params$max_features)) {
              if(best_params$max_features == "sqrt") floor(sqrt(ncol(x)))
              else if(best_params$max_features == "log2") floor(log2(ncol(x)))
              else as.numeric(best_params$max_features)
            } else floor(sqrt(ncol(x)))

            ntree_param <- if(!is.null(best_params$n_estimators)) best_params$n_estimators else ntree_param
            nodesize_param <- if(!is.null(best_params$min_samples_split)) best_params$min_samples_split else 1

            cat(sprintf("GridSearchCV best params: ntree=%d, mtry=%d, nodesize=%d, score=%.4f\n",
                       ntree_param, optimal_mtry, nodesize_param, grid_result$best_score))
          } else {
            # Fallback to tuneRF if GridSearchCV fails
            tuneRF_result <- tuneRF(x = x, y = learningmodel[,1],
                                    doBest = FALSE,
                                    ntreeTry = ntree_param,
                                    stepFactor = 1.5,
                                    improve = 0.01,
                                    trace = FALSE,
                                    plot = FALSE)
            optimal_mtry <- tuneRF_result[which.min(tuneRF_result[,2]), 1]
            nodesize_param <- 1
          }
        } else {
          # Use traditional tuneRF to find optimal mtry parameter
          tuneRF_result <- tuneRF(x = x, y = learningmodel[,1],
                                  doBest = FALSE,
                                  ntreeTry = ntree_param,
                                  stepFactor = 1.5,
                                  improve = 0.01,
                                  trace = FALSE,
                                  plot = FALSE)
          # Extract optimal mtry (the one with minimum OOB error)
          optimal_mtry <- tuneRF_result[which.min(tuneRF_result[,2]), 1]
          nodesize_param <- 1
        }
      } else {

        # Use manual mtry parameter

        optimal_mtry <- ifelse(is.null(modelparameters$mtry), floor(sqrt(ncol(x))), modelparameters$mtry)
        nodesize_param <- 1
      }

      # Build final model with optimal or manual parameters
      model <- randomForest(x = x, y = learningmodel[,1],
                           ntree = ntree_param,
                           mtry = optimal_mtry,
                           nodesize = nodesize_param,
                           
                           importance = TRUE)

 

      # Store optimal parameters in model object

      model$optimal_mtry <- optimal_mtry

      model$ntree_used <- ntree_param
      model$nodesize_used <- nodesize_param
      if(modelparameters$fs){
        featureselect<-selectedfeature(model=model,modeltype = "randomforest",tab=learningmodel,
                                       criterionimportance = "fscore",criterionmodel = "auc")
        model<-featureselect$model
        learningmodel<-featureselect$dataset
      }
      
      scorelearning =data.frame(model$votes[,lev["positif"]])
      colnames(scorelearning)<-paste(lev[1],"/",lev[2],sep="")
      predictclasslearning<-factor(levels = lev) 
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
      #predictclasslearning==model$predicted
    }   
    
    if(modelparameters$modeltype=="svm"){
      # Determine hyperparameters
      if(is.null(modelparameters$autotunesvm) || modelparameters$autotunesvm){
        # Perform hyperparameter tuning using tune.svm
        tune_result <- tune.svm(group ~ ., data = learningmodel,
                               gamma = 10^(-5:2), cost = 10^(-3:2),
                               cross=min(dim(learningmodel)[1]-2,10),
                               #kernel=c("linear", "polynomial", "radial", "sigmoid"),
                               # ranges=list(kernel=c("linear", "polynomial",
                               #                      "radial", "sigmoid")),
                               tunecontrol = tune.control(sampling = "cross"))

        # Extract best model and parameters
        # model <- tune_result$best.model
        # model$cost <- tune_result$best.parameters$cost
        # model$gamma <- tune_result$best.parameters$gamma
        cat('tunning results :  \n')
        print(tune_result)
        cost_param <- tune_result$best.parameters$cost
        gamma_param <- tune_result$best.parameters$gamma
        
      } else {
        # Use manual hyperparameters
        cat("define svm parameters manually \n")
        cost_param <- ifelse(is.null(modelparameters$cost), 1, modelparameters$cost)
        gamma_param <- ifelse(is.null(modelparameters$gamma), 0.1, modelparameters$gamma)
        # kernel_param <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)

        # model <- svm(group ~ ., data = learningmodel,
        #             kernel= kernel_param , 
        #             cost=cost_param, gamma=gamma_param,
        #             probability=FALSE)
        # model$cost <- cost_param
        # model$gamma <- gamma_param
        # model$kernel <- kernel_param
      }
      
      model <- svm(group ~ ., data = learningmodel,
                   kernel= 'radial' , #kernel_param , 
                   cost=cost_param, gamma=gamma_param,
                   type = "C-classification",
                   probability=TRUE)
      model$cost <- cost_param
      model$gamma <- gamma_param
      #model$kernel <- ifelse(is.null(modelparameters$kernel), "radial", modelparameters$kernel)

      if(modelparameters$fs){

        featureselect<-selectedfeature(model=model,modeltype = "svm",tab=learningmodel,
                                       criterionimportance = "fscore",criterionmodel = "auc")
        model<-featureselect$model
        learningmodel<-featureselect$dataset
      }
      
      # calculate the decision values for the learning set
      scorelearning <-model$decision.values
      #scorelearning = attr(e1071:::predict.svm(model, learningmodel[,-1], probability  = TRUE), "probabilities")
      if(sum(lev==(strsplit(colnames(scorelearning),split = "/")[[1]]))==0){
        scorelearning<-scorelearning*(-1)
        colnames(scorelearning)<-paste(lev[1],"/",lev[2],sep="")
      }
       
      # Obtenir les probabilités au lieu des decision values
      # pred_probs <- attr(predict(model, learningmodel[,-1], probability=TRUE), "probabilities")
      # scorelearning <- data.frame(pred_probs[, lev["positif"]])
      # colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }

    if(modelparameters$modeltype=="lightgbm"){

      # LightGBM gradient boosting
      x <- as.matrix(learningmodel[,-1])
      # IMPORTANT: Encode y so that 1 = lev["positif"] (first level), 0 = lev["negatif"] (second level)
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      # Create LightGBM dataset
      dtrain <- lgb.Dataset(data = x, label = y)
      # Determine hyperparameters
      if(is.null(modelparameters$autotunelgb) || modelparameters$autotunelgb){
        # Perform hyperparameter tuning using cross-validation
        set.seed(20011203)
        # Parameter grid search
        best_params <- list(
          objective = "binary",
          metric = "auc",
          num_leaves = 31,
          learning_rate = 0.05,
          feature_fraction = 0.9,
          bagging_fraction = 0.8,
          bagging_freq = 5,
          verbose = -1
        )

        # Cross-validation to find optimal nrounds
        cv_results <- lgb.cv(
          params = best_params,
          data = dtrain,
          nrounds = 200,
          nfold = min(5, nrow(learningmodel)-1),
          early_stopping_rounds = 10,
          verbose = -1
        )

        optimal_nrounds <- cv_results$best_iter
        # Train final model with optimal parameters
        model <- lgb.train(
          params = best_params,
          data = dtrain,
          nrounds = optimal_nrounds,
          verbose = -1
        )
        
        # Store optimal parameters
        model$optimal_nrounds <- optimal_nrounds
        model$optimal_num_leaves <- best_params$num_leaves
        model$optimal_learning_rate <- best_params$learning_rate
      } else {
        # Use manual hyperparameters
        nrounds_param <- ifelse(is.null(modelparameters$nrounds_lgb), 100, modelparameters$nrounds_lgb)
        num_leaves_param <- ifelse(is.null(modelparameters$num_leaves), 31, modelparameters$num_leaves)
        learning_rate_param <- ifelse(is.null(modelparameters$learning_rate_lgb), 0.05, modelparameters$learning_rate_lgb)
        params <- list(
          objective = "binary",
          metric = "auc",
          num_leaves = num_leaves_param,
          learning_rate = learning_rate_param,
          feature_fraction = 0.9,
          bagging_fraction = 0.8,
          bagging_freq = 5,
          verbose = -1
        )

 

        model <- lgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds_param,
          verbose = -1
        )

 

        # Store parameters
        model$optimal_nrounds <- nrounds_param
        model$optimal_num_leaves <- num_leaves_param
        model$optimal_learning_rate <- learning_rate_param
      }

      # Make predictions (probabilities)
      scorelearning <- predict(model, x)
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }

    if(modelparameters$modeltype=="naivebayes"){
      # Naive Bayes classifier
      # Check if GridSearchCV should be used
      optimal_laplace <- 0  # Default value

      if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
        # Use GridSearchCV from superml for hyperparameter tuning
        cat("Using GridSearchCV for Naive Bayes hyperparameter tuning...\n")

        # Prepare parameter grid
        param_grid <- list(
          laplace = if(!is.null(modelparameters$nb_grid_laplace)) modelparameters$nb_grid_laplace else c(0, 0.5, 1, 2, 5)
        )

        # Run GridSearchCV
        grid_result <- tryCatch({
          X_df <- as.data.frame(learningmodel[,-1])
          tune_nb_gridsearch(X = X_df, y = learningmodel[,1],
                            param_grid = param_grid,
                            n_folds = 5,
                            scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, using default laplace=0:", e$message, "\n")
          NULL
        })

        if(!is.null(grid_result)) {
          best_params <- grid_result$best_params
          optimal_laplace <- if(!is.null(best_params$laplace)) best_params$laplace else 0
          cat(sprintf("GridSearchCV best params: laplace=%.2f, score=%.4f\n",
                     optimal_laplace, grid_result$best_score))
        }
      }

      # Build model with optimal or default laplace parameter
      model <- naiveBayes(x = learningmodel[,-1], y = learningmodel[,1], laplace = optimal_laplace)

      # Store model type and optimal parameter
      model$model_type <- "naivebayes"
      model$optimal_laplace <- optimal_laplace

      # Make predictions (probabilities)
      pred_probs <- e1071:::predict.naiveBayes(model, learningmodel[,-1], type="raw")
      scorelearning <- data.frame(pred_probs[, lev["positif"]])
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }

    if(modelparameters$modeltype=="knn"){
      # K-Nearest Neighbors
      # Determine k parameter
      if(is.null(modelparameters$autotuneknn) || modelparameters$autotuneknn){
        # Check if GridSearchCV should be used
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # Use GridSearchCV from superml for comprehensive hyperparameter tuning
          cat("Using GridSearchCV for KNN hyperparameter tuning...\n")

          # Prepare parameter grid
          max_k <- min(floor(sqrt(nrow(learningmodel))), 30)
          param_grid <- list(
            n_neighbors = if(!is.null(modelparameters$knn_grid_k)) modelparameters$knn_grid_k else seq(3, max_k, by=2)
          )

          # Run GridSearchCV
          grid_result <- tryCatch({
            X_df <- as.data.frame(learningmodel[,-1])
            tune_knn_gridsearch(X = X_df, y = learningmodel[,1],
                               param_grid = param_grid,
                               n_folds = 5,
                               scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to manual CV:", e$message, "\n")
            NULL
          })

          if(!is.null(grid_result)) {
            best_params <- grid_result$best_params
            optimal_k <- if(!is.null(best_params$n_neighbors)) best_params$n_neighbors else 5
            cat(sprintf("GridSearchCV best params: k=%d, score=%.4f\n",
                       optimal_k, grid_result$best_score))
          } else {
            # Fallback to traditional CV if GridSearchCV fails
            # Automatic tuning: try different k values via cross-validation
            set.seed(20011203)
            # Test k values from 3 to min(sqrt(n), 20)
            max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
            k_values <- seq(3, max_k, by=2) # odd numbers only

            # Cross-validation to find best k
            best_k <- 3
            best_acc <- 0
            for(k_test in k_values){
              # Simple leave-one-out or 5-fold CV
              n_folds <- min(5, nrow(learningmodel))
              fold_size <- floor(nrow(learningmodel) / n_folds)
              accuracies <- numeric(n_folds)
              for(fold in 1:n_folds){
                test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, nrow(learningmodel))
                train_idx <- setdiff(1:nrow(learningmodel), test_idx)
                pred <- knn(train = learningmodel[train_idx, -1],
                           test = learningmodel[test_idx, -1],
                           cl = learningmodel[train_idx, 1],
                           k = k_test)
                accuracies[fold] <- mean(pred == learningmodel[test_idx, 1])
              }

              avg_acc <- mean(accuracies)
              if(avg_acc > best_acc){
                best_acc <- avg_acc
                best_k <- k_test
              }
            }

            optimal_k <- best_k
          }
        } else {
          # Use traditional CV for hyperparameter tuning
          # Automatic tuning: try different k values via cross-validation
          set.seed(20011203)
          # Test k values from 3 to min(sqrt(n), 20)
          max_k <- min(floor(sqrt(nrow(learningmodel))), 20)
          k_values <- seq(3, max_k, by=2) # odd numbers only

          # Cross-validation to find best k
          best_k <- 3
          best_acc <- 0
          for(k_test in k_values){
            # Simple leave-one-out or 5-fold CV
            n_folds <- min(5, nrow(learningmodel))
            fold_size <- floor(nrow(learningmodel) / n_folds)
            accuracies <- numeric(n_folds)
            for(fold in 1:n_folds){
              test_idx <- ((fold-1)*fold_size + 1):min(fold*fold_size, nrow(learningmodel))
              train_idx <- setdiff(1:nrow(learningmodel), test_idx)
              pred <- knn(train = learningmodel[train_idx, -1],
                         test = learningmodel[test_idx, -1],
                         cl = learningmodel[train_idx, 1],
                         k = k_test)
              accuracies[fold] <- mean(pred == learningmodel[test_idx, 1])
            }

            avg_acc <- mean(accuracies)
            if(avg_acc > best_acc){
              best_acc <- avg_acc
              best_k <- k_test
            }
          }

          optimal_k <- best_k
        }

      } else {
        # Use manual k parameter
        optimal_k <- ifelse(is.null(modelparameters$k_neighbors), 5, modelparameters$k_neighbors)
      }

 

      # KNN doesn't have a traditional "model" object, store parameters
      model <- list(
        train_data = learningmodel[,-1],
        train_labels = learningmodel[,1],
        optimal_k = optimal_k,
        model_type = "knn"

      )

      # Make predictions using knn with probability estimation
      # For probability, we'll use the proportion of k neighbors in each class

      scorelearning_vec <- numeric(nrow(learningmodel))

      for(i in 1:nrow(learningmodel)){
        # Leave-one-out prediction for training set
        train_idx <- setdiff(1:nrow(learningmodel), i)

        # Get k nearest neighbors
        distances <- apply(learningmodel[train_idx, -1], 1, function(row) {
          sqrt(sum((as.numeric(learningmodel[i, -1]) - as.numeric(row))^2))

        })

        k_nearest_idx <- order(distances)[1:optimal_k]
        k_nearest_labels <- learningmodel[train_idx, 1][k_nearest_idx]

        # Calculate probability as proportion of positif class

        scorelearning_vec[i] <- sum(k_nearest_labels == lev["positif"]) / optimal_k
      }

 

      scorelearning <- data.frame(scorelearning_vec)
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")

      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)

    }

    if(modelparameters$modeltype=="elasticnet"){
      # Penalized Logistic Regression (ElasticNet)
      x <- as.matrix(learningmodel[,-1])
      # IMPORTANT: Encode y so that 1 = lev["positif"] (first level), 0 = lev["negatif"] (second level)
      # This ensures that predict returns P(lev["positif"])
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      
      # Get hyperparameters (use defaults if not provided)
      alpha_param <- ifelse(is.null(modelparameters$alpha), 0.5, modelparameters$alpha)
      lambda_param <- modelparameters$lambda  # NULL for CV selection
      
      # Check if GridSearchCV should be used
      if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch && is.null(lambda_param)){
        # Use GridSearchCV from superml for comprehensive hyperparameter tuning
        cat("Using GridSearchCV for ElasticNet hyperparameter tuning...\n")
        
        # Prepare parameter grid
        param_grid <- list(
          alpha = if(!is.null(modelparameters$en_grid_alpha)) modelparameters$en_grid_alpha else c(0, 0.25, 0.5, 0.75, 1.0),
          lambda = if(!is.null(modelparameters$en_grid_lambda)) modelparameters$en_grid_lambda else c(0.001, 0.01, 0.1, 1.0)
        )
        
        # Run GridSearchCV
        grid_result <- tryCatch({
          X_df <- as.data.frame(x)
          tune_elasticnet_gridsearch(X = X_df, y = learningmodel[,1],
                                     param_grid = param_grid,
                                     n_folds = 5,
                                     scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, falling back to cv.glmnet:", e$message, "\n")
          NULL
        })
        
        if(!is.null(grid_result)) {
          best_params <- grid_result$best_params
          alpha_param <- if(!is.null(best_params$alpha)) best_params$alpha else 0.5
          lambda_param <- if(!is.null(best_params$lambda)) best_params$lambda else NULL
          
          cat(sprintf("GridSearchCV best params: alpha=%.3f, lambda=%.4f, score=%.4f\n",
                      alpha_param, lambda_param, grid_result$best_score))
          
          # Use the best parameters to fit with cv.glmnet for consistency
          set.seed(20011203)
          cvfit <- cv.glmnet(x, y, family="binomial", alpha=alpha_param,
                             type.measure="auc", nfolds=min(10, nrow(learningmodel)-1))
          lambda_param <- cvfit$lambda.min
          model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                        cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
        } else {
          # Fallback to traditional cv.glmnet if GridSearchCV fails
          set.seed(20011203)
          cvfit <- cv.glmnet(x, y, family="binomial", alpha=alpha_param,
                             type.measure="auc", nfolds=min(10, nrow(learningmodel)-1))
          lambda_param <- cvfit$lambda.min
          model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                        cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
        }
      } else if(is.null(lambda_param)){
        # Perform cross-validation to find optimal lambda if not provided
        #cat("on est dans le if du is.null(lambda_param) \n")
        set.seed(20011203)
        cvfit <- cv.glmnet(x, y, family="binomial", alpha=alpha_param,
                           type.measure="auc", nfolds=min(10, nrow(learningmodel)-1))
        lambda_param <- cvfit$lambda.min
        # if(!is.null(cvfit$glmnet.fit)){
        #   fit <- cvfit$glmnet.fit
        # } else {
        #   fit <- glmnet(x, y, family="binomial", alpha=alpha_param)
        # }
        model <- list(glmnet_model=cvfit, lambda=lambda_param, alpha=alpha_param,
                      cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
      } else {
        # Manual mode: use specified lambda and alpha parameters
        cat("Creating ElasticNet model with manual parameters: alpha=", alpha_param, ", lambda=", lambda_param, "\n")
        fit <- glmnet(x, y, family="binomial", alpha=alpha_param, lambda=lambda_param)
        model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                      cvfit=NULL, optimal_lambda=lambda_param, lambda_1se=NULL)
      }
      
      cat("la classe de model$glmnet_model est : ", class(model$glmnet_model), "\n" )
      
      # Feature selection based on non-zero coefficients
      if(modelparameters$fs){
        coef_values <- as.matrix(coef(model$glmnet_model, s=lambda_param))
        selected_features <- rownames(coef_values)[which(coef_values[-1,1] != 0)]
        if(length(selected_features) > 0){
          learningmodel <- learningmodel[, c("group", selected_features)]
          x <- as.matrix(learningmodel[,-1])
          # Refit model with selected features
          if(is.null(modelparameters$lambda)){
            cvfit <- cv.glmnet(x, y, family="binomial", alpha=alpha_param,
                               type.measure="auc", nfolds=min(10, nrow(learningmodel)-1))
            lambda_param <- cvfit$lambda.min
            # Refit model with optimal lambda to ensure we have a valid glmnet object
            fit <- glmnet(x, y, family="binomial", alpha=alpha_param, lambda=lambda_param)
            cat("class of fitted modele :  ", class(fit))
            model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                          cvfit=cvfit, optimal_lambda=lambda_param, lambda_1se=cvfit$lambda.1se)
          } else {
            fit <- glmnet(x, y, family="binomial", alpha=alpha_param, lambda=lambda_param)
            model <- list(glmnet_model=fit, lambda=lambda_param, alpha=alpha_param,
                          cvfit=NULL, optimal_lambda=lambda_param, lambda_1se=NULL)
          }
        }
      }
      
      # Make predictions (probabilities)
      # Use appropriate predict method based on model class
      if(inherits(model$glmnet_model, "cv.glmnet")){
        scorelearning <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x, s=lambda_param, type="response"))
      } else {
        scorelearning <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx=x, s=lambda_param, type="response"))
      }
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")
      
      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }

    if(modelparameters$modeltype=="xgboost"){
      # XGBoost gradient boosting
      x <- as.matrix(learningmodel[,-1])
      # IMPORTANT: Encode y so that 1 = lev["positif"] (first level), 0 = lev["negatif"] (second level)
      # This ensures that predict returns P(lev["positif"])
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)

      # Create DMatrix for XGBoost
      dtrain <- xgb.DMatrix(data = x, label = y)

      # Determine hyperparameters
      if(is.null(modelparameters$autotunexgb) || modelparameters$autotunexgb){
        # Check if GridSearchCV should be used
        if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch){
          # Use GridSearchCV from superml for comprehensive hyperparameter tuning
          cat("Using GridSearchCV for XGBoost hyperparameter tuning...\n")

          # Prepare parameter grid
          param_grid <- list(
            n_estimators = if(!is.null(modelparameters$xgb_grid_nrounds)) modelparameters$xgb_grid_nrounds else c(50, 100, 200),
            max_depth = if(!is.null(modelparameters$xgb_grid_maxdepth)) modelparameters$xgb_grid_maxdepth else c(3, 6, 9),
            learning_rate = if(!is.null(modelparameters$xgb_grid_eta)) modelparameters$xgb_grid_eta else c(0.01, 0.1, 0.3),
            gamma = if(!is.null(modelparameters$xgb_grid_gamma)) modelparameters$xgb_grid_gamma else c(0, 0.1, 0.5),
            subsample = if(!is.null(modelparameters$xgb_grid_subsample)) modelparameters$xgb_grid_subsample else c(0.8, 1.0)
          )

          # Run GridSearchCV
          grid_result <- tryCatch({
            # Convert data for superml
            X_df <- as.data.frame(x)
            tune_xgb_gridsearch(X = X_df, y = learningmodel[,1],
                               param_grid = param_grid,
                               n_folds = 5,
                               scoring = c("auc", "accuracy"))
          }, error = function(e) {
            cat("GridSearchCV failed, falling back to xgb.cv:", e$message, "\n")
            NULL
          })

          if(!is.null(grid_result)) {
            # Extract best parameters from GridSearchCV
            best_params <- grid_result$best_params

            optimal_nrounds <- if(!is.null(best_params$n_estimators)) best_params$n_estimators else 100
            optimal_max_depth <- if(!is.null(best_params$max_depth)) best_params$max_depth else 6
            optimal_eta <- if(!is.null(best_params$learning_rate)) best_params$learning_rate else 0.3
            optimal_gamma <- if(!is.null(best_params$gamma)) best_params$gamma else 0
            optimal_subsample <- if(!is.null(best_params$subsample)) best_params$subsample else 1.0
            optimal_min_child_weight <- if(!is.null(best_params$min_child_weight)) best_params$min_child_weight else 1

            cat(sprintf("GridSearchCV best params: nrounds=%d, max_depth=%d, eta=%.3f, gamma=%.3f, score=%.4f\n",
                       optimal_nrounds, optimal_max_depth, optimal_eta, optimal_gamma, grid_result$best_score))

            # Create final parameters list
            final_params <- list(
              objective = "binary:logistic",
              eval_metric = "auc",
              max_depth = optimal_max_depth,
              eta = optimal_eta,
              gamma = optimal_gamma,
              subsample = optimal_subsample,
              min_child_weight = optimal_min_child_weight
            )

            # Train final model with optimal parameters
            model <- xgb.train(
              params = final_params,
              data = dtrain,
              nrounds = optimal_nrounds,
              verbose = 0
            )

            # Store optimal parameters
            model$optimal_nrounds <- optimal_nrounds
            model$optimal_max_depth <- optimal_max_depth
            model$optimal_eta <- optimal_eta
            model$optimal_gamma <- optimal_gamma
            model$optimal_subsample <- optimal_subsample
            model$optimal_min_child_weight <- optimal_min_child_weight
          } else {
            # Fallback to traditional xgb.cv if GridSearchCV fails
            # Perform hyperparameter tuning using cross-validation
            set.seed(20011203)

            # Parameter grid search
            best_params <- list(
              objective = "binary:logistic",
              eval_metric = "auc",
              max_depth = 6,
              eta = 0.3,
              min_child_weight = 1
            )

            # Cross-validation to find optimal nrounds
            cv_results <- xgb.cv(
              params = best_params,
              data = dtrain,
              nrounds = 200,
              nfold = min(5, nrow(learningmodel)-1),
              early_stopping_rounds = 10,
              verbose = 0
            )

            optimal_nrounds <- cv_results$best_iteration

            # Train final model with optimal parameters
            model <- xgb.train(
              params = best_params,
              data = dtrain,
              nrounds = optimal_nrounds,
              verbose = 0
            )

            # Store optimal parameters
            model$optimal_nrounds <- optimal_nrounds
            model$optimal_max_depth <- best_params$max_depth
            model$optimal_eta <- best_params$eta
            model$optimal_min_child_weight <- best_params$min_child_weight
          }
        } else {
          # Use traditional xgb.cv for hyperparameter tuning
          # Perform hyperparameter tuning using cross-validation
          set.seed(20011203)

          # Parameter grid search
          best_params <- list(
            objective = "binary:logistic",
            eval_metric = "auc",
            max_depth = 6,
            eta = 0.3,
            min_child_weight = 1
          )

          # Cross-validation to find optimal nrounds
          cv_results <- xgb.cv(
            params = best_params,
            data = dtrain,
            nrounds = 200,
            nfold = min(5, nrow(learningmodel)-1),
            early_stopping_rounds = 10,
            verbose = 0
          )

          optimal_nrounds <- cv_results$best_iteration

          # Train final model with optimal parameters
          model <- xgb.train(
            params = best_params,
            data = dtrain,
            nrounds = optimal_nrounds,
            verbose = 0
          )

          # Store optimal parameters
          model$optimal_nrounds <- optimal_nrounds
          model$optimal_max_depth <- best_params$max_depth
          model$optimal_eta <- best_params$eta
          model$optimal_min_child_weight <- best_params$min_child_weight
        }

      } else {
        # Use manual hyperparameters
        nrounds_param <- ifelse(is.null(modelparameters$nrounds), 100, modelparameters$nrounds)
        max_depth_param <- ifelse(is.null(modelparameters$max_depth), 6, modelparameters$max_depth)
        eta_param <- ifelse(is.null(modelparameters$eta), 0.3, modelparameters$eta)

        params <- list(
          objective = "binary:logistic",
          eval_metric = "auc",
          max_depth = max_depth_param,
          eta = eta_param,
          min_child_weight = 1
        )

        model <- xgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds_param,
          verbose = 0
        )

        # Store parameters
        model$optimal_nrounds <- nrounds_param
        model$optimal_max_depth <- max_depth_param
        model$optimal_eta <- eta_param
        model$optimal_min_child_weight <- 1
      }

      # Make predictions (probabilities)
      scorelearning <- xgboost:::predict.xgb.Booster(model, x)
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1],"/",lev[2],sep="")

      predictclasslearning<-factor(levels = lev)
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }

    #levels(predictclassval)<-paste("test",levels(predictclasslearning),sep="")
    levels(predictclasslearning)<-paste("test",lev,sep="")
    classlearning<-learningmodel[,1]
    
    ##########
    # # Calculate Youden threshold from training data
    # youden_result <- younden(classlearning, scorelearning[,1])
    # youden_threshold <- youden_result[4]  # 4th element is the threshold
    # 
    # # Update model parameters with Youden threshold
    # modelparameters$thresholdmodel <- youden_threshold
    # 
    # # Recalculate predictions using Youden threshold instead of fixed 0.5
    # predictclasslearning <- factor(levels = lev)
    # predictclasslearning[which(scorelearning[,1] >= youden_threshold)] <- lev["positif"]
    # predictclasslearning[which(scorelearning[,1] < youden_threshold)] <- lev["negatif"]
    # predictclasslearning <- as.factor(predictclasslearning)
    # levels(predictclasslearning)<-paste("test",lev,sep="")
    
    ########
    
    reslearningmodel<-data.frame(classlearning,scorelearning,predictclasslearning)
    colnames(reslearningmodel) <-c("classlearning","scorelearning","predictclasslearning") 
    datalearningmodel<-list("learningmodel"=learningmodel,"reslearningmodel"=reslearningmodel)
    
    if (modelparameters$adjustval){
      #Validation
      colnames(validation)[1]<-"group"
      validationdiff<-validation[,which(colnames(validation)%in%colnames(learningmodel))]
      learningselect2<-learningselect
      if(transformdataparameters$log) { 
        validationdiff[,-1]<-transformationlog(x = validationdiff[,-1]+1,logtype =transformdataparameters$logtype )
        learningselect2[,-1]<-transformationlog(x = learningselect2[,-1]+1,logtype=transformdataparameters$logtype)}
      if(transformdataparameters$arcsin){
        maxlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = max,na.rm=T)
        minlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = min,na.rm=T)
        for (i in 2:dim(validationdiff)[2]){
        validationdiff[,i]<-(validationdiff[,i]-minlearn[i-1])/(maxlearn[i-1]-minlearn[i-1])
        #validationdiff[,-1]<-apply(X = as.data.frame(validationdiff[,-1]),MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
        validationdiff[which(validationdiff[,i]>1),i]<-1
        validationdiff[which(validationdiff[,i]<0),i]<-0
        validationdiff[,i]<-asin(sqrt(validationdiff[,i]))
        }     
        learningselect2[,-1]<-apply(X = learningselect2[,-1],MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
        learningselect2[,-1]<-asin(sqrt(learningselect2[,-1]))
      }
      if(transformdataparameters$standardization){
        learningselectval<<-learningselect2
        sdselect<-apply(learningselect2[,which(colnames(learningselect2)%in%colnames(validationdiff))], 2, sd,na.rm=T)
        print('sdselect')
        print(sdselect)
        validationdiff[,-1]<-scale(validationdiff[,-1],center=F,scale=sdselect[-1])
      }

      #NAstructure if NA ->0
      if(!is.null(datastructuresfeatures)){
        validationdiff[which(is.na(validationdiff),arr.ind = T)[which(which(is.na(validationdiff),arr.ind = T)[,2]%in%which(colnames(validationdiff)%in%datastructuresfeatures$names)),]]<-0
      }
      #
      validationmodel<<- replaceNAvalidation(as.data.frame(validationdiff[,-1]),toto=as.data.frame(learningmodel[,-1]),rempNA=transformdataparameters$rempNA)
      colnames(validationmodel)<-colnames(validationdiff)[-1]
      rownames(validationmodel)<-rownames(validationdiff)
      
      #prediction a partir du model
      if(modelparameters$modeltype=="randomforest"){
        scoreval <- randomForest:::predict.randomForest(object=model,type="prob",newdata = validationmodel)[,lev["positif"]]
        predictclassval<-vector(length = length(scoreval) ) 
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
        
      }
      
      if(modelparameters$modeltype=="svm"){
        if(!is.null(model)){
          # SVM validation predictions
          # print("On est dans le SVM pour la validation")
          # print(str(model))
          # print(str(validationmodel))
          
          #calculate decision values for the validation set
          scoreval =attr(e1071:::predict.svm(model,newdata =  validationmodel,decision.values=T),"decision.values")
          if(sum(lev==(strsplit(colnames(scoreval),split = "/")[[1]]))==0){scoreval<-scoreval*(-1)}
          
          # Utiliser les probabilités pour la validation
          # pred_probs_val <- attr(e1071:::predict.svm(model, newdata = validationmodel, probability=TRUE), "probabilities")
          # scoreval <- pred_probs_val[, lev["positif"]]

          predictclassval<-vector(length = length(scoreval) )
          predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
          predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
          predictclassval<-as.factor(predictclassval)
        }
        
      }

      if(modelparameters$modeltype=="elasticnet"){
        req(model$glmnet_model)
        # ElasticNet validation predictions
        x_val <- as.matrix(validationmodel)
        # scoreval <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model, newx=x_val, s=model$lambda, type="response"))
        if(inherits(model$glmnet_model, "cv.glmnet")){
          scoreval <- as.vector(glmnet:::predict.cv.glmnet(model$glmnet_model,
                                                           newx=x_val, s=model$lambda, type="response"))
        } else {
          scoreval <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx=x_val, s=model$lambda, type="response"))
        }
        # scoreval <- as.vector(glmnet::predict.glmnet(model$glmnet_model, newx=x_val, s=model$lambda, type="response"))
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }

      if(modelparameters$modeltype=="xgboost"){
        # XGBoost validation predictions
        x_val <- as.matrix(validationmodel)
        dval <- xgb.DMatrix(data = x_val)
        scoreval <- xgboost:::predict.xgb.Booster(model, dval)
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }

      if(modelparameters$modeltype=="lightgbm"){
        # LightGBM validation predictions
        x_val <- as.matrix(validationmodel)
        scoreval <- predict(model, x_val)
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }

      if(modelparameters$modeltype=="naivebayes"){
        # Naive Bayes validation predictions
        pred_probs <- e1071:::predict.naiveBayes(model, validationmodel, type="raw")
        scoreval <- pred_probs[, lev["positif"]]
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }

      if(modelparameters$modeltype=="knn"){
        # KNN validation predictions
        # Get k nearest neighbors for probability estimation
        scoreval_vec <- numeric(nrow(validationmodel))
        for(i in 1:nrow(validationmodel)){
          # Calculate distances to all training points
          distances <- apply(model$train_data, 1, function(row) {
            sqrt(sum((as.numeric(validationmodel[i, ]) - as.numeric(row))^2))
          })
          # Get k nearest neighbors
          k_nearest_idx <- order(distances)[1:model$optimal_k]
          k_nearest_labels <- model$train_labels[k_nearest_idx]
          # Calculate probability as proportion of positif class
          scoreval_vec[i] <- sum(k_nearest_labels == lev["positif"]) / model$optimal_k
        }
        scoreval <- scoreval_vec
        predictclassval<-vector(length = length(scoreval) )
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }

      if(sum(lev==(levels(predictclassval)))==0){
        predictclassval<-factor(predictclassval,levels = rev(levels(predictclassval)),ordered = TRUE)
      }
      classval<- validation[,1]
      if(sum(lev==(levels(classval)))==0){
        classval<-factor(classval,levels = rev(levels(classval)),ordered = TRUE)
      }
      
      #levels(predictclassval)<-paste("test",levels(predictclassval),sep="")
      levels(predictclassval)<-paste("test",lev,sep="")
      resvalidationmodel<-data.frame(classval,scoreval,predictclassval)
      colnames(resvalidationmodel) <-c("classval","scoreval","predictclassval") 
      auc<-auc(roc(as.vector(classval), as.vector(scoreval),quiet=T))
      datavalidationmodel<-list("validationdiff"=validationdiff,"validationmodel"=validationmodel,"resvalidationmodel"=resvalidationmodel,"auc"=auc)
      
    }
    else{datavalidationmodel<-list()}
    res<-list("datalearningmodel"=datalearningmodel,"model"=model,"datavalidationmodel"=datavalidationmodel,"groups"=lev,"parameters"=modelparameters)
  }
}


replaceNAvalidation<-function(validationdiff,toto,rempNA){
  validationdiffssNA<-validationdiff
  for(i in 1:nrow(validationdiff)){
    validationdiffssNA[i,]<-replaceNAoneline(lineNA = validationdiff[i,],toto = toto,rempNA =rempNA)
  }
  return(validationdiffssNA)
}

replaceNAoneline<-function(lineNA,toto,rempNA){
  alldata<-rbind(lineNA,toto)
  if(rempNA=="moygr"){ 
    #print("impossible de remplacer les NA par la moyenne par group pour la validation")
    linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA ="moy")[1,-1]        }
  
  else{linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA =rempNA)[1,-1]}
  
  return(linessNA)
}




ROCcurve<-function(validation,decisionvalues,maintitle="Roc curve",graph=T,ggplot=T){
  validation<-factor(validation,levels = rev(levels(validation)),ordered = TRUE)
  
  #argument : validation, vector of appartenance,
  #            decisionvalues, vector of scores
  #fulldata<-rocdata(grp = validation, pred = as.vector(decisionvalues))
  data<-roc(validation,decisionvalues)
  if(!graph){return(data.frame("sensitivity"=data$sensitivities,"specificity"=data$specificities,"thresholds"=data$thresholds))}
  if(!ggplot){plot(data)}
  if(ggplot){
    y<-rev(data$sensitivities)
    x<-rev(data$specificities)
    roc<-data.frame(x,y)
    auc<-as.numeric(auc(data))
    
    col<-gg_color_hue(3)
    roccol<-col[1]
    bin = 0.01
    diag = data.frame(x = seq(0, 1, by = bin), y = rev(seq(0, 1, by = bin)))
    p <- ggplot(data = roc, aes(x = x, y = y)) + 
      geom_point(color = roccol) +
      geom_line(color = roccol) + 
      geom_line(data = diag, aes(x = x, y = y), color =col[3])
    sp = 19
    f <- p + geom_point(data = diag, aes(x = x, y = y), color = "lightgrey", shape = sp) + 
      theme(axis.text = element_text(size = 16), 
            title = element_text(size = 15) , 
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold')
            ) + 
      labs(y = "Sensitivity", x = "1 - Specificity", title = maintitle) +
      annotate("text",x=0.2,y=0.1,label=paste("AUC = ",as.character(round(auc,digits = 3))),size=7,colour= roccol)+
      scale_x_reverse()
    
    f
  }
}

scoremodelplot<-function(class,score,names,threshold,type,graph,printnames){
  class<-factor(class,levels =rev(levels(class)))

  if(type=="boxplot"){
    boxplotggplot(class =class,score =score,names=names,threshold=threshold,
                  graph = graph)
  }
  else if(type=="points"){
    plot_pred_type_distribution(class = class, score = score,names=names,threshold=threshold,graph=graph,printnames=printnames  )
  } 
}

boxplotggplot<-function(class,score,names,threshold,maintitle="Score representation ",graph=T){
  data<-data.frame("names"=names,"class"= class,"score"=as.vector(score))
  if(!graph){return(data)}
  p<-ggplot(data, aes(x=class, y=score)) +
    scale_fill_manual( values = c("#00BFC4","#F8766D") ) +
    geom_boxplot(aes(fill=class)) +
    geom_hline(yintercept = threshold, color='red', alpha=0.6) +
    ggtitle(maintitle) + 
    theme(plot.title=element_text( size=15), 
          axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
          axis.text.y =  element_text(size = 12 , face =  'bold'),
          axis.title.x = element_text(size = 15 , face = 'bold'), 
          axis.title.y =  element_text(size = 15 , face = 'bold'),
          legend.text = element_text( size = 12 , face = 'bold'),
          legend.title = element_text(size = 14 , face =  'bold'))
  
  p
}

plot_pred_type_distribution <- function(class,score,names, threshold,maintitle="Score representation",printnames=F,graph=T) {
  #in this function the levels of the class is inverted in order to have the control group on the left side of the graph
  df<-data.frame(names,class,score)
  colnames(df)<-c("names","class","score")
  v <-rep(NA, nrow(df))
  v <- ifelse(df$score >= threshold & df$class == levels(class)[2], "TruePositiv", v)
  v <- ifelse(df$score >= threshold & df$class == levels(class)[1], "FalsePositiv", v)
  v <- ifelse(df$score < threshold & df$class ==  levels(class)[2], "FalseNegativ", v)
  v <- ifelse(df$score < threshold & df$class == levels(class)[1], "TrueNegativ", v)
  
  df$predtype <-factor(v,levels = c("FalseNegativ","FalsePositiv","TrueNegativ","TruePositiv"),ordered = T)
  if(!graph){return(df)}
  set.seed(20011203)
  if(printnames){
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      geom_text(label=names,colour=palet(df$predtype,multiple = TRUE))+
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle) + 
      theme(plot.title=element_text( size=15),
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold'),
            legend.text = element_text( size = 12 , face = 'bold'),
            legend.title = element_text(size = 14 , face =  'bold'),
            legend.position ="bottom")
  }
  else{
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle) + 
      theme(plot.title=element_text( size=15), 
            axis.text.x = element_text(size = 12 ,  face = 'bold' ) ,
            axis.text.y =  element_text(size = 12 , face =  'bold'),
            axis.title.x = element_text(size = 15 , face = 'bold'), 
            axis.title.y =  element_text(size = 15 , face = 'bold'),
            legend.text = element_text( size = 12 , face = 'bold'),
            legend.title = element_text(size = 14 , face =  'bold'),
            legend.position ="bottom")
  }
  g
  
}

palet<-function(predtype,multiple=FALSE){
  if(multiple){col<-as.character(predtype)}
  else{col<-sort(unique(as.character(predtype)))}
  col[which(col=="FalseNegativ")]<-"#C77CFF"
  col[which(col=="FalsePositiv")]<-"#00BA38"
  col[which(col=="TrueNegativ")]<-"#00BFC4"
  col[which(col=="TruePositiv")]<-"#F8766D"
  return(col)
}


selectedfeature<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype="learn"){
  rmvar<-testmodel(model=model,modeltype = modeltype,tab=tab,validation=validation,
                   criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  i=0
  tabdiff2<-tab
  while(rmvar!=0){
    i<-i+1
    print(paste(i,"eliminates features"))
    tabdiff2<-tabdiff2[,-rmvar]
    if(modeltype=="svm"){
      tune_result <- tune.svm(x=tabdiff2[,-1], y=tabdiff2[,1],
                             gamma = 10^(-5:2), cost = 10^(-3:2),
                             cross=min(dim(tabdiff2)[1]-2,10))
      model <- tune_result$best.model
      model$cost <- tune_result$best.parameters$cost
      model$gamma <- tune_result$best.parameters$gamma
    }
    if (modeltype=="randomforest"){      
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T)
    }
      rmvar<-testmodel(model=model,modeltype = modeltype,tab=tabdiff2,validation=validation,
                     criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  }
  res<-list("dataset"=tabdiff2,"model"=model)
  return(res)
}


testmodel<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype){
  #retourn la variable a enlever
  importancevar<-importancemodelsvm(model = model,modeltype=modeltype,tabdiff=tab,criterion = criterionimportance)
  lessimportantevar<-which(importancevar==min(importancevar,na.rm =T) )
  test<-vector()
  if(modeltype=="svm"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$fitted)}
    if(criterionmodel=="auc"){
      if (fstype=='learn'){aucmod<-auc(roc(tab[,1], as.vector(model$decision.values),quiet=T))}
      if (fstype=='val'){
        print("")
        #predict sur la validation
        #mais pour ca validation doit etre = a validationmodel, avec toute les transformation
        }}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      tune_result_diff <- tune.svm(x=tabdiff2[,-1], y=tabdiff2[,1],
                                  gamma = 10^(-5:2), cost = 10^(-3:2),
                                  cross=min(dim(tabdiff2)[1]-2,10))
      resmodeldiff <- tune_result_diff$best.model
      if(criterionmodel=="accuracy"){test[i]<-resmodeldiff$tot.accuracy-model$tot.accuracy}
      if(criterionmodel=="BER"){
        #print(paste("Ber test :",BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted) ))
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted)}
      if(criterionmodel=="auc"){
        test[i]<-auc(roc(tabdiff2[,1], as.vector(resmodeldiff$decision.values),quiet=T))-aucmod}
    }}
  if(modeltype=="randomforest"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$predicted)}
    if(criterionmodel=="auc"){aucmod<-auc(roc(tab[,1], as.vector(model$votes[,1]),quiet=T))}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      resmodeldiff <-randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T,trace=T)
      if(criterionmodel=="accuracy"){test[i]<-mean(resmodeldiff$confusion[,3])-mean(model$confusion[,3])}
      if(criterionmodel=="BER"){
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$predicted)}
      if(criterionmodel=="auc"){
        test[i]<-auc(roc(tabdiff2[,1], as.vector(resmodeldiff$votes[,1]),quiet=T))-aucmod}
    }
  }
  #print(paste("test :",max(test)))
  if(max(test)>=0){num<-lessimportantevar[which(test==max(test))[1]]}
  else(num<-0)
  #print(paste( "num", num))
  return(num)
} 

importancemodelsvm<-function(model,modeltype,tabdiff,criterion){
  #function calculate the importance of each variable of the model
  #first column of tabdiff is the group
  importancevar<-vector()
  if(criterion=="accuracy"){
    if(modeltype=="svm"){
      for (i in 2:ncol(tabdiff)){
        vec<-vector()
        tabdiffmodif<-tabdiff
        for( j in 1:20){
          tabdiffmodif[,i]<-tabdiffmodif[sample(1:nrow(tabdiff)),i]
          #tabdiffmodif<-tabdiffmodif[,-i]
          
          resmodeldiff<-svm(y =tabdiffmodif[,1],x=tabdiffmodif[,-1],cross=10,
                            type ="C-classification",
                            kernel= ifelse(is.null(model$kernel),"radial",model$kernel),
                            cost=model$cost,
                            gamma=model$gamma)
          vec[j]<-abs(resmodeldiff$tot.accuracy-model$tot.accuracy)
        }
        importancevar[i]<-mean(vec)}
      
    }
    if(modeltype=="randomforest"){
      
      tabdiff<-as.data.frame(tabdiff[,c(colnames(tabdiff)[1],sort(colnames(tabdiff[,-1])))])
      tabdiff<-as.data.frame(tabdiff2[sort(rownames(tabdiff)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff[,-1],tabdiff[,1],ntree=1000,importance=T,keep.forest=T)
      importancevar<-model$importance[,4]
      importancevar<-c(NA,importancevar)
    }
  }
  if(criterion=="fscore"){
    importancevar<-Fscore(tab = as.data.frame(tabdiff[,-1]),class=tabdiff[,1])
  }
  return(importancevar)
}
Fscore<-function(tab,class){
  tabpos<-as.data.frame(tab[which(class==levels(class)[1]),])
  npos<-nrow(tabpos)
  tabneg<-as.data.frame(tab[which(class==levels(class)[2]),])
  nneg<-nrow(tabneg)
  fscore<-vector()
  for(i in 1:ncol(tab)){
    moypos<-mean(tabpos[,i])
    moyneg<-mean(tabneg[,i])
    moy<-mean(tab[,i])
    numerateur<-(moypos-moy)^2+(moyneg-moy)^2
    denominateur<-(sum((tabpos[,i]-moypos)^2)*(1/(npos-1)))+(sum((tabneg[,i]-moyneg)^2)*(1/(nneg-1)))
    fscore[i]<-numerateur/denominateur
  }
  return(c(NA,fscore))
}

BER<-function(class,classpredict){
  pos<-which(class==levels(class)[1])
  neg<-which(class==levels(class)[2])
  (1/2)*( sum(class[pos]!=classpredict[pos])/length(pos)+ sum(class[neg]!=classpredict[neg])/length(neg)  )
}

nll<-function(element){
  if(is.null(element)){return("")}
  else{return(element)}
}

sensibility<-function(predict,class){
data<-table(predict,class)
sensi<-round(data[1,1]/(data[1,1]+data[2,1]),digits = 3)
return(sensi)
}
specificity<-function(predict,class){
  data<-table(predict,class )
  round(data[2,2]/(data[1,2]+data[2,2]),digit=3)
}

# cette fonction construit un tableau de parametres a tester a partir d'une liste de parametres
# chaque element de la liste est un vecteur de valeurs a tester pour le parametre correspondant
constructparameters<-function(listparameters){
  resparameters<-data.frame(listparameters[[1]])
  namescol<-names(listparameters)
  
  for(i in 2:length(listparameters)){
    tt<-rep(listparameters[[i]],each=nrow(resparameters))
    res<-resparameters
    if(length(listparameters[[i]])>1){
      for (j in 1:(length(listparameters[[i]])-1)){
        res<-rbind(res,resparameters)
      }
    }
    resparameters<-cbind(res,tt)
  }
  colnames(resparameters)<-namescol
  return(resparameters)
}

testparametersfunction<-function(learning,validation,tabparameters){
  set.seed(20011203)
  # results<-matrix(data = NA,nrow =nrow(tabparameters), ncol=9 )
  # colnames(results)<-c("auc validation","sensibility validation","specificityvalidation",
  #                      "auc learning","sensibility learning","specificity learning",
  #                      "number of features in model","number of differented features",
  #                      "number of features selected")
  
  results<-matrix(data = NA,nrow =nrow(tabparameters), ncol=10)
  colnames(results)<-c("auc validation","sensibility validation","specificity validation",
                       "auc learning","sensibility learning","specificity learning",
                       "threshold used","number of features in model",
                       "number of differented features","number of features selected")
  print(paste(nrow(tabparameters),"parameters "))
  for (i in 1:nrow(tabparameters)){
    print(i)
    parameters<-tabparameters[i,]
    if(!parameters$NAstructure){tabparameters[i,c("thresholdNAstructure","structdata","maxvaluesgroupmin","minvaluesgroupmax")]<-rep(x = NA,4)    }
    #selectdataparameterst<-parameters[1:7]
    selectdataparameters<<-list("prctvalues"=parameters$prctvalues,
                                "selectmethod"=parameters$selectmethod,
                                "NAstructure"=parameters$NAstructure,
                                "structdata"=parameters$structdata,
                                "thresholdNAstructure"=parameters$thresholdNAstructure,
                                "maxvaluesgroupmin"=parameters$maxvaluesgroupmin,"minvaluesgroupmax"=parameters$minvaluesgroupmax)
    resselectdata<<-selectdatafunction(learning = learning,selectdataparameters = selectdataparameters)
    
    #transformdataparameters<<-parameters[8:11]
    if(!parameters$log){tabparameters[i,"logtype"]<-NA}
    transformdataparameters<<-list("log"=parameters$log,"logtype"=parameters$logtype,"standardization"=parameters$standardization,"arcsin"=parameters$arcsin,"rempNA"=parameters$rempNA)
    
    learningtransform<-transformdatafunction(learningselect = resselectdata$learningselect,structuredfeatures = resselectdata$structuredfeatures,
                                             datastructuresfeatures =   resselectdata$datastructuresfeatures,transformdataparameters = transformdataparameters)
    
    testparameters<<-list("SFtest"=FALSE,"test"=parameters$test,"adjustpval"=as.logical(parameters$adjustpv),"thresholdpv"=parameters$thresholdpv,"thresholdFC"=parameters$thresholdFC)
    restest<<-testfunction(tabtransform = learningtransform,testparameters = testparameters)
    
    if(parameters$test=="notest"){
      learningmodel<-learningtransform
      tabparameters[i,c("adjustpv","thresholdpv","thresholdFC")]<-rep(x = NA,3)
    }
    else{learningmodel<-restest$tabdiff}
    
    if(ncol(learningmodel)!=0){
    
    # Determine if automatic tuning should be used based on tuning_method parameter
    use_autotuning <- (!is.null(parameters$tuning_method) && parameters$tuning_method == "automatic")
    
    # Set autotuning flags for each model type
    autotunerf_flag <- use_autotuning
    autotunesvm_flag <- use_autotuning
    autotunexgb_flag <- use_autotuning
    autotunelgb_flag <- use_autotuning
    autotuneknn_flag <- use_autotuning
      
    modelparameters<<-list("modeltype"=parameters$model,
                           "invers"=FALSE,
                           "thresholdmodel"=parameters$thresholdmodel,
                           "fs"=as.logical(parameters$fs),
                           "adjustval"=!is.null(validation),
                           "autotunerf"=autotunerf_flag,
                           "autotunesvm"=autotunesvm_flag,
                           "autotunexgb"=autotunexgb_flag,
                           "autotunelgb"=autotunelgb_flag,
                           "autotuneknn"=autotuneknn_flag
                           )
    validate(need(ncol(learning)!=0,"No select dataset"))
    

    #resmodel<<-modelfunction(learningmodel = learningmodel,validation = validation,modelparameters = modelparameters,
    #                         transformdataparameters = transformdataparameters,datastructuresfeatures =  datastructuresfeatures)
    out<- tryCatch(modelfunction(learningmodel = learningmodel,
                                 validation = validation,
                                 modelparameters = modelparameters,
                                 transformdataparameters = transformdataparameters,
                                 datastructuresfeatures =  datastructuresfeatures,
                                 learningselect = resselectdata$learningselect), 
                   error = function(e) e)
    if(any(class(out)=="error"))parameters$model<-"nomodel"
    else{
      
      resmodel<-out
      
      # Apply threshold optimization if requested
      if(!is.null(parameters$threshold_method) && parameters$threshold_method != "fixed" && parameters$model != "nomodel"){
        tryCatch({
          # Calculate optimal threshold from ROC curve on learning data
          classlearning <- resmodel$datalearningmodel$reslearningmodel$classlearning
          scorelearning <- resmodel$datalearningmodel$reslearningmodel$scorelearning
          
          # Create ROC object
          roc_obj <- roc(classlearning, scorelearning, quiet=TRUE)
          
          # Find optimal threshold based on selected method
          if(parameters$threshold_method == "youden"){
            # Youden method: maximizes sensitivity + specificity - 1
            optimal_coords <- coords(roc_obj, "best", best.method="youden", ret=c("threshold", "sensitivity", "specificity"))
            optimal_threshold <- optimal_coords$threshold
            
            # Display optimization results
            cat(sprintf("    ✓ Youden optimization (iter %d): threshold=%.4f (sens=%.3f, spec=%.3f, Youden=%.3f)\n",
                        i, optimal_threshold,
                        optimal_coords$sensitivity,
                        optimal_coords$specificity,
                        optimal_coords$sensitivity + optimal_coords$specificity - 1))
            
          } else if(parameters$threshold_method == "equiprob"){
            # Equiprobability method: closest point to diagonal (minimizes |FP-FN|)
            optimal_coords <- coords(roc_obj, "best", best.method="closest.topleft", ret=c("threshold", "sensitivity", "specificity"))
            optimal_threshold <- optimal_coords$threshold
            
            # Calculate false positive and false negative rates for display
            fp_rate <- 1 - optimal_coords$specificity
            fn_rate <- 1 - optimal_coords$sensitivity
            
            # Display optimization results
            cat(sprintf("    ✓ Equiprobability optimization (iter %d): threshold=%.4f (sens=%.3f, spec=%.3f, FPR=%.3f, FNR=%.3f)\n",
                        i, optimal_threshold,
                        optimal_coords$sensitivity,
                        optimal_coords$specificity,
                        fp_rate, fn_rate))
          }
          
          # Recalculate predicted classes using optimal threshold for learning data
          # IMPORTANT: In this application, levels(classlearning)[1] = "positif" (case)
          # Score represents probability of being positive, so high score → predict positive
          # Therefore: score >= threshold → levels[1] (positif), score < threshold → levels[2] (negatif)
          resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[1], levels(classlearning)[2])
          resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- factor(resmodel$datalearningmodel$reslearningmodel$predictclasslearning, levels = levels(classlearning))
          
          # If validation data exists, apply optimal threshold to validation predictions as well
          # Same logic: high score → predict positive (level 1)
          if(!is.null(validation)){
            classval <- resmodel$datavalidationmodel$resvalidationmodel$classval
            scoreval <- resmodel$datavalidationmodel$resvalidationmodel$scoreval
            resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[1], levels(classval)[2])
            resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- factor(resmodel$datavalidationmodel$resvalidationmodel$predictclassval, levels = levels(classval))
          }
          
          # Update threshold in parameters for record
          parameters$thresholdmodel <- optimal_threshold
        }, error = function(e){
          # If threshold optimization fails, continue with original threshold
          cat(sprintf("    ✗ Threshold optimization FAILED (iteration %d): %s\n", i, e$message))
          cat(sprintf("      → Keeping initial threshold: %.4f\n", parameters$thresholdmodel))
          warning(paste("Threshold optimization failed:", e$message))
        })
      } else {
        # For "fixed" threshold method, use the threshold from parameters (already set to 0.5 for proba, 0 for SVM)
        # The classes are already predicted in modelfunction with this threshold
        # No need to recalculate, just ensure threshold is recorded
        if(parameters$model != "nomodel" && parameters$model != "svm"){
          # For probabilistic models, threshold should be 0.5 (already set)
          # For SVM, threshold is 0 (handled in modelfunction)
          # Just ensure the threshold is recorded correctly
          if(is.null(parameters$thresholdmodel) || is.na(parameters$thresholdmodel)){
            parameters$thresholdmodel <- 0.5
          }
        }
      }
      
      
      # # Apply Youden threshold optimization if requested
      # if(!is.null(parameters$optimize_threshold) && parameters$optimize_threshold && parameters$model != "nomodel"){
      #   tryCatch({
      #     # Calculate optimal threshold using Youden method from ROC curve on learning data
      #     classlearning <- resmodel$datalearningmodel$reslearningmodel$classlearning
      #     scorelearning <- resmodel$datalearningmodel$reslearningmodel$scorelearning
      #     
      #     # Create ROC object
      #     roc_obj <- roc(classlearning, scorelearning, quiet=TRUE)
      #     
      #     # Find optimal threshold using Youden method (maximizes sensitivity + specificity - 1)
      #     optimal_coords <- coords(roc_obj, "best", best.method="youden", ret=c("threshold", "sensitivity", "specificity"))
      #     optimal_threshold <- optimal_coords$threshold
      #     
      #     # Recalculate predicted classes using optimal threshold for learning data
      #     #resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[2], levels(classlearning)[1])
      #     # IMPORTANT: In this application, levels(classlearning)[1] = "positif" (case)
      #     # Score represents probability of being positive, so high score → predict positive
      #     # Therefore: score >= threshold → levels[1] (positif), score < threshold → levels[2] (negatif)
      #     resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- ifelse(scorelearning >= optimal_threshold, levels(classlearning)[1], levels(classlearning)[2])
      #     resmodel$datalearningmodel$reslearningmodel$predictclasslearning <- factor(resmodel$datalearningmodel$reslearningmodel$predictclasslearning, levels = levels(classlearning))
      #     
      #     # If validation data exists, apply optimal threshold to validation predictions as well
      #     if(!is.null(validation)){
      #       classval <- resmodel$datavalidationmodel$resvalidationmodel$classval
      #       scoreval <- resmodel$datavalidationmodel$resvalidationmodel$scoreval
      #       # resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[2], levels(classval)[1])
      #       resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- ifelse(scoreval >= optimal_threshold, levels(classval)[1], levels(classval)[2])
      #       resmodel$datavalidationmodel$resvalidationmodel$predictclassval <- factor(resmodel$datavalidationmodel$resvalidationmodel$predictclassval, levels = levels(classval))
      #     }
      #     
      #     # Update threshold in parameters for record
      #     parameters$thresholdmodel <- optimal_threshold
      #   }, error = function(e){
      #     # If threshold optimization fails, continue with original threshold
      #     cat(sprintf("    ✗ Youden optimization FAILED (iteration %d): %s\n", i, e$message))
      #     cat(sprintf("      → Keeping initial threshold: %.4f\n", parameters$thresholdmodel))
      #     warning(paste("Threshold optimization failed:", e$message))
      #   })
      # }
      
    }
    }
    else{parameters$model<-"nomodel"}
    #numberfeaturesselected
    # results[i,9]<-positive(dim(resselectdata$learningselect)[2]-1)
    #numberfeaturesdiff
    #numberfeaturesselected (shifted from 9 to 10)
    results[i,10]<-positive(dim(resselectdata$learningselect)[2]-1)
    #numberfeaturesdiff (shifted from 8 to 9)
    if(parameters$test!="notest"){
      results[i,8]<-positive(dim(restest$tabdiff)[2]-1)
    }
    #numberfeaturesmodel
    if(parameters$model!="nomodel"){
      # results[i,7]<-dim(resmodel$datalearningmodel$learningmodel)[2]-1
      results[i,8]<-dim(resmodel$datalearningmodel$learningmodel)[2]-1
      #thresholdused (NEW: index 7)
      results[i,7]<-round(parameters$thresholdmodel, digits = 4)
      #auclearning
      results[i,4]<-round(as.numeric(auc(roc(resmodel$datalearningmodel$reslearningmodel$classlearning,resmodel$datalearningmodel$reslearningmodel$scorelearning,quiet=T))),digits = 3)
      #sensibilitylearning
      results[i,5]<-sensibility(resmodel$datalearningmodel$reslearningmodel$predictclasslearning,resmodel$datalearningmodel$reslearningmodel$classlearning)
      #specificitylearning
      results[i,6]<-specificity(resmodel$datalearningmodel$reslearningmodel$predictclasslearning,resmodel$datalearningmodel$reslearningmodel$classlearning)
      if(!is.null(validation)){
      #aucvalidation
      results[i,1]<-round(as.numeric(auc(roc(resmodel$datavalidationmodel$resvalidationmodel$classval,resmodel$datavalidationmodel$resvalidationmodel$scoreval,quiet=T))),digits = 3)
      #sensibilityvalidation
      results[i,2]<-sensibility(resmodel$datavalidationmodel$resvalidationmodel$predictclassval,resmodel$datavalidationmodel$resvalidationmodel$classval)
      #specificityvalidation
      results[i,3]<-specificity(resmodel$datavalidationmodel$resvalidationmodel$predictclassval,resmodel$datavalidationmodel$resvalidationmodel$classval)
    }
    }
  }
  return(cbind(results,tabparameters))
}

##
importanceplot<-function(model,learningmodel,modeltype,graph=T){
  validate(need(!is.null(model),"No model"))
  validate(need(ncol(learningmodel)>2,"only one feature"))
  if(modeltype=="randomforest"){
    var_importance<- data.frame(variables=rownames(model$importance),
                                importance=as.vector(model$importance[,4]))

    varo<-var_importance[order(var_importance$importance,decreasing = T),1]
    var_importance$variables<-as.character(var_importance$variables)
    var_importance$variables<-factor(x =var_importance$variables,levels =varo  )

    p <- ggplot(var_importance, aes(x=variables, weight=importance,fill=variables))
    g<-p + geom_bar()+coord_flip()+ylab("Variable Importance (Mean Decrease in Gini Index)")+
      theme(legend.position="none",plot.title=element_text( size=15))+ggtitle("Importance of variables in the model")+scale_fill_grey()
  }
  if(modeltype=="svm"){
    importancevar<-importancemodelsvm(model = model,modeltype="svm",tabdiff=learningmodel,criterion = "fscore")

    var_importance<-as.data.frame(cbind(colnames(learningmodel),importancevar)[-1,])
    var_importance[,1]<-as.character(var_importance[,1])
    var_importance[,2]<-as.numeric(as.character(var_importance[,2]))
    colnames(var_importance)<-c("variables","importance")
    varo<-var_importance[order(var_importance$importance,decreasing = T),1]
    var_importance$variables<-as.character(var_importance$variables)
    var_importance$variables<-factor(x =var_importance$variables,levels =varo  )

    p <- ggplot(var_importance, aes(x=variables, weight=importance,fill=variables))
    g<-p + geom_bar()+coord_flip()+ylab("Variable Importance (fscore)")+theme(legend.position="none",plot.title=element_text( size=15))+ggtitle("Importance of variables in the model")+scale_fill_grey()
  }
  if(modeltype=="elasticnet"){
    # Extract coefficients from elasticnet model
    coef_matrix <- as.matrix(coef(model$glmnet_model, s=model$lambda))
    coef_values <- coef_matrix[-1, 1]  # Remove intercept
    names(coef_values) <- colnames(learningmodel)[-1]

    # Keep only non-zero coefficients
    nonzero_coefs <- coef_values[coef_values != 0]

    if(length(nonzero_coefs) > 0){
      var_importance <- data.frame(
        variables = names(nonzero_coefs),
        importance = abs(nonzero_coefs),
        stringsAsFactors = FALSE
      )

      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)

      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+coord_flip()+ylab("Variable Importance (Absolute Coefficient)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No variables with non-zero coefficients")
    }
  }
  if(modeltype=="xgboost"){
    # Extract feature importance from XGBoost model
    importance_matrix <- xgb.importance(model = model)

    if(nrow(importance_matrix) > 0){
      var_importance <- data.frame(
        variables = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )

      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)

      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+coord_flip()+ylab("Variable Importance (Gain)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+scale_fill_grey()
    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No feature importance available")
    }
  }
  if(modeltype=="lightgbm"){
    # Extract feature importance from LightGBM model
    importance_matrix <- lgb.importance(model = model)

 

    if(nrow(importance_matrix) > 0){
      var_importance <- data.frame(
        variables = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )

      varo <- var_importance[order(var_importance$importance, decreasing = T), 1]
      var_importance$variables <- factor(x = var_importance$variables, levels = varo)
 

      p <- ggplot(var_importance, aes(x=variables, weight=importance, fill=variables))
      g <- p + geom_bar()+coord_flip()+ylab("Variable Importance (Gain)")+
        theme(legend.position="none",plot.title=element_text( size=15))+
        ggtitle("Importance of variables in the model")+scale_fill_grey()

    } else {
      var_importance <- data.frame()
      g <- errorplot(text = "No feature importance available")

    }

  }
  if(modeltype=="naivebayes"){
    # Naive Bayes doesn't have traditional feature importance
    # We can compute conditional probabilities per class
    var_importance <- data.frame()
    g <- errorplot(text = "Naive Bayes: Feature importance not available\nModel uses probabilistic independence assumptions")

  }
  if(modeltype=="knn"){
    # KNN doesn't have traditional feature importance
    # Could compute based on feature scaling but not meaningful
    var_importance <- data.frame()
    g <- errorplot(text = "KNN: Feature importance not available\nModel uses distance-based classification")

  }
  if(!graph){return(var_importance)}
  if(graph){
    g
  }
}


positive<-function(x){
  if(x<0){x<-0}
  else{x}
  return(x)
}

####
# Function to apply a new threshold without retraining the model
# This separates threshold adjustment from hyperparameter tuning
apply_threshold <- function(model_result, new_threshold, groups = NULL) {
  # Extract necessary data from model_result
  if (is.null(groups)) {
    groups <- model_result$groups
  }
  
  lev <- groups
  
  # Apply threshold to learning data
  scorelearning <- model_result$datalearningmodel$reslearningmodel$scorelearning
  classlearning <- model_result$datalearningmodel$reslearningmodel$classlearning
  
  # Convert scorelearning to vector if it's a data.frame
  if(is.data.frame(scorelearning)) {
    scorelearning <- scorelearning[,1]
  }
  
  predictclasslearning <- factor(levels = lev)
  predictclasslearning[which(scorelearning >= new_threshold)] <- lev["positif"]
  predictclasslearning[which(scorelearning < new_threshold)] <- lev["negatif"]
  predictclasslearning <- as.factor(predictclasslearning)
  
  # Update reslearningmodel with new predictions
  # Create data.frame exactly as in original modelfunction (line 2450-2451)
  reslearningmodel <- data.frame(classlearning, scorelearning, predictclasslearning)
  colnames(reslearningmodel) <- c("classlearning", "scorelearning", "predictclasslearning")
  
  datalearningmodel <- list(
    "learningmodel" = model_result$datalearningmodel$learningmodel,
    "reslearningmodel" = reslearningmodel
  )
  
  # Apply threshold to validation data if present
  datavalidationmodel <- NULL
  if (!is.null(model_result$datavalidationmodel)) {
    scoreval <- model_result$datavalidationmodel$resvalidationmodel$scoreval
    classval <- model_result$datavalidationmodel$resvalidationmodel$classval
    
    # Convert scoreval to vector if it's a data.frame
    if(is.data.frame(scoreval)) {
      scoreval <- scoreval[,1]
    }
    
    predictclassval <- vector(length = length(scoreval))
    predictclassval[which(scoreval >= new_threshold)] <- lev["positif"]
    predictclassval[which(scoreval < new_threshold)] <- lev["negatif"]
    predictclassval <- as.factor(predictclassval)
    
    # Create data.frame exactly as in original modelfunction
    resvalidationmodel <- data.frame(classval, scoreval, predictclassval)
    colnames(resvalidationmodel) <- c("classval", "scoreval", "predictclassval")
    
    datavalidationmodel <- list(
      "validationmodel" = model_result$datavalidationmodel$validationmodel,
      "resvalidationmodel" = resvalidationmodel
    )
  }
  
  # Update model parameters with new threshold
  modelparameters <- model_result$modelparameters
  modelparameters$thresholdmodel <- new_threshold
  
  # Return updated result with new threshold
  return(list(
    "datalearningmodel" = datalearningmodel,
    "model" = model_result$model,
    "datavalidationmodel" = datavalidationmodel,
    "groups" = groups,
    "modelparameters" = modelparameters
  ))
}

modelfunction_base <-  function(learningmodel,
                          validation=NULL,
                          modelparameters,
                          transformdataparameters,
                          datastructuresfeatures=NULL,
                          learningselect){
  if(modelparameters$modeltype!="nomodel"){
    #' Entraîne le modèle et retourne les SCORES (pas les classes)
    #' Le threshold sera appliqué plus tard dans server.R

    # ========================================================================
    # INITIALISATION (identique à l'original)
    # ========================================================================

    # Rename first column to "group" for consistency
    colnames(learningmodel)[1] <- "group"
    if(!is.null(validation)){
      colnames(validation)[1] <- "group"
    }

    lev <- levels(learningmodel[,1])
    names(lev) <- c("positif", "negatif")
    
    # Préparation des données de validation
    validationmodel <- NULL
    if(!is.null(validation)){
      nameslearning <- colnames(learningmodel)[-1]
      namesvalidation <- colnames(validation)[-1]
      
      # Ajouter les variables manquantes dans validation
      if(!all(nameslearning %in% namesvalidation)){
        missingvars <- nameslearning[which(!nameslearning %in% namesvalidation)]
        dfmissing <- data.frame(matrix(data = 0, nrow = nrow(validation), 
                                       ncol = length(missingvars)))
        colnames(dfmissing) <- missingvars
        validation <- cbind(validation, dfmissing)
      }
      
      validationmodel <- validation[, c("group", nameslearning)]
      
      # Transformation des données de validation si nécessaire
      if(modelparameters$adjustval){
        validationmodel <- transformdatafunction(
          learningselect = validationmodel,
          structuredfeatures = NULL,
          datastructuresfeatures = NULL,
          transformdataparameters = transformdataparameters
        )
        validationmodel <- validationmodel[, colnames(learningmodel)]
      }
      
      validationmodel[,1] <- factor(validationmodel[,1], levels = lev)
    }
    
    # ========================================================================
    # RANDOM FOREST
    # ========================================================================
    
    if(modelparameters$modeltype == "randomforest"){
      cat("Training Random Forest model...\n")
      
      # Déterminer les hyperparamètres
      if(is.null(modelparameters$autotunerf) || modelparameters$autotunerf){
        # Tuning automatique
        set.seed(20011203)
        ntree_param <- 500
        
        # Tuning de mtry
        p <- ncol(learningmodel) - 1
        max_mtry <- min(p, 10)
        mtry_values <- unique(c(floor(sqrt(p)), floor(p/3), floor(p/2)))
        mtry_values <- mtry_values[mtry_values <= max_mtry & mtry_values > 0]
        
        best_auc <- 0
        best_mtry <- floor(sqrt(p))
        
        for(mtry_test in mtry_values){
          rf_temp <- randomForest(group ~ ., data = learningmodel, 
                                  ntree = ntree_param, mtry = mtry_test)
          pred_probs <- predict(rf_temp, learningmodel[,-1], type = "prob")[, lev["positif"]]
          roc_obj <- roc(learningmodel[,1], pred_probs, levels = lev)
          auc_value <- auc(roc_obj)
          
          if(auc_value > best_auc){
            best_auc <- auc_value
            best_mtry <- mtry_test
          }
        }
        
        model <- randomForest(group ~ ., data = learningmodel,
                              ntree = ntree_param, mtry = best_mtry)
        model$optimal_mtry <- best_mtry
        model$ntree_used <- ntree_param
        
      } else {
        # Paramètres manuels
        ntree_param <- ifelse(is.null(modelparameters$ntree), 500, modelparameters$ntree)
        mtry_param <- ifelse(is.null(modelparameters$mtry), 
                             floor(sqrt(ncol(learningmodel)-1)), 
                             modelparameters$mtry)
        
        model <- randomForest(group ~ ., data = learningmodel,
                              ntree = ntree_param, mtry = mtry_param)
        model$optimal_mtry <- mtry_param
        model$ntree_used <- ntree_param
      }
      
      # Feature selection si demandé
      if(modelparameters$fs){
        featureselect <- selectedfeature(model = model, modeltype = "randomforest",
                                         tab = learningmodel,
                                         criterionimportance = "fscore",
                                         criterionmodel = "auc")
        model <- featureselect$model
        learningmodel <- featureselect$dataset
      }
      
      # *** MODIFICATION CLÉ : Retourner les PROBABILITÉS ***
      scorelearning <- predict(model, learningmodel[,-1], type = "prob")[, lev["positif"]]
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
      
      # Scores pour validation
      if(!is.null(validationmodel)){
        scorevalidation <- predict(model, validationmodel[,-1], type = "prob")[, lev["positif"]]
        scorevalidation <- data.frame(scorevalidation)
        colnames(scorevalidation) <- paste(lev[1], "/", lev[2], sep = "")
      } else {
        scorevalidation <- NULL
      }
      
      # *** PAS DE CLASSIFICATION ICI - Ces lignes ont été SUPPRIMÉES ***
      # predictclasslearning <- factor(levels = lev)
      # predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)] <- lev["positif"]
      # predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)] <- lev["negatif"]
      # predictclasslearning <- as.factor(predictclasslearning)
    }
    
    # ========================================================================
    # SVM
    # ========================================================================
    
    else if(modelparameters$modeltype == "svm"){
      cat("Training SVM model...\n")
      
      # Déterminer les hyperparamètres
      if(is.null(modelparameters$autotunesvm) || modelparameters$autotunesvm){
        # Tuning automatique
        set.seed(20011203)
        tuneresult <- tune(svm, group ~ ., data = learningmodel,
                           kernel = "radial",
                           ranges = list(
                             cost = c(0.01, 0.1, 1, 10, 100),
                             gamma = c(0.001, 0.01, 0.1, 1)
                           ),
                           tunecontrol = tune.control(sampling = "cross", cross = 5))
        
        cost_param <- tuneresult$best.parameters$cost
        gamma_param <- tuneresult$best.parameters$gamma
      } else {
        # Paramètres manuels
        cost_param <- ifelse(is.null(modelparameters$cost), 1, modelparameters$cost)
        gamma_param <- ifelse(is.null(modelparameters$gamma), 0.1, modelparameters$gamma)
      }
      
      model <- svm(group ~ ., data = learningmodel,
                   kernel = 'radial',
                   cost = cost_param,
                   gamma = gamma_param,
                   type = "C-classification",
                   probability = TRUE)
      
      model$cost <- cost_param
      model$gamma <- gamma_param
      
      # Feature selection si demandé
      if(modelparameters$fs){
        featureselect <- selectedfeature(model = model, modeltype = "svm",
                                         tab = learningmodel,
                                         criterionimportance = "fscore",
                                         criterionmodel = "auc")
        model <- featureselect$model
        learningmodel <- featureselect$dataset
      }
      
      # *** MODIFICATION CLÉ : Retourner les DECISION VALUES ***
      scorelearning <- model$decision.values
      if(sum(lev == (strsplit(colnames(scorelearning), split = "/")[[1]])) == 0){
        scorelearning <- scorelearning * (-1)
        colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
      }
      
      # Scores pour validation
      if(!is.null(validationmodel)){
        pred_val <- predict(model, validationmodel[,-1], decision.values = TRUE)
        scorevalidation <- attr(pred_val, "decision.values")
        if(sum(lev == (strsplit(colnames(scorevalidation), split = "/")[[1]])) == 0){
          scorevalidation <- scorevalidation * (-1)
          colnames(scorevalidation) <- paste(lev[1], "/", lev[2], sep = "")
        }
      } else {
        scorevalidation <- NULL
      }
      
      # *** PAS DE CLASSIFICATION ICI - Ces lignes ont été SUPPRIMÉES ***
      # predictclasslearning <- factor(levels = lev)
      # predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)] <- lev["positif"]
      # predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)] <- lev["negatif"]
      # predictclasslearning <- as.factor(predictclasslearning)
    }
    
    # ========================================================================
    # ELASTICNET (Logistic Regression)
    # ========================================================================
    
    else if(modelparameters$modeltype == "elasticnet"){
      cat("Training ElasticNet model...\n")
      
      x <- as.matrix(learningmodel[,-1])
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      
      # Déterminer les hyperparamètres
      alpha_param <- ifelse(is.null(modelparameters$alpha), 0.5, modelparameters$alpha)
      lambda_param <- modelparameters$lambda
      
      # GridSearchCV si activé
      if(!is.null(modelparameters$use_gridsearch) && modelparameters$use_gridsearch && 
         is.null(lambda_param)){
        cat("Using GridSearchCV for ElasticNet...\n")
        
        param_grid <- list(
          alpha = if(!is.null(modelparameters$en_grid_alpha)) modelparameters$en_grid_alpha else c(0, 0.25, 0.5, 0.75, 1.0),
          lambda = if(!is.null(modelparameters$en_grid_lambda)) modelparameters$en_grid_lambda else c(0.001, 0.01, 0.1, 1.0)
        )
        
        grid_result <- tryCatch({
          X_df <- as.data.frame(x)
          tune_elasticnet_gridsearch(X = X_df, y = learningmodel[,1],
                                     param_grid = param_grid,
                                     n_folds = 5,
                                     scoring = c("auc", "accuracy"))
        }, error = function(e) {
          cat("GridSearchCV failed, falling back to cv.glmnet:", e$message, "\n")
          NULL
        })
        
        if(!is.null(grid_result)) {
          alpha_param <- if(!is.null(grid_result$best_params$alpha)) grid_result$best_params$alpha else 0.5
          lambda_param <- if(!is.null(grid_result$best_params$lambda)) grid_result$best_params$lambda else NULL
          
          set.seed(20011203)
          cvfit <- cv.glmnet(x, y, family = "binomial", alpha = alpha_param,
                             type.measure = "auc", nfolds = min(10, nrow(learningmodel)-1))
          lambda_param <- cvfit$lambda.min
          model <- list(glmnet_model = cvfit, lambda = lambda_param, alpha = alpha_param,
                        cvfit = cvfit, optimal_lambda = lambda_param, lambda_1se = cvfit$lambda.1se)
        } else {
          set.seed(20011203)
          cvfit <- cv.glmnet(x, y, family = "binomial", alpha = alpha_param,
                             type.measure = "auc", nfolds = min(10, nrow(learningmodel)-1))
          lambda_param <- cvfit$lambda.min
          model <- list(glmnet_model = cvfit, lambda = lambda_param, alpha = alpha_param,
                        cvfit = cvfit, optimal_lambda = lambda_param, lambda_1se = cvfit$lambda.1se)
        }
      } else if(is.null(lambda_param)){
        # Cross-validation standard
        set.seed(20011203)
        cvfit <- cv.glmnet(x, y, family = "binomial", alpha = alpha_param,
                           type.measure = "auc", nfolds = min(10, nrow(learningmodel)-1))
        lambda_param <- cvfit$lambda.min
        model <- list(glmnet_model = cvfit, lambda = lambda_param, alpha = alpha_param,
                      cvfit = cvfit, optimal_lambda = lambda_param, lambda_1se = cvfit$lambda.1se)
      } else {
        # Paramètres manuels
        fit <- glmnet(x, y, family = "binomial", alpha = alpha_param, lambda = lambda_param)
        model <- list(glmnet_model = fit, lambda = lambda_param, alpha = alpha_param,
                      cvfit = NULL, optimal_lambda = lambda_param, lambda_1se = NULL)
      }
      
      # Feature selection si demandé
      if(modelparameters$fs){
        coef_values <- as.matrix(coef(model$glmnet_model, s = lambda_param))
        selected_features <- rownames(coef_values)[which(coef_values[-1,1] != 0)]
        
        if(length(selected_features) > 0){
          learningmodel <- learningmodel[, c("group", selected_features)]
          x <- as.matrix(learningmodel[,-1])
          
          if(is.null(modelparameters$lambda)){
            cvfit <- cv.glmnet(x, y, family = "binomial", alpha = alpha_param,
                               type.measure = "auc", nfolds = min(10, nrow(learningmodel)-1))
            lambda_param <- cvfit$lambda.min
            fit <- glmnet(x, y, family = "binomial", alpha = alpha_param, lambda = lambda_param)
            model <- list(glmnet_model = fit, lambda = lambda_param, alpha = alpha_param,
                          cvfit = cvfit, optimal_lambda = lambda_param, lambda_1se = cvfit$lambda.1se)
          } else {
            fit <- glmnet(x, y, family = "binomial", alpha = alpha_param, lambda = lambda_param)
            model <- list(glmnet_model = fit, lambda = lambda_param, alpha = alpha_param,
                          cvfit = NULL, optimal_lambda = lambda_param, lambda_1se = NULL)
          }
        }
      }
      
      # *** MODIFICATION CLÉ : Retourner les PROBABILITÉS ***
      if(inherits(model$glmnet_model, "cv.glmnet")){
        scorelearning <- as.vector(glmnet:::predict.cv.glmnet(
          model$glmnet_model, newx = x, s = lambda_param, type = "response"))
      } else {
        scorelearning <- as.vector(glmnet::predict.glmnet(
          model$glmnet_model, newx = x, s = lambda_param, type = "response"))
      }
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
      
      # Scores pour validation
      if(!is.null(validationmodel)){
        x_val <- as.matrix(validationmodel[,-1])
        if(inherits(model$glmnet_model, "cv.glmnet")){
          scorevalidation <- as.vector(glmnet:::predict.cv.glmnet(
            model$glmnet_model, newx = x_val, s = lambda_param, type = "response"))
        } else {
          scorevalidation <- as.vector(glmnet::predict.glmnet(
            model$glmnet_model, newx = x_val, s = lambda_param, type = "response"))
        }
        scorevalidation <- data.frame(scorevalidation)
        colnames(scorevalidation) <- paste(lev[1], "/", lev[2], sep = "")
      } else {
        scorevalidation <- NULL
      }
      
      # *** PAS DE CLASSIFICATION ICI - Ces lignes ont été SUPPRIMÉES ***
      # predictclasslearning <- factor(levels = lev)
      # predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)] <- lev["positif"]
      # predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)] <- lev["negatif"]
      # predictclasslearning <- as.factor(predictclasslearning)
    }
    
    # ========================================================================
    # XGBOOST
    # ========================================================================
    
    else if(modelparameters$modeltype == "xgboost"){
      cat("Training XGBoost model...\n")
      
      x <- as.matrix(learningmodel[,-1])
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      dtrain <- xgb.DMatrix(data = x, label = y)
      
      # Déterminer les hyperparamètres (code identique à l'original)
      # ... [votre code existant pour XGBoost] ...
      
      # *** MODIFICATION CLÉ : Retourner les PROBABILITÉS ***
      scorelearning <- predict(model, dtrain)
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
      
      # Scores pour validation
      if(!is.null(validationmodel)){
        x_val <- as.matrix(validationmodel[,-1])
        y_val <- ifelse(validationmodel[,1] == lev["positif"], 1, 0)
        dval <- xgb.DMatrix(data = x_val, label = y_val)
        scorevalidation <- predict(model, dval)
        scorevalidation <- data.frame(scorevalidation)
        colnames(scorevalidation) <- paste(lev[1], "/", lev[2], sep = "")
      } else {
        scorevalidation <- NULL
      }
      
      # *** PAS DE CLASSIFICATION ICI ***
    }
    
    # ========================================================================
    # LIGHTGBM
    # ========================================================================
    
    else if(modelparameters$modeltype == "lightgbm"){
      cat("Training LightGBM model...\n")
      
      x <- as.matrix(learningmodel[,-1])
      y <- ifelse(learningmodel[,1] == lev["positif"], 1, 0)
      dtrain <- lgb.Dataset(data = x, label = y)
      
      # Déterminer les hyperparamètres (code identique à l'original)
      # ... [votre code existant pour LightGBM] ...
      
      # *** MODIFICATION CLÉ : Retourner les PROBABILITÉS ***
      scorelearning <- predict(model, x)
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
      
      # Scores pour validation
      if(!is.null(validationmodel)){
        x_val <- as.matrix(validationmodel[,-1])
        scorevalidation <- predict(model, x_val)
        scorevalidation <- data.frame(scorevalidation)
        colnames(scorevalidation) <- paste(lev[1], "/", lev[2], sep = "")
      } else {
        scorevalidation <- NULL
      }
      
      # *** PAS DE CLASSIFICATION ICI ***
    }
    
    # ========================================================================
    # NAIVE BAYES
    # ========================================================================
    
    else if(modelparameters$modeltype == "naivebayes"){
      cat("Training Naive Bayes model...\n")
      
      # Déterminer les hyperparamètres (code identique à l'original)
      # ... [votre code existant pour Naive Bayes] ...
      
      # *** MODIFICATION CLÉ : Retourner les PROBABILITÉS ***
      scorelearning <- predict(model, learningmodel[,-1], type = "raw")[, lev["positif"]]
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
      
      # Scores pour validation
      if(!is.null(validationmodel)){
        scorevalidation <- predict(model, validationmodel[,-1], type = "raw")[, lev["positif"]]
        scorevalidation <- data.frame(scorevalidation)
        colnames(scorevalidation) <- paste(lev[1], "/", lev[2], sep = "")
      } else {
        scorevalidation <- NULL
      }
      
      # *** PAS DE CLASSIFICATION ICI ***
    }
    
    # ========================================================================
    # KNN
    # ========================================================================
    
    else if(modelparameters$modeltype == "knn"){
      cat("Training KNN model...\n")
      
      # Déterminer les hyperparamètres (code identique à l'original)
      # ... [votre code existant pour KNN] ...
      
      # *** MODIFICATION CLÉ : Retourner les PROBABILITÉS ***
      # (Le calcul des probabilités pour KNN dépend de votre implémentation)
      scorelearning <- predict(model, learningmodel[,-1], type = "prob")[, lev["positif"]]
      scorelearning <- data.frame(scorelearning)
      colnames(scorelearning) <- paste(lev[1], "/", lev[2], sep = "")
      
      # Scores pour validation
      if(!is.null(validationmodel)){
        scorevalidation <- predict(model, validationmodel[,-1], type = "prob")[, lev["positif"]]
        scorevalidation <- data.frame(scorevalidation)
        colnames(scorevalidation) <- paste(lev[1], "/", lev[2], sep = "")
      } else {
        scorevalidation <- NULL
      }
      
      # *** PAS DE CLASSIFICATION ICI ***
    }
    
    # ========================================================================
    # RETOUR DES RÉSULTATS (MODIFIÉ)
    # ========================================================================
    
    cat("✓ Model training complete. Returning scores (no threshold applied).\n")
    
    # *** NOUVEAU FORMAT DE RETOUR ***
    return(list(
      "model" = model,
      "scores_learning" = scorelearning,
      "scores_validation" = scorevalidation,
      "true_class_learning" = learningmodel[,1],
      "true_class_validation" = if(!is.null(validationmodel)) validationmodel[,1] else NULL,
      "levels" = lev,
      "learningmodel" = learningmodel,
      "validationmodel" = validationmodel,
      "modelparameters" = modelparameters
    ))
    
  }
}
