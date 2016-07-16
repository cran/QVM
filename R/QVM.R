#' @title Questionnaires Validation Module
#' @author Nery Sofia Huerta-Pacheco, Purificacion Vicente-Galindo, Mercedes Sanchez Barba,  Maria Purificacion Galindo Villardon
#'
#' @description This package does a multivariate analysis for questionnaire validation with Likert-type scale variables.
#'
#' You can learn more about this package at:
#' http://www.uv.mx/personal/nehuerta/qvm/
#'
#' @details
#' Introduce a data set of categorical information and classifed as numerical data.
#' Choose a data set (delimited csv for comma).
#' This funtion takes a data set and applies questionnaire validatioin techniques to return parameter estimates.
#'
#' @return QVM is an interface
#' @examples \dontrun{
#' ##Call to library
#' library(QVM)
#' ##Call the package
#' QVM()
#' }
#' @references
#' Allen, F., & Locker, D. (2002). A Modified Short Version of the Oral Health Impact
#' Profile for Assessing Health-Related Quality of Life in Edentulous Adults. The
#' International Journal of Prosthodontics, 15(5), 446-450.
#'
#' Baker, F. (2004). Item Response Theory: Parameter Estimation Techniques,
#' Second Edition. CRC Press.
#'
#' Cronbach, L. (1951). Coefficient alpha and the internal structure of tests.
#' Psychomerika, 16, 297–334.
#'
#' Gil Pascual, J. A. (2015). Metodología cuantitativa en educación. UNED
#'
#' Juniper, E. F., Guyatt, G. H., Streiner, D. L., & King, D. R. (1997). Clinical impact
#' versus factor analysis for quality of life questionnaire construction. Journal of
#' ClinicalEpidemiology, 233-238.
#'
#' Likert, R. (1932). A Technique for the Measurement of Attitudes. Archives of
#' Psychology, 140, 1–55.
#'
#' Martínez Arias, M. R., Hernández Lloreda, M. V., & Hernández Lloreda, M. J. (2014).
#' Psicometría. Madrid: Alianza Editorial.
#'
#' Ponsoda, V., Revuelta, J. & Abad, F. J. (2006). Modelos politómicos de respuesta al
#' ítem. Madrid: La Muralla.
#'
#' Rizopoulos, D. (2006). ltm: An R package for Latent Variable Modelling and Item
#' Response Theory Analyses, Journal of Statistical Software, 17 (5), 1-25.
#' URL http://www.jstatsoft.org/v17/i05/
#'
#' Stevens, S. S. (1946). On the Theory of Scales of Measurement. Science, 677-680.
#'
#' @export QVM
#' @import ltm
#' @import tcltk
#' @import gWidgets
#' @import IMPACT
#' @import pander
#' @import multilevel
#' @import mvtnorm
#' @import nlme
#' @import graphics
#' @import grDevices
#' @import utils
#' @importFrom stats coef
#' @importFrom stats chisq.test
#' @importFrom stats plogis
#' @importFrom psych polychoric
#'
QVM<-function(){

  mi<- new.env()

  ##LIBRARY
  options("guiToolkit"="tcltk")

  ##PRINCIPAL
  w<- gwindow("QVM - USAL",visible=FALSE,width = 700,height= 610)
  g<- ggroup(horizontal=FALSE, spacing=0, container = w)

  nb <- gnotebook(container=g,width = 700,height= 610)
  g1<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Presentation")
  g2<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Descriptive")
  g3<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Alpha")
  g4<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Impact")
  g5<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "ITR  GRM")

  ##MENU FILE
  assign("gdata",NULL, envir =mi)
  assign("YY",NULL, envir =mi)
  requireNamespace("ltm", quietly = TRUE)

  #OPEN
  abrir<-function(h,...){
    data<-tk_choose.files()
    data1<-read.csv(data)
    assign("gdata",data1, envir =mi)
  }

  ##VIEW
  ver<-function(h,...){
    gdata<-get("gdata",envir =mi)
    fix(gdata)
  }

  ##NEW
  inicio<-function(h,...){
    dispose(w)
    QVM()
  }

  #READ
  leer<-function(h,...){

    w1<- tktoplevel ( )
    tkwm.title(w1,"The Impact of Different Item-Selection")
    frame <- ttkframe(w1 , padding = c(5 ,5 ,30 ,30))
    tkpack ( frame , expand = TRUE , fill = "both")
    nested_frame <- ttkframe ( frame ) ; tkpack ( nested_frame )
    label <- ttklabel ( nested_frame , text="Null Value:")
    tkpack ( label , side = "left" )
    text_var <- tclVar ( "" )
    entry <- ttkentry ( nested_frame , textvariable=text_var)
    tkpack ( entry )
    button_frame <- ttkframe ( frame )
    tkpack ( button_frame , anchor = "ne" )
    button <- ttkbutton ( button_frame , text ="Save")
    button1 <- ttkbutton ( button_frame , text ="Ok",command = function( )tkdestroy(w1))
    sapply ( list ( button , button1 ) , tkpack)
    yyy <- function ( ) {
      msg <-  tclvalue ( text_var )
      assign("YY",msg, envir =mi)
    }
    tkconfigure (button,command =yyy)
    tkconfigure (button1 , underline = 0)
  }

  ##CLOSE
  cerrar<-function(h,...){
    dispose(w)
  }

  menulistaA<-list(u1=gaction("Open",handler=abrir),u2=gaction("View",handler=ver),u3=gaction("Refresh",handler=inicio),u4=gaction("Close",handler=cerrar))

  ##MENU METHOD

  #CD
  cd<-function(h,...){
    gdata<-get("gdata",envir =mi)
    CARDES<-function(gdata){

      # STATISTICS
      datos<-gdata
      M<-ncol(datos)
      N<-nrow(datos)
      nc<-matrix(,1,M)
      x<-matrix()
      mg<-matrix()

      # MATRIX
      print("Matrix Dimensions",quote=FALSE)
      print("Cases",quote=FALSE)
      print(N)
      print("Variables",quote=FALSE)
      print(M)

      # ONE TABLE
      print("One Way Frequency Table",quote=FALSE)
      for(i in 1:M){
        nc[i]<-colnames(datos[i])
        x<-t(table(datos[i]))
        suma<-sum(x)
        por<-sprintf("%.4f",(x/suma)*100)
        tp<-t(matrix(por))
        tx<-t(x)
        mg<-rbind(x,tp)
        tmg<-t(mg)
        colnames(tmg)<-c("Count","Frequency")
        print(nc[i],quote=FALSE)
        pandoc.table(tmg,plain.ascii = TRUE)
        print("-----------------------",quote=FALSE)
        mg<-matrix()
        x<-matrix()
      }

      # TWO TABLES
      print("Two Way Frequency Table",quote=FALSE)
      for(i in 1:M){
        for(j in 2:M){
          if(i==j){

          }else{
            t2<-table(datos[,c(i,j)])
            t2p<-t2/N
            print(t2p,quote=FALSE)
            chi<-chisq.test(table(datos[,c(i,j)]))
            X<-(paste("X-squared =",sprintf("%.2f",chi$statistic)))
            Y<-(paste("df =",chi$parameter))
            Z<-(paste("p-value =",sprintf("%.2f",chi$p.value)))
            print(paste("Chi-squared Test -",X,Y,Z),quote=FALSE)
            print("----------------------------",quote=FALSE)
          }
        }
      }

      # Polychoric
      print("Polychoric Correlation",quote=FALSE)
      z<-polychoric(datos)
      corz<-z$rho
      pandoc.table(corz,plain.ascii = TRUE)

    }
    ##g2
    tbl<-glayout(container=g2)
    gseparator(horizontal=TRUE, container=g2)
    outputArea <- gtext(container=g2, expand=TRUE,width = 700,height= 600)
    out <- capture.output(CARDES(gdata))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }


  #Alpha
  alpha<-function(h,...){
    gdata<-get("gdata",envir =mi)
    ALPHA<-function(gdata){

      Datos<-gdata
      n<-ncol(Datos)
      al<-cronbach(Datos)
      colnames(Datos)
      N<-al$N
      Alfa<-al$Alpha
      for(i in 1:n){
        al<-cronbach(Datos[,-i])
        Alfa[i+1]<-al$Alpha
      }
      print("Cronbach's Alpha",quote=FALSE)
      print("----------------------------",quote=FALSE)
      alp<-matrix(Alfa,,1)
      rownames(alp)<-c("General",paste("-",colnames(Datos)))
      colnames(alp)<-"Alpha"
      pandoc.table(alp,plain.ascii = TRUE)
    }
    ##g3
    tbl<-glayout(container=g3)
    gseparator(horizontal=TRUE, container=g3)
    outputArea <- gtext(container=g3, expand=TRUE)
    size(outputArea) <- c(700, 600)
    out <- capture.output(ALPHA(gdata))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }

  #Impact
  imp<-function(h,...){
    #PARAMETERS
    gdata<-get("gdata",envir =mi)
    YY<-get("YY",envir =mi)
    IMPACTO<-function(gdata,YY){
      y<-as.numeric(YY)
      x<-gdata
      imz<-IMPACT(x,y)
      pandoc.table(imz,plain.ascii = TRUE)
    }
    ##g4
    tbl<-glayout(container=g4)
    gseparator(horizontal=TRUE, container=g4)
    outputArea <- gtext(container=g4, expand=TRUE)
    size(outputArea) <- c(700, 600)
    out <- capture.output(IMPACTO(gdata,YY))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea,out)
  }

  #Tri
  tri<-function(h,...){
    gdata<-get("gdata",envir =mi)
    TRI<-function(gdata){

      # INFORMATION

      a<-gdata
      N<-nrow(a)
      M<-ncol(a)
      na<-colnames(a)
      #Polychoric
      z<-polychoric(a)
      corz<-z$rho
      print("----------------------------",quote=FALSE)
      print("Polychoric Correlation",quote=FALSE)
      print("----------------------------",quote=FALSE)
      pandoc.table(corz,plain.ascii = TRUE)

      compz<-eigen(corz)
      ecra<-matrix(compz$values,M,1)

      # Parameters
      c<-grm(a,Hessian=TRUE,IRT.param=TRUE)
      ce<-coef(c)
      MCE<-ncol(ce)
      mz<-matrix(ce,M,MCE)
      rownames(mz)<-na
      cce<-colnames(ce)
      uce<-toupper(cce)
      colnames(mz)<-uce
      print("----------------------------",quote=FALSE)
      print("Parameters",quote=FALSE)
      print("----------------------------",quote=FALSE)
      pandoc.table(mz,plain.ascii = TRUE)
      print("----------------------------",quote=FALSE)

      betas <- c$coefficients
      zrange = c(-3, 3)
      z = seq(zrange[1], zrange[2], length = 31)

      iprobs <-
        function (betas, z) {
          n <- length(z)
          gammas <- lapply(betas, function (x) {
            nx <- length(x)
            cbind(plogis(matrix(x[-nx], n, nx - 1, TRUE) - x[nx] * z), 1)
          })
          lapply(gammas, function (x) {
            nc <- ncol(x)
            cbind(x[, 1], x[, 2:nc] - x[, 1:(nc - 1)])
          })
        }

      cprobs <-
        function (betas, z, eps = .Machine$double.eps^(1/3)) {
          lapply(betas, function (x, z) {
            nx <- length(x)
            out <- plogis(x[-nx] - matrix(x[nx] * z, nx - 1, length(z), TRUE))
            if (any(ind <- out == 1))
              out[ind] <- 1 - eps
            if (any(ind <- out == 0))
              out[ind] <- eps
            rbind(out, 1)
          }, z = z)
        }

      infoprobs <-
        function (betas, z) {
          cpr <- cprobs(betas, z)
          ipr <- iprobs(betas, z)
          sum.cprs <- lapply(cpr, function (x) {
            nr <- nrow(x)
            t((1 - rbind(x[1, ], x[-nr, ] + x[-1, ]))^2)
          })
          betas. <- sapply(betas, function (x) x[length(x)])
          for (i in 1:length(betas))
            sum.cprs[[i]] <- betas.[i]^2 * ipr[[i]] * sum.cprs[[i]]
          do.call(cbind, lapply(sum.cprs, rowSums))
        }

      cpr<-infoprobs(betas, z)
      M<-ncol(cpr)
      names<-colnames(cpr)
      #Test Information Function
      print("Test Information Function",quote=FALSE)
      print("----------------------------",quote=FALSE)
      cg<-1
      for(i in 1:M){
        print(paste("--- Item:",names[i],"---"),quote=FALSE)
        ci<-t(cbind(z,sprintf("%.4f",cpr[,i])))
        pandoc.table(ci,plain.ascii = TRUE)
        cg<-cpr[,i]+cg
      }
      sg<-t(cbind(z,sprintf("%.4f",cg)))
      row.names(sg)<-c("z","")
      print(paste("---","Total Test Information","---"),quote=FALSE)
      pandoc.table(sg,plain.ascii = TRUE)
      mcg<-max(cg)
      mmcg<-mcg/M
      mcg1<-sprintf("%.4f",mcg)
      mmcg1<-sprintf("%.4f",mmcg)

      print(paste("The maximun value of estimations for the General Information Function is",mcg1,"which responds to a mean of expected information of",mmcg1),quote=FALSE)

      #PLOT - GENERAL
      dev.new()
      plot(c,items=0,main=paste("General Information Function"),type="IIC",lty=1,labels=NULL,xlab="Ability",ylab="Information",col="darkblue")

      #PLOT - ITEMS
      for(i in 1:M){
        dev.new()
        par(mfrow=c(1,2))
        color<-c("darkblue","darkred","darkgreen","orange","darkorchid4","darkcyan","darkorange4","gray","black")
        plot(c,items=i,main=paste("Category Characteristic Curves","","Item:","",na[i]),type="ICC",lty=1,legend=T,cx="left",cex=.5,xlab="Ability",ylab="Probability",col=color)
        plot(c,items=i,main=paste("Test Information Function","","Item:","",na[i]),type="IIC",lty=1,labels=NULL,xlab="Ability",ylab="Information",col="darkblue",ylim=c(0,4))
      }
    }
    ##g5
    tbl<-glayout(container=g5)
    gseparator(horizontal=TRUE, container=g5)
    outputArea <- gtext(container=g5, expand=TRUE)
    size(outputArea) <- c(700, 600)
    out <- capture.output(TRI(gdata))
    dispose(outputArea)
    if(length(out)>0)
      add(outputArea, out)
  }
  imp1<-list(una=gaction("Null Value",handler=leer),dos=gaction("Impact",handler=imp))

  menulistaZ<-list(u0=gaction("Descriptive",handler=cd),u1=gaction("Alpha",handler=alpha),Impact=imp1,u3=gaction("IRT GRM",handler=tri))

  ##MENU HELP

  #Manual
  y1<- function(h,..) gmessage("http://www.uv.mx/personal/nehuerta/qvm/",title="Link")

  #Credits
  y3<- function(h,..) gmessage("Statistics Department, University of Salamanca",title="Credits")

  menulistaY<-list(u0=gaction("Handbook",handler=y1),u2=gaction("Credits",handler=y3))

  ##MENU SELECTION
  mb_list <-list(File=menulistaA,Method=menulistaZ,Help=menulistaY)
  gmenu(mb_list, container=g)

  ##g1
  #General
  tmp1 <- gframe("", container=g1, expand=TRUE,horizontal=FALSE)
  tg<-glabel("                   Questionnaries Validation Module                  ",container=tmp1)
  font(tg) <- list(weight="bold",size= "x-large",family="sans",align ="center",spacing = 5)

  visible(w) <- TRUE

}
