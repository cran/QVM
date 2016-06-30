#' @title Questionnaires Validation Module
#' @author Nery Sofia Huerta-Pacheco, Purificacion Vicente-Galindo
#'
#' @description This package does a multivariate analysis for questionnaire validation with Likert-type scale variables.
#'
#' You can learn more about this package at:
#' http://www.uv.mx/personal/nehuerta/qvm
#'
#' @details
#' Introduce a data set of categorical information and classifed as numerical data.
#' This funtion takes a data set and applies questionnaire validatioin techniques to return parameter estimates.
#'
#' @return QVM is an interface
#' @examples \dontrun{
#' ##Install package
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
#' @importFrom psych polychoric
#'
QVM<-function(){

  mi<- new.env()

  ##Bibliotecas
  options("guiToolkit"="tcltk")

  ##Pantalla Principal
  w<- gwindow("QVM - USAL",visible=FALSE,width = 700,height= 610)
  g<- ggroup(horizontal=FALSE, spacing=0, container = w)

  nb <- gnotebook(container=g,width = 700,height= 610)
  g1<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Presentation")
  g2<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Descriptive")
  g3<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Alpha")
  g4<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "Impact")
  g5<- ggroup(horizontal=FALSE, spacing=0, container = nb,label = "ITR  GRM")

  ##MENU DE ARCHIVO
  assign("gdata",NULL, envir =mi)
  assign("YY",NULL, envir =mi)
  requireNamespace("ltm", quietly = TRUE)

  #Abrir
  abrir<-function(h,...){
    data<-tk_choose.files()
    data1<-read.csv(data)
    assign("gdata",data1, envir =mi)
  }

  ##Ver
  ver<-function(h,...){
    gdata<-get("gdata",envir =mi)
    fix(gdata)
  }

  ##Re-inicio
  inicio<-function(h,...){
    dispose(w)
    QVM()
  }

  #Leer
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

  ##Cerrar
  cerrar<-function(h,...){
    dispose(w)
  }

  menulistaA<-list(u1=gaction("Open",handler=abrir),u2=gaction("View",handler=ver),u3=gaction("Refresh",handler=inicio),u4=gaction("Close",handler=cerrar))

  ##MENU DE TECNICAS

  #CD
  cd<-function(h,...){
    gdata<-get("gdata",envir =mi)
    CARDES<-function(gdata){

      # Estadisicas basicas
      datos<-gdata
      M<-ncol(datos)
      N<-nrow(datos)
      nc<-matrix(,1,M)
      x<-matrix()
      mg<-matrix()

      # Dimension de la matriz
      print("Matrix Dimensions",quote=FALSE)
      print("Cases",quote=FALSE)
      print(N)
      print("Variables",quote=FALSE)
      print(M)

      # Tablas de frecuencias por variable
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
        #tm<-(tmg,quote=FALSE)
        pandoc.table(tmg,plain.ascii = TRUE)
        print("-----------------------",quote=FALSE)
        mg<-matrix()
        x<-matrix()
      }

      # Tablas de frecuencias de dos variable con chi-cuadrada
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

      # Correlaciones policoricas
      print("Polychoric Correlation",quote=FALSE)
      z<-polychoric(datos)
      corz<-z$rho
      pandoc.table(corz,plain.ascii = TRUE)

    }
    ##g2
    tbl<-glayout(container=g2)
    gseparator(horizontal=TRUE, container=g2)
    outputArea <- gtext(container=g2, expand=TRUE,width = 700,height= 600)
    #size(outputArea) <- c(700, 600)
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
      #Variable<-paste("V 1 - V",dim(Datos)[2])
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

  #Impacto
  imp<-function(h,...){
    #PARAMETROS DE ENTRADA
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

      # PRESENTACIÓN DE LA INFORMACIÓN

      a<-gdata
      N<-nrow(a)
      M<-ncol(a)
      na<-colnames(a)
      #correlaciones
      z<-polychoric(a)
      corz<-z$rho
      print("----------------------------",quote=FALSE)
      print("Polychoric Correlation",quote=FALSE)
      print("----------------------------",quote=FALSE)
      pandoc.table(corz,plain.ascii = TRUE)
      #El equivalente normal de los puntos de corte
      enz<-z$tau
      print("----------------------------",quote=FALSE)
      print("Cut of point",quote=FALSE)
      print("----------------------------",quote=FALSE)
      pandoc.table(enz,plain.ascii = TRUE)

      compz<-eigen(corz)
      ecra<-matrix(compz$values,M,1)

      #Variabilidad explicada por los  componentes

      # Porcentaje de variabilidad explicada por componente
      i<-1
      mcra<-matrix(,M,1)
      while(i<=M){
        vcra<-compz$values[i]/M
        mcra[i,1]<-vcra
        i<-i+1
      }

      # Porcentaje de variabilidad acumulada explicada por componente
      suma<-0
      mcra1<-matrix(,M,1)
      for(i in 1:M){
        mcra1[i,1]<-mcra[i,1]+suma
        suma<-mcra1[i,1]
      }

      # Matriz general
      print("----------------------------",quote=FALSE)
      print("Summary Table",quote=FALSE)
      print("----------------------------",quote=FALSE)
      mgcra<-matrix(M,3)
      mgcra<-cbind(ecra,mcra,mcra1)
      mgcra1<-matrix(sprintf("%.5f",mgcra),M,3)
      colnames(mgcra1)<-c("Eigenvalues","Inertia","A.Inertia")
      rownames(mgcra1)<-c(1:M)
      #print(mgcra1,quote=FALSE)
      pandoc.table(mgcra1,plain.ascii = TRUE)

      #GRÁFICO DE EIGENVALORES
      e<-c(ecra,ecra[0])
      pa<-sprintf("%.2f",mcra1)
      dev.new()
      gre<-plot(e,bg="black",main="Component Variability",cex.main=1,xlab="Components",ylab="Eigenvalues",lty=2,lwd=1,type="l")
      points(e,bg="black",pch=20)
      text(e,labels=pa,adj=c(.7,1.4),cex=.6,col="black") # OJO CON ESTE

      # Estimación de parametros

      c<-grm(a,Hessian=TRUE,IRT.param=TRUE)
      #c<-grm(a,Hessian=TRUE,constrained=FALSE,IRT.param=TRUE,start.val = NULL,na.action= ,control = list(method="BFGS",iter.qN=20,GHk=7))
      ce<-coef(c)
      MCE<-ncol(ce)
      mz<-matrix(ce,M,MCE)
      rownames(mz)<-na
      colnames(mz)<-colnames(ce)
      print("----------------------------",quote=FALSE)
      print("Parameters",quote=FALSE)
      print("----------------------------",quote=FALSE)
      pandoc.table(mz,plain.ascii = TRUE)
      print("----------------------------",quote=FALSE)

      dev.new()
      plot(c,items=0,main=paste("General Information Function"),type="IIC",lty=1,labels=NULL,xlab="Ability",ylab="Information",col="darkblue")


      #Gráficas una por una
      for(i in 1:M){
        dev.new()
        par(mfrow=c(1,2))
        color<-c("darkblue","darkred","darkgreen","orange","darkorchid4","darkcyan","darkorange4","gray","black")
        plot(c,items=i,main=paste("Category Characteristic Curves","","Item:","",na[i]),type="ICC",lty=1,legend=T,cx="left",cex=.5,xlab="Ability",ylab="Probability",col=color)
        plot(c,items=i,main=paste("Test Information Function","","Item:","",na[i]),type="IIC",lty=1,labels=NULL,xlab="Ability",ylab="Information",col="darkblue")
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

  ##MENU DE AYUDA

  #Manual
  y1<- function(h,..) gmessage("http://www.uv.mx/personal/nehuerta/qvm/",title="Link")

  #Creditos
  y3<- function(h,..) gmessage("Statistics Department, University of Salamanca",title="Credits")

  menulistaY<-list(u0=gaction("Handbook",handler=y1),u2=gaction("Credits",handler=y3))

  ##MENU PRINCIPAL DE SELECCION
  mb_list <-list(File=menulistaA,Method=menulistaZ,Help=menulistaY)
  gmenu(mb_list, container=g)

  ##g1
  #Informacion
  tmp1 <- gframe("", container=g1, expand=TRUE,horizontal=FALSE)
  tg<-glabel("                   Questionnaries Validation Module                  ",container=tmp1)
  font(tg) <- list(weight="bold",size= "x-large",family="sans",align ="center",spacing = 5)

  visible(w) <- TRUE

}
