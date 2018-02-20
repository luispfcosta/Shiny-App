library(shiny)
library(shinyjs)
library(data.table)
library(shinythemes)
library(forecast)
library(htmltools)
library(bsplus)
library(DT)
library(ggplot2)
library(rsconnect)
library(shinyTime)

options(shiny.maxRequestSize = 170*1024^2,encoding = "utf8")


ui <- fluidPage(theme = shinytheme("flatly"),
  
  fluidPage(
    br(),
    fluidRow(column(6,
                    img(height = 160, 
                        width = 370, 
                        src = "hospitalb.png"
                    )),
             br(),
             br(),
             h1("Web-App de Previsão")
             
    ),
    hr()),
  
  fluidPage(
    navbarPage(title="Powered by Luís Costa",
               
               tabPanel("Importação Base de Dados",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput(inputId = "dados1","Insira Base de Dados Transformada"),
                            helpText("Tamanho máximo 9MB"),
                            hr(),
                            fileInput(inputId = "dados2","Insira Base de Dados Original"),
                            helpText("Tamanho máximo 9MB"),
                            useShinyjs(),
                            radioButtons("remove", "Remover Base de Dados Original:",
                                         choiceNames = list(
                                           "Não",
                                           "Sim"
                                         ),
                                         choiceValues = list(
                                           "Não", "Sim"
                                         ),inline = T),
                            hr(),
                            textInput('vec1', 'Dias de feriado da Base de Dados Original', ""),
                            helpText("(separados por vírgula) Ex: 2016-01-01,2016-12-25,..."),
                            verbatimTextOutput("oid1")
                          ),
                          
                          
                          mainPanel(uiOutput("tb"),
                                    tabsetPanel(
                                      tabPanel("Visualização Dados",
                                               br(),
                                               verbatimTextOutput("visualiza"),
                                               column(8, DT::dataTableOutput('x1')),
                                               downloadButton("downloadData", "Download")
                                               
                                      ),
                                      tabPanel("Gráfico", plotOutput("grafimport"),downloadButton("down", "Download Plot")),
                                      tabPanel("Resumo Importação", verbatimTextOutput("summary"))
                                      
                                    )
                          )
                        )),
               navbarMenu("Previsão",
                          tabPanel("Previsão Mensal",
                                   fluidPage(
                                     column(width = 12,
                                            HTML(
                                              "<div class='alert alert-info'>",
                                              "<strong>ATENÇÂO!</strong> Para poder fazer previsão é necessário fazer primeiro a importação das base de dados. ",
                                              "</div>"
                                            )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(
                                         dateInput("date1", "Período de Previsão:", value = "2016-01-01"),
                                         
                                         dateInput("date2","até:", value = "2016-12-31"),
                                         hr(),
                                         textInput('vec2', 'Dias de feriado no período de previsão:', ""),
                                         helpText("(separados por vírgula) Ex: 2016-01-01,2016-02-09,..."),
                                         verbatimTextOutput("ferimensal"),
                                         tags$style(HTML('#botaomes{background-color:orange}')),
                                         actionButton("botaomes", "Prever")
                                       ),
                                       mainPanel(
                                         tabsetPanel(
                                           
                                           tabPanel("Tabela Previsão", column(8,DT::dataTableOutput("tableprevmensal"))),
                                           tabPanel("Gráfico Previsão",plotOutput("gprevmensal"))

                                         )
                                       )
                                     )
                                   )
                          ),
                          tabPanel("Previsão Diária",
                                   fluidPage(
                                     column(width = 12,
                                            HTML(
                                              "<div class='alert alert-info'>",
                                              "<strong>ATENÇÂO!</strong> Para poder fazer previsão é necessário fazer primeiro a importação das base de dados. ",
                                              "</div>"
                                            )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(
                                         dateInput("datedia1", "Dia de Previsão:", value = "2016-01-01"),
                                         hr(),
                                         radioButtons("rb", "Dia de previsão é feriado?",
                                                      choiceNames = list(
                                                        "Não",
                                                        "Sim"
                                                      ),
                                                      choiceValues = list(
                                                        "Não", "Sim"
                                                      )),
                                         hr(),
                                         column(width = 3,numericInput("ex1",value = "",label = "t-7"),textOutput("diaante1")),
                                         column(width = 3,numericInput("ex2",value = "",label = "t-6"),textOutput("diaante2")),
                                         column(width = 3,numericInput("ex3",value = "",label = "t-5"),textOutput("diaante3")),
                                         column(width = 3,numericInput("ex4",value = "",label = "t-4"),textOutput("diaante4")),
                                         br(),
                                         column(width = 3,numericInput("ex5",value = "",label = "t-3"),textOutput("diaante5")),
                                         column(width = 3,numericInput("ex6",value = "",label = "t-2"),textOutput("diaante6")),
                                         column(width = 3,numericInput("ex7",value = "",label = "t-1"),textOutput("diaante7"),br()),
                                         
                                         hr(),
                                         #textInput('vec3', 'Nº Admissões nos 7 dias anteriors ao dia de previsão', ""),
                                         #helpText("(separados por vírgula) Ex: 200,339,429,..."),
                                         #verbatimTextOutput("feridiaria"),
                                         hr(),
                                         textInput('vec4', 'Dias de feriado nos 7 dias anteriores ao dia de previsão: ', ""),
                                         helpText("(separados por vírgula) Ex: 2016-01-01,2016-02-09,..."),
                                         verbatimTextOutput("feridiaria2"),
                                         tags$style(HTML('#prevbotao{background-color:orange}')),
                                         actionButton("prevbotao", "Prever")
                                       ),
                                       mainPanel(verbatimTextOutput("coisa"),
                                                 tabsetPanel(
                                                   tabPanel("Previsão Dia", br(),column(7,DT::dataTableOutput("tableprevdiaria")))
                                                   
                                                 )
                                       )
                                     )
                                   )
                          )),
               tabPanel("Estatísticas",
                        
                        tabsetPanel(
                          tabPanel("Visualização de Dados",
                                   br(),
                                   fileInput("ficheiro","Insira o ficheiro:"),
                                   DT::dataTableOutput('tabelaficheiro')
                                   ),
                          tabPanel("Gráficos Summary",
                                   br(),
                                   sidebarLayout(      
                                     
                                     # Define the sidebar with one input
                                     sidebarPanel(
                                       radioButtons("variaveisaver", "Escolha uma variável:",
                                                    choiceNames = list(
                                                      "Sexo",
                                                      "Idade",
                                                      "Destino",
                                                      "Cor de Triagem"
                                                    ),
                                                    choiceValues = list(
                                                      "Sexo",
                                                      "Idade",
                                                      "Destino",
                                                      "Cor de Triagem"
                                                    ))
                                       
                                     ),
                                     
                                     # Create a spot for th  e barplot
                                     mainPanel(
                                       plotOutput("urgencia16")
                                       
                                     )
                                     
                                   )
                                   
                          )
                          
                        )
               ),
               tabPanel("Sobre",
                        fluidPage(
                          
                          fluidPage(
                            column(6, "Esta aplicação foi desenvolvida em conjunto com o Hospital de Braga de forma a prever o número
                                   diário de pacientes que recorrem ao serviço de urgência deste hospital. No entanto para além disso e possível 
                                   obtermos outras informações sobre o que se passa em tempo real nesse serviço."
                                   ,
                                   column(4, "")
                            )),
                          br(),
                          br(),
                          column(7,""),
                          column(8,img(height = 270,
                                       width = 260, 
                                       src = "luis.jpg")
                          )
                          
                        ),
                        br(),
                        "O meu nome é Luís Paulo Ferrira da Costa, comecei a estudar Matemática em 2012 e acabado a licenciatura em 2015. Tendo posteriormente, em 2015 dado início ao Mestrado em Estatistica."
               )
               
    )),
  fluidPage(
    hr(),
    p("Copyright 2017 Luís Costa & Inês Sousa")
  )
  
    )

modelo<-function(df){
  lambda<-BoxCox.lambda(df$n.admiss)
  n.admiss.trans<-BoxCox(df$n.admiss,lambda)
  mm<-lm(n.admiss.trans~t+dia.semana+mes+feriados,data=df)
  
  ts<-arima(residuals(mm),order=c(7,0,0))
  coef_fixo<-coefficients(mm)
  
  return.modelo=NULL
  return.modelo$ts=ts
  return.modelo$fixo=coef_fixo
  return.modelo$lambda=lambda
  
  return(return.modelo)
}

matriz.desenho.diaria <-function(de,ate=de,feriados.pred=NA){
  
  data.pred<-seq(as.Date(de),as.Date(ate),by="days")
  
  col1<-rep(1,length(data.pred))
  
  # alterar depois 15339 para qualquer coisa a depender de 1 jan 2012
  
  t.pred<-(as.numeric(data.pred)-15339)[1]:tail(as.numeric(data.pred)-15339,n=1)
  
  terca.pred<-ifelse(as.factor(weekdays(data.pred))=="terça-feira",1,0)
  quarta.pred<-ifelse(as.factor(weekdays(data.pred))=="quarta-feira",1,0)
  quinta.pred<-ifelse(as.factor(weekdays(data.pred))=="quinta-feira",1,0)
  sexta.pred<-ifelse(as.factor(weekdays(data.pred))=="sexta-feira",1,0)
  sabado.pred<-ifelse(as.factor(weekdays(data.pred))=="sábado",1,0)
  domingo.pred<-ifelse(as.factor(weekdays(data.pred))=="domingo",1,0)
  
  fevereiro.pred<-ifelse(as.factor(months(data.pred))=="fevereiro",1,0)
  marco.pred<-ifelse(as.factor(months(data.pred))=="março",1,0)
  abril.pred<-ifelse(as.factor(months(data.pred))=="abril",1,0)
  maio.pred<-ifelse(as.factor(months(data.pred))=="maio",1,0)
  junho.pred<-ifelse(as.factor(months(data.pred))=="junho",1,0)
  julho.pred<-ifelse(as.factor(months(data.pred))=="julho",1,0)
  agosto.pred<-ifelse(as.factor(months(data.pred))=="agosto",1,0)
  setembro.pred<-ifelse(as.factor(months(data.pred))=="setembro",1,0)
  outubro.pred<-ifelse(as.factor(months(data.pred))=="outubro",1,0)
  novembro.pred<-ifelse(as.factor(months(data.pred))=="novembro",1,0)
  dezembro.pred<-ifelse(as.factor(months(data.pred))=="dezembro",1,0)
  
  feriados.pred<-as.Date(feriados.pred)
  ff.pred<-ifelse(data.pred%in%feriados.pred,1,0)
  
  df.pred<-data.frame(col1,t.pred,domingo.pred,quarta.pred,quinta.pred,sabado.pred,sexta.pred,terca.pred,
                      abril.pred,agosto.pred,dezembro.pred,fevereiro.pred,julho.pred,junho.pred,maio.pred,marco.pred,novembro.pred,
                      outubro.pred,setembro.pred,ff.pred)
  
  returnData=NULL
  returnData$df.pred=df.pred
  returnData$data.pred=data.pred
  
  return(returnData)
}


server <- function(input, output,session){
  
  
  #######################################################################################################################################################
  ############## Resto do código da Função Server     ################################################################################################### 
  
  
  output$oid1 <- renderPrint({
    if(input$vec1==""){
      tabela="Não existem feriados"
      
    }
    else{
      tabela=as.data.frame(noquote(strsplit(input$vec1, ",")[[1]]))
      colnames(tabela)<-"Datas Introduzidas:"
    }
    
    return(tabela)
  }
  )
  observeEvent(input$remove, {
    if(input$remove=="Sim"){
      disable("vec1")
      
    }
    else{
      enable("vec1")
      return(data2())
      
    }
  })
  
  data2 <- reactive({
    file1<-input$dados1
    file2<-input$dados2
    if(is.null(file2) && is.null(file1)){
      return()
    }
    else if(is.null(file2) || input$remove=="Sim"){
      urg.geral.1 <- read.csv2(file=file1$datapath,header=T,col.names=c("Data","n.admiss","t","dia.semana","mes","feriados"),fileEncoding = "latin1")
      urg.geral.1$Data<-as.Date(urg.geral.1$Data)
      urg.geral.1$dia.semana<-relevel(urg.geral.1$dia.semana,ref="segunda-feira")
      urg.geral.1$mes<-relevel(urg.geral.1$mes,ref="janeiro")
      urg.geral.1$feriados<-relevel(urg.geral.1$feriados,ref="nao feriado")
      
      
      return(urg.geral.1)
    }
    else{
      urg.geral.1 <- read.csv2(file=file1$datapath,header=T,col.names=c("Data","n.admiss","t","dia.semana","mes","feriados"),fileEncoding = "latin1")
      urg.geral.1$Data<-as.Date(urg.geral.1$Data)
      urg.geral.1$dia.semana<-relevel(urg.geral.1$dia.semana,ref="segunda-feira")
      urg.geral.1$mes<-relevel(urg.geral.1$mes,ref="janeiro")
      urg.geral.1$feriados<-relevel(urg.geral.1$feriados,ref="nao feriado")
      
      
      urg.geral.2 <- read.csv2(file=file2$datapath,header=T,fileEncoding = "latin1")
      n.admiss.2<-as.vector(unlist(by(urg.geral.2,urg.geral.2$Data.Admissão,function(x){dim(x)[1]})))
      
      Data.2<-as.Date(unique(urg.geral.2$Data.Admissão))
      
      t.2<-as.numeric(Data.2)-15339
      
      dia.semana.2<-as.factor(weekdays(Data.2))
      dia.semana.2<-relevel(dia.semana.2,ref="segunda-feira")
      
      mes.2<-as.factor(months(Data.2))
      
      feriados.atual<-unlist(strsplit(input$vec1,","))
      feriados.atual<-as.Date(feriados.atual)
      feriados.2<-as.factor(ifelse(Data.2%in%feriados.atual,"feriado","nao feriado"))
      feriados.2<-relevel(feriados.2,ref="nao feriado")
      
      urg.geral.2<-data.frame(Data=Data.2,n.admiss=n.admiss.2,t=t.2,
                              dia.semana=dia.semana.2,mes=mes.2,feriados=feriados.2)
      
      urg.geral<-rbind(urg.geral.1,urg.geral.2)
      urg.geral<-data.frame(Data=urg.geral$Data,n.admiss=urg.geral$n.admiss,t=round(urg.geral$t,digits = 0),
                            dia.semana=urg.geral$dia.semana,mes=urg.geral$mes,feriados=urg.geral$feriados)
      return(urg.geral)
    }
    
  })
  
  
  
  output$x1 = DT::renderDataTable(data2(), server = TRUE, selection = "none",rownames= FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')
    
  ))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("coisa_update", "csv", sep=".")
    },
    content = function(file) {
      write.csv2(data2(), file,sep = ",",row.names = F,fileEncoding = "latin1")
    }
  )
  
  output$summary <- renderPrint({
    if(is.null(input$dados1) && is.null(input$dados2)){
      paste("Ainda não inseriu uma base de dados!!")
    }
    else{
      summary(data2())
    }
    
  })
  
  
  
  output$ferimensal <- renderPrint({
    if(input$vec2==""){
      tabela2="Não existem feriados"
      
    }
    else{
      tabela2=as.data.frame(noquote(strsplit(input$vec2, ",")[[1]]))
      colnames(tabela2)<-"Datas Introduzidas:"
    }
    return(tabela2)
  }
  )
  
  output$feridiaria <- renderPrint({
    if(input$vec3==""){
      paste("Não inseriu valores dos 7 dias anteriores ao dia de previsão!!")
      
    }
    else if(length(strsplit(input$vec3, ",")[[1]])==7){
      data.7dias = input$datedia1 - 7:1
      data.7dias=as.data.frame(data.7dias)
      tabela2=as.data.frame(noquote(strsplit(input$vec3, ",")[[1]]))
      tabeladias=cbind(data.7dias,tabela2)
      colnames(tabeladias)<-c("Data","Nº Admissões")
      
      return(tabeladias)
    }
    else{
      paste("Não inseriu valores dos 7 dias anteriores ao dia de previsão!!")
      
    }
    
  }
  )
  
  matriz.desenho <- function(){
    a=input$date1
    b=input$date2
    data.pred<-seq(as.Date(a),as.Date(b),by="days")
    data.pred2=as.character(data.pred)
    
    col1<-rep(1,length(data.pred))
    t.pred<-(as.numeric(data.pred)-15339)[1]:tail(as.numeric(data.pred)-15339,n=1)
    
    terca.pred<-ifelse(as.factor(weekdays(data.pred))=="terça-feira",1,0)
    quarta.pred<-ifelse(as.factor(weekdays(data.pred))=="quarta-feira",1,0)
    quinta.pred<-ifelse(as.factor(weekdays(data.pred))=="quinta-feira",1,0)
    sexta.pred<-ifelse(as.factor(weekdays(data.pred))=="sexta-feira",1,0)
    sabado.pred<-ifelse(as.factor(weekdays(data.pred))=="sábado",1,0)
    domingo.pred<-ifelse(as.factor(weekdays(data.pred))=="domingo",1,0)
    
    fevereiro.pred<-ifelse(as.factor(months(data.pred))=="fevereiro",1,0)
    marco.pred<-ifelse(as.factor(months(data.pred))=="março",1,0)
    abril.pred<-ifelse(as.factor(months(data.pred))=="abril",1,0)
    maio.pred<-ifelse(as.factor(months(data.pred))=="maio",1,0)
    junho.pred<-ifelse(as.factor(months(data.pred))=="junho",1,0)
    julho.pred<-ifelse(as.factor(months(data.pred))=="julho",1,0)
    agosto.pred<-ifelse(as.factor(months(data.pred))=="agosto",1,0)
    setembro.pred<-ifelse(as.factor(months(data.pred))=="setembro",1,0)
    outubro.pred<-ifelse(as.factor(months(data.pred))=="outubro",1,0)
    novembro.pred<-ifelse(as.factor(months(data.pred))=="novembro",1,0)
    dezembro.pred<-ifelse(as.factor(months(data.pred))=="dezembro",1,0)
    
    feriados.pred=noquote(strsplit(input$vec2, ",")[[1]])
    
    ff.pred<-ifelse(data.pred2%in%feriados.pred,1,0)
    
    df.pred<-data.frame(col1,t.pred,domingo.pred,quarta.pred,quinta.pred,sabado.pred,sexta.pred,terca.pred,
                        abril.pred,agosto.pred,dezembro.pred,fevereiro.pred,julho.pred,junho.pred,maio.pred,marco.pred,novembro.pred,
                        outubro.pred,setembro.pred,ff.pred)
    
    returnData=NULL
    returnData$df.pred=df.pred
    returnData$data.pred=data.pred2
    
    return(returnData)
  }
  
  
  pred.mensal<-function(){
    
    modelo.df=modelo(data2())
    
    m.des=matriz.desenho() 
    
    n.trans.pred<-as.matrix(m.des$df.pred)%*%as.matrix(modelo.df$fixo)+predict(modelo.df$ts,n.ahead=length(m.des$data.pred),start=tail(length(df),n=1)+1)$pred
    
    se.pred<-predict(modelo.df$ts,n.ahead=length(m.des$data.pred),start=tail(length(m.des$data.pred),n=1)+1)$se
    
    low.pred<-n.trans.pred-1.96*se.pred
    upper.pred<-n.trans.pred+1.96*se.pred
    
    Limite_Inferior<-InvBoxCox(low.pred,modelo.df$lambda)
    Limite_Superior<-InvBoxCox(upper.pred,modelo.df$lambda)
    Valor_Previsto<-InvBoxCox(n.trans.pred,modelo.df$lambda)
    
    df.final<-data.frame(m.des$data.pred,round(Limite_Inferior,2),round(Valor_Previsto,2),round(Limite_Superior,2))
    colnames(df.final)<-c("Data","Limite_Inferior","Valor_Previsto","Limite_Superior")
    
    returnMensal=NULL
    returnMensal$df.final=df.final
    returnMensal$Valor_Previsto=Valor_Previsto
    returnMensal$Data=m.des$data.pred
    returnMensal$Limite_Inferior=Limite_Inferior
    returnMensal$Limite_Superior=Limite_Superior
    
    return(returnMensal)
  }
  
  output$gprevmensal <- renderPlot({
    previsao=pred.mensal()
    valorprevisao=previsao$Valor_Previsto
    dataprevisao=previsao$Data
    limitesuperior=previsao$Limite_Superior
    limiteinferior=previsao$Limite_Inferior

    
      plot(1:length(dataprevisao),y=valorprevisao,type="l",xaxt="n",
           main="Gráfico Previsão Mensal",
           xlab="Dia do Mês",ylab="Numero Pacientes",ylim = c(min(limiteinferior)-50,max(limitesuperior)+50))
      lines(1:length(dataprevisao),limiteinferior,col="red")
      lines(1:length(dataprevisao),limitesuperior,col="red")
      #axis(1,at=c(1:length(dataprevisao)),labels = c(1:length(dataprevisao)), las=2,col="blue")
      #legend(x=33,y=400,legend = c("Prev Mensal"),col=c("black"),pch = 19,bty = "n")
    
  })
  
  ###############################################################################################################################
  #################################### Previsão Diária   ########################################################################
  
  pred.diaria <-function(){
    
    dia = input$datedia1
    
    if(input$rb=="Não"){feriado.dia=NA}else{feriado.dia =input$datedia1}
    
    data.pred.7dias = dia - 7:1
    dia.pred.7dias = as.factor(weekdays(data.pred.7dias))
    
    modelo.df = modelo(data2())
    
    ll = modelo.df$lambda
    
    m.des.diaria = matriz.desenho.diaria(de=dia,feriados.pred=feriado.dia)
    
    feriado.7dias=strsplit(input$vec4, ",")[[1]]
    
    m.des.7dias = matriz.desenho.diaria(de=data.pred.7dias[1] , ate=data.pred.7dias[7], feriados.pred = feriado.7dias)
    
    mu.7dias = as.matrix(m.des.7dias$df.pred)%*%as.matrix(modelo.df$fixo)
    
    dias.anteriores.7=c(input$ex1,input$ex2,input$ex3,input$ex4,input$ex5,input$ex6,input$ex7)
    #reais.7dias=as.numeric(strsplit(input$vec3, ",")[[1]])
    
    r.7dias = BoxCox(dias.anteriores.7,ll) - mu.7dias
    
    n.trans<-as.matrix(m.des.diaria$df.pred)%*%as.matrix(modelo.df$fixo)+
      coef(modelo.df$ts)[1]*r.7dias[7] + 
      coef(modelo.df$ts)[2]*r.7dias[6] +
      coef(modelo.df$ts)[3]*r.7dias[5] +
      coef(modelo.df$ts)[4]*r.7dias[4] +
      coef(modelo.df$ts)[5]*r.7dias[3] + 
      coef(modelo.df$ts)[6]*r.7dias[2] + 
      coef(modelo.df$ts)[7]*r.7dias[1]
    #se.pred<-predict(arg.modelo$ts,n.ahead=length(m.desenho$data.pred),start=tail(length(m.desenho$data.pred),n=1)+1)$se
    
    #low.pred<-n.trans.pred-1.96*se.pred
    #upper.pred<-n.trans.pred+1.96*se.pred
    
    #Limite_Inferior<-InvBoxCox(low.pred,arg.modelo$lambda)
    #Limite_Superior<-InvBoxCox(upper.pred,arg.modelo$lambda)
    
    Valor_Previsto<-InvBoxCox(n.trans,ll)
    
    
    df.final<-data.frame(dia,round(Valor_Previsto,2))
    colnames(df.final)<-c("Data","Valor_Previsto")
    
    #df.final<-data.frame(data.pred,round(Limite_Inferior,2),round(Valor_Previsto,2),round(Limite_Superior,2))
    #colnames(df.final)<-c("Data","Limite_Inferior","Valor_Previsto","Limite_Superior")
    
    returnDiaria=NULL
    returnDiaria$df.final=df.final
    returnDiaria$Valor_Previsto=Valor_Previsto
    returnDiaria$m.des.diaria=m.des.diaria
    returnDiaria$m.des.7dias=m.des.7dias
    returnDiaria$data.pred.7dias=data.pred.7dias
    
    return(returnDiaria)
  }
  
  diasanteriores <- reactive({
    dia = input$datedia1
    data.pred.7dias = dia - 7:1
    
    return(data.pred.7dias)
  })
  
  output$diaante1 <- renderText({
    a=diasanteriores()
    b=as.character(a[1]) 
    
    return(b)
  })
  
  output$diaante2 <- renderText({
    a=diasanteriores()
    b=as.character(a[2])
    
    return(b)
  })
  
  output$diaante3 <- renderText({
    a=diasanteriores()
    b=as.character(a[3])
    
    return(b)
  })
  
  output$diaante4 <- renderText({
    a=diasanteriores()
    b=as.character(a[4])
    
    return(b)
  })
  
  output$diaante5 <- renderText({
    a=diasanteriores()
    b=as.character(a[5])
    
    return(b)
  })
  
  output$diaante6 <- renderText({
    a=diasanteriores()
    b=as.character(a[6])
    
    return(b)
  })
  
  output$diaante7 <- renderText({
    a=diasanteriores()
    b=as.character(a[7])
    
    return(b)
  })
  
  
  
  output$feridiaria2 <- renderPrint({
    if(input$vec4==""){
      tabela3="Não existem feriados"
      
    }
    else{
      tabela3=as.data.frame(noquote(strsplit(input$vec4, ",")[[1]]))
      colnames(tabela3)<-"Datas Introduzidas:"
    }
    return(tabela3)
  }
  )
  
  previsao.delay <- eventReactive(input$prevbotao, {
    a=pred.diaria()
    
    return(a$df.final)
  })
  
  output$tableprevdiaria = DT::renderDataTable(previsao.delay(), server = FALSE, selection = "none",rownames= FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')
    
  ))
  
  
  previsao.delay2 <- eventReactive(input$botaomes, {
    a=pred.mensal()
    
    return(a$df.final)
  })
  
  output$tableprevmensal = DT::renderDataTable(previsao.delay2(), server = FALSE, selection = "none",rownames= FALSE,options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')
    
  ))
  
  
  
  data <- reactive({
    file1<-input$dados1
    file2<-input$dados2
    if(is.null(file2) && is.null(file1)){
      return()
    }
    else if(is.null(file2)){
      urg.geral.1 <- read.csv2(file=file1$datapath,header=T,col.names=c("Data","n.admiss","t","dia.semana","mes","feriados"),fileEncoding = "latin1")
      urg.geral.1$Data<-as.Date(urg.geral.1$Data)
      urg.geral.1$dia.semana<-relevel(urg.geral.1$dia.semana,ref="segunda-feira")
      urg.geral.1$mes<-relevel(urg.geral.1$mes,ref="janeiro")
      urg.geral.1$feriados<-relevel(urg.geral.1$feriados,ref="nao feriado")
      return(urg.geral.1)
    }
    else{
      urg.geral.1 <- read.csv2(file=file1$datapath,header=T,col.names=c("Data","n.admiss","t","dia.semana","mes","feriados"),fileEncoding = "latin1")
      urg.geral.1$Data<-as.Date(urg.geral.1$Data)
      urg.geral.1$dia.semana<-relevel(urg.geral.1$dia.semana,ref="segunda-feira")
      urg.geral.1$mes<-relevel(urg.geral.1$mes,ref="janeiro")
      urg.geral.1$feriados<-relevel(urg.geral.1$feriados,ref="nao feriado")
      
      
      urg.geral.2 <- read.csv2(file=file2$datapath,header=T,fileEncoding = "latin1")
      n.admiss.2<-as.vector(unlist(by(urg.geral.2,urg.geral.2$Data.Admissão,function(x){dim(x)[1]})))
      
      Data.2<-as.Date(unique(urg.geral.2$Data.Admissão))
      
      t.2<-as.numeric(Data.2)-15339
      
      dia.semana.2<-as.factor(weekdays(Data.2))
      dia.semana.2<-relevel(dia.semana.2,ref="segunda-feira")
      
      mes.2<-as.factor(months(Data.2))
      
      feriados.atual<-unlist(strsplit(input$vec1,","))
      feriados.atual<-as.Date(feriados.atual)
      feriados.2<-as.factor(ifelse(Data.2%in%feriados.atual,"feriado","nao feriado"))
      feriados.2<-relevel(feriados.2,ref="nao feriado")
      
      urg.geral.2<-data.frame(Data=Data.2,n.admiss=n.admiss.2,t=t.2,
                              dia.semana=dia.semana.2,mes=mes.2,feriados=feriados.2)
      
      urg.geral<-rbind(urg.geral.1,urg.geral.2)
      urg.geral<-data.frame(Data=urg.geral$Data,n.admiss=urg.geral$n.admiss,t=round(urg.geral$t,digits = 0),
                            dia.semana=urg.geral$dia.semana,mes=urg.geral$mes,feriados=urg.geral$feriados)
      
      returnDados=NULL
      returnDados$n.admiss=urg.geral$n.admiss
      returnDados$Data=urg.geral$Data
      
      return(urg.geral)
    }
  })
  
  output$grafimport <- renderPlot({
    if(is.null(input$dados1) && is.null(input$dados2)){
      return()
    }
    else{
      dados=data2()
      
      n.admiss=dados$n.admiss
      Data=dados$Data
      
      p1 <- ggplot(dados, aes(x = dados$Data, y = dados$n.admiss))
      p2=p1 + geom_smooth()  + geom_line() + labs(x = "Data",y="Nº de Admissões") + labs(caption = "(Dados 01-01-2012 até data inserida na importação)") 
      
      return(p2) 
    }
  })
  
  output$down <- downloadHandler(
    filename =  function() {
      paste("gráfico_importação", "pdf", sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      pdf(file,width = 14) # open the pdf device
      dados=data2()
      print(ggplot(dados, aes(x = dados$Data, y = dados$n.admiss)) + geom_smooth()  + geom_line(aes(color = dados$n.admiss)))
      
      dev.off()  # turn the device off
      
    } 
  )
  
  output$visualiza <- renderPrint({
    if(is.null(input$dados1) && is.null(input$dados2)){
      paste("Ainda não inseriu uma base de dados!!")
    }
  })
  
  output$urgencia16 <- renderPlot({
    estatisticas()
  })
  
  #################################################################################################################################################
  ################### Separador Estatisticas   ####################################################################################################
  
  estatisticas <- reactive({
    urg.geral=ficheiroexcell()
    if(input$variaveisaver=="Sexo"){
      sex=barplot(summary(urg.geral$Sexo))su
      return(sex)
    }
    else if(input$variaveisaver=="Idade"){
      age=hist(urg.geral$Idade)
      return(age)
    }
    else if(input$variaveisaver=="Destino"){
      destiny=barplot(summary(urg.geral$Destino))
      return(destiny)
    }
    else if(input$variaveisaver=="Cor de Triagem"){
      triag=barplot(summary(urg.geral$Cor.Triagem))
      return(triag)
    }
    else if(is.null(urg.geral)){
      return()
    }
  })

  
  ficheiroexcell <- function(){
    file3=input$ficheiro
    if(is.null(file3)){
      return()
    }
    else{
    urgenciafile <- fread(file=file3$datapath,header=T)
    urgenciafile$Data.Admissão<-as.Date(urgenciafile$Data.Admissão)
    urgenciafile$Sexo=as.factor(urgenciafile$Sexo)
    urgenciafile$Cor.Triagem=as.factor(urgenciafile$Cor.Triagem)
    urgenciafile$Destino2=as.factor(urgenciafile$Destino2)
    urgenciafile=urgenciafile[,-c(1,2,8,9,10,12,13,14,15,16,17)]
    }
    return(urgenciafile)
  }
  
  output$tabelaficheiro <- DT::renderDataTable({
    DT::datatable(ficheiroexcell(),rownames = F,filter = "top",selection = "none",width = 700,options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json')
      
    ))
  })

  
}

shinyApp(ui = ui, server = server)
