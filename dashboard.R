library(shiny)
require(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)

r=read.csv("W:/Study Material/DV/Project/Primary_Census_Abstract_Total_Table_For_India.csv")
pop_state=subset(r,Level=="STATE"&TRU=="Total",select=c(TOT_P,Name))
sex_ratio=subset(r,Level=="STATE"&TRU=="Total",select=c(Name,TOT_M,TOT_F))
tru_work=subset(r,Level=="India",select=c(TRU,TOT_WORK_M,TOT_WORK_F))
main_work_share= subset(r,Level=="India"&TRU=="Total",select=c(MAIN_CL_P,MAIN_AL_P,MAIN_HH_P,MAIN_OT_P))
work_share_plot_data=c(main_work_share$MAIN_CL_P,main_work_share$MAIN_AL_P,main_work_share$MAIN_HH_P,main_work_share$MAIN_OT_P)
piepercent=round(100*work_share_plot_data/sum(work_share_plot_data),1)
nonwork=subset(r,Level=="India",select=c(TRU,NON_WORK_P,NON_WORK_M,NON_WORK_F))
tru = subset(r,Level=="India",select=c(TOT_P,TRU))
r11 <-  c(833748852,377106125)
labelsr11 <-  c("Rural", "Urban")
piepercent<- round(100 * r11 / 1210854977, 1)
literate_state=subset(r,Level=="STATE"&TRU=="Total",select=c(Name,P_LIT))
HH_RU=subset(r,Level=="STATE"&(TRU=="Rural"|TRU=="Urban"),select=c(Name,TRU,No_HH))
marg_work_share=subset(r,Level=="India"&TRU=="Total",select=c(MARG_CL_P,MARG_AL_P,MARG_HH_P,MARG_OT_P))
marg_work_share2=data.frame(t(marg_work_share))
f_lit=subset(r,Level=="STATE"&TRU=="Total",select=c(Name,F_LIT))
nonwork_RU=subset(r,Level=="STATE"&(TRU=="Rural"|TRU=="Urban"),select=c(Name,TRU,NON_WORK_P))



header <- dashboardHeader(title = "2011 Indian Census Analytics")  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Non-interactive Plots", tabName = "review_1", icon = icon("signal",lib='glyphicon')),
    menuItem("Interactive Plots", tabName = "review_2", icon = icon("stats",lib='glyphicon')),
    menuItem("About", tabName = "about", icon = icon("info-sign",lib='glyphicon'))
  )
)



frow1 <- fluidRow(
  
  box(
    title = "Population according to states"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("pop_state", height = "400px")
  )
  
  ,box(
    title = "Male and female count"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("sex_ratio", height = "400px")
  )
  
)

frow2 <- fluidRow(
  
  box(
    title = "Total male working population"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("tru_work", height = "400px")
  )
  
  ,box(
    title = "Total female working population"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("tru_work_1", height = "400px")
  )
)

frow3 <- fluidRow(
  
  box(
    title = "Main workers population share"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("main_work_share", height = "400px")
  )
  
  ,box(
    title = "Unemployed population in different types of areas"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("nonwork", height = "400px")
  )
)

frow4 <- fluidRow(
  
  box(
    title = "Rural and urban population ratio"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("tru", height = "400px")
  )
  
  ,box(
    title = "Literate population in different states"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("literate_state", height = "400px")
  )
)

frow5 <- fluidRow(
  
  box(
    title = "Households in states at different levels"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("HH_RU", height = "550px")
  )
  
  ,box(
    title = "Literate female population in different states"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("f_lit", height = "550px")
  )
)

frow6 <- fluidRow(
  
  box(
    title = "Non working population according to region in different states"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("nonwork_RU", height = "500px")
  )
  
  ,box(
    title = "Population according to states"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("pop_state_int", height = "500px")
  )
)

frow7 <- fluidRow(
  
  box(
    title = "Marginal workers population share"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("marg_work_share", height = "400px", width="950px")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(
      "review_1",frow1,frow2,frow3,frow4
    ),
    tabItem(
      "review_2",frow5,frow6,frow7
    ),
    tabItem(
      "about",
      fluidPage(
        h1("This project has been made by:-"),
        h3("Arush Saxena"),
        br(),
        h1("Acknowledgement:-"),
        p("I am very thankful to my guiding professor, Lydia Jane G. for providing me with the opportunity to work on this project through which I learnt something very functional."),
        p("I got the opportunity to explore many new aspects of the concerned subject throughout the duration of this project.")
      )
    )
  )
  
)

ui <- dashboardPage(title = 'CSE3020 Project Review 3', header, sidebar, body, skin='purple')

server <- function(input, output) { 
  
  
  output$pop_state <- renderPlot({
    ggplot(pop_state,aes(x=TOT_P,y=Name))+geom_bar(stat="identity",colour="blue")+xlab("Total Population")+ylab("State")
  })
  
  output$sex_ratio <- renderPlot({
    ggplot(sex_ratio)+geom_point(aes(TOT_M,Name,col='Male'))+geom_point(aes(TOT_F,Name,col='Female'))+xlab('Population')+ylab('State')
  })
  
  output$tru_work <- renderPlot({
    ggplot(tru_work, aes(TRU,TOT_WORK_M, fill=TRU))+geom_bar(stat="identity")+xlab("Area")+ylab("Population")
  })
  
  output$tru_work_1 <- renderPlot({
    ggplot(tru_work, aes(TRU,TOT_WORK_F, fill=TRU))+geom_bar(stat="identity")+xlab("Area")+ylab("Population")
  })
  
  output$main_work_share <- renderPlot({
    pie(work_share_plot_data,labels=piepercent,col=rainbow(length(work_share_plot_data)))
    legend("topright",c("MAIN_CL_P","MAIN_AL_P","MAIN_HH_P","MAIN_OT_P"),cex=1.5,fill=rainbow(length(work_share_plot_data)))
  })
  
  output$nonwork <- renderPlot({
    ggplot(nonwork)+geom_point(size=5,aes(TRU,NON_WORK_P,col="Total"))+geom_point(size=5,aes(TRU,NON_WORK_M,col="Male"))+geom_point(size=5,aes(TRU,NON_WORK_F,col="Female"))+xlab("Area")+ylab("Non Working Population")
  })
  
  output$tru <- renderPlot({
    pie(r11, labels = piepercent, col = rainbow(length(r11)))
    legend("topright", c("URBAN", "RURAL"),cex = 1.3, fill = rainbow(length(r11)))
  })
  
  output$literate_state <- renderPlot({
    ggplot(literate_state, aes(P_LIT,Name))+geom_bar(stat="identity",fill="tomato3")+xlab('Literate population')+ylab('State')
  })
  
  output$HH_RU <- renderPlotly({
    HH_RU_plot=ggplot(HH_RU,aes(No_HH,Name,text=paste("State: ",Name,"<br>Region: ",TRU,"<br>No. of households: ",No_HH)))+geom_bar(aes(fill=TRU),position="dodge",stat="identity")+xlab("Number of households")+ylab("State")+labs(fill="Region")
    ggplotly(HH_RU_plot, tooltip = "text")
  })
  
  output$marg_work_share <- renderPlotly({
    colnames(marg_work_share2)=c("Values")
    plot_ly(data=marg_work_share2,labels=c("Marginal Cultivator Population","Marginal Agriculture Labourers Population","Marginal Household Industries Population","Marginal Other Workers Population"),values=~Values,type="pie",textinfo="label+percent",insidetextorientation = "radial")
  })
  
  output$f_lit <- renderPlotly({
    f_lit_plot=plot_ly(f_lit,x=~Name,y=~F_LIT,type='scatter',mode='markers')%>%layout(xaxis=list(title='States'),yaxis=list(title='Female literate population'),showlegend=FALSE)
    add_lines(f_lit_plot)
  })
  
  output$nonwork_RU <- renderPlotly({
    nonwork_RU_plot=ggplot(nonwork_RU,aes(x=Name,y=NON_WORK_P,fill=TRU,text=paste("State: ",Name,"<br>Region: ",TRU,"<br>Non working population: ",NON_WORK_P)))+geom_bar(stat="identity",position="fill")+xlab("States")+ylab("Non working population")+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(fill="Region")
    ggplotly(nonwork_RU_plot, tooltip = "text")
  })
  
  output$pop_state_int <- renderPlotly({
    plot_ly(data=pop_state,x=~TOT_P,y=~Name,type='bar')%>%layout(xaxis=list(title='Total Population'),yaxis=list(title='State',tickmode='linear'))
  })
  
}

shinyApp(ui, server)
