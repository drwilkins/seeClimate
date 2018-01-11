#Temp Data Source:
#Kaufman DS, Schneider DP, McKay NP, et al. 2009. Recent warming reverses long-term arctic cooling. Science 325: 1236–1239.

#CO2 Data Source:
#C. D. Keeling, S. C. Piper, R. B. Bacastow, M. Wahlen, T. P. Whorf, M. Heimann, and H. A. Meijer, Exchanges of atmospheric CO2 and 13CO2 with the terrestrial biosphere and oceans from 1978 to 2000. I. Global aspects, SIO Reference Series, No. 01-06, Scripps Institution of Oceanography, San Diego, 88 pages, 2001.

library(shiny);require(ggplot2);require(reshape2);require(DT)
temp<-read.csv("Kaufman Temp data.csv")
CO2<-read.csv("Keeling CO2 data.csv")[,-13]
CO2$my<-paste(CO2$Mn,CO2$Yr,sep="-")

#Temperature data sources
dfnames<-c("01-Glacial Sediments_Alaska_yr730-2000","02-Sediments_Alaska_yr1-2000","03-Tree Rings_Alaska_yr720-2000","04-Glacial Sediments_Alaska_yr460-2000","05-Ice Isotopes_Devon Island_yr1-1980","06-Glacial Sediments_Ellesmere Island_yr1-2000","07-Ice Isotopes_Baffin Island_yr1-1980","08-Glacial Sediments_Ellesmere Island_yr1-2000","09-Glacial Sediments_Baffin Island_yr980-2000","10-Glacial Sediments_Baffin Island_750-2000","11-Sediments_Greenland_yr1-1940","12-Ice Isotopes_Greenland_yr1-1980","13-Ice Isotopes_Greenland_yr1-2000","14-Ice Isotopes_Greenland_yr1-1990","15-Ice Isotopes_Greenland_yr550-1980","16-Ice Isotopes_Greenland_yr1-1990","17-Sediments_Iceland_yr1-2000","18-Tree Rings_yr1-2000","19-Glacial Sediments OC_Finland_yr1-1800","20-Glacial Sediments x-ray_Finland_yr1-1800","21-Glacial Sediments Thickness_Finland_yr1-1800","22-Tree Rings_NW Siberia_yr1-2000","23-Tree Rings_Siberia_yr1-2000","24-Overall Average")

#CO2 data sources
#from 
dfnames.CO2<-c("American Samoa"="SAM", "Baja California"="BCS","Baring Head, New Zealand"="NZD","Christmas Island"="CHR", "Mauna Loa Observatory, Hawaii"="MLO", "Pt. Barrow, Alaska"="PTB", "South Pole"="SPO")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel(windowTitle="SeeClimate",h1("SeeClimate: Exploring Long-Term Climate Change Datasets",style="font-family: 'Courier New';color: #444444;")),
  p("Choose a dataset and start exploring!",style="font-family: 'Courier New';color: #444444;"),
   
tabsetPanel( 
##########
### Temp Panel
  tabPanel("Temperature",
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("datachoice","Choose Temperature Dataset(s)",dfnames,multiple=T,selected="") , 
        radioButtons("whatplot","What to Plot?",c("Points","Smoother","Points + Smoother"),"Points") ,
        checkboxInput("locReg","Fit A Line?",FALSE),
        conditionalPanel("input.locReg",
        sliderInput(inputId="rng",label=("Year Range for Line"),min=0,max=2017,value=c(0,2017),timeFormat = "%Y",step=1),
        DT::dataTableOutput("lineEq")) ),
      
      # Show a plot of the generated distribution
      mainPanel(fluidRow(
      plotOutput("tempgraf")
      ))
   )#end sidebarLayout
  ),#end tabPanel


#####################  
#CO2 Panel  
tabPanel("CO2",
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("datachoice.CO2","Choose CO2 Dataset(s)",dfnames.CO2,multiple=T,selected="") , 
        radioButtons("whatplot.CO2","What to Plot?",c("Points","Smoother","Points + Smoother"),"Points") ,
        checkboxInput("locReg_CO2","Fit A Line?",FALSE),
        conditionalPanel("input.locReg_CO2",
        sliderInput(inputId="rng.CO2",label=("Year Range for Line"),min=0,max=2017,value=c(0,2017),timeFormat = "%Y",step=1),
        DT::dataTableOutput("lineEq.CO2")) ),
      
      # Show a plot of the generated distribution
      mainPanel(fluidRow(
      plotOutput("CO2graf")
      ))
   )#end sidebarLayout
  )#end tabPanel
  )#end tabsetpanel
)#End fluidPage



#SSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
## SERVER SIDE
server <- function(input, output) {
  
  #Watch user range values (for local regression)
  vals<-reactiveValues()
  observe(if(is.null(input$datachoice)){vals$usrdf_melt<-temp;vals$colindx<-"";vals$df.rng<-temp
  }else{
    vals$echo<-T
    vals$colindx<-as.numeric(sort(gsub("-(.+)","",input$datachoice)))+1 
    vals$usrdf_melt<-melt(temp[,c(1,vals$colindx)],value.name="Temp",id="Year",variable.name="Dataset" )
    vals$df.rng<-subset(vals$usrdf_melt,Year>=input$rng[1]&Year<=input$rng[2])
    })
  
  
#Temp plot
   output$tempgraf <- renderPlot({
     if(is.null(input$datachoice)){ggplot(data.frame())+geom_blank()+theme_bw()}
  
     else{ #Begin BIG ELSE
     #Global plot params (before plotting) 
     g<-ggplot(vals$usrdf_melt,aes(x=Year,y=Temp,col=Dataset))+theme_linedraw()+xlim(0,2000)+theme(axis.text=element_text(size=13),axis.title=element_text(size=18,face="bold"))+ylab("Relative Temperature (ºC)")+geom_hline(yintercept=0,col="gray60",linetype="dashed")+annotate("text",x=2,y=0.2,label="2k Year Average Temp",col="gray60",hjust=0)
    
  # How to Plot (Radio Buttons)
       if(input$whatplot=="Points"){
         G<-g+geom_point()
       }else{if(input$whatplot=="Smoother"){
         G<-g+geom_smooth()
       }else{
         G<-g+geom_point()+geom_smooth(se=F)} } 
     G
  
  # # Fitting a line
     if(input$locReg==T){
       G<-G+geom_smooth(method="lm",data=vals$df.rng,se=F,size=3)
       G
      }else{G}

    }#end BIG ELSE
 })#End renderPlot
   
  #Output data table
  output$lineEq<- DT::renderDataTable(
    {datasets<-unique(vals$df.rng$Dataset)
    model<-data.frame(Dataset=datasets,t(sapply(datasets,function(x){
      coeffs<-coef(lm(Temp~Date2,data=subset(vals$df.rng,Dataset==x)))
      
        })))
    names(model)[2]<-"Intercept"
    model<-format(model,digits=4,scientific=T)
    },options=list(#iDisplayLength=5, # initial number of records
                     aLengthMenu=c(5,10),# records/page options
                     bPaginate=F,#Don't paginate
                      bLengthChange=F, # show/hide records per page dropdown
                     bFilter=F, # global search box on/off
                     bInfo=F# information on/off (how many records filtered, etc)
                     #aoColumnDefs = list(list(sWidth="300px", aTargets=c(list(0),list(1))))    # custom column size                       
                    ))#End RenderDataTable

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #CO2 Plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  #Watch user range values (for local regression)
  vals.CO2<-reactiveValues()
  observe(if(is.null(input$datachoice.CO2)){vals.CO2$usrdf_melt<-CO2;vals.CO2$df.rng<-CO2
  }else{
    vals.CO2$usrdf_melt<-subset(CO2,Dataset%in%input$datachoice.CO2)
    vals.CO2$df.rng<-subset(vals.CO2$usrdf_melt,Yr>=input$rng.CO2[1]&Yr<=input$rng.CO2[2])
    }#end else
    )#end observe
  
  
#Carbon Dioxide plot
   output$CO2graf <- renderPlot({
     if(is.null(input$datachoice.CO2)){ggplot(data.frame())+geom_blank()+theme_bw()}
  
     else{ #Begin BIG ELSE
     #Global plot params (before plotting) 
     g.CO2<-ggplot(vals.CO2$usrdf_melt,aes(x=Date2,y=CO2,col=Dataset))+theme_linedraw()+xlim(1958,2017)+theme(axis.text=element_text(size=13),axis.title=element_text(size=18,face="bold"))+ylab(expression("CO"[2]*" Concentration (ppm)"))+xlab("Year")#+geom_hline(yintercept=0,col="gray60",linetype="dashed")+annotate("text",x=2,y=0.2,label="2k Year Average Temp",col="gray60",hjust=0)
    
  # How to Plot (Radio Buttons)
       if(input$whatplot.CO2=="Points"){
         G.CO2<-g.CO2+geom_point()
       }else{if(input$whatplot.CO2=="Smoother"){
         G.CO2<-g.CO2+geom_smooth()
       }else{
         G.CO2<-g.CO2+geom_point()+geom_smooth(se=F)} } 
     G.CO2
  
  # # Fitting a line
     if(input$locReg_CO2==T){
       G.CO2<-G.CO2+geom_smooth(method="lm",data=vals.CO2$df.rng,se=F,size=3)
       G.CO2
      }else{G.CO2}

    }#end BIG ELSE
 })#End renderPlot
   
  #Output data table
  output$lineEq.CO2<- DT::renderDataTable(
    {datasets.CO2<-unique(vals.CO2$df.rng$Dataset)
    model.CO2<-data.frame(Dataset=datasets.CO2,t(sapply(datasets.CO2,function(x){
      coeffs.CO2<-coef(lm(CO2~Date2,data=subset(vals.CO2$df.rng,Dataset==x)))
      
    })))
    names(model.CO2)[2]<-"Intercept"
    model.CO2<-format(model.CO2,digits=4,scientific=T)
    },options=list(#iDisplayLength=5, # initial number of records
                     aLengthMenu=c(5,10),# records/page options
                     bPaginate=F,#Don't paginate
                     bLengthChange=F, # show/hide records per page dropdown
                     bFilter=F, # global search box on/off
                     bInfo=F# information on/off (how many records filtered, etc)
                     #aoColumnDefs = list(list(sWidth="300px", aTargets=c(list(0),list(1))))    # custom column size                       
                    ))
  
}#End Server

# Run the application 
shinyApp(ui = ui, server = server)

  
  
  