## Visualisierung der PCA als Rotation
######################################
## Martin Frey
## 2.6.2020
##

library(shiny)

ui <- fluidPage(
    titlePanel(HTML("PCA als Rotation")),
    sidebarLayout(
        sidebarPanel(sliderInput("winkel",
                        HTML("Rotationswinkel (-180° bis 180°)"),
                        min = -180,
                        max = 180,
                        step = 1,
                        value = 0)
        ),
        mainPanel(
           plotOutput("distPlot",  width = "100%")
        )
    )
)


server <- function(input, output) {
    output$distPlot <- renderPlot({
        X <- matrix(c(-1.5, -0.5, -2, -1.5, -1, 0, 2.5, 0.5, 2, 1.5), ncol=2, byrow=TRUE)
        rownames(X) <- 1:5
        res <- prcomp(X, scale=FALSE)
        par(mfrow=c(1,1), mar=c(4, 4,3,1), las=1, cex=1.3,mgp=c(2.2,1,0),pty="s")
        plot(1,1, ylim=c(-3, 3), xlim=c(-3, 3), type="n", xaxs="i",  yaxs="i", 
             ylab=expression('x'[2]), xlab=expression('x'[1]))
        grid(nx=12, ny=12)
        points(x=X[,1], y=X[,2], pch=16)
        text(x=X[,1], y=X[,2], labels= rownames(X), pos=4)
        arrows(x0=-1.9, y0=0, x1 = 1.9, y1 = 0, 
               length = 0.15, angle = 30,col="darkgrey", lwd=2)
        arrows(x0=0, y0=-1.9, x1 = 0, y1 = 1.9, 
               length = 0.15, angle = 30,col="darkgrey", lwd=2)
        arrows(x0=-cos((90-input$winkel)/180*pi)*1.9, y0=-sin((90-input$winkel)/180*pi)*1.9, x1 = cos((90-input$winkel)/180*pi)*1.9, y1 = sin((90-input$winkel)/180*pi)*1.9, 
               length = 0.15, angle = 30,col="grey", lwd=2, lty=2)
        arrows(y1=-sin(input$winkel/180*pi)*1.9, x1=cos(input$winkel/180*pi)*1.9, y0 = sin(input$winkel/180*pi)*1.9, x0 = -cos(input$winkel/180*pi)*1.9, 
               length = 0.15, angle = 30,col="red", lwd=2, lty=2)
        
        Varianz =(res$sdev[1]^2-res$sdev[2]^2)/2*cos((input$winkel+acos(res$rotation[1,1])*180/pi)*(pi/90))+res$sdev[2]^2+(res$sdev[1]^2-res$sdev[2]^2)/2
        title(paste("Varianz entlang der ersten Hauptkomponente", round(Varianz,3)), line=1.5, 
              cex=1.1)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
