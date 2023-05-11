library(ggplot2)

# Function
fun <- function(x) {
  (-x*(x-6))/3
}

# Plot
curve(fun, from = 0, to = 6,
      xlab = "centralization of wage setting", 
      ylab = "Real wage")


s <- seq(0, 6, length.out = 1000)
g <- tibble(x =s, y= fun(s))
g <- g %>%
  filter(y > 1.5)
gg <- ggplot(data = g,
      mapping = aes(x,y))+geom_line()
gg <- gg + ggtitle("Hump-Shaped Curve") + 
xlab("Degree of Centralization") + ylab("Real Wage") + 
 coord_cartesian(xlim=c(0.3,5.7), ylim=c(1.3,3.3))


zg <- gg + theme(

           #axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           ###axis.title.x=element_blank(),
           #axis.title.y=element_blank(),
           legend.position="none",
           #panel.background=element_blank(),
           #panel.border=element_blank(),
           #panel.grid.major=element_blank(),
           #panel.grid.minor=element_blank(),
           #plot.background=element_blank(),
           plot.title = element_text(hjust = 0.5)
)
  zg
  
  hg <- gg + ggtitle("Hump-Shaped Curve") + 
    xlab("Power of Labour Union") + ylab("Unemployment") + 
    coord_cartesian(xlim=c(0.3,5.7), ylim=c(1.3,3.3))
  
  
  bg <- hg + theme(
    
    #axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    ###axis.title.x=element_blank(),
    #axis.title.y=element_blank(),
    legend.position="none",
    #panel.background=element_blank(),
    #panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
  bg
