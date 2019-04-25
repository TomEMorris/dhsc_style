#attempt at a dh_plot package

#mostly copied from bbc plot:
#https://github.com/bbc/bbplot

#theme_gov from:
#https://github.com/ukgovdatascience/govstyle



#' dhsc style 
#'
#' This function formats a ggplot object. It is mostly copied from the bbc plot package linked below
#' https://github.com/bbc/bbplot
#'
#' @param
#' @return
#' @export
dhsc_style <- function() {
  windowsFonts(Helvetica = "TT Arial")
  font <- "Helvetica"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=20,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=16,
                                          margin=ggplot2::margin(9,0,9,0)),
    #plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    #axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=12,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 12,  hjust = 0)
  )
}

#' gov style 
#'
#' This function formats a ggplot object. It is copied from the govstyle package linked below:
#' https://github.com/ukgovdatascience/govstyle
#'
#' @param
#' @return
#' @export
theme_gov <- function(
  base_size = 12,
  base_colour = "gray40",
  axes = "x"
) {
  
  if (!axes %in% c("n","x","y","xzy")) {
    
    stop("axes must be one of 'n', 'x', 'y', or 'xy'")
    
  }
  
  ## Set x and y axis colour
  
  x_col = "white"
  y_col = "white"
  
  if (axes == "x") {
    
    x_col = base_colour
    y_col = "white"
    
  }
  
  if (axes == "y") {
    
    x_col = "white"
    y_col = base_colour
    
  }
  
  if (axes == "xy") {
    
    x_col = base_colour
    y_col = base_colour
    
  }
  
  theme(
    legend.position = "none",
    
    ## Adjust tick marks
    
    axis.ticks = ggplot2::element_blank(),
    #axis.ticks = element_line(colour = "gray40"),
    #axis.ticks.y = element_blank(),
    #axis.ticks.length = grid::unit( -2, "mm"),
    
    ## Adjust the axis lines
    
    axis.line = ggplot2::element_line(colour = base_colour),
    axis.line.x = ggplot2::element_line(colour = x_col),
    axis.line.y = ggplot2::element_line(colour = y_col),
    
    ## Set the overall text attributes
    
    text = ggplot2::element_text(
      face = "plain", colour = base_colour, size = base_size,
      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.8
    ),
    axis.text = ggplot2::element_text(colour = base_colour),
    plot.title = ggplot2::element_text(face = "bold", hjust = 1, colour = "black", vjust = -2.5),
    
    ## Axis title attributes. Adjustments of
    
    axis.title.y = ggplot2::element_text(hjust = 1, vjust = 1),
    axis.title.x = ggplot2::element_text(hjust = 1, vjust = 0),
    
    ## Background attributes (currently all blank)
    
    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    
    ##Adjust the margin around the plot. This is highly sensitive to plot, so
    ##probably needs to be set on a plot by plot basis.
    
    #plot.margin = grid::unit(c(0,5,5,-30), "mm"),
    
    ## Strip attributes for facet grid and facet wrap
    
    strip.background =   ggplot2::element_blank(),
    strip.text =         ggplot2::element_text(color = "black", face = "bold", size = base_size + 1),
    strip.text.x =       ggplot2::element_text(),
    strip.text.y =       ggplot2::element_text(angle = -90),
    
    complete = FALSE
  )
}

#' list_dhsc_colours 
#'
#' This function lists the DHSC colours that are available in the package
#' 
#'
#' @param
#' @return
#' @export
list_dhsc_colours <- function(){
  dhsc_colours <- c(dh_Teal = "#00ad93",
                    dh_Grey = "#616265",
                    dh_Red = "#cc092f",
                    dh_Orange = "#e57200",
                    dh_Plum = "#8b2346",
                    dh_Sky = "#34b6e4",
                    dh_Blue = "#0063be",
                    dh_GreenL = "#2eb135",
                    dh_GreenD = "#006652",
                    dh_Yellow = "#ecac00",
                    dh_Pink = "#cd66cc",
                    dh_Purple = "#512698")
  return(dhsc_colours)
}

