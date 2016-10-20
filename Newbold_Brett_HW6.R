---
  HW6
---

  require(ggplot2)
  require(grid)

---
  2. Create a simple scatter plot of Weight ("Carat") and Price using Color as a facet.
---

  plot2 <- ggplot(diamonds, aes(carat,price)) +   # Begins the plot using the data from diamonds, x-axis is weight, y-axis is price
    geom_point(aes(colour = factor(color)), size = .8) +   # Creates scatterplot with various colors for each factor in 
    # "color" column of diamonds, decrease the size of each point
    xlab("Weight") +   # Labeling x-axis
    ylab("Price") +    # Labeling y-axis
    ggtitle("Diamonds - Weight to Price by Color") +   # Labeling the title
    theme(
      # Making the title of the plot blue, larger font, and closer to plot
      plot.title = element_text(color = "blue", size=17, margin = margin(0,0,4,0)),
      axis.title = element_text(size=8.5), # Smaller titles for the axes 
      axis.text = element_text(color = "gray58", size=6.8), # Smaller and lighter text on axes
      axis.ticks = element_line(color = "gray58"), # Lighter ticks on axes
      legend.title = element_text(size=7, face = "bold"), # Smaller yet bolder title for legend
      legend.text = element_text(size=6.5), # Smaller text inside legend
      plot.margin = unit(c(7,4,3,4),"mm"), # Creating more surrounding whitespace
      panel.grid.minor = element_line(color = "gray95") # Suddle minor grid lines
    )

  plot2 # Display the plot in R

  # I wanted to give the option to display the plot as it appears in the homework. Creating a PDF allowed me to 
  # duplicate the examples as best as I could because displying in RStudio is dependent on the dimensions of the
  # user's plot window

  ggsave("plot2.pdf", plot2, width = 6.8, height = 4.7) # Save plot as PDF with ideal dimensions
  system('open "plot2.pdf"') # Open PDF



---
  3. Remove the non-linearity and replot
---
  
  plot3 <- ggplot(diamonds, aes(log(carat),log(price))) +   # Transforming weight (carat) and price using log
                                                          # This creates linearity in our scatter plot
  geom_point(aes(colour = factor(color)), size =.8) +   # Creates scatterplot with various colors for each factor in "color" column of diamonds
    xlab("Weight") +   # Labeling x-axis
    ylab("Price") +    # Labeling y-axis
    ggtitle("Diamonds - Weight to Price (Linear)") +   # Labeling the title
    theme(
      # Making the title of the plot blue, larger font, and closer to plot
      plot.title = element_text(color = "blue", size=17, margin = margin(0,0,4,0)),
      axis.title = element_text(size=8.5), # Smaller titles for the axes 
      axis.text = element_text(color = "gray58", size=6.8), # Smaller and lighter text on axes
      axis.ticks = element_line(color = "gray58"), # Lighter ticks on axes
      legend.title = element_text(size=7, face = "bold"), # Smaller yet bolder title for legend
      legend.text = element_text(size=6.5), # Smaller text inside legend
      plot.margin = unit(c(7,4,3,4),"mm"), # Creating more surrounding whitespace
      panel.grid.minor = element_line(color = "gray95") # Suddle minor grid lines
    )

  plot3 # Display in window
  ggsave("plot3.pdf", plot3, width = 6.8, height = 4.7) # Saving with ideal dimensions
  system('open "plot3.pdf"') # Displaying PDF on screen

---
  4. Remove the linear trend
---
    
  r <- resid(lm(log(price)~log(carat), diamonds)) # Creates the residuals of the linear model
  
  plot4 <- ggplot(diamonds, aes(x=log(carat), y=r)) + # Uses the residuals on the y-axis and transformed weight on x-axis
    geom_point(aes(colour = factor(color)), size=.7) +   # Creates scatterplot with various colors for each factor in "color" column of diamonds
    xlab("Weight") +   # Labeling x-axis
    ylab("Price Residuals") +    # Labeling y-axis
    ggtitle("Diamonds - Weight to Price by Color") +   # Labeling the title
    theme(
      plot.title = element_text(color = "blue", size=13, margin = margin(0,0,4,0)),   # Changing the color of the title
      axis.title = element_text(size=6.3), # Smaller titles for the axes 
      axis.text = element_text(color = "gray58", size=5), # Smaller and lighter text on axes
      axis.ticks = element_line(color = "gray58"), # Lighter ticks on axes
      legend.position = "top", # Placing the legend on top of plot
      legend.key.size = unit(.35, "cm"), # Decreasing size of the legend keys
      legend.title = element_text(size=6.5), # Smaller yet bolder title for legend
      legend.text = element_text(size=5.5), # Smaller text inside legend
      plot.margin = unit(c(1,1,.5,.5),"mm"), # Creating more surrounding whitespace
      panel.grid.minor = element_line(color = "gray95") # Suddle minor grid lines
    )
  
  plot4 # Display in window  
  
  ggsave("plot4.pdf", plot4, width = 6.8, height = 4.7) # Save PDF
  system('open "plot4.pdf"') # Open PDF
  
---
    5. Use the grid package to create an overlay of density histograms of price and carat with the previous.
---
    
  r <- resid(lm(log(price)~log(carat), diamonds)) # Creates the residuals of the linear model
  
  main <- ggplot(diamonds, aes(x=log(carat), y=r)) +  # # Uses the residuals on the y-axis and transformed weight on x-axis
    geom_point(aes(colour = factor(color)), size = .7) +   # Creates scatterplot with various colors for each factor in "color" column of diamonds
    xlab("Weight") +   # Labeling x-axis
    ylab("Price Residuals") +    # Labeling y-axis
    ggtitle("Diamonds - Weight to Price by Color") +   # Labeling the title
    guides(col = guide_legend(nrow = 1)) +  # Making the legend one row
    theme(
      plot.title = element_text(color = "blue", size=17, margin = margin(4,0,0,0)), # Changing the color of the title
      legend.position = "top",  # Placing the legend on top of plot
      axis.title = element_text(size=8), # Smaller titles for the axes 
      axis.text = element_text(color = "gray58", size=7), # Smaller and lighter text on axes
      axis.ticks = element_line(color = "gray58"), # Lighter ticks on axes
      legend.position = ("top"), # Placing the legend on top of plot
      legend.key.size = unit(.6, "cm"), # Decreasing the size of the legend keys
      legend.title = element_text(size=7, face = "bold"), # Smaller yet bolder title for legend
      legend.text = element_text(size=6), # Smaller text inside legend
      panel.grid.minor = element_line(color = "gray95") # Suddle minor grid lines
    )
  
  p1 <- ggplot(diamonds, aes(price, ..density.., fill=color)) + # Using density on the y axis, price for x axis,
                                                                # and stacking the variable color from diamonds
    geom_histogram(binwidth = 90) + # Making plot a histogram with specific binwidth
    theme(
      legend.position = "none", # Making the legend disappear 
      axis.title = element_blank(), # Stripping the titles on the axes
      axis.text = element_text(color = "gray58", size=5), # Smaller and lighter text on axes
      axis.ticks = element_line(color = "gray58"), # Lighter ticks on axes
      plot.margin=unit(c(0,0,0,0),"mm"), # Removing surrounding whitespace
      panel.grid.minor = element_line(color = "gray95") # Suddle minor grid lines
    )
  
  p2 <- ggplot(diamonds, aes(carat, ..density.., fill=clarity)) + # Using density on the y axis, carat for the x
                                                                  # axis, and stacking the variable clarity from diamonds
    geom_histogram(binwidth = .08) + # Making plot a histogram with specific binwidth 
    theme(
      legend.position="none", # Making the legend disappear
      axis.title = element_blank(), # Stripping the titles on the axes
      axis.text = element_text(color = "gray58", size=5), # Smaller and lighter text on axes
      axis.ticks = element_line(color = "gray58"), # Lighter ticks on axes
      plot.margin = unit(c(0,0,0,0),"mm"), # Removing surrounding whitespace
      panel.grid.minor = element_line(color = "gray95") # Suddle minor grid lines
    ) 
  
  subvp1 <- viewport(width=.4,height=.2,x=.275,y=.19) # Creating viewport with specific areas and locations
  subvp2 <- viewport(width=.4,height=.2,x=.788,y=.73)
  main # Displays the main plot in RStudio
  print(p1, vp=subvp1) # Displays first density histogram in RStudio
  print(p2, vp=subvp2) # Displays second density histogram in RStudio
  
  pdf("plot5.pdf", width = 6.8, height = 4.7); # Creating one PDF of the plots to eventually open
  print(main); 
  print(p1, vp=subvp1); 
  print(p2, vp=subvp2);
  dev.off()
  system('open "plot5.pdf"')
