### GGPLOT2 NOTES

##### SCATTERPLOTS #####

# Change the command below so that cyl is treated as factor
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_point()

# Edit to add a color aesthetic mapped to column: disp
# Size aesthetic can be added in the same way to categorical variables
ggplot(mtcars, aes(wt, mpg, color = disp)) +
  geom_point() 

# Make the points 40% opaque
ggplot(diamonds, aes(carat, price, color = clarity)) +
  geom_point(alpha = 0.4) 
 
# Establish the base layer
plt_mpg_vs_wt <- ggplot(mtcars, aes(wt, mpg))

# Use text layer and map fcyl to label
plt_mpg_vs_wt +
  geom_text(aes(label = fcyl)) 

# A hexadecimal color
my_blue <- "#4ABEFF"

# Add text layer with label rownames(mtcars) and color red
geom_text(label = rownames(mtcars), color = 'red')

# Set the axis labels
labs(x = "Number of Cylinders", y = "Count")

# Set the fill color scale
scale_fill_manual("Transmission", values = palette)

# Set the position
ggplot(mtcars, aes(fcyl, fill = fam)) +
  geom_bar(position = "dodge")

# Plot 0 vs. mpg
ggplot(mtcars, aes(mpg, 0)) +
  # Add jitter
  geom_point(position="jitter")

# Set the y-axis limits
+ ylim(-2, 2)

# Alter the point positions by jittering, width 0.3
plt_mpg_vs_fcyl_by_fam + geom_point(position = position_jitter(width = 0.3))

# Now jitter and dodge the point positions
plt_mpg_vs_fcyl_by_fam + geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.3))

# Or set jitter in geom_point
geom_point(alpha = 0.5, position = "jitter")

# Another jitter alternative
geom_point(alpha = 0.5, position = position_jitter(width = 0.1))

#### HISTOGRAMS #####

# Map y to ..density.. to show frequency densities 
ggplot(mtcars, aes(mpg, y = ..density..)) +
  geom_histogram(binwidth = 1)

##### BAR PLOTS #####

# Change position to use the functional form, with width 0.2 ####GREAT OFFSET LOOK
geom_bar(position = position_dodge(width = 0.2))

# Add a bar layer with position "fill"
geom_bar(position = "fill") +
  # Add a brewer fill scale with default palette
   scale_fill_brewer()

##### LINE PLOTS #####

# Plot multiple time-series by coloring by species
ggplot(fish.tidy, aes(x = Year, y = Capture, color = Species)) +
  geom_line()

###### THEMES ########

# Remove legend entirely
plot + theme(legend.position = "none")

# Position the legend at the bottom of the plot
plot + theme(legend.position = "bottom")

# Position the legend inside the plot at (0.6, 0.1)
plot + theme(legend.position = c(0.6, 0.1))

plot +
  theme(
    # For all rectangles, set the fill color to grey92
    rect = element_rect(fill = "grey92"),
    # For the legend key, turn off the outline
    legend.key = element_rect(color = NA))
 
plot +  theme( # Turn off axis ticks
    axis.ticks = element_blank(),
    # Turn off the panel grid
    panel.grid = element_blank())

    # Add major y-axis panel grid lines back
        panel.grid.major.y = element_line(
          color = "white",
          size = 0.5,
          linetype = "dotted",
    # Set the axis text color to grey25
        axis.text = element_text(color = "grey25"),
    # Set the plot title font face to italic and font size to 16
        plot.title = element_text(size = 16, face = "italic"),
     # Set the axis tick length to 2 lines
        axis.ticks.length = unit(2, "lines")
        
      )
        
##### THEMES #####
        
# Add a black and white theme
   plot + theme_bw()
        
# Save the theme as theme_recession
      theme_recession <- theme(
          rect = element_rect(fill = "grey92"),
          legend.key = element_rect(color = NA),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "white", size = 0.5, linetype = "dotted"),
          axis.text = element_text(color = "grey25"),
          plot.title = element_text(face = "italic", size = 16),
          legend.position = c(0.6, 0.1))
        

# Set theme_tufte_recession as the default theme
theme_set(theme_tufte_recession)

# Add a geom_segment() layer, geom_text() layer, 
ggplot(gm2007, aes(x = lifeExp, y = country, color = lifeExp)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 30, yend = country), size = 2) +
  geom_text(aes(label = lifeExp), color = "white", size = 1.5)

## Setting a theme: 
plt_country_vs_lifeExp +
  step_1_themes +
  geom_vline(xintercept = global_mean, color = "grey40", linetype = 3) +
  step_3_annotation + annotate("curve", x = x_start, y = y_start, xend = x_end, yend = y_end) +
    arrow = arrow(length = unit(0.2, "cm"), type = "closed") +
    color = "grey40"

## Density Ridges:
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016')

            