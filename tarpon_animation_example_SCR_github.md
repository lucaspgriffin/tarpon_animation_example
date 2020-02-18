plots and animations github
================

We’ll first load packages and data.

This dataset is a subset of three Atlantic Tarpon tagged with acoustic
transmitters and is part of the Bonefish and Tarpon Trust Acoustic
Telemetry Tagging Project.

See Griffin, Lucas P., et al. “Keeping up with the Silver King: Using
cooperative acoustic telemetry networks to quantify the movements of
Atlantic tarpon (Megalops atlanticus) in the coastal waters of the
southeastern United States.” Fisheries Research 205 (2018): 65-76. for
more details on the larger and ongoing study.

This data should not be repurposed for anything beyond this example.

``` r
# install or load packages
install.packages('dplyr') # data manipulation
install.packages('lubridate') # data manipulation for dates
install.packages("ggplot2") # standard plotting package
install.packages('ggmap') # allows for mapping
install.packages('scales') # date time breaks in ggplots

# need to download gganimate / transformr to get the most updated code (great packages, thanks to Thomas Lin Pedersen)
install.packages("devtools")
library(devtools)
devtools::install_github("thomasp85/gganimate") # animation package
devtools::install_github("thomasp85/transformr") # allows for geom_path animation 
```

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(ggplot2)
library(ggmap)
```

    ## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.

    ## Please cite ggmap if you use it! See citation("ggmap") for details.

``` r
library(scales)
library(gganimate)
library(transformr)


# data
setwd("~/github/tarpon_animation_example")
load("Tarpon.RData") # our Tarpon object is a subset of three tarpon. Object = Tarpon

load("SE_USA.RData") # map of SE USA. Object = SE_USA
```

Plot our detections and generic pathways across the SE USA using
ggmaps.

``` r
# Generate a base map...to do this on your own, you'll need to register an API key address with google. I have not included mine here and have chosen to generate this map at an earlier time. The code with # in front of it demonstrates how one might produce such a base map.

# register_google(key = "API #####") # use your google API address to be able to generate ggmaps

# SE_USA <- get_googlemap(center = c(lon=-82.5, lat=30),
#                       zoom = 5 ,size = c(640,640), scale = 2,
#                       maptype = c("hybrid"))

plot(SE_USA)
```

![](tarpon_animation_example_SCR_github_files/figure-gfm/plot%20via%20ggmap-1.png)<!-- -->

``` r
ggmap(SE_USA, extent='normal')+
  scale_x_continuous(limits=c(min(Tarpon$Longitude)-.5, max(Tarpon$Longitude)+.5))+ # limit our x axis
  scale_y_continuous(limits=c(min(Tarpon$Latitude)-.5, max(Tarpon$Latitude)+.5))+ # limit our y axis
  ylab("Latitude") + # labels
  xlab("Longitude")+ # labels
  geom_point(data = Tarpon, aes(x=Longitude,y=Latitude,col=Transmitter)) + # our points we'll be plotting
  geom_path(data=Tarpon,aes(x=Longitude,y=Latitude,col=Transmitter)) + # our connections to the points
  theme(strip.text = element_text(face="bold", size=8, lineheight=5.0), legend.position="top") + # format strip text and legend position
  facet_wrap(~Transmitter) # plot each transmitter individually
```

    ## Warning: Removed 3 rows containing missing values (geom_rect).

![](tarpon_animation_example_SCR_github_files/figure-gfm/plot%20via%20ggmap-2.png)<!-- -->

Explore Latitude across time plots

``` r
ggplot(data=Tarpon, aes(x=UTC, y=Latitude))+ # set up the plot
  geom_path() + # indicate we want lines connecting observations
  geom_point() + # indicate we want points
  scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%b %y")) + # x axis date formating
  xlab("") + # blank x label
  theme_bw() + # cleaner look
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
        strip.text = element_text(face="bold", size=8, lineheight=5.0)) + # easier to read
  facet_wrap(~ Transmitter, ncol = 1) # plot each transmitter individually within one column
```

![](tarpon_animation_example_SCR_github_files/figure-gfm/latitude%20plots%20-%20part%20one-1.png)<!-- -->

Here, we set up our latitude plots at the month level, not the
month-year level. This takes a bit of data
manipulation.

``` r
Tarpon$moday <- strftime(Tarpon$UTC, format="%m-%d") # construct a month day variable from detection time (dropping the year portion)
Tarpon$moday_pos <- as.POSIXct(Tarpon$moday, format="%m-%d") # force it back to the generic posix format, it'll assume each detection occurs across the same year (2020)

# Again, since our transmitters are detected across years we need to construct a grouping variable so they can be plotted over one another

Tarpon$Year <- strftime(Tarpon$UTC, format="%Y") # select just year portion of detection time
Tarpon$Transmitter_Year = paste(Tarpon$ID, Tarpon$Year, sep = "_") # paste the detection year to the transmitter name, this will serve as our grouping variable in a few steps 


Migration_plot = 
  ggplot()+
  geom_path(data=Tarpon %>% arrange(moday_pos), # make sure to arrange by our time variable (if not, the last point will wrap around )
            aes(x=moday_pos, y=Latitude, group = Transmitter_Year, col = Year), # color each year
            alpha = 0.4)+
  geom_point(data=Tarpon %>% arrange(moday_pos), 
             aes(x=moday_pos, y=Latitude, group = Transmitter_Year, col = Year), # color each year
             alpha = 0.5) + 
  scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%b")) + # format our date time breaks (abbreviated)
  xlab("")+ # no x axis label
  theme_bw() +  # cleaner look
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
        legend.position = "top") + # format our x axis text and legend position
  facet_grid(~Transmitter) # plot each transmitter individually

Migration_plot 
```

![](tarpon_animation_example_SCR_github_files/figure-gfm/latitude%20plots%20-%20part%20two-1.png)<!-- -->

Above we have a plot that shows latitude movements across years at the
month level for three transmitters. However, this is from Jan - Jan (the
defualt), we want it to be more representative of a tarpon’s journey.
We’ll manipulate the code to plot from March - March instead.

Note, there could very well be much easier ways to produce these outputs
below, I welcome modifications to the code via
GitHub.

``` r
#  We want to examine a time frame between March - March, this is provides a more complete picture of a tarpon's journey. 
Tarpon$month <- strftime(Tarpon$UTC, format="%B") # select just the month portion of the detection time
Tarpon$QYear = ifelse(Tarpon$month == "January", 1,
                      ifelse(Tarpon$month == "February", 1,
                             ifelse(Tarpon$month == "March", 1,0))) # As of now, we have ggplot2 set to plot onto 2020. If we label our months that we want to wrap around, we'll trick ggplot to plot onto 2020-2021 with the correct April - April month order. 

Tarpon$moday_pos_mod = ymd(Tarpon$moday_pos) + years(Tarpon$QYear) # now add a year to our selected months (i.e., now it'll be April 2020, May 2020, June 2020, July 2020, Aug 2020, Sept 2020, Oct 2020, Nov 2020, Dec 2020, Jan 2021, Feb 2021, March 2021)

Tarpon$moday_pos_mod = as.POSIXct(Tarpon$moday_pos_mod) # convert to posix so we can plot

range(Tarpon$moday_pos_mod) # looks good April 2020 - March 2021
```

    ## [1] "2020-03-31 20:00:00 EDT" "2021-03-28 20:00:00 EDT"

``` r
Migration_plot_mod = 
  ggplot()+
  geom_path(data=Tarpon %>% arrange(moday_pos_mod), # make sure to arrange by our time variable (if not, the last point will wrap around )
            aes(x=moday_pos_mod, y=Latitude, group = Transmitter_Year,
            col = Transmitter_Year), # this time, color by transmitter year combo.. it'll look pretty in the animation
            alpha = 0.4)+
  geom_point(data=Tarpon %>% arrange(moday_pos_mod), 
             aes(x=moday_pos_mod, y=Latitude, group = Transmitter_Year,
            col = Transmitter_Year), alpha = 0.5) + 
  scale_x_datetime(breaks = date_breaks("1 months"),labels = date_format("%b")) + # format our date time breaks (abbreviated)
  xlab("")+ # no x axis label
  theme_bw() +  # cleaner look
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
        legend.position = "top") + # format our x axis text and legend position
  facet_grid(~Transmitter) # plot each transmitter individually

Migration_plot_mod
```

![](tarpon_animation_example_SCR_github_files/figure-gfm/latitude%20plots%20-%20part%20three-1.png)<!-- -->

``` r
# save your plot
ggsave("Migration_plot_mod.tiff", units = "in")
```

    ## Saving 7 x 5 in image

Animiate our
plot

``` r
Migration_plot_mod_animation = Migration_plot_mod + transition_reveal(moday_pos_mod) # transition through our date variable
Migration_plot_mod_animation_final = animate(Migration_plot_mod_animation,fps = 6) # modify the frames per second (fps) to make it a smoother and slower transition period

Migration_plot_mod_animation_final
```

![](tarpon_animation_example_SCR_github_files/figure-gfm/latitude%20plots%20-%20part%20four-1.gif)<!-- -->

``` r
anim_save("Migration_plot_mod_animation_final.gif")
```
