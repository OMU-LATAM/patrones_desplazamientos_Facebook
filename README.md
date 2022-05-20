## Introduction

When people use the Facebook app with location services enabled, an
approximation of the latitude and longitude of the device is received at
regular intervals. Location information is used in a variety of ways,
such as providing an in-app features or content that is more relevant to
people. **Facebook Disaster Maps** provide information about where
people are and how they move in a aggregated way, preserving privacy
with some processes like masking and random noise but making the data
usable and interpretable. The information is presented at two levels: by
tile (squares) or by administrative region (such as population
censuses).

The region is divided into squares of 2 km x 2 km. Facebook detects the
coordinates of people and classifies them according to the tile in which
they have been the longest in the eight hours prior to the time of data
collection. For example, if the dataset refers to 8 a.m. and the person
was at home, Facebook will classify that person in the corresponding
tile and assign an **“origin”** on it. If the person spent most of the
next eight hours on another tile, that person will show up in the second
tile’s aggregated data as **“destination”**. If it appears on the same
tile, the destination will be that same tile as origin. There is a
sample of the *Movement between Administrative Regions* processed
dataset as a R console output. 

    head(trips, keepnums = FALSE)

    ##   start_polygon_name end_polygon_name trips length_km       date     city
    ## 1           Santiago        San Ramón    52     10.42 2021-10-05 Santiago
    ## 2           Santiago             Buin    47     32.49 2021-10-05 Santiago
    ## 3         La Florida     San Bernardo  1509     10.26 2021-10-05 Santiago
    ## 4        San Joaquín      Puente Alto   163     11.83 2021-10-05 Santiago
    ## 5        San Joaquín         Recoleta    90     10.00 2021-10-05 Santiago
    ## 6           Santiago         Vitacura   878     11.00 2021-10-05 Santiago

## Population Bias

From the point of view of the administrative regions, there may be
differences in the use of Facebook applications and/or the acceptance of
the use of geospatial location. The High Resolution Density Maps dataset
takes official information from censuses and population projections of
each country and locates the population based on its density and
quantity geospatially through machine learning mechanisms. A spatial
join is made to this information together with the polygons of the
municipalities and in this way the population of each of them is
obtained. Therefore, we tried to remove this bias by taking sums of all
trips from each of the origins and comparing them with the corresponding
population of each region. From there, expansion factors or weights are
extracted that we apply to the trips of each administrative region to
have a more “real” notion of the flows of people between municipalities.

## Short or null trips

Trips that begin and end in the same tile are removed from the average
distances analysis. This is because we want the calculation of average
distances to be based only on actual trips and not people standing still
in one place. Subsequently, these trips are expanded with the same
methodology and are subtracted from the trips within the same
administrative region. In this way, this bias is eliminated, but there
is the limitation of not counting some trips that could have lasted 1.9
km o less for the calculation.

    head(avg_dist, keepnums = FALSE)

    ##        adm_region avg_dist       date         city
    ## 1 Almirante Brown 8.691195 2021-10-05 Buenos Aires
    ## 2      Avellaneda 5.945217 2021-10-05 Buenos Aires
    ## 3     Berazategui 7.838638 2021-10-05 Buenos Aires
    ## 4         Berisso 4.200039 2021-10-05 Buenos Aires
    ## 5        Brandsen 2.175535 2021-10-05 Buenos Aires
    ## 6         Campana 2.467033 2021-10-05 Buenos Aires

## Output

After performing all the operations mentioned above, tables containing
average distances traveled and trips made in each of the tiles and
administrative regions are obtained. This datasets can easily be
statisticaly analyzed and graphed. For example, we can get a scatterplot
of distances and trips for a specific city.

    trips %>% 
      filter(date=="2021-10-10", length_km > 0.5, trips < 10000) %>% 
      ggplot() +
      geom_point(aes(length_km, trips, 
                     color = start_polygon_name), size = .4) +
      theme(legend.position = "none") + labs(x = "Distance (km.)", y = "Trips (n)")

<img src="images/unnamed-chunk-3-1.png" width="80%" style="display: block; margin: auto;" />

Through this model and inferring that at dawn the majority of the
population sleeps and in the morning they go to work, educate
themselves, relax or carry out various daily tasks, it is possible to
obtain in a general way the average distances traveled by the people of
each administrative region and the number of trips made. These
indicators allow describing patterns of mobility of people, through
which various analyzes of public transport policies can be made.

For example, in Buenos Aires average travel distances of around 3 and 4
kilometers are observed, which allows an incentive for sustainable
mobility policies since they are distances that are very likely to be
covered by **bicycle**.

![Average distances in Buenos Aires](images/example.png)

Another brief insight of the research carried out is about analyzing the
average distances and trips based on accessibility to public transport
and the central location of offices and jobs in cities. As long as
economically active areas are accessible from mass transportation such
as trains or BRT, the average distances will correspond to the distance
from the centroid of the administrative region in question to the
administrative center of the area.

Finally, origin and destination matrices were obtained from the data.
These matrices take the number of people who moved from one
administrative region to another and group them based on the sum of
trips by region. This type of information allows to know the flows of
people in a concise way and generate diagnoses on the use of public
transport in the area and the possibilities of investment based on
demand and supply.

## Dashboard

To present the information in a dynamic and concise way, we proceeded to
create a dashboard. All the statistical and design processes were made
in RStudio with R programming language. This dashboard was generated in
Shiny and can be accessed from the following link:

[Dashboard access](https://jfulponi.shinyapps.io/dashboard_omu/)

There the maps and the origin and destination matrices described above
can be found for each Latin American city of the study for a variety of
dates.
