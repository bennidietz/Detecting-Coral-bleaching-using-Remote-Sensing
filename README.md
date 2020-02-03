# Detecting Coral bleaching using Remote Sensing 
 
We chose as a taks for our Remote Sensing study project at the Institute of Geoinformatics in Münster to detect the coral bleaching process for the last years on the Maldives.
To do that we executed the following steps:
1. Data acquisition - we chose __planetScope data__ due to a high spatial resolution of 3 meters ([Image courtesy of Planet Labs, Inc.](https://www.planet.com/))
2. Subdeviding the data into extens where we found a high occurrence of corals
3. Chosing certain pixel that we expect to be corals
4. Calculate a normalized darkness of each pixel for the time series between 2016 and 2019
5. Making a trend analysis

Steps 2 to 5 can be tracked in this following [R script](https://github.com/bennidietz/Detecting-Coral-bleaching-using-Remote-Sensing/blob/master/how_to.R)
