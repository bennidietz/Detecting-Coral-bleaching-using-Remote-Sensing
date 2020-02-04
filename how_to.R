rm(list = ls())
# import used libraries
library(rgdal)
library(raster)

# here: download the crops
working_dir = "U:/PlanetScope/" #  set your working directory to the folder where the crops are in

# load all crops
for (j in 1:5) {
  for (i in list.files(gsub(" ", "",paste(working_dir, gsub(" ", "",paste("Crop", gsub(" ", "",paste(j, "/")))))))) {
    load(gsub(" ", "",paste(working_dir, gsub(" ", "",paste("Crop", gsub(" ", "",paste(j, gsub(" ", "",paste("/", i)))))))))
  }
}

# function that takes in the absoulte x and y coordinates of a pixel and
# returns the index of the corresponding pixel
getPixelIndex = function(raster_stack, x, y) {
  x_index = round((x - extent(raster_stack)[1])/3)+1
  y_index = abs(round((y - extent(raster_stack)[4])/3))+1
  return(c(y_index,x_index))
}

plotRGB(crop1_2016_1, r=3,g=2,b=1, stretch="lin",colNA="purple") # plot an image
drawExtent() # execute the function, double click on that pixel and take the xmin and ymin to get index of pixel:
getPixelIndex(crop1_2016_1, 347271.2 ,340107.4) # for exmaple: these two coordinates gives the pixel with the index [99,21]

# function to normalize the raster: devide the actual pixel darkness by the mean pixel darkness of a whole image
normalizeRaster=function(raster){
  raster$darkness = raster[[1]] + raster[[2]] + raster[[3]]
  normalized = (raster$darkness-max(values(raster$darkness)))/(min(values(raster$darkness))-max(values(raster$darkness)))
  return(normalized)
}

# function to get the darkness values of the time series for a specific pixel coordinate
# one function for each extent
getDarknessValuesCrop1 = function(y, x) {
  return(c(normalizeRaster(crop1_2016_1)[y,x],
           normalizeRaster(crop1_2016_2)[y,x],
           normalizeRaster(crop1_2017)[y,x],
           normalizeRaster(crop1_2018)[y,x],
           normalizeRaster(crop1_2019)[y,x]))
}

getDarknessValuesCrop2= function(y, x) {
  return(c(normalizeRaster(crop2_2016_1)[y,x],
           normalizeRaster(crop2_2016_2)[y,x],
           normalizeRaster(crop2_2017)[y,x],
           normalizeRaster(crop2_2018)[y,x],
           normalizeRaster(crop2_2019)[y,x]))
}
getDarknessValuesCrop3 = function(y, x) {
  return(c(normalizeRaster(crop3_2016_1)[y,x],
           normalizeRaster(crop3_2016_2)[y,x],
           normalizeRaster(crop3_2017)[y,x],
           normalizeRaster(crop3_2018)[y,x],
           normalizeRaster(crop3_2019)[y,x]))
}
getDarknessValuesCrop4 = function(y, x) {
  return(c(normalizeRaster(crop4_2016_1)[y,x],
           normalizeRaster(crop4_2016_2)[y,x],
           normalizeRaster(crop4_2017)[y,x],
           normalizeRaster(crop4_2018)[y,x],
           normalizeRaster(crop4_2019)[y,x]))
}

getDarknessValuesCrop5 = function(y, x) {
  return(c(normalizeRaster(crop5_2016_1)[y,x],
           normalizeRaster(crop5_2016_2)[y,x],
           normalizeRaster(crop5_2017)[y,x],
           normalizeRaster(crop5_2018)[y,x],
           normalizeRaster(crop5_2019)[y,x]))
}

# take all indices of pixels that can be expected as corals and put them into a vector:
darknessTimeSeries_Crop1 = c(getDarknessValuesCrop1(99,21), getDarknessValuesCrop1(91,42), getDarknessValuesCrop1(115,40), getDarknessValuesCrop1(137,39), getDarknessValuesCrop1(127,23),
          getDarknessValuesCrop1(30,11), getDarknessValuesCrop1(64,22), getDarknessValuesCrop1(55,34))
table_Crop1 =data.frame(matrix(darknessTimeSeries_Crop1,ncol=5,byrow=TRUE), stringsAsFactors = FALSE) # make a table out of it
colnames(table_Crop1) = c("2016_1", "2016_2","2017", "2018", "2019") # reanme the columns
table_Crop1
