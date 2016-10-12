
// Get min/max values of an image


var ImgMinMax = function(image){
  
  return(
    
    image.reduceRegion({
      reducer: ee.Reducer.minMax(),
      geometry: image.geometry(),
      scale: image.projection().nominalScale(),
      maxPixels: 1e10
    }));
};