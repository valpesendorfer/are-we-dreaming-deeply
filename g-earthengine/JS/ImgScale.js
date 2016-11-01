// This function scales an imput image to 8bit (0-255) with values clamped bewteen 1-99 percentiles
// currently only implemented for single Band Image!!!


var ImgScale = function(image){

  var bands = image.select("..").bandNames();

  var percentiles = image.reduceRegion({

    reducer: ee.Reducer.percentile([1,99]),
    geometry: image.geomety(),
    scale: image.projection().nominalScale(),
    maxPixels:1e12
  }).values();

  im1 = ee.Image.constant(percentiles.get(0));
  im99 = ee.Image.constant(percentiles.get(1));

  return image.addBands(image.select(bands).subtract(im1).multiply(255).divide(im99.subtract(im1)).round(),null,true);


}
