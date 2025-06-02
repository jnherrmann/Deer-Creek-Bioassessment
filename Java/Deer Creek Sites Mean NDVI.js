// Script for downloading mean annual NDVI for Deer Creek watershed

// Calculates a mean NDVI value for each region in a defined shapefile
// Code adapted from script by N. Kattler (Nov. 2020)
// Full description of that script can be found here: https://knowyourspace.dk/2020/12/20/mean_ndvi_per_polygon_gee/


// Region defined by shapefile saved under your GEE "Assets" tab
// In this case, it's a CSV file with all site locations as coordinate points
var SSI_sites = ee.FeatureCollection('users/jessicaherrmann/SSI-sites');
Map.addLayer(SSI_sites, null, 'SSI_sites');


// CAN MODIFY HERE - Set your date range //
// You can change these start/end dates to fit your dataset.
// This script is set up to calculate mean NDVI by site and USGS water year.
var WYstart = '1999'
var WYend = '2000'
var startdate = WYstart+'-10-01';
var enddate = WYend+'-09-30';



/// PLEASE DO NOT MODIFY BEYOND THIS POINT ///



// MODIS NDVI scale is 0.0001, so we need to correct the scale
var NDVI = function(image) {
  return image.expression('float(b("NDVI")/10000)')
};

// Bands of interest
// We're interested in the NDVI data and also need the SummaryQA band to identify lofi pixels for masking.
var bands = ['NDVI', 'SummaryQA'];

// Create an image collection
var modis = ee.ImageCollection('MODIS/061/MOD13Q1')
  .filterBounds(SSI_sites)
  .filterDate(startdate,enddate)
  .select(bands);

// Pixel quality masking
// We will be masking any low-quality pixels with a code of -1, 2, or 3.

// Explanation of pixel codes:
//     [-1]: Fill/No Data-Not Processed. 
//     [0]:  Good data      - Use with confidence 
//     [1]:  Marginal data  - Useful, but look at other QA information 
//     [2]:  Snow/Ice       - Target covered with snow/ice 
//     [3]:  Cloudy         - Target not visible, covered with cloud


var maskQA = function(modis) {
  var summaryqaband = modis.select('SummaryQA')
    var mask = summaryqaband.neq(-1).and(summaryqaband.neq(2)).and(summaryqaband.neq(3))
        return modis.updateMask(mask);
};

var modismasked = modis.map(maskQA);

// Map.addLayer(modismasked);   // Checking that the mask worked.
                                // You can change the bands masked in the function above to verify.

// print(modismasked, 'MOD13Q1-masked-list');

// Now we select only the NDVI from the list of bands
var modisNDVI = modismasked.map(NDVI)
print('MODIS after .map', modisNDVI)

// This is an ImageCollection, containing a list of images that are within the start date/end date
// We want to make this a single image, aka "mosaic"

var mosaic = modisNDVI.mosaic()
  .clip(SSI_sites);
print('mosaic', mosaic);

// Another alternative is to get the mean of the images from start date/end date:
// var meanNDVI = modisNDVI.mean()
//   .clip(SSI_sites);
// print('meanNDVI', meanNDVI);

// Set visualisation parameters
var colour = {
min: 0.0,
max: 1.0,
palette: [
'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
'66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
'012E01', '011D01', '011301'
],
};

Map.addLayer(mosaic, colour, 'spatial mosaic');

// Add reducer output to the Features in the collection.
var MeansOfFeatures = mosaic.reduceRegions({
  collection: SSI_sites,
  reducer: ee.Reducer.mean(),  // Taking the weighted arithmetic mean of the mosaic
  scale: 250,                  // Scale should match the resolution of the dataset (250m).
});

print(ee.Feature(MeansOfFeatures.first()))

// Remove the content of .geo column (the geometry)
// https://gis.stackexchange.com/a/328257
var featureCollection = MeansOfFeatures.map(function(feat){
  var nullfeat = ee.Feature(null)
  return nullfeat.copyProperties(feat)
})

// Export the featureCollection (table) as a CSV file to your Google Drive account
Export.table.toDrive({
  collection: featureCollection,
  description: 'modis250_ndvi_SSI',
  folder: 'NDVI_pixelcorrected',
  fileFormat: 'CSV'
});

