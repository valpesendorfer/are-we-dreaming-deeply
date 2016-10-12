// Color palettes for vizualisation in GEE //

// Function to generate random colors

function getRandomColor() {
    var letters = '0123456789ABCDEF';
    var color = '#';
    for (var i = 0; i < 6; i++ ) {
        color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
}

//NDVI

var viz_ndvi = {min: -0.2, max: 0.8, palette: 'FFFFFF, CE7E45, DF923D, F1B555, FCD163, 99B718, 74A901, 66A000, 529400,' +
    '3E8601, 207401, 056201, 004C00, 023B01, 012E01, 011D01, 011301'}; //paletted
	
var viz_ndvi = {min: -0.2, max: 0.8, palette: '000000, 00FF00'}; //stretched black-green



//NDWI 

var viz_ndwi = {min: 0.5, max: 1, palette: ['00FFFF', '0000FF']};


//Sentinel-2

var viz_s2FC = {min:0,max:5000,bands:['B8','B4','B3']}; //False Color Infrared

var viz_s2TC = {min:0,max:5000,bands:['B4','B3','B2']}; //True Color

//Landsat 8

var viz_ls8 = {min: 0, max: 0.5, bands: ['B5', 'B4', 'B3'], gamma: [0.95, 1.1, 1]};
