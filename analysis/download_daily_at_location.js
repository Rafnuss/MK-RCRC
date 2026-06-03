
var name = "Mwamba"
var position = [39.988580, -3.378363] //Mwamba
// var position = [5.914877, 46.117215] // defile de lecluse
Map.addLayer(ee.Geometry.Point(position), {color: 'red'})

var start = "1966-09-01"
var end = "2024-01-01"


var selectors = ['temperature_2m', 'total_precipitation', 'surface_pressure', 'u_component_of_wind_10m', 'v_component_of_wind_10m']
var era5 = ee.ImageCollection("ECMWF/ERA5/DAILY");

// var selectors = 'NDVI'
// var era5 = ee.ImageCollection('MODIS/061/MOD13A2');

var era5_export = era5
    .select(selectors)
    .filter(ee.Filter.date(start, end))
    .map(function (im) { return im.set("time", im.date().format()); })
    .map(function (im) { return im.reduceRegions(ee.FeatureCollection([ee.Feature(ee.Geometry.Point(position))]), ee.Reducer.first(), 1000).first().copyProperties(im, ['time']); });


Export.table.toDrive({
    collection: era5_export,
    fileNamePrefix: 'ERA5_daily_'+ name + '_' + start + '_' + end,
    selectors: selectors,
})

