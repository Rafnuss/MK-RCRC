var era5 = ee.ImageCollection("ECMWF/ERA5_LAND/HOURLY");

// Example script to load and visualize ERA5 climate reanalysis parameters in
// Google Earth Engine
var stepDuration = 24 // in ms
var start = "2019-01-01"
var end = "2019-01-03"
var location = ee.FeatureCollection([ee.Feature(ee.Geometry.Point([-3.378363, 39.988580]))]);

var oeel = require('users/OEEL/lib:loadAll')
print(era5.first())
// Daily mean 2m air temperature
era5 = era5
    .select(['temperature_2m', 'total_precipitation', 'surface_pressure', 'u_component_of_wind_10m', 'v_component_of_wind_10m'])
    .filter(ee.Filter.date(start, end));



var daily = oeel.ImageCollection.movingWindow({
    collection: era5,
    filter: ee.Filter.maxDifference(stepDuration / 2 * 3600 * 1000, "system:time_start", null, "system:time_start"),
    reducer: ee.Reducer.mean().combine(ee.Reducer.sum(), '', true),
    copyProperties: true,
    estimationCollection: oeel.FeatureCollection.fromList({
        list: ee.List.sequence(ee.Date(start).advance(stepDuration / 2, 'hour').millis(), ee.Date(end).millis(), stepDuration * 3600 * 1000),
        propertyName: "system:time_start"
    })
})

daily = daily.map(function (im) { return im.set("time", im.date().format()); })

print(daily.first())

var sampled = daily.map(function (im) {
    return im.reduceRegions(location, ee.Reducer.first(), 1000).first().copyProperties(im, ['time']);
});

print(sampled.first())

Export.table.toDrive({
    collection: sampled,
    //	description:,
    //	folder:,
    fileNamePrefix: 'extraact_ERA5_' + start + '_' + end,
    //	fileFormat:,
    selectors: ['temperature_2m_mean', 'total_precipitation_sum', 'surface_pressure_mean', 'u_component_of_wind_10m_mean', 'v_component_of_wind_10m_mean', 'time'],
    //	maxVertices:,
})