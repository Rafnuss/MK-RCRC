library(frictionless)
library(GeoLocatoR)
library(zen4R)

## Publish Data Pacakge
# See https://raphaelnussbaumer.com/GeoLocatoR/articles/create-from-geopressuretemplate.html

# Create the datapackage
zenodo <- ZenodoManager$new(token = keyring::key_get(service = "ZENODO_PAT"))

z <- zenodo$getDepositionByConceptDOI("10.5281/zenodo.16804931")
pkg <- zenodo_to_gldp(z)

# Add data
pkg <- add_gldp_geopressuretemplate(pkg)

# print(pkg)

# Check package
validate_gldp(pkg)

dir.create("data/datapackage", showWarnings = FALSE)
write_package(pkg, "data/datapackage/")
