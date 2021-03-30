
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lsatTS - an R package to make sense of Landsat greeneness time-series

<!-- badges: start -->

<!-- badges: end -->

**Logan T. Berner, Jakob J. Assmann, Richard Massey (TBC), Signe Normand
and Scott Goetz**

Building upon the workflow developed for [Berner et
al. 2020](https://www.nature.com/articles/s41467-020-18479-5) the
**lsatTS** package facilitates:

  - The export of time-series for the whole Landsat record from point
    coordinates (“sites”).
  - The cross-calibration of VIs derived from the time-series to account
    for sparse observations.
  - The definition of growing season characteristics such as the maximum
    NDVI.

## Content

1.  [Installation](#1-installation)
2.  [Preparation and extraction](#2-preparation-and-extraction)
3.  [Cross calibration of
    time-series](#3-cross-calibration-of-time-series)
4.  [Defining growing season
    characteristics](#4-defining-growing-season-characteristics)
5.  [Citation](#5-citation)
6.  [Contact](#6-contact)
7.  [Contributions](#7-contributions)
8.  [References](#8-references)
9.  [License](#9-license)

## 1\. Installation

You can install the package using `devtools` as follows:

``` r
# install.packages("devtools")
devtools::install_github("jakobjassmann/lsatTS")
```

For the preparation and extractions scripts you will also have to make
sure you have the `rgee` package installed, fully configured and the
Earth Engine initalized for the current R session. You can find out how
to do that on the [rgee website](https://r-spatial.github.io/rgee/).

-----

[\[to top\]](#content)

## 2\. Preparation and extraction

Before you start you will have to determine whether you will extract
data for point coordinates or for a polygon area.

Please note, the time-series extraction from the Google Earth Engine
*only* work on a point coordinate basis. For each coordinate supplied
`lsat_extract_ts()` the function will pull down all Landsat pixels that
overlap with this coordinate.

If you don’t have point coordinates, but rather an area of interest you
would like to retrieve the time-series for, you can determine all pixel
centers from a Landsat 8 scene that fall within your polygon using the
`lsat_get_pixel_centres()` function. See flow chart below. This section
shows examples for using both functions.

![](man/figures/Landsat%20R%20Package%20-%20Preparation%20and%20Extraction.jpg)

**Setting up the environment**

Let’s prepare the environment for the extractions:

``` r
# Load packages for data handling etc.
library(sf)
library(dplyr)
library(purrr)
library(rgee)

# Load lsatTS package
# library(lsatTS)
source("R/lsat_get_pixel_centers.R") # @Logan this will eventually be done by just loading the package
source("R/lsat_export_ts.R") # @Logan this will eventually be done by just loading the package

# Intialize the Earth Engine with rgee
ee_Initialize()
```

**Getting pixel centers using lsat\_get\_pixel\_centers()**

Next, we assume you have no point coordinates ready yet, but would like
to extract the Landsat time-series for a polygon. So we start with
`lsat_get_pixel_centers()`. This function is a convenience helper
function that determines the Landsat 8 grid pixel centers within a
polygon (adding an optional buffer). Below are two examples that show
how it works.

*Tip: You can download the WRS2 scene boundaries kml file from USGS and
specify it in the function call to avoid downloading it every time the
function is called. See `?lsat_get_pixel_centers` for more info.*

**Please note:** It is not advisable to determine pixel centers for very
large polygons. See `?lsat_get_pixel_centers` for more on this.

*First*, a single polygon:

``` r
# Specify a region 
test_poly <- st_polygon(
list(matrix(c(-138.90125, 69.58413,
               -138.88988, 69.58358,
               -138.89147, 69.58095,
               -138.90298, 69.57986,
               -138.90125, 69.58413),
             ncol = 2, byrow = T)))
test_poly_sf <- st_sf(st_sfc(test_poly, crs = 4326))

# Use lsat_get_pixel_centers to retrieve pixel centers and plot to a file that can be added to this documentation.
# We set plot_map to T to view 
pixel_list_test_poly <- lsat_get_pixel_centers(test_poly_sf, plot_map = "man/figures/lsat_get_pixel_centers.png")
```

Here is a capture of what you would see in the map view port of R
Studio:

![](man/figures/lsat_get_pixel_centers.png)

*Second*, for multiple polygons:

``` 

## Ge pixel centers for multiple regions
# Create multi-polygon sf
ellesmere <- st_polygon(list(matrix(c(-75.78526, 78.86973, 
                                      -75.78526, 78.87246,
                                      -75.77116, 78.87246, 
                                      -75.77116, 78.86973, 
                                      -75.78526, 78.86973),
                                      ncol = 2, byrow = T)))
yamal <- st_polygon(list(matrix(c(68.54580, 70.18874, 
                                  68.54580, 70.19145,
                                  68.55379, 70.19145, 
                                  68.55379, 70.18874,
                                  68.54580, 70.18874), 
                                  ncol = 2, byrow = T)))
toolik <- st_polygon(list(matrix(c(-149.60686, 68.62364, 
                                   -149.60686, 68.62644, 
                                   -149.59918, 68.62644, 
                                   -149.59918, 68.62364, 
                                   -149.60686, 68.62364), 
                                   ncol = 2, byrow = T)))
test_regions_sf <- st_sfc(ellesmere, yamal, toolik, crs = 4326) %>% st_sf() %>%
   mutate(region = c("ellesmere", "yamal", "toolik"))

# Split and map lsat_get_pixel_centers using dplyr and purrr wihout plotting
pixel_list <- test_regions_sf %>%
    split(.$region) %>%
    map(lsat_get_pixel_centers,
        pixel_prefix_from = "region") %>%
    bind_rows()
  
# Let's look at the returned sf object:
pixel_list
```

**Exporting time-series from the Earth Engine using
lsat\_extract\_ts()**

Now that we have point coordinates ready we can use `lsat_extract_ts()`
to extract the Landsat time-series from the Earth Engine. See below for
how it is done. To speed things up for this tutorial we define a small
number of points grabbed from the `pixel_list` defined above.

The `lsat_extract_ts()` function will accept any sf pbject that has got
a point feature collection. It also requires one column with unique
identifiers for each “site”/“pixel\_id” these can be specified with
`pixel_id_from =`. If you have an attribute column called “pixel\_id”
such as that generated by `lsat_get_pixel_centers()` you will not have
to specify anything extra.

`lsat_extract_ts()` issues a task to the Earth Engine that exports the
data to your Google Drive. The output folder is by default
`/lsatTS_export/` you can change the folder using the relevant arguments
(see `?lsat_extract_ts`). More importantly is though that for larger
data sets of points the time-series will have to be export in chunks.
You can let the function chunk the data automatically (no argumnets
needed), set the chunk size (ue `max_chunk_size =`) or define the chunks
based on a column in the dataset (use `chunks_from =`). Examples for all
are shown below.

**Please note:** There is a reason we decided to export the data in
small chunks. For this function there are two bottlenecks: 1) transfer
of the point data to the Earth Engine and 2) export of time-series from
the Earth Engine. The latter is particularly important. Larger chunks
are prone to cause more errors and exceed the user limit set on exports
by Google. It is safer and perhaps more efficient to issue smaller
chunks and bind them back together later rather than exporting one big
mass of time-series. We found that 250 points is a happy medium (at time
of writing about 3h export).

    # Generate test points
    test_points_sf <- st_sfc(sf::st_point(c(-149.6026, 68.62574)),
                              sf::st_point(c(-149.6003, 68.62524)),
                              sf::st_point(c(-75.78057, 78.87038)),
                              sf::st_point(c(-75.77098, 78.87256)),
                              sf::st_point(c(68.54736, 70.19058)),
                              sf::st_point(c(68.54814, 70.19112)), crs = 4326) %>%
       st_sf() %>%
       mutate(pixel_id = c("toolik_1",
                           "toolik_2",
                           "ellesmere_1",
                           "ellesmere_1",
                           "yamal_1",
                         "yamal_2"),
              region = c("toolik", "toolik",
                         "ellesmere", "ellesmere",
                         "yamal", "yamal"))
     # Export time-series using lsat_export_ts()
     task_list <- lsat_export_ts(test_points_sf)
    
     # Export time-series using with a chunk size of 2
     # task_list <- lsat_export_ts(test_points_sf, max_chunk_size = 2)
    
     # Export time-series in chunks by column
     # task_list <- lsat_export_ts(test_points_sf, chunks_from = "region")

The function returns the task objects generated by rgee for each chunk
to be exported. You can monitor progress of the task using rgee’s
ee\_monitoring() or the GEE WebAPI.

*Tip: Should the export of a chunk fail for some reason you can reissue
the export task using the `this_chunk_only =` option. See below.*

``` r
# re-export a chunk
# reexport_task <- lsat_export_ts(test_points_sf, chunks_from = "region", this_chunk_only = "yamal")
```

Once the tasks are completed, open your Google Drive and check the
`/lsatTS_export/` folder (or the folder you specified) and retrieve the
data to process it in steps 3 and 4 below. Please note that you can use
the Google Drive Backup tool as well as rgee’s `ee_drive_to_local()`
[function](https://r-spatial.github.io/rgee/reference/ee_drive_to_local.html)
to copy the data automatically to a local drive.

[\[to top\]](#content)

## 3\. Cross-calibration of time-series

![](man/figures/Landsat%20R%20Package%20-%20Cross-calibration%20of%20time-series.jpg)

[\[to top\]](#content)

## 4\. Defining growing season characteristics

![](man/figures/Landsat%20R%20Package%20-%20Defining%20growing%20season%20characteristics.jpg)

[\[to top\]](#content)

## 5\. Citation

When using this package please citing:

Berner, Logan T., Richard Massey, Patrick Jantz, Bruce C. Forbes, Marc
Macias-Fauria, Isla Myers-Smith, Timo Kumpula, et al. 2020. Summer
Warming Explains Widespread but Not Uniform Greening in the Arctic
Tundra Biome. Nature Communications 11, no. 1: 4621.
<https://doi.org/10.1038/s41467-020-18479-5>.

[\[to top\]](#content)

## 6\. Contact

Logan T. Berner and Jakob J. Assmann Email Logan at:
<Logan.Berner@nau.edu> Email Jakob at: <j.assmann@bio.au.dk>

[\[to top\]](#content)

## 7\. Contributions

Logatn T. Berner wrote the analysis functions (items 3 and 4). Jakob J.
Assmann wrote the extraction and preparation functions (item 1). Richard
Massey wrote the original Python code for the `lsat_export_ts()`
function, later refined and transferred to JavaScript and R by Jakob
Assmann. Singe Normand and Scoet Goetz provided funding (?) and
mentorship for this project. Jakob J. Assmann faciliated package
development. Logan T Berner and Jakob J. Assmann prepared the code for
publication and wrote the documnetation.

Thank you to all the testers: Tester A, Tester B and Tester C.

[\[to top\]](#content)

## 8\. References

**rgee**

C Aybar, Q Wu, L Bautista, R Yali and A Barja (2020) rgee: An R package
for interacting with Google Earth Engine Journal of Open Source Software
URL <https://github.com/r-spatial/rgee/>.

## 9\. License

[MIT LICENSE](LICENSE)

[\[to top\]](#content)
