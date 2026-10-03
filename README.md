# sasxmlimport

`sasxmlimport` imports OpenClinica SAS Data and Syntax exports into R.

The package works with OpenClinica exports containing:

-   `SAS_DATA.xml`
-   `SAS_MAP.xml`
-   `SAS_FORMAT.sas` (optional)

It reconstructs datasets from the XMLMap, converts variables to appropriate R types, optionally applies SAS value labels, and can clean OpenClinica-generated table and variable names.

## Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("lidiyamishieva/sasxmlimport")
```

Then load the package:

``` r
library(sasxmlimport)
```

## Usage

Import an OpenClinica SAS export:

``` r
dat <- import_sas_xml("path/to/SAS_export")
```

The directory should contain at least:

``` text
SAS_DATA.xml
SAS_MAP.xml
```

If `SAS_FORMAT.sas` is available, value labels are applied by default.

## Arguments

``` r
import_sas_xml(
  path,
  apply_formats = TRUE,
  clean_names = FALSE,
  date_formats = c(
    "%Y-%m-%d",
    "%d.%m.%Y",
    "%Y/%m/%d"
  ),
  verbose = TRUE
)
```

-   `path`: directory containing the OpenClinica SAS export files.
-   `apply_formats`: whether to apply labels from `SAS_FORMAT.sas`.
-   `clean_names`: whether to remove leading underscores and trailing numeric identifiers from imported names.
-   `date_formats`: date formats tried when converting XML date values to `Date`.
-   `verbose`: whether to print a short import summary.

Name cleaning is applied after XML extraction, type conversion, format matching, and validation so that source-file references are matched using their original names.

## Returned object

`import_sas_xml()` returns a list with the following components:

-   `typed`: imported tables with original coded values and converted R types.
-   `display`: a display copy with SAS value labels applied where available.
-   `metadata`: variable metadata extracted from `SAS_MAP.xml`.
-   `name_map`: mappings between original and cleaned names.
-   `formats`: parsed SAS format assignments and value definitions.
-   `checks`: import validation results

## License

MIT
