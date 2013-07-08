RODProt
=======

An R Client for Interacting with Data Encoded in one of the [Open Data Protocols](http://www.dataprotocols.org/en/latest/)  Standards. This package currently supports working with [JSON Table Schemas](http://www.dataprotocols.org/en/latest/json-table-schema.html) and [Data Packages](http://www.dataprotocols.org/en/latest/data-packages.html), but we plan to continue development to support other formats, as well.

## Installation

You can run the following code to use the [devtools](https://github.com/hadley/devtools) R package to install this package from this GitHub repo:

    library(devtools)
    install_github("RODProt", "QBRC")

which should then allow you to load and use the package, e.g.

    > library(RODProt)
    > read_json_table("http://raw.github.com/QBRC/RODProt/4f401d0773bcfa613c6640fad8cce356b28ecf95/inst/extdata/mixed.json")
      A       B
    1 4    test
    2 5 another
    3 6   final

Or to get some more traditional CSV data as it might exist in the Simple Data Format:

    > library(RODProt)
	> pkg <- read_data_package("http://raw.github.com/datasets/house-prices-us/0b5042bbe9a2c2537a3bd708287321396b534710/datapackage.json")
	> file <- get_resource(pkg, "national", overlook.types=TRUE)
	> file
	   Year Composite.US
	1  Year           27
	2  1987           14
	3  1988           15
	4  1989           16
	5  1990           20
	6  1991           17
	7  1992           18
	8  1993           19
	9  1994           21
	10 1995           22
	11 1996           23
	12 1997           24
	13 1998           25
	14 1999           26
	15 2000            1
	16 2001            2
	17 2002            3
	18 2003            8
	19 2004           10
	20 2005           11
	21 2006           13
	22 2007           12
	23 2008            9
	24 2009            6
	25 2010            7
	26 2011            4
	27 2012            5
    
Note that the `overlook.types` parameter must be set to `TRUE` if you're attempting to load a file that has an unsupported data type (such as `date`). Support for more data types is in the works, though patches are welcome!

Similarly, we can download data from http://data.okfn.org

	> library(RODProt)
	> pkg <- read_data_package("http://data.okfn.org/data/cpi/datapackage.json")
	> cpi <- get_resource(pkg, "cpi.csv", overlook.types=TRUE)
	Warning message:
	In get_resource(pkg, "cpi.csv", overlook.types = TRUE) :
	  Both a relative path and a global URL were specified for a resource. Giving preference to the URL.
	> head(cpi)
	  Country.Name Country.Code Year       CPI
	1  Afghanistan          AFG 2004  89.16959
	2  Afghanistan          AFG 2005 100.00000
	3  Afghanistan          AFG 2006 103.48966
	4  Afghanistan          AFG 2007 121.03186
	5  Afghanistan          AFG 2008 148.51621
	6  Afghanistan          AFG 2009 128.87374
	
Again, the `overlook.types` directive will be necessary until types such as dates are supported.
