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
