# RHiveS2
Extension of RJDBC package allowing to connect R to HiveServer2 through JDBC driver. Provides support to majority of tidyverse/dplyr functionalities

## Installation
``` r
# install.packages("devtools")
devtools::install_github("stud-th/RHiveS2")
```
## Usage
``` r
# Loading JDBC HiveDriver and connection:
cp=c("hive-jdbc-<version>-standalone.jar","hadoop-common-<version>.jar","commons-configuration-<version>.jar")
.jinit(classpath=cp)
conn <- dbConnect( HiveS2("org.apache.hive.jdbc.HiveDriver","hive-jdbc-<version>.jar",identifier.quote="`"),                          
                       host ="jdbc:hive2://<HS2 host>:",
                       port = "<HS2 port>",
                       schema = "<database>", 
                       user = "<user>", 
                       password = "<password>")


dbListTables(conn)
df <- dplyr::tbl(conn, "foo")
```
## Notice

Some functionalities may not be supported when using the newest release of dbplyr, version < 2.0.0  is recommended

To downgrade package version use:
``` r
devtools::install_version("dbplyr", version = "1.4.4", repos = "http://cran.us.r-project.org")
```
