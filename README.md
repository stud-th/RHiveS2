# RHiveS2
Extension of RJDBC package allowing to connect R to HiveServer2 through JDBC driver.

## Installation
``` r
# install.packages("devtools")
devtools::install_github("stud-th/RHiveS2")
```
## Connection
``` r
# Loading JDBC HiveDriver and connection:
cp=c("hive-jdbc-<version>-standalone.jar","hadoop-common-<version>.jar","commons-configuration-<version>.jar")
.jinit(classpath=cp)
drv <- JDBC("org.apache.hive.jdbc.HiveDriver","hive-jdbc-<version>.jar",identifier.quote="`")
conn <- dbConnect( HiveS2("org.apache.hive.jdbc.HiveDriver","hive-jdbc-<version>.jar",identifier.quote="`"),                          host ="jdbc:hive2://<HS2 host>:",
                       port = "<HS2 port>",
                       schema = "<database>", 
                       user = "<user>", 
                      password = "<password>")


dbListTables(conn)
```

