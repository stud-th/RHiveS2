# HiveS2
Extension of RJDBC package allowing to connect R to HiveServer2 through JDBC driver.

## Installation
``` r
# install.packages("devtools")
devtools::install_github("stud-th/HiveS2")
```
## Connection
``` r
# Loading JDBC HiveDriver and connection:
cp=c("/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/hive-jdbc-1.2.1.spark2-standalone.jar","/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/commons-configuration-1.6.jar","/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/hadoop-common-2.7.3.jar")
.jinit(classpath=cp)
drv <- HiveS2("org.apache.hive.jdbc.HiveDriver","/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/hive-jdbc-1.2.1.spark2-standalone.jar",identifier.quote='`')
conn <- DBI::dbConnect(drv, "jdbc:hive2://localhost:10000/", "", "")
```

