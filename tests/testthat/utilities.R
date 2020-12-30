HiveS2_TestConnection <- function(){
  # https://stackoverflow.com/questions/46613651/how-to-setup-spark-home-variable
  #spark_home=Sys.getenv("SPARK_HOME")
  cp=c("/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/hive-jdbc-1.2.1.spark2-standalone.jar","/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/commons-configuration-1.6.jar","/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/hadoop-common-2.7.3.jar")
  .jinit(classpath=cp)

  conn <- DBI::dbConnect(HiveS2("org.apache.hive.jdbc.HiveDriver","/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/hive-jdbc-1.2.1.spark2.jar",identifier.quote='`'),
                         host ="jdbc:hive2://localhost:",
                         port = "10000",
                         schema = "default"
  )

}
names_to_as <- function(x, names = names2(x), con = NULL) {
  if (length(x) == 0) {
    return(character())
  }
  names_esc <- sql_escape_ident(con, names)
  as <- ifelse(names == "" | names_esc == x, "", paste0(" AS ", names_esc))

  paste0(x, as)
}
