#' @import methods
#' @import RJDBC
#' @include HiveS2Driver.R helper-extr-pcgs.R
#'
#'
#' dbConnect function adapted from RJDBC creating and setting parameters for HiveS2Connection
#' @param drv HiveS2Driver object
#' @export
#' @rdname HiveS2Connection-class
setMethod("dbConnect", "HiveS2Driver",
          function(
            drv,
            host,
            port,
            schema=NULL,
            user='',
            password='',
            ...
          )
          {
            #check dbplyr version
            dbplyr_version <- try(as.character(utils::packageVersion('dbplyr')))
            if (inherits(dbplyr_version, 'try-error')) {
              warning('dbplyr not available')
            } else if (utils::compareVersion(dbplyr_version, '1.4.4') < 0) {
              warning(paste('recommended dbplyr version is =< 1.4.4. Using version ', as.character(utils::packageVersion('dbplyr')), "may couse unexpected errors."))
            }

            jc <- .jcall("java/sql/DriverManager","Ljava/sql/Connection;","getConnection", as.character(paste(host,port,"/",schema,sep = ""))[1], as.character(user)[1], as.character(password)[1], check=FALSE)
            if (is.jnull(jc) && !is.jnull(drv@jdrv)) {
              oex <- .jgetEx(TRUE)
              p <- .jnew("java/util/Properties")
              if (length(user)==1 && nzchar(user)) .jcall(p, "Ljava/lang/Object;", "setProperty", "user", as.character(user))
              if (length(password)==1 && nzchar(password)) .jcall(p, "Ljava/lang/Object;", "setProperty", "password", as.character(password))
              l <- list(...)
              if (length(names(l))) for (n in names(l)) .jcall(p, "Ljava/lang/Object;", "setProperty", n, as.character(l[[n]]))
              jc <- .jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", as.character(as.character(paste(host,port,"/",schema,sep = "")))[1], p, check=FALSE)
              print(as.character(paste(host,port,"/",schema,sep = "")))
            }
            .verify.JDBC.result(jc, "Unable to connect to the client")
            new("HiveS2Connection", jc=jc,
                identifier.quote=drv@identifier.quote,
                host = host,
                port = port,
                schema_name = schema,
                username = as.character(user))},
          valueClass="HiveS2Connection")
