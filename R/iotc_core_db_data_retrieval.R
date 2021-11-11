#' Sets the 'debug' mode for DB queries
#' @export
data_debug_queries = function(mode = NA) {
  if(is.na(mode)) {
    debug = as.logical(Sys.getenv("IOTC_LIBS_QUERY_DEBUG"))

    return(!is.na(debug) & debug)
  } else {
    Sys.setenv(IOTC_LIBS_QUERY_DEBUG = mode)
  }
}

#' Performs and SQL query through a provided ODBC connection and return its results as a \code{data.table}
#'
#' @param connection An ODBC connection to a RDBMS server
#' @param query The query to perform
#' @return The results of executing \code{query} through \code{connection} as a data table
#' @examples
#' query(connection = DB_IOTDB(), query = "SELECT * FROM V_LEGACY_NC")
#' query(connection = DB_IOTCSTATISTICS(), query = "SELECT * FROM CL_FISHING_GROUNDS WHERE CL_FISHING_GROUND_TYPE_ID = 2")
#' @export
query = function(connection = DB_IOTDB(), query = "SELECT 1;") {
  if(data_debug_queries())
    print(
      paste(
        "Querying",
        dbGetInfo(connection)$servername,
        "for",
        stri_replace(
          str = stri_replace(
                  str = query,
                  regex = "\\n",
                  replacement = " ",
                  mode = "all"
          ),
          regex = "[\\s]+",
          replacement = " ",
          mode = "all")
      )
    )


  return(
    data.table(
      dbGetQuery(
        connection,
        query
      )
    )
  )
}

#' Performs an SQL query onto an instance of the IOTDB database hosted by a specified MSSQL server and returns its results as a \code{data.table}
#'
#' @param server The server name / IP hosting the IOTDB database
#' @param query The query to perform
#' @return A data table with the results of executing \code{query} on the instance of IOTDB hosted by \code{server}
#' @examples
#' query(server = "IOTCS09", query = "SELECT * FROM V_LEGACY_NC")
#' query(server = "localhost", query = "SELECT * FROM CL_FISHING_GROUNDS WHERE CL_FISHING_GROUND_TYPE_ID = 2")
#' @export
query_IOTDB = function(server = "IOTCS09", query = "SELECT 1;") {
  return (query(DB_IOTDB(server), query))
}

#' Performs an SQL query onto an instance of the IOTC_STATISTICS database hosted by a specified MSSQL server and returns its results as a \code{data.table}
#'
#' @param server The server name / IP hosting the IOTDB database
#' @param query The query to perform
#' @return A data table with the results of executing \code{query} on the instance of IOTC_STATISTICS hosted by \code{server}
#' @examples
#' query(query = "SELECT * FROM CL_FISHING_GROUNDS WHERE CL_FISHING_GROUND_TYPE_ID = 2")
#' query(server = "localhost", query = "SELECT * FROM CL_FISHING_GROUNDS WHERE CL_FISHING_GROUND_TYPE_ID = 2")
#' @export
query_IOTCSTATISTICS = function(server = "IOTCS09", query = "SELECT 1;") {
  return (query(DB_IOTCSTATISTICS(server), query))
}
