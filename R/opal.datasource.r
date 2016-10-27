#-------------------------------------------------------------------------------
# Copyright (c) 2016 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get datasources from a opal.
#' 
#' @param opal Opal object.
#' @export
opal.datasources=function(opal) {
  .extractJsonField(.get(opal, "datasources"))
}

#' Get a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @export
opal.datasource=function(opal, datasource) {
  .extractJsonField(.get(opal, "datasource", datasource))
}

#' Get tables of a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @export
opal.tables <- function(opal, datasource) {
  .extractJsonField(.get(opal, "datasource", datasource, "tables"));
}

#' Get a table of a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param counts Flag to get the number of variables and entities.
#' @export
opal.table <- function(opal, datasource, table, counts=FALSE) {
  if (counts) {
    .extractJsonField(.get(opal, "datasource", datasource, "table", table, query=list(counts="true"))); 
  } else {
    .extractJsonField(.get(opal, "datasource", datasource, "table", table));  
  }  
}

#' Get variables of a table from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @export
opal.variables <- function(opal, datasource, table) {
  .extractJsonField(.get(opal, "datasource", datasource, "table", table, "variables"))
}

#' Get a variable of a table from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param variable Name of the variable in the table.
#' @export
opal.variable <- function(opal, datasource, table, variable) {
  .extractJsonField(.get(opal, "datasource", datasource, "table", table, "variable", variable))
}

#' Get a vector of values (for each locale) matching the given attribute namespace and name. Vector is null if no such attribute is found.
#' 
#' @param attributes A list of attributes, usually variable or category attributes.
#' @param namespace Optional attribute namespace.
#' @param name Required attribute name.
#' @export
opal.attribute_values <- function(attributes, namespace=NULL, name="label") {
  rval <- c()
  if (length(attributes) == 0) return(rval)
  
  for (attr in attributes) {
    if (identical(attr$name, name) && identical(attr$namespace, namespace) && nchar(attr$value)>0) {
      if (is.null(attr$locale)) {
        rval <- append(rval, attr$value)
      } else {
        rval <- append(rval, paste0("[", attr$locale, "] ", attr$value))  
      }
    }
  }
  rval
}
