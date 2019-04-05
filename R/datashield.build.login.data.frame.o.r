#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#'@title Builds a dataframe to login to datashield
#'@description This function generates a valid data frame, that can be used to login
#'to some data computers (i.e. opal servers). The data frame models a double-entry table. The
#'columns are defined as server, url, user, password and table name. Each row holds the information 
#'in relation to one data computer. The values for each column are passed  to the function as arguments.   
#'
#'@param data.computers.name A vector of characters listing all the names of the data computers
#'@param data.computers.url A vector of characters listing each data computer HTTP address. The format is https://[TCPIP address or host name][:port]
#'@param data.computers.table.name A vector of characters listing the name of the table stored in a data computer 
#'@param users.id A vector of characters listing a valid user name to log on on each server.
#'@param users.password A vector of characters listing the password for each user to log in to a data computer.
#'@return a data frame formatter in this manner: (server,url,user,password,table). If the arguments are not correct. Then a data.frame with no rows is created.
#'
#'The expactactions are as follow:
#'Expectation no 0: the return value is a data.frame 
#'Expectation no 1: the number of columns is equal 5. 
#'Expectation no 2: the number of rows is equal to the number of servers
#'Expectation no 3: the number of rows is equal to 0, if the length of url, user, or table is smaller than the length of server
#'Expectation no 4: the number of row is 0, if any of the urls does not start with http
#'@author Patricia Ryser-Welch
#'@export
datashield.build.login.data.frame.o <- function (data.computers.name, data.computers.url, data.computers.table.name,  users.id, users.password) 
{
  #assign the arguments to the data frame format.
  server <- as.character(data.computers.name)
  url <- as.character(data.computers.url)
  user <- as.character(users.id)
  password <- as.character(users.password)
  table <- as.character(data.computers.table.name)
  return.data.frame  =  data.frame(server = character(0), user = character(0),user= character(0), password = character(0),table=character(0))
  colnames(return.data.frame) <- c('server','url','user','password','table')
  NO.COLUMNS = 5
  
  #Verify the length of each vector is the same
  expected.elements = length(server) * NO.COLUMNS
  total.elements = length(server) + length(url) + length(user) + length(password) + length(table)
  
  
  if (expected.elements != total.elements)
  {
    stop("The length of the vectors passed as arguments are not the same length.")
  }
  else 
  {
    if (!all(startsWith(url,"http"))) 
    {
      stop("Make sure each url starts with http")
    }
    if (!all(startsWith(url,"https")))
    {
      warning("Each url should starts with https")
    }
    return(data.frame(server,url,user,password,table))
  }
}

