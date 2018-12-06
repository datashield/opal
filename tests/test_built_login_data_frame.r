
#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#'@title Builds a datagframe to login to datashield
#'@description This function generates a valid data frame, that can be used to login
#'to some data computers (i.e. opal servers). The data frame models a double-entry table. The
#'columns are defined as server, url, user, password and table name. Each row holds the information 
#'in relation to one data computer. The values for each column are passed  to the function as arguments.   
#'
#'@param data.computers.name A vector of characters listing all the names of the data computers
#'@param data.computers.url A vector of characters listing each data computer HTTP address. The format is http://[TCPIP address or host name][:port]
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
#'
#'@export datashield.build.login.data.frame.o
library(opal)
library(testthat)


test_that("format of returned dataframe",
{
    print("format of returned dataframe")
    server <- c("study1", "study2", "study3")
    url <- c("http://192.168.56.100:8080","http://192.168.56.100:8080","http://192.168.56.100:8080")
    user <- c("administrator","administrator","administrator")
    password <- c("datashield_test&","datashield_test&","datashield_test&")
    table <- c("DASIM.DASIM1", "DASIM.DASIM2", "DASIM.DASIM3")
    login.info <-datashield.build.login.data.frame.o(server,url,table,user,password)
    print(login.info)
    expect_that(login.info,is_a("data.frame"))
    expect_that(length(login.info), equals(5))
    print(colnames(login.info)[1])
    expect_that(colnames(login.info)[1], equals('server'))
    expect_that(colnames(login.info)[2], equals('url'))
    expect_that(colnames(login.info)[3], equals('user'))
    expect_that(colnames(login.info)[4], equals('password'))
    expect_that(colnames(login.info)[5], equals('table'))
})

test_that ("correct content",
{
    print("correct content")
    server <- c("study1", "study2", "study3")
    url <- c("http://192.168.56.100:8080","http://192.168.56.100:8080","http://192.168.56.100:8080")
    user <- c("administrator","administrator","administrator")
    password <- c("datashield_test&","datashield_test&","datashield_test&")
    table <- c("DASIM.DASIM1", "DASIM.DASIM2", "DASIM.DASIM3")
    login.info <-datashield.build.login.data.frame.o(server,url,table,user,password)
    print(login.info)
    expect_that(nrow(login.info), equals(length(server)))
   
    urls = as.vector(login.info$url)
    print(urls)
    expect_that(all(startsWith(urls,"http")),is_true())
})


test_that ("incorrect urls",
{
               print("correct content")
               server <- c("study1", "study2", "study3")
               url <- c("192.168.56.100:8080","http://192.168.56.100:8080","http://192.168.56.100:8080")
               user <- c("administrator","administrator","administrator")
               password <- c("datashield_test&","datashield_test&","datashield_test&")
               table <- c("DASIM.DASIM1", "DASIM.DASIM2", "DASIM.DASIM3")
               login.info <-datashield.build.login.data.frame.o(server,url,table,user,password)
               print(login.info)
               expect_that(nrow(login.info), equals(0))
               print(url)
               expect_that(all(startsWith(url,"http")),is_false())
})


