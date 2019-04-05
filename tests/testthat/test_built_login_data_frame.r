#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

library(opal)
library(testthat)


init.correct.data <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.url.http <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('//192.168.56.100:8080','https://datashield.example.org','https://datashield.example.org')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}


init.incorrect.server <- function()
{
    server <- c('study1')
    url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.user <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
    user <- c('administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}


init.incorrect.url <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('//192.168.56.100:8080')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.password <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
    user <- c('administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.table <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('https://datashield.example.org','https://datashield.example.org','https://datashield.example.org')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}



test_that('Incorrect format',
{
  print ('server')
  expect_error(init.incorrect.server())
  print ('URL')
  expect_error(init.incorrect.url())
  print ('password')
  expect_error(init.incorrect.password())
  print ('user')
  expect_error(init.incorrect.user())
  print ('table')
  expect_error(init.incorrect.table())
  print ('end of expectation no 0  -----')
  print ('')
})
          

test_that ('The number of columns is equal five -- correct data',
{
  login.info <- init.correct.data()
  expect_that(login.info,is_a('data.frame'))
  expect_that(length(login.info), equals(5))
  print(colnames(login.info)[1])
  expect_that(colnames(login.info)[1], equals('server'))
  expect_that(colnames(login.info)[2], equals('url'))
  expect_that(colnames(login.info)[3], equals('user'))
  expect_that(colnames(login.info)[4], equals('password'))
  expect_that(colnames(login.info)[5], equals('table'))
  print ('end of expectation no 1  -----')
  print ('')
})

test_that ('The url start with http(s)',
{
  print ('URL')
  expect_error(init.incorrect.url.http())
  print ('correct data')
  login.info <- init.correct.data()
  url <- as.vector(login.info$url)
  expect_that(all(startsWith(url,'http')),is_true())  
  print ('end of expectation no 4 -----')
  print ('')
})


