
#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#'@title test the function datashield.build.login.data.frame.o(server,url,table,user,password)
#'@description These functions set the expectations of the function https://www.tidyverse.org/articles/2017/12/testthat-2-0-0/

#'The expectations are as follow:
#'Expectation no 0: the return value is a data.frame 
#'Expectation no 1: the number of columns is equal 5. 
#'Expectation no 2: the number of rows is equal to the number of servers
#'Expectation no 3: the number of rows is equal to 0, if the length of url, user, or table is smaller than the length of server
#'Expectation no 4: the number of row is 0, if any of the urls does not start with http
#'
#'@author Patricia Ryser-Welch

library(opal)
library(testthat)


init.correct.data <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('http://192.168.56.100:8080','http://192.168.56.100:8080','http://192.168.56.100:8080')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.url.http <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('//192.168.56.100:8080','https://192.168.56.100:8080','https://192.168.56.100:8080')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}


init.incorrect.server <- function()
{
    server <- c('study1')
    url <- c('https://192.168.56.100:8080','https://192.168.56.100:8080','https://192.168.56.100:8080')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.user <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('https://192.168.56.100:8080','https://192.168.56.100:8080','https://192.168.56.100:8080')
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
    url <- c('https://192.168.56.100:8080','https://192.168.56.100:8080','https://192.168.56.100:8080')
    user <- c('administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM1', 'DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}

init.incorrect.table <- function()
{
    server <- c('study1', 'study2', 'study3')
    url <- c('https://192.168.56.100:8080','https://192.168.56.100:8080','https://192.168.56.100:8080')
    user <- c('administrator','administrator','administrator')
    password <- c('datashield_test&','datashield_test&','datashield_test&')
    table <- c('DASIM.DASIM2', 'DASIM.DASIM3')
    return(datashield.build.login.data.frame.o(server,url,table,user,password))
}



test_that('Expectation no 0 - return a data frame',
{
   print('Expectation no 0 : format of returned dataframe ------')
  
   for (i in 1:6)
   {
       if (i == 1)
       {
           print ('server')
           login.info <- init.incorrect.server()
       }
       else if (i == 2)
       {
           print ('URL')
           login.info <- init.incorrect.url()
       }
       else if (i ==3)
       {
           print ('password')
           login.info <- init.incorrect.password()
       }
       else if (i == 4)
       {
           print ('user')
           login.info <- init.incorrect.user()
       }
       else if (i == 5)
       {
           print ('table')
           login.info <- init.incorrect.table()
       }
       else if (i == 6)
       {
           print ('correct data')
           login.info <- init.correct.data()
       }
       expect_that(login.info,is_a('data.frame'))
   }
    
   print ('end of expectation no 0  -----')
   print ('')
   
 })
          

test_that ('Expectation no 1 - the number of columns is equal five -- correct data',
{
    print('Expectation no 1 - the number of columns is equal five ---- ')
    
    for (i in 1:6)
    {
        if (i == 1)
        {
            print ('server')
            login.info <- init.incorrect.server()
        }
        else if (i == 2)
        {
            print ('URL')
            login.info <- init.incorrect.url()
        }
        else if (i ==3)
        {
            print ('password')
            login.info <- init.incorrect.password()
        }
        else if (i == 4)
        {
            print ('user')
            login.info <- init.incorrect.user()
        }
        else if (i == 5)
        {
            print ('table')
            login.info <- init.incorrect.table()
        }
        else if (i == 6)
        {
            print ('correct data')
            login.info <- init.correct.data()
        }
    
        
        expect_that(length(login.info), equals(5))
        print(colnames(login.info)[1])
        expect_that(colnames(login.info)[1], equals('server'))
        expect_that(colnames(login.info)[2], equals('url'))
        expect_that(colnames(login.info)[3], equals('user'))
        expect_that(colnames(login.info)[4], equals('password'))
        expect_that(colnames(login.info)[5], equals('table'))
    }    
    
    print ('end of expectation no 1  -----')
    print ('')
})


test_that (' Expections no 2 and 3 - The number of rows is equal to the number of servers',
{
    print('Expectations no 2 and 3 - the number of rows is equal five or 0 ---- ')
    
    for (i in 1:6)
    {
        if (i == 1)
        {
            print ('server')
            login.info <- init.incorrect.server()
        }
        else if (i == 2)
        {
            print ('URL')
            login.info <- init.incorrect.url()
        }
        else if (i ==3)
        {
            print ('password')
            login.info <- init.incorrect.password()
        }
        else if (i == 4)
        {
            print ('user')
            login.info <- init.incorrect.user()
        }
        else if (i == 5)
        {
            print ('table')
            login.info <- init.incorrect.table()
        }
        else if (i == 6)
        {
            print ('correct data')
            login.info <- init.correct.data()
        }
        
        server <- as.vector(login.info$server)
        
        if (i == 6) #correct data 
        {    
            expect_that(nrow(login.info), equals(length(server)))
        }
        else # incorrect data
        {
            expect_that(nrow(login.info), equals(0))
        }
    }    
    
    print ('end of expectations no 2 and no 3 -----')
    print ('')
    
})
    


test_that ('Expectation no 4 - The url start with http ',
{
    print('Expectation no 4 - the url stars with http ---- ')   
    for (i in 1:2)
    {
        if (i == 1)
        {
            print ('URL')
            login.info <- init.incorrect.url.http()
            expect_that(nrow(login.info), equals(0))
           
        }
        else if (i == 2) # correct data
        {
            print ('correct data')
            login.info <- init.correct.data()
            url <- as.vector(login.info$url)
            expect_that(all(startsWith(url,'http')),is_true())
        }
    }
    
    
    print ('end of expectation no 4 -----')
    print ('')
})


