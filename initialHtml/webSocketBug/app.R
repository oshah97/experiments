library(httpuv)
library(jsonlite)
library(RUnit)
#--------------------------------------------------------------------------------
.lastMessage <- NULL;
#--------------------------------------------------------------------------------
configureWebSocketServer <- function(wsCon)
{
   wsCon <- new.env(parent=emptyenv())
   wsCon$open <- FALSE
   wsCon$wsID <- NULL
   wsCon$ws <- NULL
   wsCon$result <- NULL

   wsCon$call = function(req) { # "call" processes http requests
     wsUrl = paste(sep='',
                   '"',
                  "ws://",
                  ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                  '"')
    list(
      status = 200L,
      headers = list('Content-Type' = 'text/html'),
      body = c(file="index.html"))
     }
   wsCon$onWSOpen = function(ws) {
      wsCon$ws <- ws
      ws$onMessage(function(binary, rawMessage) {
          #print(fromJSON(rawMessage))
          .lastMessage <<- fromJSON(rawMessage);
         }) # onMessage
       wsCon$open <- TRUE
       } # onWSOpen

   return(wsCon)

} # configureWebSocketServer
#--------------------------------------------------------------------------------
my.send <- function(wsCon, msg)
{
  #  browser()
    wsCon$ws$send(toJSON(msg, auto_unbox=TRUE))

} # send
#--------------------------------------------------------------------------------
init <- function()
{
   wsCon <- configureWebSocketServer()
   port <- 8690
   browseURL(sprintf("http://localhost:%d", port))
   wsCon$id <- startDaemonizedServer("0.0.0.0", port, wsCon)
   return(wsCon)

} # init
#--------------------------------------------------------------------------------
demo <- function(wsCon) {
    
   printf("about to send test1 to browser for capitalization")
   my.send(wsCon, "test1")
   Sys.sleep(2)

   printf("about to send a second message...");
   my.send(wsCon, "WebSocket Works!")


} # demo
#--------------------------------------------------------------------------------
toUpperCase <- function(wsCon, string)
{
    .lastMessage <<- NULL
    msg <- list(cmd="toUpperCase", callback="handleResult", payload=string)
    my.send(wsCon, msg)

    while(is.null(.lastMessage)){
       Sys.sleep(0.1)
       }

    return(.lastMessage$payload)

} # toUPpperCase
#--------------------------------------------------------------------------------
toLowerCase <- function(wsCon, string)
{
    .lastMessage <<- NULL
    msg <- list(cmd="toLowerCase", callback="handleResult", payload=string)
    my.send(wsCon, msg)

    while(is.null(.lastMessage)){
       Sys.sleep(0.1)
       }

    return(.lastMessage$payload)

} # toLowerCase
#--------------------------------------------------------------------------------
toVectorSum <-  function(wsCon, vector=c(1,2,3)) {
    
    .lastMessage <<- NULL
    msg <- list(cmd="toVectorSum", callback="handleResult", payload=vector)
    my.send(wsCon, msg)

    while(is.null(.lastMessage)){
        Sys.sleep(0.1)
        }
    return(.lastMessage$payload)
    
    } #toVectorSum
#--------------------------------------------------------------------------------
toMatrixPrint <- function(wsCon, mat=matrix(
                                     c(1,2,3,4,5,6,7,8,9),
                                     nrow=3,
                                     ncol=3,
                                     byrow=TRUE)) {
    
    .lastMessage <<- NULL
    msg <- list(cmd="toMatrixPrint",
                callback="handleResult",
                payload=mat)
    my.send(wsCon, msg)
    while(is.null(.lastMessage)) {
          Sys.sleep(0.1)
          }
    return(.lastMessage$payload)
    
    }#toMatrixPrint
#--------------------------------------------------------------------------------
tortureTest <- function() {

    print("=== test sends differnt sized matrices through a websocket and prints time is takes to receive the return message")
    print("=== tests eight matrices; final matrix is 500 x 1000; results print below matrix size text")
    print("=== test estimated to take between 10 - 15 minutes")

    mat <- matrix(1:100, nrow=10)
    print("=== 10x10 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))
    
    mat <- matrix(1:10000, nrow=100)
    print("=== 100x100 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))
    
    mat <- matrix(1:40000, nrow=200)
    print("=== 200x200 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))


    mat <- matrix(1:90000, nrow=300)
    print("=== 300x300 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))

    mat <- matrix(1:160000, nrow=400)
    print("=== 400x400 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))

    mat <- matrix(1:250000, nrow=500)
    print("=== 500x500 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))

    mat <- matrix(1:375000, nrow=750)
    print("=== 500x750 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))

    mat <- matrix(1:500000, nrow=1000)
    print("=== 500x1000 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))

    }#TortureTest
#--------------------------------------------------------------------------------
tortureTestSmall <- function() {
    
    print("=== test 100 x 100 matrix")
    mat <- matrix(1:10000, nrow=100)
    print(system.time(mat2 <- toMatrixPrint(wsCon, mat)))
    print(paste(sep=" ",
                'Number of bytes:',
                nchar(toJSON(.lastMessage))))
    print(paste(sep=" ",
                'Number of rows:',
                nrow(mat2),
                'Number of columns:',
                ncol(mat2)))
    
    }#tortureTestSmall
#--------------------------------------------------------------------------------
tortureTestMedium <- function() {
    
    mat <- matrix(1:100, nrow=10)
    print("=== 10x10 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))
    print(paste(sep=" ",
                'Number of bytes:',
                nchar(toJSON(.lastMessage))))
    print(paste(sep=" ",
                'Number of rows:',
                nrow(mat2),
                'Number of columns:',
                ncol(mat2)))
    
    mat <- matrix(1:10000, nrow=100)
    print("=== 100x100 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))
    print(paste(sep=" ",
                'Number of bytes:',
                nchar(toJSON(.lastMessage))))
    print(paste(sep=" ",
                'Number of rows:',
                nrow(mat2),
                'Number of columns:',
                ncol(mat2)))
    
    mat <- matrix(1:40000, nrow=200)
    print("=== 200x200 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))
    print(paste(sep=" ",
                'Number of bytes:',
                nchar(toJSON(.lastMessage))))
    print(paste(sep=" ",
                'Number of rows:',
                nrow(mat2),
                'Number of columns:',
                ncol(mat2)))


    mat <- matrix(1:90000, nrow=300)
    print("=== 300x300 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))
    print(paste(sep=" ",
                'Number of bytes:',
                nchar(toJSON(.lastMessage))))
    print(paste(sep=" ",
                'Number of rows:',
                nrow(mat2),
                'Number of columns:',
                ncol(mat2)))

    mat <- matrix(1:160000, nrow=400)
    print("=== 400x400 matrix")
    print(system.time((mat2 <- toMatrixPrint(wsCon, mat))))
    print(paste(sep=" ",
                'Number of bytes:',
                nchar(toJSON(.lastMessage))))
    print(paste(sep=" ",
                'Number of rows:',
                nrow(mat2),
                'Number of columns:',
                ncol(mat2)))

    }#tortureTestMedium
#--------------------------------------------------------------------------------
handleResult <- function(message) {
    
    printf("---handleResult")
    .lastMessage <- message$payload
    NULL

    }#handleResult
#--------------------------------------------------------------------------------
test_packageVersion <- function() {
    
    checkTrue(packageVersion("RUnit") >= "0.4.31")
    checkTrue(packageVersion("jsonlite") >= "1.5")
    checkTrue(packageVersion("httpuv") >= "1.3.5")

    }#test_packageVersion
#--------------------------------------------------------------------------------
