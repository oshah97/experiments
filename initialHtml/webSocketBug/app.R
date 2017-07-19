library(httpuv)
library(jsonlite)
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
   port <- 8654
   browseURL(sprintf("http://localhost:%d", port))
   wsCon$id <- startDaemonizedServer("0.0.0.0", port, wsCon)
   return(wsCon)

} # init
#--------------------------------------------------------------------------------
demo <- function(wsCon)
{
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
handleResult <- function(message) {
    printf("---handleResult")
    .lastMessage <<- message$payload
    NULL
    }
#--------------------------------------------------------------------------------
