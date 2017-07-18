
R version 3.4.0 (2017-04-21) -- "You Stupid Darkness"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin15.6.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> > if(identical(getOption('pager'), file.path(R.home('bin'), 'pager'))) options(pager='cat') # rather take the ESS one 
> options(STERM='iESS', str.dendrogram.last="'", editor='emacsclient', show.error.locations=TRUE)
> new()
Error in getClass(Class, where = topenv(parent.frame())) : 
  argument "Class" is missing, with no default
> library(httpuv)
#--------------------------------------------------------------------------------
setup <- function(wsCon)
{
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
         print(rawMessage)
         }) # onMessage
       wsCon$open <- TRUE
       } # onWSOpen

   wsCon

} # setup
#--------------------------------------------------------------------------------
send <- function(wsCon, msg)
{
  wsCon$ws$send(msg)
  
} # send
#--------------------------------------------------------------------------------
wsCon <- new.env(parent=emptyenv())
#--------------------------------------------------------------------------------
demo <- function()
{
   wsCon <- setup(wsCon)
   port <- 8765
   browseURL(sprintf("http://localhost:%d", port))
   wsCon$id <- startDaemonizedServer("0.0.0.0", port, wsCon)

   Sys.sleep(2)
   send(wsCon, "test1")
   Sys.sleep(2)

   send(wsCon, "test2")
   Sys.sleep(2)

} # demo
#--------------------------------------------------------------------------------
library(httpuv)
Error in library(httpuv) : there is no package called ‘httpuv’
> #--------------------------------------------------------------------------------
> setup <- function(wsCon)
+ {
+    wsCon$open <- FALSE
+    wsCon$wsID <- NULL
+    wsCon$ws <- NULL
+    wsCon$result <- NULL
+ 
+    wsCon$call = function(req) { # "call" processes http requests
+      wsUrl = paste(sep='',
+                    '"',
+                   "ws://",
+                   ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
+                   '"')
+     list(
+       status = 200L,
+       headers = list('Content-Type' = 'text/html'),
+       body = c(file="index.html"))
+      }
+    wsCon$onWSOpen = function(ws) {
+       wsCon$ws <- ws
+       ws$onMessage(function(binary, rawMessage) {
+          print(rawMessage)
+          }) # onMessage
+        wsCon$open <- TRUE
+        } # onWSOpen
+ 
+    wsCon
+ 
+ } # setup
> #--------------------------------------------------------------------------------
> send <- function(wsCon, msg)
+ {
+   wsCon$ws$send(msg)
+   
+ } # send
> #--------------------------------------------------------------------------------
> wsCon <- new.env(parent=emptyenv())
> #--------------------------------------------------------------------------------
> demo <- function()
+ {
+    wsCon <- setup(wsCon)
+    port <- 8765
+    browseURL(sprintf("http://localhost:%d", port))
+    wsCon$id <- startDaemonizedServer("0.0.0.0", port, wsCon)
+ 
+    Sys.sleep(2)
+    send(wsCon, "test1")
+    Sys.sleep(2)
+ 
+    send(wsCon, "test2")
+    Sys.sleep(2)
+ 
+ } # demo
> #--------------------------------------------------------------------------------
> 
> demo
function()
{
   wsCon <- setup(wsCon)
   port <- 8765
   browseURL(sprintf("http://localhost:%d", port))
   wsCon$id <- startDaemonizedServer("0.0.0.0", port, wsCon)

   Sys.sleep(2)
   send(wsCon, "test1")
   Sys.sleep(2)

   send(wsCon, "test2")
   Sys.sleep(2)

}
> 