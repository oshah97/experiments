test_dataFrame <- function() {
    checkTrue(is.list(dataFrameRaw))
    checkTrue(is.matrix(mat))
    checkTrue(is.data.frame(col_nodes))
    checkTrue(is.data.frame(row_nodes))
}  
