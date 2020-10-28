spearman_cor <- function(df, col_name1, col_name2) {
    data <- cbind(df[[col_name1]], df[[col_name2]])
    ranks <- apply(data, 2, rank)
    ranks_difference <-
        apply(ranks, 1, function(x) (as.numeric(x[1]) - as.numeric(x[2])) ** 2)
    ranks_sum <- sum(ranks_difference)
    speraman_cor_coeff <- 
        1 - ((6 * ranks_sum) / 
                 (dim(data)[1] * (dim(data)[1] - 1) * (dim(data)[1] + 1)))
    speraman_cor_coeff
}
