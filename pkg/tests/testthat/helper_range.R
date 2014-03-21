
## wait until user + system time is >= t
wait.while.process <- function(t) {
        s <- proc.time()
        while (TRUE) {
                d <- proc.time() - s
                if (d[1] + d[2] >= t) break
        }
}

