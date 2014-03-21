## --------------------------------------------------------------------------
##
## Check internal functions pos2coord() and coord2pos()
##
## --------------------------------------------------------------------------

pos2coord <- rvgtest:::pos2coord
coord2pos <- rvgtest:::coord2pos

## --------------------------------------------------------------------------

context("[pos2coord] - (internal function)")

## --------------------------------------------------------------------------

test_that("[p2c-000] calling pos2coord: NULL", {
        expect_identical(pos2coord(1L, NULL), 1L)
})

test_that("[p2c-001] calling pos2coord: 1-dim", {
        expect_identical(pos2coord(1L, 6L), 1L)
        expect_identical(pos2coord(4L, 6L), 4L)
        expect_identical(pos2coord(6L, 6L), 6L)
})

test_that("[p2c-002] calling pos2coord: 2-dim", {
        expect_identical(pos2coord( 1L, c(3L,4L)), c(1L,1L))
        expect_identical(pos2coord( 7L, c(3L,4L)), c(1L,3L))
        expect_identical(pos2coord(12L, c(3L,4L)), c(3L,4L))

        expect_identical(pos2coord( 2L, c(1L,4L)), c(1L,2L))
})

test_that("[p2c-003] calling pos2coord: 3-dim", {
        expect_identical(pos2coord( 1L, c(3L,4L,5L)), c(1L,1L,1L))
        expect_identical(pos2coord( 3L, c(3L,4L,5L)), c(3L,1L,1L))
        expect_identical(pos2coord( 8L, c(3L,4L,5L)), c(2L,3L,1L))
        expect_identical(pos2coord(20L, c(3L,4L,5L)), c(2L,3L,2L))
        expect_identical(pos2coord(60L, c(3L,4L,5L)), c(3L,4L,5L))

        expect_identical(pos2coord(5L, c(3L,1L,5L)), c(2L,1L,2L))
})

test_that("[p2c-004] calling pos2coord: 4-dim", {
        expect_identical(pos2coord(  1L, c(3L,4L,5L,6L)), c(1L,1L,1L,1L))
        expect_identical(pos2coord(284L, c(3L,4L,5L,6L)), c(2L,3L,4L,5L))
        expect_identical(pos2coord(360L, c(3L,4L,5L,6L)), c(3L,4L,5L,6L))

        expect_identical(pos2coord( 56L, c(3L,4L,1L,6L)), c(2L,3L,1L,5L))
})

test_that("[p2c-i01] calling pos2coord with invalid arguments: pos", {
        msg <- "Argument 'pos' missing"
        expect_error(pos2coord(dims=c(2L,3L)),  msg)

        msg <- "Argument 'pos' invalid"
        expect_error(pos2coord(pos=1.2, dims=c(2L,3L)),  msg)
        expect_error(pos2coord(pos=c(1L,2L), dims=c(2L,3L)),  msg)
})
        
test_that("[p2c-i02] calling pos2coord with invalid arguments: dims", {
        msg <- "Argument 'dims' missing"
        expect_error(pos2coord(pos=2L),  msg)

        msg <- "Argument 'dims' invalid"
        expect_error(pos2coord(pos=2L, dims=1.3),  msg)
        expect_error(pos2coord(pos=2L, dims=integer()),  msg)
        expect_error(pos2coord(pos=2L, dims=0L),  msg)
        expect_error(pos2coord(pos=2L, dims=c(2L,0L,3L)),  msg)

        msg <- "Argument 'pos' out of range"
        expect_error(pos2coord(pos=25L, dims=c(2L,3L,4L)),  msg)
})
        
## --------------------------------------------------------------------------

context("[coord2pos] - (internal function)")

## --------------------------------------------------------------------------

test_that("[c2p-000] calling coord2pos: NULL", {
        expect_identical(coord2pos(1L, NULL), NULL)
})

test_that("[c2p-001] calling coord2pos: 1-dim", {
        expect_identical(coord2pos(1L, 6L), 1L)
        expect_identical(coord2pos(4L, 6L), 4L)
        expect_identical(coord2pos(6L, 6L), 6L)
})

test_that("[c2p-002] calling coord2pos: 2-dim", {
        expect_identical(coord2pos(c(1L,1L), c(3L,4L)), 1L)
        expect_identical(coord2pos(c(3L,1L), c(3L,4L)), 3L)
        expect_identical(coord2pos(c(1L,2L), c(3L,4L)), 4L)
        expect_identical(coord2pos(c(2L,3L), c(3L,4L)), 8L)
        expect_identical(coord2pos(c(3L,4L), c(3L,4L)),12L)

        expect_identical(coord2pos(c(1L,2L), c(1L,4L)), 2L)
})

test_that("[c2p-003] calling coord2pos: 3-dim", {
        expect_identical(coord2pos(c(1L,1L,1L), c(3L,4L,5L)), 1L)
        expect_identical(coord2pos(c(3L,1L,1L), c(3L,4L,5L)), 3L)
        expect_identical(coord2pos(c(2L,3L,1L), c(3L,4L,5L)), 8L)
        expect_identical(coord2pos(c(2L,3L,2L), c(3L,4L,5L)),20L)
        expect_identical(coord2pos(c(3L,4L,5L), c(3L,4L,5L)),60L)

        expect_identical(coord2pos(c(2L,1L,2L), c(3L,1L,5L)), 5L)
})

test_that("[c2p-004] calling coord2pos: 4-dim", {
        expect_identical(coord2pos(c(1L,1L,1L,1L), c(3L,4L,5L,6L)),  1L)
        expect_identical(coord2pos(c(2L,3L,4L,5L), c(3L,4L,5L,6L)),284L)
        expect_identical(coord2pos(c(3L,4L,5L,6L), c(3L,4L,5L,6L)),360L)

        expect_identical(coord2pos(c(2L,3L,1L,5L), c(3L,4L,1L,6L)), 56L)
})

test_that("[c2p-i01] calling coord2pos with invalid arguments: coord", {
        msg <- "Argument 'coord' missing"
        expect_error(coord2pos(dims=c(2L,3L)),  msg)

        msg <- "Argument 'coord' invalid"
        expect_error(coord2pos(coord=1.2,         dims=c(2L,3L)),  msg)
        expect_error(coord2pos(coord=integer(),   dims=c(2L,3L)),  msg)
        expect_error(coord2pos(coord=0L,          dims=c(2L,3L)),  msg)
        expect_error(coord2pos(coord=c(1L,0L,3L), dims=c(2L,3L)),  msg)
})
        
test_that("[c2p-i02] calling coord2pos with invalid arguments: dims", {
        msg <- "Argument 'dims' missing"
        expect_error(coord2pos(coord=c(1L,2L)),  msg)

        msg <- "Argument 'dims' invalid"
        expect_error(coord2pos(coord=c(1L,2L), dims=c(1.3,3)),  msg)
        expect_error(coord2pos(coord=c(1L,2L), dims=c(0L,3L)),  msg)
        expect_error(coord2pos(coord=c(1L,2L), dims=c(2L,0L)),  msg)

        msg <- "Arguments 'coord' and 'dims' have different length"
        expect_error(coord2pos(coord=c(1L,2L), dims=c(1L,2L,3L)),  msg)

        msg <- "Argument 'coord' out of range"
        expect_error(coord2pos(coord=c(1L,4L), dims=c(2L,3L)),  msg)
        expect_error(coord2pos(coord=c(4L,2L), dims=c(2L,3L)),  msg)
})

## --------------------------------------------------------------------------
