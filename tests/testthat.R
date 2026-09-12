library(testthat)
library(foresty)

# Drawing a figure opens a device, and a device opened while the tests run is
# the default one, which writes an Rplots.pdf into the test directory and
# leaves it there. Sending that drawing to the null device instead keeps the
# directory -- and the tarball built from it -- clean. Tests that need a device
# of their own open and close it, and the null device is what they come back to.
grDevices::pdf(NULL)

test_check("foresty")

grDevices::dev.off()
