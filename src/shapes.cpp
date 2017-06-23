// [[Rcpp::depends(imager)]]
#define cimg_display_type 0
#include <imager.h>
#include <cstdint>
#include <cmath>
using namespace cimg_library;
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector C_solid_blobs(NumericVector img_inp) {
    CImg<double> img = as<CImg<double>>(img_inp);
    // define the resulting bool image, set all to true
    CImg<bool> dest(img.width(), img.height(), 1, 1, true);

    CImg<double> N(3, 3); // Define a 3x3 neighborhood as a 3x3 image.

    // We'll loop over all color channels and swipe through the image with 3x3
    // square mask and see if the neighbourhood is homogenous on each channel.
    cimg_forC(img, k) { // loop on color channels
        cimg_for3x3(img, x, y, 0, k, N, double) {
            // if there is no variance, all pixels are equal
            dest(x, y) = dest(x, y) && (N.variance() < 1e-10);
        }
    }
    // return an imager pixset object
    return wrap(dest);
}
