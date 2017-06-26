// [[Rcpp::depends(imager)]]
#define cimg_display_type 0
#include <imager.h>
#include <cstdint>
#include <cmath>
using namespace cimg_library;
using namespace Rcpp;

bool equal(double a, double b) {
    return fabs(a -b) < 1e-16;
}

// [[Rcpp::export]]
LogicalVector C_solid_blobs(NumericVector img_inp, IntegerMatrix stencil) {
    int loc_x, loc_y;

    CImg<double> img = as<CImg<double>>(img_inp);
    int w = img.width();
    int h = img.height();

    // define the resulting bool image, set all to true
    CImg<bool> dest(w, h, 1, 1, true);

    // We loop over all color channels and swipe through the image with the
    // stencil mask and see if the neighbourhood is homogenous on each channel.
    cimg_forXYC(img, x, y, c) {
        // iterate neighbourhood according to stencil
        for(int i = 0; i < stencil.nrow(); i++) {
            loc_x = x + stencil(i, 0);
            loc_y = y + stencil(i, 1);

            // check whether we're still inside the image
            if(loc_x >= 0 && loc_x < w && loc_y >= 0 && loc_y < h) {
                // Set destination pixel to false if current pixel isn't the
                // same as the center one.
                // We use && because once one channel is different, it is
                // different for all eternity.
                dest(x, y) = dest(x, y) &&
                             equal(img(loc_x, loc_y, 0, c), img(x, y, 0, c));
            }
        }
    }
    // return an imager pixset object
    return wrap(dest);
}
