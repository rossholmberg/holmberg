#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector interpolate(NumericVector x_in,
                          NumericVector y_in,
                          NumericVector x_out ) {
    
    int length = x_out.size();
    int length_in = x_in.size();
    NumericVector output( length );
    int nearest_ref_before, nearest_ref_after;
    NumericVector value_dist( length_in );
    NumericVector value_dist_invert( length_in );
    double nearest_y_before;
    double nearest_y_after;
    double diff_x_before;
    double diff_x_after;
    double missing = NA_REAL;
    
    // work on output "y" values one at a time
    for( int i = 0; i < length; i++ ) {
        
        // if the "x" value is out of range, return a missing value
        if( x_out[i] < min( x_in ) ) {
            output[ i ] = missing;
        } else if( x_out[i] > max( x_in ) ) {
            output[ i ] = missing;
            
        // otherwise, work on calculating an output result    
        } else {
            
            // create a vector of distances to each point in "x_in"
            value_dist = x_out[i] - x_in;
            
            // also create an inverted verion (for "negative" distances)
            value_dist_invert = value_dist * -1;
            // move any "negative" distances "out of the way"
            value_dist[ value_dist < 0 ] = INFINITY;
            
            // check for one or more exact matches
            if( min( value_dist ) == 0 ) {
                
                // use the exact match(es) and move on
                NumericVector zerodistData = y_in[ value_dist == 0 ];
                output[ i ] = mean( zerodistData );
                
            // if there isn't an exact match
            } else {
                
                // move "negative" distances out of the way on the inverted vector
                value_dist_invert[ value_dist_invert < 0 ] = max( value_dist_invert ) + 1;
                
                // locate the nearest matches before and after the given "x" value
                nearest_ref_before = which_min( value_dist );
                nearest_ref_after = which_min( value_dist_invert );
                
                // extract those nearest matches from the dataset
                nearest_y_before = y_in[ nearest_ref_before ];
                nearest_y_after = y_in[ nearest_ref_after ];
                
                // also extract the "x distance" to each of those matches
                diff_x_before = x_out[i] - x_in[ nearest_ref_before ];
                diff_x_after = x_in[ nearest_ref_after ] - x_out[i];
                
                // now calculate the output value. We use a "weighted mean" equation here,
                // weighting each "y" value by the inverse of its corresponding "x distance".
                // ie: a nearer match will be weighted higher than a more distant match
                output[ i ] =
                    ( ( nearest_y_before / diff_x_before ) + ( nearest_y_after / diff_x_after ) ) /
                        ( 1 / diff_x_before  +  1 / diff_x_after );
                
            }
            
        }
        
    }
    
    // the output "y" vector is now filled, return it to the user
    return output;
    
}



/*** R
# We'll create two overlapping time series datasets with different intervals between points
# NOTE this will work with any x and y axes that C++ can convert to numeric,
#   just make sure the two x variables have matching scales
#   (eg: Date class and POSIXct class will convert differently,
#   but numeric and integer would be fine)
set.seed( 2 )
n <- 100
x_in <- seq.POSIXt( as.POSIXct( "2016-02-05 10:01:25", tz = "UTC" ),
                    by = 10,
                    length.out = n/10 )
x_out <- seq.POSIXt( from = min( x_in ),
                     to = max( x_in ),
                     by = 1 )

# we also create a test "y" dataset using a "random walk" algorithm
y_in <- numeric( length = n/10 )
for( i in 2:length( y_in ) ) {
    y_in[ i ] <- y_in[ i-1 ] + rnorm( 1 )
}

# now interpolate the input data to fit the new (output) timeseries
y_out <- interpolate( x_in = x_in,
                      y_in = y_in,
                      x_out = x_out
)

# plot both the input and output datasets to check validity
library( ggplot2 )
ggplot() +
    geom_path( mapping = aes( x = x_out, y = y_out ),
               alpha = 0.4, col = "red" ) +
    geom_point( mapping = aes( x = x_out, y = y_out ),
                col = "red", alpha = 0.2 ) +
    geom_point( mapping = aes( x = x_in, y = y_in ) ) +
    labs( x = "x", y = "y" )
*/
