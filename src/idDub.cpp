
#include <Rcpp.h>
using namespace Rcpp;

// A c++ implementation of inverse distance weighted interpolation

// [[Rcpp::export]]

NumericVector idDub( int i,
                     NumericVector inputlat,
                     NumericVector inputlon,
                     NumericMatrix inputdata,
                     NumericVector outputlat,
                     NumericVector outputlon ) {
    
    double output_lat = outputlat[i];
    double output_lon = outputlon[i];
    int ncols = inputdata.cols();
    int nrowsin = inputdata.rows();
    NumericVector outputdata( ncols );
    NumericVector dist( nrowsin );
    NumericVector datarunning( nrowsin );
    NumericVector fill_nas( ncols, NumericVector::get_na() );
    
    
    dist = sqrt( 
        pow( ( inputlat - output_lat ), 2 ) + 
            pow( ( inputlon - output_lon ) , 2 ) 
    );
    
    // if there's a perfectly aligned datapoint, use it (assume there's only 1)
    // if( min( dist ) == 0 ) {
    //     outputdata( row, _ ) = inputdata( dist == 0, _ );
    //     continue;
    // };
    
    // otherwise, perform a full scale inverse distance weighted interpolation
    for( int col = 0; col < ncols; ++col ) {
        datarunning = inputdata( _ , col );
        NumericVector datarunning_touse = datarunning[ is_na( datarunning ) == FALSE ];
        NumericVector dist_touse = dist[ is_na( datarunning ) == FALSE ];
        
        outputdata[ col ] = 
            sum( datarunning_touse / dist_touse ) / sum( 1 / dist_touse );
    }
    
    return outputdata;
    
}



