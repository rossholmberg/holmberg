#include <Rcpp.h>
using namespace Rcpp;
//' calculate data based on inverse distance weighted interpolation
//'
//'
//' @param inputlat latitudes of input data
//' @param inputlon longitudes of input data
//' @param inputdata input data
//' @param outputlat latitudes of output data
//' @param outputlon longitudes of output data
//' @param landmask aligned data for masking
//' 
//' @useDynLib holmberg
//' 
//' @keywords inverse distance weighted average
//' @export
//'
// [[Rcpp::export]]
NumericMatrix idDub_allRows( NumericVector inputlat,
                     NumericVector inputlon,
                     NumericMatrix inputdata,
                     NumericVector outputlat,
                     NumericVector outputlon,
                     NumericVector landmask ) {
    
    double output_lat;
    double output_lon;
    int ncols = inputdata.cols();
    int nrowsin = inputdata.rows();
    int nrowsout = outputlat.size();
    NumericMatrix outputdata( nrowsout, ncols );
    NumericVector dist( nrowsin );
    NumericVector datarunning( nrowsin );
    
    for( int i = 0; i < outputdata.rows(); ++i ) {
        
        output_lat = outputlat[i];
        output_lon = outputlon[i];
        
        // check if we're masking this out
        if( landmask[i] == 1 ) {
            
            NumericVector fill_nas( ncols, NumericVector::get_na() );
            outputdata( i, _ ) = fill_nas;
            
        } else {
            
            // create a distance vector
            dist = sqrt( 
                pow( ( inputlat - output_lat ), 2 ) + 
                    pow( ( inputlon - output_lon ) , 2 ) 
            );
            
            // if there's a perfectly aligned datapoint, use it (assume there's only 1)
            if( min( dist ) == 0 ) {
                
                int zerodist = which_min( dist );
                outputdata( i, _ ) = inputdata( zerodist, _ );
                
            } else {
                
                // otherwise, perform a full scale inverse distance weighted interpolation
                for( int col = 0; col < ncols; ++col ) {
                    datarunning = inputdata( _ , col );
                    NumericVector datarunning_touse = datarunning[ is_na( datarunning ) == FALSE ];
                    NumericVector dist_touse = dist[ is_na( datarunning ) == FALSE ];
                    
                    outputdata( i, col ) = 
                        sum( datarunning_touse / dist_touse ) / sum( 1 / dist_touse );
                }
                
            }
        }
        
    }
    
    
    return outputdata;
    
}



