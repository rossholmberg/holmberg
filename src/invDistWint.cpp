
#include <Rcpp.h>
using namespace Rcpp;


//' A c++ implementation of inverse distance weighted interpolation
//' @param inputlat vector of input latitudes
//' @param inputlon vector of input longitudes
//' @param inputdata vector of input data
//' @param outputlat vector of output latitudes
//' @param outputlon vector of output longitudes
//' 
//' @useDynLib holmberg
//' @export
// [[Rcpp::export]]
NumericVector invDistWInt(NumericVector inputlat,
                          NumericVector inputlon,
                          NumericVector inputdata,
                          NumericVector outputlat,
                          NumericVector outputlon ) {
    
    int noutput = outputlat.size();
    int ninput = inputdata.size();
    NumericVector outputdata( noutput );
    NumericVector dist( ninput );
    NumericVector inputOnDist( ninput );
    
    for(int i = 0; i < noutput; ++i) {
        
        dist = sqrt( 
            pow( ( inputlat - outputlat[i] ), 2 ) + 
                pow( ( inputlon - outputlon[i]) , 2 ) 
        );
        
        // if there's a perfectly aligned datapoint, use it (assume there's only 1)
        if( min( dist ) == 0 ) {
            NumericVector zeroDistData = inputdata[ dist == 0 ];
            outputdata[ i ] = zeroDistData[ 1 ];
            continue;
        };
        
        // otherwise, perform a full scale inverse distance weighted interpolation
        outputdata[i] = sum( inputdata / dist ) / sum( 1 / dist );
        
    }
    
    return outputdata;
    
}

