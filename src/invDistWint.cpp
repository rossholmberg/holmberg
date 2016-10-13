
#include <Rcpp.h>
using namespace Rcpp;

// A c++ implementation of inverse distance weighted interpolation

// [[Rcpp::export]]
NumericVector invDistWInt(NumericVector inputlat,
                          NumericVector inputlon,
                          NumericVector inputdata,
                          NumericVector outputlat,
                          NumericVector outputlon) {
    
    int noutput = outputlat.size();
    
    int ninput = inputlat.size();
    
    NumericVector outputdata( noutput );
    
    NumericVector dist( ninput );
    
    for(int i = 0; i < noutput; ++i) {
        
        outputdata[i] = 99999;
        
        dist = sqrt( pow( (inputlat - outputlat[i]), 2 ) + pow( (inputlon - outputlon[i]), 2 ) );
        
        double total = 0, total_w = 0;
        
        for( int p = 0; p < ninput; ++p ) {
            
            if( dist[p] == 0 ) {
                outputdata[i] = inputdata[p];
                break;
            };
            
            if( inputdata[p] == 99999 ) {
                continue;
            };
            
            total += inputdata[p] * ( 1 / dist[p] );
            total_w += ( 1 / dist[p] );
            
        };
        
        if( outputdata[i] == 99999 ) {
            outputdata[i] = total / total_w;
        }
    }
    
    return outputdata;
    
}
