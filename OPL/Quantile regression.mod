/*
    Quantile regression using B-splines OPL model
    Copyright (C) 2014  Anton Antonov

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

	Written by Anton Antonov, 
	antononcube@gmail.com,
	Windermere, Florida, USA.
*/

/*
  The full IBM ILOG CPLEX Optimization Studio consists of the CPLEX Optimizer for mathematical programming, the IBM ILOG CPLEX CP Optimizer for constraint programming, the Optimization Programming Language (OPL), and a tightly integrated IDE.

  IBM and ILOG are trademarks or registered trademarks of International Business Machines Corp., in many jurisdictions worldwide. Other product and service names might be trademarks of IBM or other companies. A current list of IBM trademarks is available on the Web at Copyright and trademark information.

*/

/* Version 1.0 */
/*
  OPL code for the calculation of a regression quantile given with the parameter q with first order B-spline basis using k equaly spaced knots.
  The knots are spaced between  [ min(d in data) d.x,  max(d in data) d.x] . The first and last end points of the interval are B-spline knots.
  The data is supplied by an external file with extension .dat, e.g.
  data = {<10 12.2> <10.2 13.3> ...}
  
  A good possible extension is to give the user the possibility to specify the B-spline basis knots.
  I am not sure how easy is to program using higher order B-spline basis. The only piecewise functions supported by OPL are piecewise linear.
*/

tuple Point {
   float x;
   float y;
};
  
// Data points provided by an external file
{Point} data = ...;

// Number of knots parameter
int k = 7;

// Quantile parameter
float q = 0.75;

float dataMin = min(d in data) d.x;
float dataMax = max(d in data) d.x;
float cLen = ( dataMax - dataMin ) / (k-1);

dvar float+ u[data];
dvar float+ v[data];
dvar float+ bs[1..k];

  
minimize sum(d in data) q*u[d] + sum(d in data) (1-q)*v[d];
  
subject to {

	forall ( d in data ) {
	  (sum ( i in 1..k ) bs[i] * (piecewise{ 0-> dataMin + (i-2)*cLen; 1/cLen -> dataMin + (i-1)*cLen; -1/cLen -> dataMin + i*cLen; 0}( dataMin + (i-2)*cLen, 0) d.x) ) + u[d] - v[d] == d.y;  	  
	}
}
   
execute postProcess{
    writeln("\n=== Regression quantile display ===");
    writeln("Quantile = " + q);
    writeln("B-Spline functions and weights:");
  	
	for( var i =1; i <= k; i ++ ) {
	  	var v1 = dataMin + (i-2)*cLen;
	  	var v2 = dataMin + (i-1)*cLen;
	  	var v3 = dataMin + i*cLen;
	  	var v4 = 1/cLen;
	  	var v5 = -1/cLen;	
  		writeln(bs[i] + " piecewise{ 0 -> " + v1 + "; " + v4 + " -> " + v2 + "; " + v5 + " -> " + v3 + "; 0 } ( " + v1 + ", 0 )" );
	}   		
}    
