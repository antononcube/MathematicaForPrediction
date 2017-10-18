//# Tries with frequencies Java implementation
//# Copyright (C) 2016  Anton Antonov
//#
//# This program is free software: you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation, either version 3 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//#
//# You should have received a copy of the GNU General Public License
//# along with this program.  If not, see <http://www.gnu.org/licenses/>.
//#
//# Written by Anton Antonov,
//# antononcube @ gmail. com ,
//# Windermere, Florida, USA.
//#
//# Version 1.0
//# The Java code in this file corresponds to the Mathematica package
//# "Tries with frequencies" also written by Anton Antonov:
//# https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequencies.m .
//# There is also an R package with implementing that functionality:
//# https://github.com/antononcube/MathematicaForPrediction/blob/master/R/TriesWithFrequencies.R .
//# Both packages are part of the MathematicaForPrediction project at GitHub.
//#
//# For detailed explanations see the blog post:
//# "Tries with frequencies for data mining",
//# https://mathematicaforprediction.wordpress.com/2013/12/06/tries-with-frequencies-for-data-mining/ .


import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;



public class Experiments {

    public static void main( String args[] ) {

        basic();
    }

    public static void basic() {

//		List<String> sampleSeq = new ArrayList<String>() {{
//			add("arm"); add("arms"); add("arc"); add("bar"); add("bark"); add("barman"); add("arcola"); }};

        List<String> sampleSeq = new ArrayList<String>() {{
            add("a;r;m"); add("a;r;m;s"); add("a;r;c"); add("b;a;r"); add("b;a;r;k"); add("b;a;r;m;a;n"); add("a;r;c;o;l;a"); }};


//		List< List<String> > sampleSeqList = new ArrayList<>();
//
//        for ( String s : sampleSeq ) {
//            sampleSeqList.add( Arrays.asList( s.split("") ) );
//        }
//
//        System.out.println( sampleSeqList );
//
//        Trie strie = TrieFunctions.create( sampleSeqList );

        Trie strie = TrieFunctions.createBySplit( sampleSeq, ";");

        System.out.println("strie = ");
        System.out.println( strie );
        System.out.println();

//        sampleSeq = new ArrayList<String>() {{
//            add("bar"); add("barks"); add("barkeep"); add("barn"); add("balm");
//            add("car"); add("care"); add("caress"); add("card"); add("cold"); add("colder"); }};

        sampleSeq = new ArrayList<String>() {{
            add("barks"); add("barkers"); add("barked"); add("barkeeps"); add("barkeepers");
            add("barking"); add("balm"); }};

        strie = TrieFunctions.createBySplit( sampleSeq, "");

        System.out.println( "shrink trie:");
        System.out.println( TrieFunctions.shrink( strie, ":" ) );
        System.out.println();

        System.out.println( "shrink internal nodes only trie:");
        System.out.println( TrieFunctions.shrinkInternalNodes( strie, ":", 1.0 ) );
        System.out.println();

        List<String> sword = new ArrayList() {{ add("b"); add("a"); add("r"); }};
        System.out.println("For " + sword );
        System.out.println( "contains: " + TrieFunctions.contains( strie, sword ) );
        System.out.println( "position: " + TrieFunctions.position( strie, sword ) );
        System.out.println( "complete match: " + TrieFunctions.hasCompleteMatch( strie, sword ));
        System.out.println();

        sword = new ArrayList() {{ add("b"); add("a"); }};

        System.out.println("For " + sword );
        System.out.println( "contains: " + TrieFunctions.contains( strie, sword ) );
        System.out.println( "position: " + TrieFunctions.position( strie, sword ) );
        System.out.println( "complete match: " + TrieFunctions.hasCompleteMatch( strie, sword ));
        System.out.println();

        Trie ftrie = TrieFunctions.retrieve( strie, sword );
        System.out.println("ftrie = ");
        System.out.println( ftrie + "\n" );

        Trie pstrie = TrieFunctions.nodeProbabilities( strie );

        System.out.println("pstrie = ");
        System.out.println( pstrie + "\n" );


        System.out.println("strie = ");
        System.out.println( strie + "\n" );

        sword = new ArrayList() {{ add("a"); add("r"); }};
        System.out.println( "words for:" + sword );
        System.out.println( TrieFunctions.getWords( strie, sword ) );
        System.out.println();

//        System.out.println( "paths to JSON:");
//        System.out.println( TrieFunctions.pathsToJSON( TrieFunctions.rootToLeafPaths( strie) ) );
//        System.out.println();

        System.out.println( "paths strie:");
        System.out.println( TrieFunctions.rootToLeafPaths( strie) );
        System.out.println();

        System.out.println( "paths pstrie:");
        System.out.println( TrieFunctions.rootToLeafPaths( pstrie) );
        System.out.println();


        System.out.println( "node counts:");
        System.out.println( TrieFunctions.nodeCounts( strie) );
        System.out.println();

        System.out.println( "leaf probabilities :");
        System.out.println( TrieFunctions.leafProbabilities( pstrie ) );
        System.out.println();
        System.out.println( TrieFunctions.leafProbabilitiesJSON( pstrie ) );
        System.out.println();

        System.out.println( "randomChoice :");
        System.out.println( TrieFunctions.randomChoice( pstrie, true ) );
        System.out.println();
        System.out.println( TrieFunctions.randomChoice( pstrie, false ) );
        System.out.println();


//        sampleSeq = new ArrayList<String>() {{ add("ar"); add("as"); }};
//        List< List<String> > sampleSeqList = new ArrayList<>();
//        for ( String s : sampleSeq ) {
//            sampleSeqList.add( Arrays.asList( s.split("") ) );
//        }
//
//        strie = TrieFunctions.create( sampleSeqList );
//
//        System.out.println( strie.toJSON() );


//        Trie mstrie = TrieFunctions.map( strie, ( String k, Double v) -> { return new TrieFunctions.Pair<String, Double>( k + ":M", 2*v ); } );
//
//        System.out.println( "Map function over nodes :");
//        System.out.println( mstrie );
//        System.out.println();
//
//        Trie mstrie2 = mstrie.clone();
//
//        System.out.println( "Clone of a trie :");
//        System.out.println( mstrie2 );
//        System.out.println();
//
//        System.out.println( "mstrie equal mstrie2 ? :");
//        System.out.println( mstrie.equals( mstrie2 ) );
//        System.out.println();
//
//        System.out.println( "mstrie equal strie ? :");
//        System.out.println( mstrie.equals( strie ) );
//        System.out.println();
//
//        pstrie = TrieFunctions.nodeProbabilities( strie );
//        System.out.println("Node probabilities trie:");
//        System.out.print( pstrie );
//        System.out.println("\n");
//
//        Trie pstrie2 = TrieFunctions.removeByThreshold( pstrie, 0.34, ".*" );
//        System.out.println("Node probabilities trie reduced by threshold:");
//        System.out.print( pstrie2 );
//        System.out.println("\n");
//
//        pstrie = TrieFunctions.nodeProbabilities( strie );
//        System.out.println("Node probabilities trie:");
//        System.out.print( pstrie );
//        System.out.println("\n");
//
//        Trie pstrie3 = TrieFunctions.shrinkByThreshold( pstrie2, "~", 0.7 );
//        System.out.println("Node probabilities trie shrunk by threshold:");
//        System.out.print( pstrie3 );
//        System.out.println("\n");
//
//        Trie pstrie4 = TrieFunctions.removeByKeyRegex( pstrie, "r", null );
//        System.out.println("Node probabilities trie reduced by key regex pattern:");
//        System.out.print( pstrie4 );
//        System.out.println("\n");
//
//        Trie pstrie5 = TrieFunctions.removeByParetoFraction( pstrie, 0.9,null );
//        System.out.println("Node probabilities trie reduced by Pareto fraction:");
//        System.out.print( pstrie5 );
//        System.out.println("\n");
    }

}
