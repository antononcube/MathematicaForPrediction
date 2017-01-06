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


import java.util.*;


public class TrieFunctions {

    //! @description Makes a base trie from a list
    //! @param chars a list of objects
    //! @param val value (e.g. frequency) to be assigned
    //! @param bottomVal the bottom value
    protected static Trie make( List<String> chars, Double val, Double bottomVal ) {
        if ( chars == null || chars.size() == 0 ) {
            return null;
        }

        if ( bottomVal == null ) {
            bottomVal = val;
        }

        // First node
        Trie res = new Trie( chars.get( chars.size() - 1 ), bottomVal );

        for ( int i = chars.size()-2; i >= 0; i-- ) {
            Map<String, Trie> children = new HashMap<>();
            children.put( res.getKey(), res );
            res = new Trie( chars.get(i), val, children );
        }

        Trie res2 = new Trie( "", val);
        res2.setChildren( new HashMap<>() );
        res2.getChildren().put( res.getKey(), res );

        return res2;
    }

    protected static Trie make( List<String> chars, Double val ) {
        return make( chars, val, null );
    }

    protected static Trie make( List<String> chars ) {
        return make( chars, 1.0, null );
    }

    protected static Trie create1( List< List<String> > words) {
        if ( words == null || words.isEmpty() ) {
            return null;
        }
        Trie res = make( words.get(0) );

        for (  int i = 1; i < words.size(); i++ ) {
            res = insert( res, words.get(i) );
        }

        return res;
    }

    public static Trie create( List< List<String> > words) {
        if ( words == null || words.isEmpty() ) {
            return null;
        }

        if ( words.size() <= 15 ) {
            return create1( words );
        }

        return merge(
                create( words.subList( 0, words.size() / 2 ) ),
                create( words.subList( words.size() / 2, words.size()  ) )
        );
    }

    public static List< List<String> > splitWords( List< String > words, String regex ) {
        List< List<String> > seqList = new ArrayList<>();

        for ( String s : words ) {
            seqList.add( Arrays.asList( s.split(regex) ) );
        }

        return seqList;
    }

    public static Trie createBySplit( List< String > words, String regex ) {
        return create( splitWords( words, regex ) );
    }

    public static Trie merge( Trie tr1, Trie tr2 ) {

        Trie res = new Trie();
        res.setChildren( new HashMap<>() );

        if ( tr1 == null ) {

            return tr2;

        } else if ( tr2 == null ) {

            return tr1;

        } else if ( ! tr1.getKey().equals( tr2.getKey() ) ) {

            res.getChildren().putAll( tr1.getChildren() );
            res.getChildren().putAll( tr2.getChildren() );

            return res;

        } else if ( tr1.getKey().equals( tr2.getKey() ) ) {

            if ( tr1.getChildren() == null ) {
                tr2.setValue( tr1.getValue() + tr2.getValue() );
                return tr2;
            } else if ( tr2.getChildren() == null ) {
                tr1.setValue( tr1.getValue() + tr2.getValue() );
                return tr1;
            }

            res.setKey( tr1.getKey() );
            res.setValue( tr1.getValue() + tr2.getValue() );

            for ( Map.Entry<String, Trie> elem1 : tr1.getChildren().entrySet() ) {

                if ( tr2.getChildren().get( elem1.getKey() ) == null ) {
                    res.getChildren().put( elem1.getKey(), elem1.getValue() );
                } else {
                    res.getChildren().put( elem1.getKey(), merge( elem1.getValue(), tr2.getChildren().get( elem1.getKey() ) ) );
                }
            }

            for ( Map.Entry<String, Trie> elem2 : tr2.getChildren().entrySet() ) {

                if ( tr1.getChildren().get( elem2.getKey() ) == null ) {
                    res.getChildren().put( elem2.getKey(), elem2.getValue() );
                }
            }

            return res;
        }

        return null;
    }


    public static Trie insert( Trie tr, List<String> word ) {
        return insert( tr, word, null );
    }

    public static Trie insert( Trie tr, List<String> word, Double value ) {

        if ( value == null ) {
            return merge( tr, make( word, 1.0, null ) );
        } else {
            return merge( tr, make( word, 0.0, value) );
        }
    }

    public static List<String> position( Trie tr, List<String> word ) {

        if ( word == null || word.isEmpty() ) {
            return null;
        } else {
            if ( tr.getChildren() == null ) {
                return null;
            }

            Trie pos = tr.getChildren().get( word.get(0) );
            if ( pos == null ) {
                return null;
            } else {
                List<String> res = new ArrayList<>( );
                res.add( word.get(0) );
                List<String> rpos = position( pos, word.subList( 1, word.size() ) );
                if ( rpos == null || rpos.isEmpty() ) {
                  return res;
                } else {
                    res.addAll( rpos );
                    return res;
                }
            }
        }
    }

    public static List< List<String> > mapPosition( Trie tr, List< List<String> > words ) {
        List< List<String> > res = new ArrayList<>();
        for ( List<String> s : words ) {
            res.add( position( tr, s) );
        }
        return res;
    }


    public static Trie retrieve( Trie tr, List<String> word ) {

        if ( word == null || word.isEmpty() ) {
            return tr;
        } else {
            if ( tr.getChildren() == null ) {
                return tr;
            }
            Trie pos = tr.getChildren().get( word.get(0) );
            if ( pos == null ) {
                return tr;
            } else {
                return retrieve( pos, word.subList( 1, word.size() ) );
            }
        }
    }

    public static List< Trie > mapRetrieve( Trie tr, List< List<String> > words ) {
        List< Trie > res = new ArrayList<>();
        for ( List<String> s : words ) {
            res.add( retrieve( tr, s) );
        }
        return res;
    }

    public static Boolean completeMatch( Trie tr, List<String> word ) {
        Trie subTr = retrieve( tr, word );

        if (  subTr.getChildren() == null || subTr.getChildren().isEmpty() ) {
            return true;
        } else {
            double chValue = 0;
            for( Trie ch : subTr.getChildren().values() ) {
                chValue += ch.getValue();
            }
            return chValue < tr.getValue();
        }
    }

    public static List<Boolean> mapCompleteMatch( Trie tr, List< List<String> > words ) {
        List<Boolean> res = new ArrayList<>();
        for ( List<String> s : words ) {
            res.add( completeMatch( tr, s) );
        }
        return res;
    }


    public static Boolean contains( Trie tr, List<String> word ) {
        List<String> pos = position( tr, word);
        if ( pos.size() < word.size() ) {
            return false;
        } else {
            return completeMatch( tr, pos );
        }
    }

    public static List<Boolean> mapContains( Trie tr, List< List<String> > words ) {
        List<Boolean> res = new ArrayList<>();
        for ( List<String> s : words ) {
            res.add( contains( tr, s) );
        }
        return res;
    }

    public static Trie nodeProbabilities( Trie tr ) {
        Trie res = nodeProbabilitiesRec( tr );
        res.setValue(1.0);
        return res;
    }

    protected static Trie nodeProbabilitiesRec( Trie tr ) {
        double chSum=0;

        if ( tr == null || tr.getChildren() == null || tr.getChildren().isEmpty() ) {
            return tr;
        }

        if ( tr.getValue() == 0 ) {
            chSum = 0;
            for ( Trie ch : tr.getChildren().values() ) {
                chSum += ch.getValue();
            }
        } else {
            chSum = tr.getValue();
        }

        Map<String, Trie> resChildren = new HashMap<>();

        for ( Map.Entry<String, Trie> elem : tr.getChildren().entrySet() ) {
            Trie chNode = nodeProbabilitiesRec( elem.getValue() );
            chNode.setValue( chNode.getValue() / chSum );
            resChildren.put( elem.getKey(), chNode );
        }

        return new Trie( tr.getKey(), tr.getValue(), resChildren );
    }

    protected static class Pair<T1, T2> implements Map.Entry<T1, T2> {
        T1 key;
        T2 value;
        public T1 getKey() { return key; }
        public T2 getValue() { return value; }
        public Pair(T1 k, T2 v) { key=k; value=v; }
        @Override
        public T2 setValue(T2 v) {
            this.value = v;
            return value;
        }

        public String toString(){
            return "{" + getKey() + ", " + getValue() + "}";
        }
    }

    protected static void toRows(
            List< List< Map.Entry<String, Double> > > rows,
            Trie tr,
            List< Map.Entry<String, Double> > path  )
    {
        TrieFunctions tp = new TrieFunctions();

        List< Map.Entry<String, Double> > currentPath = new ArrayList<>();
        currentPath.addAll( path );
        currentPath.add( new Pair( tr.getKey(), tr.getValue() ) );

        if ( tr.getChildren() == null || tr.getChildren().size() == 0 ) {
            rows.add( currentPath );
        } else {
            for ( Trie ch : tr.getChildren().values() ) {
                toRows( rows, ch, currentPath );
            }
        }
    }

    public static List< List< Map.Entry<String, Double> > > rootToLeafPaths( Trie tr ) {
        List< List< Map.Entry<String, Double> > > rows = new ArrayList();
        List< Map.Entry<String, Double> > path = new ArrayList();

        toRows( rows, tr, path);

        return rows;
    }

}
