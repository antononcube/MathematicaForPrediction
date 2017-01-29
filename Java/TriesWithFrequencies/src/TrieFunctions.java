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

    ///**************************************************************
    /// Core functions -- creation, merging, insertion, node frequencies
    ///**************************************************************

    //! @description Makes a base trie from a list
    //! @param chars a list of objects
    //! @param val value (e.g. frequency) to be assigned
    //! @param bottomVal the bottom value
    protected static Trie make(List<String> chars, Double val, Double bottomVal) {
        if (chars == null || chars.size() == 0) {
            return null;
        }

        if (bottomVal == null) {
            bottomVal = val;
        }

        // First node
        Trie res = new Trie(chars.get(chars.size() - 1), bottomVal);

        for (int i = chars.size() - 2; i >= 0; i--) {
            Map<String, Trie> children = new HashMap<>();
            children.put(res.getKey(), res);
            res = new Trie(chars.get(i), val, children);
        }

        Trie res2 = new Trie("", val);
        res2.setChildren(new HashMap<>());
        res2.getChildren().put(res.getKey(), res);

        return res2;
    }

    protected static Trie make(List<String> chars, Double val) {
        return make(chars, val, null);
    }

    protected static Trie make(List<String> chars) {
        return make(chars, 1.0, null);
    }

    //! @description Creates a trie from a given list of list of strings. (Non-recursively.)
    protected static Trie create1(List<List<String>> words) {
        if (words == null || words.isEmpty()) {
            return null;
        }
        Trie res = make(words.get(0));

        for (int i = 1; i < words.size(); i++) {
            res = insert(res, words.get(i));
        }

        return res;
    }

    //! @description Creates a trie from a given list of list of strings. (Recursively.)
    public static Trie create(List<List<String>> words) {
        if (words == null || words.isEmpty()) {
            return null;
        }

        if (words.size() <= 15) {
            return create1(words);
        }

        return merge(
                create(words.subList(0, words.size() / 2)),
                create(words.subList(words.size() / 2, words.size()))
        );
    }

    //! @description Slits each string of a list of string using a given regex.
    public static List<List<String>> splitWords(List<String> words, String regex) {
        List<List<String>> seqList = new ArrayList<>();

        for (String s : words) {
            seqList.add(Arrays.asList(s.split(regex)));
        }

        return seqList;
    }

    //! @description Creates a trie by splitting each of the strings in the given list of strings.
    public static Trie createBySplit(List<String> words, String regex) {
        return create(splitWords(words, regex));
    }

    public static Trie merge(Trie tr1, Trie tr2) {

        Trie res = new Trie();
        res.setChildren(new HashMap<>());

        if (tr1 == null) {

            return tr2;

        } else if (tr2 == null) {

            return tr1;

        } else if (!tr1.getKey().equals(tr2.getKey())) {

            res.getChildren().putAll(tr1.getChildren());
            res.getChildren().putAll(tr2.getChildren());

            return res;

        } else if (tr1.getKey().equals(tr2.getKey())) {

            if (tr1.getChildren() == null) {
                tr2.setValue(tr1.getValue() + tr2.getValue());
                return tr2;
            } else if (tr2.getChildren() == null) {
                tr1.setValue(tr1.getValue() + tr2.getValue());
                return tr1;
            }

            res.setKey(tr1.getKey());
            res.setValue(tr1.getValue() + tr2.getValue());

            for (Map.Entry<String, Trie> elem1 : tr1.getChildren().entrySet()) {

                if (tr2.getChildren().get(elem1.getKey()) == null) {
                    res.getChildren().put(elem1.getKey(), elem1.getValue());
                } else {
                    res.getChildren().put(elem1.getKey(), merge(elem1.getValue(), tr2.getChildren().get(elem1.getKey())));
                }
            }

            for (Map.Entry<String, Trie> elem2 : tr2.getChildren().entrySet()) {

                if (tr1.getChildren().get(elem2.getKey()) == null) {
                    res.getChildren().put(elem2.getKey(), elem2.getValue());
                }
            }

            return res;
        }

        return null;
    }

    //! @description Inserts a "word" (a list of strings) into a trie.
    public static Trie insert(Trie tr, List<String> word) {
        return insert(tr, word, null);
    }

    //! @description Inserts a "word" (a list of strings) into a trie with a given associated value.
    public static Trie insert(Trie tr, List<String> word, Double value) {

        if (value == null) {
            return merge(tr, make(word, 1.0, null));
        } else {
            return merge(tr, make(word, 0.0, value));
        }
    }

    //! @description Converts the counts (frequencies) at the nodes into node probabilities. Changes the object!
    //! @param tr a trie object
    public static Trie nodeProbabilities(Trie tr) {
        Trie res = nodeProbabilitiesRec(tr);
        res.setValue(1.0);
        return res;
    }

    //! @description Recursive step function for converting node frequencies into node probabilities.
    //! @param tr a trie object
    protected static Trie nodeProbabilitiesRec(Trie tr) {
        double chSum = 0;

        if (tr == null || tr.getChildren() == null || tr.getChildren().isEmpty()) {
            return new Trie(tr.getKey(), tr.getValue());
        }

        if (tr.getValue() == 0) {
            // This is a strange case -- that generally should not happen.
            chSum = 0;
            for (Trie ch : tr.getChildren().values()) {
                chSum += ch.getValue();
            }
        } else {
            chSum = tr.getValue();
        }

        Map<String, Trie> resChildren = new HashMap<>();

        for (Map.Entry<String, Trie> elem : tr.getChildren().entrySet()) {
            Trie chNode = nodeProbabilitiesRec(elem.getValue());
            chNode.setValue(chNode.getValue() / chSum);
            resChildren.put(elem.getKey(), chNode);
        }

        return new Trie(tr.getKey(), tr.getValue(), resChildren);
    }


    ///**************************************************************
    /// Retrieval functions
    ///**************************************************************

    //! @description Find the position of a given word (or part of it) in the trie.
    //! @param tr a trie object
    //! @param word a list of strings
    public static List<String> position(Trie tr, List<String> word) {

        if (word == null || word.isEmpty()) {
            return null;
        } else {
            if (tr.getChildren() == null) {
                return null;
            }

            Trie pos = tr.getChildren().get(word.get(0));
            if (pos == null) {
                return null;
            } else {
                List<String> res = new ArrayList<>();
                res.add(word.get(0));
                List<String> rpos = position(pos, word.subList(1, word.size()));
                if (rpos == null || rpos.isEmpty()) {
                    return res;
                } else {
                    res.addAll(rpos);
                    return res;
                }
            }
        }
    }

    //! @description Optimization of position over a list of words.
    //! @param tr a trie object
    //! @param words a list of lists of strings
    public static List<List<String>> mapPosition(Trie tr, List<List<String>> words) {
        List<List<String>> res = new ArrayList<>();
        for (List<String> s : words) {
            res.add(position(tr, s));
        }
        return res;
    }

    //! @description Retrieval of a sub-trie corresponding to a "word".
    //! @param tr a trie object
    //! @param word a list of strings
    public static Trie retrieve(Trie tr, List<String> word) {

        if (word == null || word.isEmpty()) {
            return tr;
        } else {
            if (tr.getChildren() == null) {
                return tr;
            }
            Trie pos = tr.getChildren().get(word.get(0));
            if (pos == null) {
                return tr;
            } else {
                return retrieve(pos, word.subList(1, word.size()));
            }
        }
    }

    //! @description Optimization of retrieve over a list of words.
    //! @param tr a trie object
    //! @param words a list of lists of strings
    public static List<Trie> mapRetrieve(Trie tr, List<List<String>> words) {
        List<Trie> res = new ArrayList<>();
        for (List<String> s : words) {
            res.add(retrieve(tr, s));
        }
        return res;
    }

    //! @description For a given trie finds if the retrievable part of a word is complete match.
    //! @param tr a trie object
    //! @param word a list of strings
    //! @details Despite the name this function works on the part of the word that can be found in the trie.
    public static Boolean completeMatch(Trie tr, List<String> word) {
        Trie subTr = retrieve(tr, word);

        if (subTr.getChildren() == null || subTr.getChildren().isEmpty()) {
            return true;
        } else {
            double chValue = 0;
            for (Trie ch : subTr.getChildren().values()) {
                chValue += ch.getValue();
            }

            return chValue < subTr.getValue();
        }
    }

    //! @description Optimization of completeMatch over a list of words.
    //! @param tr a trie object
    //! @param words list of words
    public static List<Boolean> mapCompleteMatch(Trie tr, List<List<String>> words) {
        List<Boolean> res = new ArrayList<>();
        for (List<String> s : words) {
            res.add(completeMatch(tr, s));
        }
        return res;
    }

    //! @description Does the trie object tr contains a word.
    //! @param tr a trie object
    //! @param word a word to be checked
    public static Boolean contains(Trie tr, List<String> word) {
        List<String> pos = position(tr, word);
        if (pos == null || pos.size() < word.size()) {
            return false;
        } else {
            return completeMatch(tr, pos);
        }
    }

    //! @description Does the trie object tr contains each of the list of words.
    //! @param tr a trie object
    //! @param words a list of words
    public static List<Boolean> mapContains(Trie tr, List<List<String>> words) {
        List<Boolean> res = new ArrayList<>();
        for (List<String> s : words) {
            res.add(contains(tr, s));
        }
        return res;
    }


    public static class Pair<T1, T2> implements Map.Entry<T1, T2> {
        T1 key;
        T2 value;

        public T1 getKey() {
            return key;
        }

        public T2 getValue() {
            return value;
        }

        public Pair(T1 k, T2 v) {
            key = k;
            value = v;
        }

        @Override
        public T2 setValue(T2 v) {
            this.value = v;
            return value;
        }

        public String toString() {
            return "{" + getKey() + ", " + getValue() + "}";
        }

        public String toJSON() {
            return "{ \"key\" : \"" + getKey() + "\"" + ", \"value\" : " + getValue() + "}";
        }
    }

    //! @description Converts to rows a trie for a given path.
    protected static void toRows(
            List<List<Map.Entry<String, Double>>> rows,
            Trie tr,
            List<Map.Entry<String, Double>> path) {

        List<Map.Entry<String, Double>> currentPath = new ArrayList<>();
        currentPath.addAll(path);
        currentPath.add(new Pair(tr.getKey(), tr.getValue()));

        if (tr.getChildren() == null || tr.getChildren().isEmpty() ) {

            rows.add(currentPath);

        } else {

            double sum = 0;

            for (Trie ch : tr.getChildren().values()) {
                sum += ch.getValue();
            }

            //System.out.println( sum + " " + tr.getValue() );
            if ( tr.getValue() >= 1.0 &&  sum < tr.getValue() ||
                    tr.getValue() < 1.0 && sum < 1.0 ) {
                rows.add(currentPath);
            }

            for (Trie ch : tr.getChildren().values()) {
                toRows(rows, ch, currentPath);
            }

        }
    }

    //! @description Finds the paths from the root of a trie to the leaves.
    //! @param tr a trie object
    public static List<List<Map.Entry<String, Double>>> rootToLeafPaths(Trie tr) {
        List<List<Map.Entry<String, Double>>> rows = new ArrayList();
        List<Map.Entry<String, Double>> path = new ArrayList();

        toRows(rows, tr, path);

        return rows;
    }

    //! @description Converts a list of root-to-leaf paths into JSON.
    //! @param paths a list of lists with Map.Entry elements
    public static String pathsToJSON(List<List<Map.Entry<String, Double>>> paths) {
        String res;
        int k = 0;

        res = "[";
        for (List<Map.Entry<String, Double>> ps : paths) {
            if (k > 0) {
                res += ",";
            }
            k++;
            res += "[";
            int g = 0;
            for (Map.Entry<String, Double> p : ps) {
                if (g > 0) {
                    res += ",";
                }
                g++;
                res += "{ \"key\" : \"" + p.getKey() + "\"" + ", \"value\" : " + p.getValue() + "}";
            }
            res += "]";
        }
        res += "]";
        return res;
    }


    //! @description Finds all words in the trie tr that start with the word searchWord.
    //! @param tr a trie object
    //! @param sword search word
    public static List<List<String>> getWords(Trie tr, List<String> sword) {

        List<String> pos = position(tr, sword);

        if (pos == null || pos.isEmpty() || pos.size() < sword.size()) {

            return null;

        } else {

            List<List<Map.Entry<String, Double>>> paths = rootToLeafPaths(retrieve(tr, sword));

            List<List<String>> res = new ArrayList<>();
            for (List<Map.Entry<String, Double>> ps : paths) {

                List<String> w = new ArrayList<>();

                w.addAll(pos.subList(0, pos.size() - 1));

                for (Map.Entry<String, Double> p : ps) {
                    w.add(p.getKey());
                }
                res.add(w);
            }
            return res;
        }
    }

    ///**************************************************************
    /// Shrinking functions
    ///**************************************************************

    //! @description Shrinks a trie by finding prefixes.
    //! @param tr a trie object
    public static Trie shrink(Trie tr) {
        return shrinkRec(tr, "", -1, 0);
    }

    //! @description Shrinks a trie by finding prefixes.
    //! @param tr a trie object
    //! @param delimiter a delimiter to be used when strings are joined
    public static Trie shrink(Trie tr, String delimiter) {
        return shrinkRec(tr, delimiter, -1,0);
    }

    //! @description Shrinks a trie by finding prefixes.
    //! @param tr a trie object
    //! @param delimiter a delimiter to be used when strings are joined
    public static Trie shrinkByThreshold(Trie tr, String delimiter, double threshold ) {
        return shrinkRec(tr, delimiter, threshold,0);
    }

    //! @description Shrinking recursive function.
    //! @param tr a trie object
    //! @param delimiter a delimiter for the concatenation of the node keys
    //! @param threshold if negative automatic shrinking test is applied
    //! @param n recursion level
    protected static Trie shrinkRec(Trie tr, String delimiter, double threshold, int n) {
        Trie trRes = new Trie();
        Boolean rootQ = ((n == 0) && tr.getKey().equals(""));

        if (tr.getChildren() == null || tr.getChildren().isEmpty()) {

            return tr;

        } else if (!rootQ && tr.getChildren().size() == 1) {
            List<Trie> arr = new ArrayList<Trie>(tr.getChildren().values());
            boolean shrinkQ = false;

            if (threshold < 0) {
                shrinkQ = tr.getValue() == arr.get(0).getValue();
            } else {
                shrinkQ = arr.get(0).getValue() >= threshold;
            }

            if ( shrinkQ )  {
                // Only one child and the current node does not make a complete match:
                // proceed with recursion and join with result.

                Trie chTr = shrinkRec(arr.get(0), delimiter, threshold, n + 1);

                trRes.setKey(tr.getKey() + delimiter + chTr.getKey());
                trRes.setValue(tr.getValue());

                if (!(chTr.getChildren() == null || chTr.getChildren().isEmpty())) {
                    trRes.setChildren(chTr.getChildren());
                }

            } else {
                // Only one child but the current node makes a complete match.

                Trie chTr = shrinkRec(arr.get(0), delimiter, threshold,n + 1);

                trRes.setKey(tr.getKey());
                trRes.setValue(tr.getValue());
                trRes.setChildren(new HashMap<String, Trie>());
                trRes.getChildren().put(chTr.getKey(), chTr);
            }

            return trRes;

        } else {
            // No shrinking at this node. Proceed with recursion.
            Map<String, Trie> recChildren = new HashMap<>();

            for (Trie chTr : tr.getChildren().values()) {
                Trie nTr = shrinkRec(chTr, delimiter, threshold, n + 1);
                recChildren.put(nTr.getKey(), nTr);
            }

            trRes.setKey(tr.getKey());
            trRes.setValue(tr.getValue());
            trRes.setChildren(recChildren);

            return trRes;
        }
    }

    ///**************************************************************
    /// Statistics functions
    ///**************************************************************

    public static List<Integer> nodeCounts(Trie tr) {
        // The result would like { "total"->23, "internal"->12, leaves->"11" }.

        Pair<Integer, Integer> res = nodeCountsRec(tr, 0, 0);

        return new ArrayList<Integer>()
           {{ add(res.getKey() + res.getValue()); add(res.getKey()); add(res.getValue()); }};
    }

    protected static Pair<Integer, Integer> nodeCountsRec(Trie tr, int nInternal, int nLeaves) {

        if ( tr.getChildren() == null || tr.getChildren().isEmpty() ) {

            return new Pair<Integer, Integer>( nInternal, nLeaves + 1);

        } else {

            Pair<Integer, Integer> res = new Pair(nInternal,nLeaves);

            for (Trie chTr : tr.getChildren().values()) {
                res = nodeCountsRec(chTr, res.getKey(), res.getValue() );
            }

            return new Pair<Integer, Integer>( res.getKey()+1, res.getValue() );
        }
    }

    ///**************************************************************
    /// General traversal functions
    ///**************************************************************

    //! @description Interface for functions to be applied on the key and value of a Trie node.
    public interface TrieKeyValueFunction {
        Pair<String, Double> apply( String k, Double val );
    }

    //! @description Map a function over the key and value of each node in a trie.
    public static Trie map( Trie tr,  TrieKeyValueFunction func ) {

        if (tr == null ) {

            return null;

        } else if ( func == null ) {

            return tr.clone();

        } else if ( tr.getChildren() == null || tr.getChildren().isEmpty()) {

            Pair<String, Double> pres = func.apply( tr.getKey(), tr.getValue() );

            return new Trie( pres.getKey(), pres.getValue());

        } else {

            Map<String, Trie> resChildren = new HashMap<>();

            for (Map.Entry<String, Trie> elem : tr.getChildren().entrySet()) {

                Trie chNode = map(elem.getValue(), func);

                resChildren.put(elem.getKey(), chNode);
            }

            Pair<String, Double> pres = func.apply(tr.getKey(), tr.getValue());

            return new Trie(pres.getKey(), pres.getValue(), resChildren);
        }
    }

    //! @description Interface for functions to be applied on a Trie node.
    public interface TrieNodeFunction {
        Trie apply( Trie node );
    }

    //! @description Map a function over the key and value of each node in a trie.
    //! @param tr a trie object
    //! @param preFunc a function object to be applied before the recursive call to map
    //! @param postFunc a function object to be applied after the recursive call to map
    //! @return Another trie object. Each node of the resulting trie is new Trie object.
    public static Trie map( Trie tr,  TrieNodeFunction preFunc, TrieNodeFunction postFunc ) {

        if (tr == null ) {

            return null;

        } else if ( preFunc == null && postFunc == null ) {

            return tr.clone();

        } else {

            Trie res;
            Map<String, Trie> resChildren = null;

            if ( preFunc != null ) {
                res = preFunc.apply(tr);
            } else {
                res = tr;
            }

            if ( tr.getChildren() == null || tr.getChildren().isEmpty()) {

                resChildren = null;

            } else {

                resChildren = new HashMap<>();

                for (Map.Entry<String, Trie> elem : res.getChildren().entrySet()) {

                    Trie chNode = map(elem.getValue(), preFunc, postFunc);

                    resChildren.put(elem.getKey(), chNode);
                }

            }

            if ( postFunc != null ) {
                res = postFunc.apply(res);
            }

            res.setChildren( resChildren );

            return res;
        }
    }

    ///**************************************************************
    /// Removal functions
    ///**************************************************************

    //! @description A class for removal by a threshold.
    private static class ThresholdRemoval implements TrieNodeFunction {
        ThresholdRemoval() { threshold = 1; belowThresholdQ = true; postfix = null; }
        ThresholdRemoval( double th ) { threshold = th; belowThresholdQ = true; postfix = null; }
        ThresholdRemoval( double th, boolean bQ ) { threshold = th; belowThresholdQ = bQ; postfix = null; }
        ThresholdRemoval( double th, boolean bQ, String tm ) {
            threshold = th; belowThresholdQ = bQ; postfix = tm;
        }

        public double threshold;
        public boolean belowThresholdQ;
        public String postfix;

        public Trie apply( Trie tr ) {
            if( tr.getChildren() == null || tr.getChildren().isEmpty() ) {
                return tr.clone();
            } else {
                Map<String, Trie> resChildren = new HashMap<>();
                double removedSum = 0;

                for (Map.Entry<String, Trie> elem : tr.getChildren().entrySet()) {

                    if ( belowThresholdQ && elem.getValue().getValue() >= threshold ||
                            !belowThresholdQ && elem.getValue().getValue() < threshold ) {
                        resChildren.put( elem.getKey(), elem.getValue() );
                    } else {
                        if ( postfix != null ) {
                            removedSum = removedSum + elem.getValue().getValue();
                        }
                    }
                }

                if ( postfix != null && removedSum > 0 ) {
                    resChildren.put(postfix, new Trie(postfix, removedSum ) );
                }

                Trie res = new Trie( tr.getKey(), tr.getValue() );
                res.setChildren( resChildren );

                return res;
            }
        }
    }

    //! @description Remove nodes with values below a specified threshold.
    public static Trie removeByThreshold( Trie tr, double threshold ) {
        return removeByThreshold( tr, threshold, true, null );
    }

    //! @description Remove nodes with values below a specified threshold.
    public static Trie removeByThreshold( Trie tr, double threshold, String postfix) {
        return removeByThreshold( tr, threshold, true, postfix );
    }

    //! @description Remove nodes with values below/above a specified threshold.
    public static Trie removeByThreshold( Trie tr, double threshold, boolean belowThresholdQ ) {
        return removeByThreshold( tr, threshold, belowThresholdQ, null );
    }

    //! @description Remove nodes with values below or above a specified threshold.
    public static Trie removeByThreshold( Trie tr, double threshold, boolean belowThresholdQ, String postfix ) {

        ThresholdRemoval thRemovalObj = new ThresholdRemoval( threshold, belowThresholdQ, postfix);

        return map( tr, thRemovalObj, null );
    }

    private static class ByKeyRegexRemoval implements TrieNodeFunction {
        ByKeyRegexRemoval(String kp ) { keyPattern = kp; postfix = null; }
        ByKeyRegexRemoval(String kp, String rt ) { keyPattern = kp; postfix = rt; }

        public String keyPattern;
        public String postfix;

        public Trie apply( Trie tr ) {
            if( tr.getChildren() == null || tr.getChildren().isEmpty() ) {
                return tr.clone();
            } else {
                Map<String, Trie> resChildren = new HashMap<>();
                double removedSum = 0;

                for (Map.Entry<String, Trie> elem : tr.getChildren().entrySet()) {

                    if ( !elem.getKey().matches( keyPattern )) {
                        resChildren.put( elem.getKey(), elem.getValue() );
                    } else {
                        if ( postfix != null ) {
                            removedSum = removedSum + elem.getValue().getValue();
                        }
                    }
                }

                if ( postfix != null && removedSum > 0 ) {
                    resChildren.put(postfix, new Trie(postfix, removedSum ) );
                }

                Trie res = new Trie( tr.getKey(), tr.getValue() );
                res.setChildren( resChildren );

                return res;
            }
        }
    }

    //! @description Remove nodes with keys satisfying a regexp.
    public static Trie removeByKeyRegex( Trie tr, String keyRegex ) {

        ByKeyRegexRemoval kpRemovalObj = new ByKeyRegexRemoval( keyRegex );

        return map( tr, kpRemovalObj, null );
    }

    //! @description Remove nodes with keys satisfying a regexp and replace them laterally with postfix.
    public static Trie removeByKeyRegex( Trie tr, String keyRegex, String postfix ) {

        ByKeyRegexRemoval kpRemovalObj = new ByKeyRegexRemoval( keyRegex, postfix);

        return map( tr, kpRemovalObj, null );
    }

    private static class ByParetoFractionRemoval implements TrieNodeFunction {
        ByParetoFractionRemoval( ) {
            this.paretoFraction = 0.8;
            this.removeBottomElementsQ = true;
            this.postfix = null;
        }
        ByParetoFractionRemoval(double paretoFraction ) {
            this.paretoFraction = paretoFraction;
            this.removeBottomElementsQ = true;
            this.postfix = null;
        }
        ByParetoFractionRemoval(double paretoFraction, boolean removeBottomElementsQ ) {
            this.paretoFraction = paretoFraction;
            this.removeBottomElementsQ = removeBottomElementsQ;
            this.postfix = null;
        }
        ByParetoFractionRemoval(double paretoFraction, boolean removeBottomElementsQ, String postfix ) {
            this.paretoFraction = paretoFraction;
            this.removeBottomElementsQ = removeBottomElementsQ;
            this.postfix = postfix;
        }

        public double paretoFraction;
        public boolean removeBottomElementsQ;
        public String postfix;

        public Trie apply( Trie tr ) {
            if( tr.getChildren() == null || tr.getChildren().isEmpty() ) {
                return tr.clone();
            } else {
                Map<String, Trie> resChildren = new HashMap<>();
                double removedSum = 0;
                double cumSum = 0;
                double threshold = 0;
                List<Double> valCumSums = new ArrayList<Double>();
                List<Trie> childrenList = new ArrayList<Trie>( tr.getChildren().values() );

                // Calculate the cumulative sum
                for ( Trie elem : childrenList ) {
                    cumSum += elem.getValue();
                }

                Collections.sort(childrenList, (Trie a1, Trie a2) -> a2.getValue().compareTo( a1.getValue() ) );

                threshold = paretoFraction * cumSum;

                // Remove children
                removedSum = 0; cumSum = 0;
                for ( Trie elem : childrenList ) {

                    if ( removeBottomElementsQ && cumSum <= threshold ||
                            !removeBottomElementsQ && cumSum > threshold ) {
                        resChildren.put( elem.getKey(), elem );
                    } else {
                        if ( postfix != null ) {
                            removedSum = removedSum + elem.getValue();
                        }
                    }
                    cumSum += elem.getValue();
                }

                if ( postfix != null && removedSum > 0 ) {
                    resChildren.put(postfix, new Trie(postfix, removedSum ) );
                }

                if ( resChildren.isEmpty() ) {
                    return tr.clone();
                }

                Trie res = new Trie(tr.getKey(), tr.getValue());
                res.setChildren(resChildren);

                return res;
            }
        }
    }

    //! @description Remove nodes with values below/above a Pareto threshold.
    public static Trie removeByParetoFraction( Trie tr, double paretoFraction) {

        ByParetoFractionRemoval removalObj = new ByParetoFractionRemoval( paretoFraction );

        return map( tr, removalObj, null );
    }

    //! @description Remove nodes with values below a Pareto thresholds and replace them laterally with postfix.
    public static Trie removeByParetoFraction( Trie tr, double paretoFraction, String postfix ) {

        ByParetoFractionRemoval removalObj = new ByParetoFractionRemoval( paretoFraction, true, postfix );

        return map( tr, removalObj, null );
    }

    //! @description Remove nodes with values below/above a Pareto threshold.
    public static Trie removeByParetoFraction( Trie tr, double paretoFraction, boolean removeBottomElementsQ ) {

        ByParetoFractionRemoval removalObj = new ByParetoFractionRemoval( paretoFraction, removeBottomElementsQ, null);

        return map( tr, removalObj, null );
    }

    //! @description Remove nodes with values below/above a Pareto threshold and replace them laterally with postfix.
    public static Trie removeByParetoFraction( Trie tr, double paretoFraction, boolean removeBottomElementsQ, String postfix ) {

        ByParetoFractionRemoval removalObj = new ByParetoFractionRemoval( paretoFraction, removeBottomElementsQ, postfix );

        return map( tr, removalObj, null );
    }
}