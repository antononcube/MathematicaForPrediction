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


import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Trie {

	protected String key;
	protected Double value;
	protected Map<String, Trie> children;

	public String getKey() {
		return key;
	}
	public void setKey(String key) {
		this.key = key;
	}
	public Double getValue() {
		return value;
	}
	public void setValue(Double value) {
		this.value = value;
	}
	public Map<String, Trie> getChildren() {
		return children;
	}
	public void setChildren( Map<String, Trie> children ) {
		this.children = children;
	}

	public Trie( ) { }

	public Trie( String key, Double value ) {
		this.setKey( key );
		this.setValue( value );
	}

	public Trie( String key, Double value, Map<String,Trie> children ) {
		this.setKey( key );
		this.setValue( value );
		this.setChildren(children);
	}

	protected String toStringRec( int n ) {
		String offset = "";
		String childStr = "";
		int k=0;
		for ( int i = 0; i < n; i++ ){
			offset = offset + " ";
		}
		if ( this.getChildren() != null && !this.getChildren().isEmpty() ) {
			for ( Trie elem : this.getChildren().values() ) {
				if ( k == 0  ) {
					childStr = "\n" + offset + elem.toStringRec( n+1 );
				} else {
					childStr = childStr + ",\n" + offset + elem.toStringRec( n+1 );
				}
				k++;
			}
		} else {
			childStr = "";
		}
		return "[ key=" + this.getKey() + ", value=" + this.getValue() + ", children=" + childStr + "]";
	}

	public String toString( ) { 
		return this.toStringRec( 1 );
	}


	protected String toJSONRec( int maxLevel, int n ) {
		String childStr = "";
		int k = 0;
		if ( this.getChildren() != null && ( maxLevel < 0 || n <= maxLevel ) ) {
			for ( Trie elem : this.getChildren().values() ) {
				if ( k == 0  ) {
					childStr = elem.toJSONRec( maxLevel,n+1 );
				} else {
					childStr = childStr + ", " + elem.toJSONRec( maxLevel,n+1 );
				}
				k++;
			}
			childStr = "[" + childStr + "]";
		} else {
			childStr = "[]";
		}
		return "{ \"key\":" + "\"" + this.getKey() +  "\"" + ", \"value\":" + this.getValue() + ", \"children\":" + childStr + "}";
	}

	public String toJSON( int maxLevel ) {
		return this.toJSONRec( maxLevel, 1 );
	}

	public String toJSON( ) {
		return this.toJSONRec( -1, 1 );
	}

	//! @description Deep copy of a trie.
	public Trie clone() {

		Trie res = new Trie();

		res.setKey( this.getKey() );
		res.setValue( this.getValue() );

		if ( !(this.getChildren() == null || this.getChildren().isEmpty() ) ) {

			Map<String, Trie> resChildren = new HashMap<>();

			for ( Trie elem : this.getChildren().values() ) {
				resChildren.put( elem.getKey(), elem.clone() );
			}

			res.setChildren( resChildren );
		}

		return res;
	}

	//! @description Deep comparison of a trie.
	public Boolean equals( Trie tr ) {

		if ( !this.getKey().equals( tr.getKey() ) || !this.getValue().equals( tr.getValue() ) ) {
			return false;
		}

		Boolean b = !(this.getChildren() == null || this.getChildren().isEmpty() );
		Boolean bTr = !(tr.getChildren() == null || tr.getChildren().isEmpty() );

		if ( b && bTr ) {

			if ( this.getChildren().size() != tr.getChildren().size() ) {
				return false;
			}

			for ( Trie elem : this.getChildren().values() ) {

				if ( !tr.getChildren().containsKey( elem.getKey() ) ) {
					return false;
				}

				if ( ! elem.equals( tr.getChildren().get( elem.getKey() ) ) ) {
					return false;
				}
			}

			return true;

		} else if ( b != bTr ) {

			return false;

		}

		return true;
	}
}

