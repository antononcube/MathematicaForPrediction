--[[
    Tests for Functional Parsers in Lua
    Copyright (C) 2013-2016  Anton Antonov
	
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
	antononcube @ gmail . com,
	Windermere, Florida, USA.
--]]

--[[

	Lua is free software distributed under the terms of the MIT license reproduced here.
	Lua may be used for any purpose, including commercial purposes, at absolutely no cost.
	No paperwork, no royalties, no GNU-like "copyleft" restrictions, either.
	Just download it and use it (http://www.lua.org) .

    See the full Lua licence at : http://www.lua.org/license.html .
	
--]]

require "./FunctionalParsers"



function dPredQ(d) 
   if mf.MemberQ({"0","1","2","3","4","5","6","7","8","9"}, d) then 
	  return true
   else
	  return false
   end
end

pDigit = parse.satisfy( dPredQ )
-- pNumber = fp.ParsePredicate( dPredQ )

res =  pDigit( { "2", "t" } )
print( table.unpack( res[1][1] ) )
print( res[1][2] )

print("-----------------------")
print("parse.alt")
pAorB = parse.alt( parse.symbol("a"), parse.symbol("b") )

res =  pAorB( { "a", "b" } )
print( "#res = ", #res, res )
mf.Print(res)
print()


res =  pAorB( { "k", "b" } )
print( "#res = ", #res, res )
mf.Print(res)
print()

res = parse.symbol("a")({"a","b"})
mf.Print("parse.symbol(\"a\")({\"a\",\"b\"})", res)


res1 = mf.ParseComposeWithResults( parse.symbol("b"), res )
mf.Print("res1= ", res1)

print("-----------------------")
print("parse.seq")
pAB = parse.seq( pAorB, pAorB )
res =  mf.Map( pAB, { {"a","b"}, {"b","b"}, {"b","a"}} )
mf.Print( "#res = ", #res, " ", res )


print("-----------------------")
print("parse.seqr")
pBar = parse.seqr( parse.alt( parse.symbol("a"), parse.symbol("the") ), parse.symbol("bar") )
res =  mf.Map( pBar, { {"a","bar"}, {"the","bar"} } )
mf.Print( "#res = ", #res, " ", res )


print("-----------------------")
print("parse.parenthesized")
pPAB = parse.parenthesized( pAorB )
res =  mf.Map( pPAB, { {"(","b",")"}, {"(","a",")"}, {"(","b",")"}  } )
mf.Print( "#res = ", #res, " ", res )


print("-----------------------")
print("parse.seql")
pPAB = parse.parenthesized( parse.seql( parse.symbol("a"), parse.option1( parse.symbol("b") ) ) )
res =  mf.Map( pPAB, { {"(","a","b",")"}, {"(","a",")"} } )
mf.Print( "#res = ", #res, " ", res )


print("-----------------------")
print("parse.many1")
res =  parse.many1( pAorB )( { "a",  "b", "a", "a" } )
mf.Print( "#res = ", #res, " ", res )

print("-----------------------")
print("parse.many")
res =  parse.many( pAorB )( { "a",  "b", "a", "a" } )
mf.Print( "#res = ", #res, " ", res )

print("-----------------------")
print("parse.listof")
pLAorB = parse.listof( pAorB, parse.symbol("*") )
res = pLAorB( { "a", "*", "b", "*", "a", "*", "a" } )
mf.Print( "#res = ", #res, " ", res )

print("-----------------------")
print("parse.chainleft")
pCLAorB = parse.chainleft( pAorB, parse.symbol("*"), function (s1, s2) return s1..s2 end )
res = pCLAorB( { "a", "*", "b", "*", "a", "*", "a" } )
mf.Print( "#res = ", #res, " ", res )


print("-----------------------")
print("parse.chainright")
pCRAorB = parse.chainright( pAorB, parse.symbol("*"), function (s1, s2) return s1..s2 end )
res = pCLAorB( { "a", "*", "b", "*", "a", "*", "a" } )
mf.Print( "#res = ", #res, " ", res )


print("-----------------------")
print("parsing a number")
--pNumber = parse.listof( pDigit, parse.epsilon )
pNumber = parse.apply( function (parsed) 
						  return mf.Fold( function (acc, s) return 10*acc + s-"0" end, 0, parsed) 
					   end, 
					   parse.many1( pDigit ) )
res = pNumber( { "1", "6", "7", "0"} )
mf.Print( "#res = ", #res, " ", res )
print( type(res[1][2]))