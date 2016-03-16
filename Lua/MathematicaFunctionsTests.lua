--[[
    Tests for emulating Mathematica functions in Lua
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

#!/usr/local/bin/lua 

-- load the file with definitions
require "./MathematicaFunctions"

-- ********
-- Tests for Mathematica-like functions 
-- ********

--[[ Map and FoldList
print()
print("====================")
print( "Map and FoldList")

t = {1,2,3,4,5,6,7,8}

t2 = mf.Map( function (x) return x^2 end, t)

print( table.unpack(t2))

t3 = mf.FoldList( function (a,b) return a+b end, 0, t)

print( table.unpack(t3))
--]]

---[[ Map and Transpose
print()
print("====================")
print( "Map and Transpose" )
t1 = {1,2,3,4,5,6}
t2 = {2,4,6,8,9,7}

t2 = mf.Map( function (x) return 10*x end, t2)

t3 = mf.Transpose( {t1, t2} )

for i,e in ipairs(t3) do
   print( table.unpack(e) )
end
---]]

--[[
print()
print("====================")
print( "ReverseRules and Replace" )

t2 = {2,4,6,8,9,7}
s = mf.ReverseRules( t2 )

for j, e in pairs(s) do
   print( j, " ", e)
end

print( table.unpack( mf.Replace( {1,2,3,4}, s) ) )
print( table.unpack( mf.Replace( {7,5,2,10}, s) ) )

print()

t2 = {"a","b","c","d","e","f"}
s = mf.ReverseRules( t2 )

for j, e in pairs(s) do
   print( j, " ", e)
end

print( table.unpack( mf.Replace( {1,5,6}, t2) ) )
print( table.unpack( mf.Replace( {"a","f","c"}, s) ) )
--]]

--[[
print()
print("====================")
print( "MemberQ")

print( " mf.MemberQ(t2,\"d\") ", mf.MemberQ(t2,"d") )
print( " mf.MemberQ(t2,\"k\") ", mf.MemberQ(t2,"k") )
--]]

---[[
print()
print("====================")
print( "Join")

et = {} 
print( table.unpack( mf.Join( {1,5,6}, {"a",{},et,"b"}, {"k",{1,23},12} )))

res = mf.Apply( mf.Join, { {1,5,6}, {"a",{},et,"b"}, {"k",{1,23},12} } )
print( unpack(res) )

---]] 



--[[
print()
print("====================")
print( "ShallowTally")

print("list:")
t = {1,4,6,"a","k","a",1,4,1,4}
print( table.unpack( t ) )

print("tally:")
res = mf.ShallowTally( t )
for i, v in ipairs(res) do 
   print( table.unpack(v) ) 
end
--]] 

--[[
print()
print("====================")
print( "ShallowUnion")

print("list:")
t = {1,4,6,"a","k","a",1,4,1,4}
print( table.unpack( t ) )

print("union:")
res = mf.ShallowUnion( t )
print( table.unpack(res) )
--]]


--[[
print()
print("====================")
print( "Equal")

print("list:")
t1 = {1,4,6,"a","b"}
t2 = {1,4,6,"a","b"}
t3 = {5, "4", 1}
t4 = {5, 6, t1, t3 }
t5 = {5, 6, t2, t3 }

print( "t1= ", table.unpack(t1))
print( "t2= ", table.unpack(t2))
print( "t3= ", table.unpack(t3))
print( "t4= ", table.unpack(t4))
print( "t5= ", table.unpack(t5))

print( "t1==t2 ", mf.Equal( t1, t2 ) )

print( "t1==t3 ", mf.Equal( t1, t3 ) )

print( "t4==t5 ", mf.Equal( t4, t5 ) )


t6 = { "head", t3, {"h", t1 } }
t7 = { "head", t3, {"h", t2 } }
t8 = { "head", t3, {"h", {1,4,6,"c","b"} }}

print( "t6= ", table.unpack(t6))
print( "t7= ", table.unpack(t7))
print( "t8= ", table.unpack(t8))

print( "t6==t7 ", mf.Equal(t6,t7))
print( "t6==t8 ", mf.Equal(t6,t8))
--]]

--[[
print()
print("====================")
print( "Append and AppendTo")

print("list:")
t = {1,4,6,"a","k","a"}
t1 = mf.Append( t, 67 )
print( "t=", t, " t1=", t1);
print( "t = ", table.unpack( t ) )
print( "t1= ", table.unpack( t1 ) )

mf.AppendTo(t1, 68)
print( "t1= ", table.unpack( t1 ) )
--]]

--[[
print()
print("====================")
print( "Prepend")

print("list:")
t = {1,4,6,"a","k","a"}
t1 = mf.Prepend( t, 67 )
print( "t=", t, " t1=", t1);
print( "t = ", table.unpack( t ) )
print( "t1= ", table.unpack( t1 ) )
--]]



--[[
print()
print("====================")
print( "GatherBy")

print()
print("simple list, simple function:")
t = {1,4,1,"a","k","a"}
print( "t= ", table.unpack( t ) )

t1 = mf.GatherBy( t, function (x) return x end )

print("the gathered list:" )
for i, v in ipairs(t1) do
   print( "t1= ", table.unpack( v ) )
end

print()
print("simple list, complicated function:")
t = {1,4,4,5,6,4,3,2,3}
print( "t= ", table.unpack( t ) )

t1 = mf.GatherBy( t, function (x) return x/2 - math.floor(x/2) end )

print("the gathered list:" )
for i, v in ipairs(t1) do
   print( table.unpack( v ) )
end

print()
print("complicated list, complicated function:")
t = {{1,"a"},{2,"b"},{4,"d"},{5,"m"},{6,"t"}}
for i, v in ipairs(t) do
   print( "t= ", table.unpack( v ) )
end

t1 = mf.GatherBy( t, function (x) return x[1]/2 - math.floor(x[1]/2) end )

print("the gathered list:" )
for i, v in ipairs(t1) do
   print(i,":")
   for j, u in ipairs(v) do
	  print( "  ", table.unpack( u ) )
   end
end
--]]


---[[
print()
print("====================")
print( "Tally")

print("list:")
t = {1,4,6,"a",{"k",1},"a",1,{"k",1},1,4}
mf.Print(t)

print("tally:")
res = mf.Tally( t )
mf.Print(res)
---]] 

--[[
print()
print("====================")
print( "Union")

print("list:")
t = {1,4,6,"a",{"k",1},"a",1,{"k",1},1,4}
print( table.unpack( t ) )

print("union:")
res = mf.Union( t )
print( table.unpack( res ) )
for i, v in ipairs(res) do
   if type( v ) == "table" then
	  print( table.unpack( v ) )
   else
	  print( v ) 
   end
end
--]]

--[[
print()
print("====================")
print( "Print")
t = {1,4,6,"a",{"k",1},"a",1,{"k",{{"m",mf.Join,6,7,{},0}}},1,4}
mf.Print( t )
print()
--]]


--[[
print()
print("====================")
print( "Characters and StringSplit")
str = "MnS4 + H2O -> H2 + Mn + S"
mf.Print( mf.Characters(str) )
mf.Print( mf.StringSplit(str) )
print()
--]]


--[[
print()
print("====================")
print( "Flatten")

print("list:")
t = {1,4,6,"a",{"k",{{"2w",13,15,16,17,{24,25,{33,32}}}, "5"}},"a",1,{"k",1},1,4}
mf.Print(t)

print("Flatten:")
res = mf.Flatten( t )
mf.Print(res)

print("Flatten[t,2]:")
res = mf.Flatten( t, 2 )
mf.Print(res)
--]] 


--[[
print()
print("====================")
print( "Min and Max")

print("list:")
t = {1,4,6,{2,{13,15,16,17,{24,25,{33,32}},5}},1,{2,1},1,4}
mf.Print(t)

print("Min:")
res = mf.Min( mf.Flatten( t ) )
mf.Print(res)

print("Max:")
res = mf.Max( mf.Flatten( t ) )
mf.Print(res)
--]] 

--[[
print()
print("====================")
print( "Range")

res = mf.Range( 2.5, 11.7, 2.3 )
mf.Print(res)

res = mf.Range( 12.5, -1.2, -3.3 )
mf.Print(res)
--]] 


---[[
print()
print("====================")
print( "Part")

mat = {{1,2,3,4},{21,22,23,24},{31,32,33,34}}

mf.Print( mat )

res = mf.Part( mat, 2 )
mf.Print( "mat[[2]] = ", res )

res = mf.Part( mat, {1,3} )
mf.Print( "mat[[{1,3}]] = ", res )

res = mf.Part( mat, nil, 3 )
mf.Print( "mat[[All, 3]] = ", res)

res = mf.Part( mat, nil, {1,4} )
mf.Print( "mat[[All, {1,4}]] = ", res)

res = mf.Part( mat, {2,3}, {1,2} )
mf.Print("mat[[{2,3}, {1,2}]] = ", res)
---]] 

---[[
print()
print("====================")
print( "RandomSample")

list = {1,2,3,4,5,6,7,8}

math.randomseed(os.time())
mf.Print( mf.RandomSample(list,3))