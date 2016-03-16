--[[
    Emulating Mathematica functions in Lua
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


mf = {}

--[[
local modname = ...
local mf = {}
_G[modname] = mf
package.loaded[modname] = mf
--]]

-- ************
-- Equal
-- ************
function mf.Equal( a, b )
   if ( type(a) == "table" and type(b) == "table" ) then
	  if ( #a ~= #b ) then 
		 return false
	  else
		 local eq = true
		 local i = 1
		 while ( eq and i <= #a ) do
			eq = mf.Equal( a[i], b[i] )
			i = i + 1
		 end
		 return eq
	  end
   else
	  return a == b
   end
end

-- ********************
-- Basic list functions
-- ********************

function mf.First( t )
	assert( type(t) == "table", "First:: table expected as a first argument.")
	return t[1]
end

function mf.Last( t )
	assert( type(t) == "table", "Last:: table expected as a first argument.")
	return t[#t]
end

function mf.Rest( t )
	assert( type(t) == "table", "Rest:: table expected as a first argument.")
	local res={}
	for i, e in ipairs(t) do
		if i > 1 then
			res[i-1] = e
		end
	end
	return res
end

function mf.Most( t )
	assert( type(t) == "table", "Most:: table expected as a first argument.")
	local res={}
	for i, e in ipairs(t) do
		if i < #t then
			res[#res+1] = e
		end
	end
	return res
end

-- ************
-- Append
-- ************
function mf.Append( t, a )
	assert( type(t) == "table", "Append:: table is expected as a first argument.")
	local res = {}
	for j, e in ipairs(t) do
		res[j] = e
	end
	res[ #t + 1] = a
	return res
end

function mf.AppendTo( t, a )
	assert( type(t) == "table", "AppendTo:: table is expected as a first argument.")
	t[#t + 1] = a
	return t
end

-- ************
-- Prepend
-- ************
function mf.Prepend( t, a )
	assert( type(t) == "table", "Prepend:: table is expected as a first argument.")
	local res = {a}
	for j, e in ipairs(t) do
		res[#res + 1] = e
	end
	return res
end

function mf.PrependTo( t, a )
	assert( type(t) == "table", "PrependTo:: table is expected as a first argument.")
	table.insert(t,1,a)
	return t
end


-- ************
-- Part
-- ************

function GetColumns ( list, inds )
   assert( type(list) == "table", "GetColumns:: first argument is expected to be a table." ) 
   assert( type(inds) == "table", "GetColumns:: second argument is expected to be a table." )
   local res = {};
   local row = {};   
   for i, e in ipairs(list) do
	  row = {}
	  for j, l in ipairs(inds) do
		 row[#row + 1] = e[l]
	  end
	  res[#res + 1] = row
   end
   return res
end

function GetColumn ( list, ind )
   assert( type(list) == "table", "GetColumn:: first argument is expected to be a table." )
   assert( type(ind) == "number", "GetColumn:: second argument is expected to be a number." )
   local res = {};
   for i, e in ipairs(list) do
	  assert( type(e) == "table", "GetColumn:: an element is not a table.")
	  res[#res + 1] = e[ind]
   end
   return res
end

function GetRows ( list, inds )
   assert( type(list) == "table", "GetRows:: first argument is expected to be a table." )
   assert( type(inds) == "table", "GetRows:: second argument is expected to be a table." )
   local res = {};
   if #inds == 0 then return list end
   for i, e in ipairs(inds) do
	  res[#res + 1] = list[e]
   end
   return res
end

function mf.Part( ... )
   local args = {...}
   local list
   local rinds
   local cinds
   local res
   list, rinds, cinds = ...

   assert( type(list) == "table", "Part:: the first argument is expected to be a table." )
   assert( rinds ~= nil or cinds ~= nil, "Part:: at least two arguments are expected." )
   assert( 
	  ( rinds == "all" or rinds == nil or type(rinds) == "number" or mf.VectorQ(rinds, function (x) return type(x) == "number" end)) and
	  ( cinds == "all" or cinds == nil or type(cinds) == "number" or mf.VectorQ(cinds, function (x) return type(x) == "number" end)),
		 "Part:: the second argument should be a number or a list of numbers.")

   if rinds == "all" then rinds = nil end
   if cinds == "all" then cinds = nil end

   if cinds == nil and type(rinds) == "number" then

	  return list[rinds]

   elseif cinds == nil and type(rinds) == "table" then

	  return GetRows(list, rinds)

   elseif rinds == nil or type(rinds) == "table" and #rinds == 0 then

	  if type(cinds) == "number" then
		 return GetColumn(list, cinds)
	  elseif type(cinds) == "table" then 
		 return GetColumns(list, cinds)
	  end

   else

	  if type(rinds) == "number" then		 
		 res = {list[rinds]}
	  else
		 res = GetRows(list,rinds)
	  end

	  if type(cinds) == "number" then
		 return GetColumn(res, cinds)
	  else
		 return GetColumns(res, cinds)
	  end

   end

   return list
end

-- ************
-- Join
-- ************
function mf.Join( ... )
   local args = {...}
   local b = true
   for i, v in ipairs(args) do
	  b = b and (type(v) == "table")
   end
   assert( b, "Join:: tables are expected as arguments.")
   local res = {}
   local clen = 1
   for i=1, #args do 
	  for j=1, #args[i] do
		 if ( not ( type( args[i][j] ) == "table" and #args[i][j] == 0 ) ) then
			res[clen] = args[i][j]
			clen = clen + 1
		 end 
	  end
   end
   return res
end


-- ************
-- Reverse
-- ************
function mf.Reverse( list )
   assert( type(list) == "table", "Reverse:: a table is expected as an argument." )
   local res = {}
   for i=#list, 1, -1 do
	  res[#res + 1] = list[i]
   end
   return res
end


-- ************
-- Flatten
-- ************
function mf.Flatten( ... )
   local args = {...}
   local level = -1
   assert( type(args[1]) == "table", "Flatten:: a table is expected as a first argument." )
   if #args > 1 then
	  assert( type(args[2]) == "number" and args[2] > 0, "Flatten:: a positibe integer is expected as a second argument." )
	  level = args[2]
   end

   local res = {}

   function FlattenList( list, l )
	  for i, e in ipairs(list) do
		 if type(e) == "table" and ( l < level or level == -1 ) then
			FlattenList( e, l + 1 )
		 else
			res[#res+1] = e
		 end		 
	  end
   end
   
   FlattenList( args[1], 0 )

   return res
end


-- ************
-- Transpose
-- ************
function mf.Transpose( args )
   local msg = "Transpose:: a table of at least two tables is expected as an argument."
   assert( type(args) == "table" and #args > 1, msg )

   -- check all arguments are tables with the same length
   assert( type(args[1]) == "table",  msg)
   for i, e in ipairs(args) do
	  assert( type(args[1]) == "table" and #args[i] == #args[1], "Transpose:: the elements of the argument are expected to be tables of the same length." )
   end

   -- transpose
   local res = {}
   for i=1,#args[1] do
	  local tuple={}
	  for j=1,#args do
		 tuple[#tuple + 1] = args[j][i] 
	  end
	  res[#res + 1] = tuple
   end
   return res
end


-- ************
-- Apply
-- ************
function mf.Apply( func, tbl )
   assert( type(func) == "function", "Apply:: function is expected as a first argument.")
   assert( type(tbl) == "table", "Apply:: tablw is expected as a second argument.")
   return func( table.unpack(tbl) )
end


-- ************
-- Map
-- ************
function mf.Map( func, list )
	assert( type(func) == "function", "Map:: function expected as a first argument.")
	assert( type(list) == "table", "Map:: table expected as a second argument.")
	local t={}
	for i, e in ipairs(list) do
		t[i] = func(e)
	end
	return t
end


-- ************
-- MapThread
-- ************
function mf.MapThread( func, ...)
	assert( type(func) == "function", "MapThread:: function expected as a first argument.")
	local arg = {n=select('#',...),...}
	assert( arg.n >= 2,
		"MapThread:: more than two arguments are expected. Expected are a function and two or more tables of the same length."
	)
	assert( type(arg[1]) == "table", "MapThread:: table is expected as a second argument.")
	local len = #arg[1]
	for i=2, arg.n, 1 do
	--print(i, type(arg[i]))
		if type(arg[i]) ~= "table" or #arg[i] ~= len then
			print("MapThread:: tables with equal lengths are expected after the first argument.")
			return nil
		end
	end
	local res={}
	local nargs=arg.n
	for i=1,len,1 do
		local arglist = {}
		for j=1,nargs,1 do
			arglist[j] = arg[j][i]
		end
		res[i] = func(table.unpack(arglist))
	end
	return res
end


-- ************
-- Fold
-- ************
function mf.Fold( func, initVal, list )
	assert( type(func) == "function", "Fold:: function expected as a first argument.")
	assert( type(list) == "table", "Fold:: third argument is not a table.")
	local res = initVal
	for i, e in ipairs(list) do
		res = func( res, e)
	end
	return res
end

function mf.FoldList( func, initVal, list )
	assert( type(func) == "function", "FoldList:: function expected as a first argument.")
	assert( type(list) == "table", "FoldList:: third argument is not a table.")
	local res= {}
	local r = initVal
	for i, e in ipairs(list) do
		r = func( r, e)
		res[i] = r
	end
	return res
end


-- ************
-- Rules
-- ************
function mf.Replace( list, rules )
	assert( type(rules) == "table", "Replace:: table is expected as a second argument")
	if type(list) == "table" then
	   local res = {}
	   local t;
	   for j, e in ipairs(list) do
		  if ( type(e) == "table" ) then
			 t = mf.Replace(e, rules) 
		  else
			 t = e
		  end
		  res[j] = rules[t] or t
		end
		return res
	else
		return rules[list] or list
	end
end

function mf.ReverseRules( t )
	assert( type(t) == "table", "ReverseRules:: table is expected as an argument.")
	local res={}
	for j, e in pairs(t) do 
		res[e] = j
	end
	return res
end

-- ************
-- Thread
-- ************

-- ************
-- Range
-- ************
function mf.Range( sr, er, step )
   step = step or 1
   assert( type(sr) == "number" and type(er) == "number" and type(step) == "number", 
		   "Range:: only numbers are expected as arguments.")
   local t = {}
   local k = sr
   if step > 0 then
	  while k <= er do
		 t[#t + 1] = k
		 k = k + step
	  end
   else
	  while k >= er do
		 t[#t + 1] = k
		 k = k + step
	  end
   end
   return t
end


-- ************
-- MemberQ
-- ************
function mf.MemberQ( list, sn )
	assert( type(list) == "table", "MemberQ:: table is expected as a first argument.")
	for j, e in ipairs(list) do 
	   if ( mf.Equal( sn, e ) ) then
		  return true, j;
	   end
	end
	return false, nil
end


-- *******************
-- Gather and GatherBy
-- *******************
function mf.GatherBy ( list, byFunc )
   assert( type(list) == "table", "GatherBy:: table is expected as a first argument.")
   local uniqueVals = {}
   local vals = {}
   for i, v in ipairs(list) do
	  vals[#vals + 1] = byFunc(v)
	  if ( not mf.MemberQ( uniqueVals, vals[#vals] ) ) then
		 uniqueVals[#uniqueVals+1] =  vals[#vals]
	  end
   end
   local rrules = mf.ReverseRules( uniqueVals )
   local res = {}
   for i = 1, #uniqueVals do
	  res[#res + 1] = {}
   end
   local ind
   for i, v in ipairs(vals) do
	  ind = rrules[v]
	  mf.AppendTo( res[ind], list[i] )
   end
   return res
end   


-- *****************************
-- ShallowTally and ShallowUnion
-- *****************************
-- This Tally definition is shallow because it does not use mf.Equal.
-- In general, it should be faster than the Tally using mf.Equal.
function mf.ShallowTally ( list )
   assert( type(list) == "table", "ShallowTally:: table is expected as a first argument.")
   local tally = {};
   local ci = 0;
   for i, v in ipairs(list) do
	  if ( tally[v] == nil ) then
		 tally[v] = 1
	  else
		 tally[v] = tally[v] + 1
	  end
   end
   local res = {}
   for val, count in pairs(tally) do
	  res[#res+1] = {val,count} 
   end
   return res
end 

function mf.ShallowUnion ( list )
   assert( type(list) == "table", "ShallowUnion:: table is expected as a first argument.")
   local t = mf.ShallowTally(list)
   local res = {}
   for i, v in ipairs(t) do
	  res[#res+1] = v[1]
   end
   return res
end 


-- ************
-- Union
-- ************
function mf.Union( list )
   assert( type(list) == "table", "Union:: table is expected as a first argument.")
   local uniqueVals = {}
   for i, v in ipairs(list) do
	  if ( not mf.MemberQ( uniqueVals, v ) ) then
		 uniqueVals[#uniqueVals+1] = v 
	  end
   end
   return uniqueVals
end


-- ************
--  Tally
-- ************
function mf.Tally ( list )
   assert( type(list) == "table", "Tally:: table is expected as a first argument.")
   local uniqueVals = {}
   local counts = {}
   local inQ, ind
   for i, v in ipairs(list) do
	  inQ, ind = mf.MemberQ( uniqueVals, v )
	  if ( not inQ ) then
		 uniqueVals[#uniqueVals + 1] = v
		 counts[#counts + 1] = 1
	  else
		 assert( ind ~= nil, "Tally:: result from MemberQ is wrong!" )
		 counts[ind] = counts[ind] + 1
	  end
   end
   return mf.Transpose( {uniqueVals, counts} )
end 


-- ************
-- Total
-- ************
-- It would be nice to implement Total's second argument, 
-- the levels at which the summation is done.
function mf.Total( list ) 
   local msg = "Total:: a table of numbers is expected as an argument."
   assert( type(list) == "table", msg )
   local res = 0
   for i,e in pairs(list) do
	  assert( type(e)=="number", msg )
	  res = res + e
   end
   return res
end


-- ************
-- Min
-- ************
function mf.Min( list ) 
   local msg = "Min:: a table of numbers is expected as an argument."
   assert( type(list) == "table", msg )
   local minel = list[1]
   local mind = 1

   for i,e in pairs(list) do
	  assert( type(e)=="number", msg )
	  if list[i] < minel then
		 minel = list[i]
		 mind = i
	  end
   end
   return minel, mind
end


-- ************
-- Max
-- ************
function mf.Max( list ) 
   local msg = "Max:: a table of numbers is expected as an argument."
   assert( type(list) == "table", msg )
   local maxel = list[1]
   local maxd = 1

   for i,e in pairs(list) do
	  assert( type(e)=="number", msg )
	  if list[i] > maxel then
		 maxel = list[i]
		 maxd = i
	  end
   end
   return maxel, maxd
end


-- ************
-- Select
-- ************
function mf.Select( list, func )
   assert( type(list) == "table", "Select:: first argument is not a table.")
   assert( type(func) == "function", "Select:: function expected as a second argument.")
   local res = {}
   for i=1, #list do
	  if func( list[i] ) then
		 res[#res + 1] = list[i]
	  end
   end
   return res
end


-- ************
-- Print
-- ************
function mf.PrintTable( tbl, fullStringsQ )	
   assert( type(tbl) == "table", "PrintTable:: table is expected as a first argument.")
   io.write("{")
   for i=1, #tbl do
	  if type( tbl[i] ) == "table" then
		 mf.PrintTable( tbl[i], fullStringsQ )
	  elseif type( tbl[i] ) == "function" then
		 io.write( "function" )
	  elseif fullStringsQ and type( tbl[i] ) == "string" then
		 io.write( "\"", tbl[i], "\"" )
	  else
		 io.write(tbl[i])
	  end
	  if i ~= #tbl then
		 io.write(",")
	  end
   end
   io.write("}")   
   return nil
end   

function mf.Print(...)
   local args = {...}
   for i=1,#args do
	  if( type( args[i] ) == "table" ) then
		 mf.PrintTable( args[i] )
	  elseif type( args[i] ) == "function" then
		 io.write( "function" )	 
	  else
		 io.write( args[i] )
	  end
   end
   io.write("\n")

   return nil
end

function mf.PrintFullStrings(...)
   local args = {...}
   for i=1,#args do
	  if( type( args[i] ) == "table" ) then
		 mf.PrintTable( args[i], true )
	  elseif type( args[i] ) == "function" then
		 io.write( "function" )	 
	  elseif type( args[i] ) == "string" then
		 io.write( "\"", args[i], "\"" )
	  else
		 io.write( args[i] )
	  end
   end
   io.write("\n")

   return nil
end

-- ************
-- Characters
-- ************
function mf.Characters( s )
   assert( type(s) == "string", "Characters:: a string is expected as a first argument.")
   local res = {}
   for i=1, #s do
	  res[#res + 1] = string.sub(s,i,i)
   end
   return res
end

-- ************
-- StringSplit
-- ************
function mf.StringSplit( s )
   assert( type(s) == "string", "StringSplit:: a string is expected as a first argument.")
   local words = {}
   for w in string.gmatch(s, "%S+") do
	  words[#words + 1] = w
   end
   return words
end

-- ******************************************
-- StringQ, ListQ, TableQ, NumberQ, MatrixQ
-- ******************************************
function mf.StringQ( s )
   return type(s)=="string"
end

function mf.ListQ( s )
   return type(s)=="table"
end

function mf.TableQ( s )
   return type(s)=="table"
end

function mf.NumberQ( s )
   return type(s)=="number"
end

function mf.FunctionQ( s )
   return type(s)=="function"
end

function mf.VectorQ( ... )
   local args = {...}
   assert( #args > 0, "VectorQ:: one or two arguments are expected. A table and an optional test function.")
   if #args == 1 then
	  return type(args[1]) == "table"
   elseif type(args[1]) == "table" then
	  local tfunc = args[2]
	  for i, e in ipairs(args[1]) do
		 if not tfunc(e) then
			return false
		 end
	  end
	  return true
   end
   return false
end

function mf.MatrixQ( s )
   local len
   if type(s) ~= "table" then
	  return false
   else
	  for i, e in ipairs(s) do
		 if type(e) ~= "table" then
			return false
		 elseif i == 1 then
			len = #e
		 else
			if len ~= #e then
			   return false
			end
		 end
	  end
	  return true, #s, len
   end
   return false 
end


-- ************
-- RandomSample
-- ************
function mf.RandomSample( tbl, n )
   assert( type(tbl) == "table", "RandomSample:: a table is expected as a first argument.")
   assert( type(n) == "number", "RandomSample:: an integer is expected as a second argument.")

   local res = {}
   local pickedInds = {}
   local j

   for i = 1, n do 
	  if #tbl > n then
		 repeat
			j = math.random( #tbl )
		 until not mf.MemberQ( pickedInds, j )
	  else
		 j = math.random( n )
	  end
	  res[#res+1] = tbl[j]
	  pickedInds[#pickedInds+1] = j
   end

   return res
end


-- ***********
-- Deep copy
-- ***********
function mf.DeepCopy(t)
   if type(t) ~= 'table' then return t end
   local mt = getmetatable(t)
   local res = {}
   for k,v in pairs(t) do
	  if type(v) == 'table' then
		 v = mf.DeepCopy(v)
	  end
	  res[k] = v
   end
   setmetatable(res,mt)
   return res
end

-- ********************************************************************************
-- Return values
-- ********************************************************************************	 
return mf