--[[
    Functional Parsers in Lua
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

fp = {}

-- The argument of a "final" parser is a table.
-- A table of table-pairs is returned: a list of successes.
-- Most of the parser definitions below give functions that are parsers.

-- include Mathematica-like functions 
require "../MathematicaFunctions"


-- ********************************************************************************
-- Basic parsers
-- ********************************************************************************
function fp.ParseSymbol ( s )
   if type(s) == "string" or type(s) == "number" then
	  return
		 function (xs) 
		 if ( xs[1] == s ) then 
			return { { mf.Rest(xs), xs[1] } } 
		 else 
			return {} 
		 end 
		 end
   else 
	  return nil
   end
end

function fp.ParsePredicate ( pred )
   if type(pred) == "function" then
	  return 
		 function (xs) 
		 if ( #xs > 0 and pred(xs[1]) ) then 
			return { { mf.Rest(xs), xs[1]} } 
		 else 
			return {} 
		 end 
		 end
   else 
	  return nil
   end
end

fp.ParseEpsilon = function (xs) return {{xs,{}}} end

function fp.ParseSucceed (v)
   return function (xs) return {{xs,v}} end
end

fp.ParseFail = function (xs) return {} end

-- ********************************************************************************
-- Basic combinators
-- ********************************************************************************

function fp.ParseAlternativeComposition (...) 
   local args = {...}
   return
	  function (xs)
	  local res = {}
	  for i, v in ipairs(args) do
		 res[#res+1] = v(xs)
	  end
	  return mf.Apply( mf.Join, res )
	  end
end


function mf.ParseComposeWithResults( p, res )
   assert( type(res) == "table", "ParseComposeWithResults:: table is expected as a second argument")
   if ( #res == 0 ) then
	  return res
   else
	  return mf.Apply( mf.Join,
					   mf.Map ( 
						  function (r)
							 local t
							 if #r == 0 then
								return r
							 else
								t = p(r[1]) 
								if #t == 0 then
								   return t
								else
								   return mf.Map( function (s) return {s[1],{r[2],s[2]}} end, t )
								end
							 end
						  end,
						  res )
					 )
   end   
end


function fp.ParseSequentialComposition (...)
   local args = {...};
   -- a check that all elements of args are parsers(functions) would be nice
   if ( args == nil or #args == 0 ) then
	  return {}
   elseif ( #args == 1 ) then
	  return function (xs) return args[1](xs) end
   else
   --elseif ( #args > 1 ) then
	  return 
		 function (xs) 
		 return 
			mf.Fold( 
			function (str,p) 
			   return mf.ParseComposeWithResults(p, str) 
			end,
			args[1](xs), 
			mf.Rest(args) ) 
		 end
   end
end


-- ********************************************************************************
-- Next combinators
-- ********************************************************************************

function fp.ParseApply(f, p)
   return 
	  function (xs)
	  return 
		 mf.Map( 
		 function (s) 
			return {s[1],f(s[2])} 
		 end, 
		 p(xs) )
	  end
end


function fp.ParseApply2(fNo, fYes, p)
   return 
	  function (xs)
	  return mf.Map(
		 function (s) 
			if #s[2]==0 then
			   return {s[1], fNo}
			else
			   return {s[1], fYes(s[2])} 
			end
		 end,
		 p(xs) )
	  end
end


function fp.ParseJust(p)
   return 
	  function (xs)
	  return mf.Select(p(xs), function (x) return #x[1]==0 end )
	  end
end


function fp.ParseShortest(p)
   return 
	  function (xs)
	  local parsed = p(xs)
	  if ( #parsed == 0 ) then
		 return parsed
	  else
		 table.sort( parsed, function (x, y) return #x[1] < #y[1] end )
		 return {parsed[1]}
	  end
	  end 
end


function fp.ParseSequentialCompositionPickLeft( p1, p2 )
   return 
	  function (xs)
	  return fp.ParseApply( function (r) return r[1] end, fp.ParseSequentialComposition(p1,p2))(xs)
	  end
end


function fp.ParseSequentialCompositionPickRight( p1, p2 )
   local p = fp.ParseSequentialComposition(p1,p2)
   return 
	  function (xs)
	  return fp.ParseApply( function (r) return r[2] end, fp.ParseSequentialComposition(p1,p2))(xs)
	  end
end

-- ********************************************************************************
-- Second next combinators
-- ********************************************************************************

function fp.ParsePack( s1, p, s2 )
   return
	  fp.ParseSequentialCompositionPickLeft(
	  fp.ParseSequentialCompositionPickRight(s1, p), s2)
end

function fp.ParseParenthesized(p)
   return
	  fp.ParsePack(fp.ParseSymbol("("), p, fp.ParseSymbol(")") )
end

function fp.ParseBracketed(p)
   return
	  fp.ParsePack(fp.ParseSymbol("["), p, fp.ParseSymbol("]") )
end

function fp.ParseCurlyBracketed(p)
   return
	  fp.ParsePack(fp.ParseSymbol("{"), p, fp.ParseSymbol("}") )
end

function fp.ParseOption1( p )
   return 
	  function (xs)
	  local res = p(xs)
	  if #res == 0 then
		 return {{xs,{}}}
	  else
		 return res
	  end
	  end
end

function fp.ParseMany1( p )
   return 
	  function (xs)
	  local t = {}
	  local res = fp.ParseShortest( fp.ParseOption1(p) )(xs)
	  while not ( type(res) == "table" and ( #res == 0 or #(res[1][2]) == 0 ) ) do
		 t[#t+1] = res[1][2]
		 res = fp.ParseShortest( fp.ParseOption1(p) )(res[1][1])
	  end
	  return {{res[1][1],t}}
	  end
end

function fp.ParseMany( p )
   return fp.ParseAlternativeComposition( fp.ParseMany1(p), fp.ParseSucceed( {} ) )
end
   
function fp.ParseListOf( p, separatorParser )
   return 
	  fp.ParseAlternativeComposition( 
	  fp.ParseApply( function (s) return mf.Prepend(s[2], s[1]) end,
					 fp.ParseSequentialComposition( p, 
													fp.ParseMany( fp.ParseSequentialCompositionPickRight( separatorParser, p ) ) ) 
				   ), 
	  fp.ParseSucceed( {} ) )
end

function fp.ParseChainLeft( p, separatorParser, func )
   return
	  fp.ParseAlternativeComposition( 
	  fp.ParseApply( function (s) return mf.Fold( function (acc, op) return func(acc,op[2]) end, s[1], s[2]) end,
					 fp.ParseSequentialComposition( p, 
													fp.ParseMany( fp.ParseSequentialComposition( separatorParser, p ) ) ) 
				   ), 
	  fp.ParseSucceed( {} ) )
end


function fp.ParseChainRight( p, separatorParser, func )
   return
	  fp.ParseAlternativeComposition( 
	  fp.ParseApply( function (s) return mf.Fold( function (acc, op) return func(acc,op[2]) end, s[2], mf.Reverse(s[1]) ) end,
					 fp.ParseSequentialComposition( fp.ParseMany( fp.ParseSequentialComposition( separatorParser, p ) ),
													p
												  )					 
				   ), 
	  fp.ParseSucceed( {} ) )
end


-- ********************************************************************************
-- EBNF parsers
-- ********************************************************************************

EBNFSymbolTest = function (s) return type(s)=="string" and ( s=="|" or s=="," or s=="=" or s==";" ) end
NonTerminalTest = function (s) return string.match(s,"^<[%w%-_]+>$") end
InQuotesTest = function (s) return string.match(s,"^'.+'$") or string.match(s,"^\".+\"$") end

function AddHead(h) 
   return function (s) return {h,s} end
end

pGTerminal = fp.ParsePredicate( function (s) return mf.StringQ(s) and InQuotesTest(s) and not EBNFSymbolTest(s) end )

pGNonTerminal = fp.ParsePredicate( function (s) return mf.StringQ(s) and NonTerminalTest(s) and not EBNFSymbolTest(s) end )

pGOption = fp.ParseApply( AddHead("EBFNOption"), fp.ParseBracketed(pGExpr) )

pGRepetition = fp.ParseApply( AddHead("EBNFRepetition"), fp.ParseCurlyBracketed(pGExpr) )

function pGNode (xs) 
   return
	  fp.ParseAlternativeComposition(
	  fp.ParseApply( AddHead("EBNFTerminal"), pGTerminal ),
	  fp.ParseApply( AddHead("EBNFNoneTerminal"), pGNonTerminal ),
	  fp.ParseParenthesized( pGExpr ),
	  pGOption,
	  pGRepetetion ) (xs)
end

pGTerm = fp.ParseApply( AddHead("EBNFSequence"), fp.ParseListOf( pGNode, fp.ParseSymbol(",") ) )

pGExpr = fp.ParseApply( AddHead("EBNFAlternatives"), fp.ParseListOf( pGTerm, fp.ParseSymbol("|") ) )


pGRule = fp.ParseApply( AddHead("EBNFRule"), 
						fp.ParseSequentialComposition( 
						   pGNonTerminal, 
						   fp.ParseSequentialCompositionPickLeft(
							  fp.ParseSequentialCompositionPickRight( fp.ParseSymbol("="), pGExpr ),
							  fp.ParseSymbol(";") ) ) )

fp.pEBNF = fp.ParseApply( AddHead("EBNF"), fp.ParseMany1( pGRule ) )

-- ********************************************************************************
-- parse table with shorter names
-- ********************************************************************************
parse = {}
parse.symbol = fp.ParseSymbol
parse.satisfy = fp.ParsePredicate
parse.epsilon = fp.ParseEpsilon
parse.succeed = fp.ParseSucceed
parse.fail = fp.ParseFail
parse.alt = fp.ParseAlternativeComposition
parse.seq = fp.ParseSequentialComposition
parse.seql = fp.ParseSequentialCompositionPickLeft
parse.seqr = fp.ParseSequentialCompositionPickRight
parse.parenthesized = fp.ParseParenthesized
parse.option1 = fp.ParseOption1
parse.many1 = fp.ParseMany1
parse.many = fp.ParseMany
parse.listof = fp.ParseListOf
parse.chainleft = fp.ParseChainLeft
parse.chainright = fp.ParseChainRight
parse.apply = fp.ParseApply
parse.shortest = fp.ParseShortest

-- ********************************************************************************
-- return values
-- ********************************************************************************
return fp, parse

