(*
    Soundex Mathematica package
    Copyright (C) 2017  Anton Antonov

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
*)

(*
    Mathematica is (C) Copyright 1988-2017 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: Soundex *)
(* :Context: Soundex` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-11-11 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

    # Introduction

    For the Soundex general idea and algorithm see [1]. Here is the opening paragraph of [1]:

      "Soundex is a phonetic algorithm for indexing names by sound, as pronounced in English.
       The goal is for homophones to be encoded to the same representation so that they can be matched
       despite minor differences in spelling. The algorithm mainly encodes consonants;
       a vowel will not be encoded unless it is the first letter. Soundex is the most widely known
       of all phonetic algorithms (in part because it is a standard feature of popular database
       software such as DB2, PostgreSQL, MySQL, SQLite, Ingres, MS SQL Server and Oracle).
       Improvements to Soundex are the basis for many modern phonetic algorithms."


    # Algorithm steps

    The code below is based on the second variant in [1].

      1. Save the first letter. Remove all occurrences of 'h' and 'w' except first letter.

      2. Replace all consonants (including the first letter) with digits as follows:

          b, f, p, v → 1
          c, g, j, k, q, s, x, z → 2
          d, t → 3
          l → 4
          m, n → 5
          r → 6

      3. Replace all adjacent same digits with one digit.

      4. Remove all occurrences of a, e, i, o, u, y except the first letter.

      5. If the first symbol is a digit replace it with the letter saved on step 1.

      6. Append 3 zeros if the result contains less than 3 digits.
         Remove all characters except the first letter and 3 digits after it.


    # References

    [1] Wikipedia entry, Soundex, https://en.wikipedia.org/wiki/Soundex .

*)

BeginPackage["Soundex`"];

Soundex::usage = "Soundex[word] finds the Soundex phonetic index of a given string.";


Begin["`Private`"];

Clear[Soundex];
Soundex[word_String] :=
    Block[{sword = ToLowerCase[word], f, res},
    (*1*)
      f = StringTake[sword, {1, 1}];
      res = StringDelete[StringTake[sword, {2, -1}], RegularExpression["[hw]"]];
      (*2*)
      res =
          StringReplace[
            f <> res,
            {RegularExpression["[bfpv]"] -> "1",
              RegularExpression["[cgjkqsxz]"] -> "2",
              RegularExpression["[dt]"] -> "3",
              "l" -> "4",
              RegularExpression["[mn]"] -> "5",
              "r" -> "6"}];
      (*3*)
      res = StringReplace[res, (x_ ~~ x_) :> x];
      (*4*)
      res = StringDelete[res, RegularExpression["[aeiouy]"]];
      (*5*)
      If[StringLength[res] == 0, res = f];
      res = StringReplace[res, StartOfString ~~ DigitCharacter :> f];
      (*6*)
      If[StringLength[res] < 4,
        res = res <> "000";
      ];
      res = StringTake[res, 4];
      res
    ];

End[]; (* `Private` *)

EndPackage[]