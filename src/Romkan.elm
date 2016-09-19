module Romkan exposing
  ( toKatakana
  , toHiragana
  , toKana
  , toRoma
  , kunreiToHepburn
  , hepburnToKunrei
  )

{-| [Romkan](https://github.com/spect88/romkan-elm) is a library for
conversion between Japanese Romaji and Kana.

This is just an Elm port
of [the Haskell port](https://github.com/karlvoigtland/romkan-hs)
of [the Python port](http://www.soimort.org/python-romkan)
of [Ruby/Romkan](http://0xcc.net/ruby-romkan).

# Kana <-> Romaji

@docs toKatakana, toHiragana, toKana, toRoma

# Romaji <-> Romaji

@docs kunreiToHepburn, hepburnToKunrei

-}

import Debug exposing (crash)
import String

import Combine exposing
  ( Parser, primitive, parse
  , string, succeed, andThen, map, many, or, choice
  )
import Combine.Char exposing (anyChar)

import Romkan.Internal exposing
  ( romKanAList, romKanAList_H, kanRomAList, kanRomAList_H
  , kunreiToHepburnAList, hepburnToKunreiAList
  )

{-| Convert a Romaji (ローマ字) to a Katakana (片仮名). -}
toKatakana : String -> String
toKatakana =
  convertRoma romKanSubs

{-| Convert a Romaji (ローマ字) to a Hiragana (平仮名). -}
toHiragana : String -> String
toHiragana =
  convertRoma romKanSubs_H

{-| Convert a Romaji (ローマ字) to a Katakana (片仮名). (same as toKatakana) -}
toKana : String -> String
toKana =
  toKatakana

{-| Convert a Kana (仮名) to a Hepburn Romaji (ヘボン式ローマ字). -}
toRoma : String -> String
toRoma =
  doParse (processText normalizeN') << doParse (processText kanRomSubs)

{-| Convert a Kunrei-shiki Romaji (訓令式ローマ字) to a Hepburn Romaji (ヘボン式ローマ字). -}
kunreiToHepburn : String -> String
kunreiToHepburn =
  convertRoma kunreiToHepburnSubs

{-| Convert a Hepburn Romaji (ヘボン式ローマ字) to a Kunrei-shiki Romaji (訓令式ローマ字). -}
hepburnToKunrei : String -> String
hepburnToKunrei =
  convertRoma hepburnToKunreiSubs

-- normalize romaji then apply parser
convertRoma : Parser String -> String -> String
convertRoma p t =
  doParse (processText p) (normalizeRoma t)

-- run parser, if fails throw error
doParse : Parser String -> String -> String
doParse p t =
  let
    (result, _) = parse p t
  in
    case result of
      Err err ->
        Debug.crash <|
          "Internal Error, parser should not be able to fail: " ++
          String.join ", " err
      Ok kat -> kat

-- replace "nn" with either "n" or "n'"
normalizeNN : Parser String
normalizeNN =
  string "nn" `andThen` \_ ->
    peekChar `andThen` \cM ->
      case cM of
        Nothing -> succeed "n"
        Just c ->
          if (isEndOfLine c || notInClass "aiueoyn" c)
          then succeed "n"
          else succeed "n'"

-- remove unnecessary apostrophes
normalizeN' : Parser String
normalizeN' =
  string "n'" `andThen` \_ ->
    peekChar `andThen` \cM ->
      case cM of
        Nothing -> succeed "n"
        Just c ->
          if (isEndOfLine c || notInClass "aiueoyn" c)
          then succeed "n"
          else succeed "n'"

isEndOfLine : Char -> Bool
isEndOfLine c =
  c == '\r' || c == '\n'

notInClass : String -> Char -> Bool
notInClass chars c =
  not << List.member c <| String.toList chars

normalizeN : Parser String
normalizeN =
  processText (normalizeNN `or` normalizeN')

normalizeRoma : String -> String
normalizeRoma s =
  doParse normalizeN <| String.toLower s

-- sweep parser across input text, passing through as-is un-matched characters
processText : Parser String -> Parser String
processText p =
  many (p `or` map String.fromChar anyChar) |> map String.concat

-- create a single substitution parser
sub : (String, String) -> Parser String
sub (src, dst) =
  string src |> map (\_ -> dst)

-- romaji to katakana substitutions
romKanSubs : Parser String
romKanSubs =
  choice <| List.map sub romKanAList

-- romaji to hiragana substitutions
romKanSubs_H : Parser String
romKanSubs_H =
  choice <| List.map sub romKanAList_H

-- katakana and hiragana to romaji substitutions
kanRomSubs : Parser String
kanRomSubs =
  choice <| List.map sub <| kanRomAList ++ kanRomAList_H

-- kunrei romaji to hepburn romaji substitutions
kunreiToHepburnSubs : Parser String
kunreiToHepburnSubs =
  choice <| List.map sub kunreiToHepburnAList

-- hepburn romaji to kunrei romaji substitutions
hepburnToKunreiSubs : Parser String
hepburnToKunreiSubs =
  choice <| List.map sub hepburnToKunreiAList

-- returns maybe next character without consuming any input
peekChar : Parser (Maybe Char)
peekChar =
  primitive <| \cx ->
    case String.uncons cx.input of
      Just (h, _) -> (Ok (Just h), cx)
      Nothing -> (Ok (Nothing), cx)
