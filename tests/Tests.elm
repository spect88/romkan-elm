module Tests exposing (all)

import Test exposing (..)
import Expect

import Romkan exposing (..)

all : Test
all =
  describe "Romkan"
    [ describe "toKatakana"
        [ toKatakana "kanji" `shouldEqual` "カンジ"
        , toKatakana "kanzi" `shouldEqual` "カンジ"
        , toKatakana "kannji" `shouldEqual` "カンジ"
        , toKatakana "chie" `shouldEqual` "チエ"
        , toKatakana "tie" `shouldEqual`"チエ"
        , toKatakana "kyouju" `shouldEqual` "キョウジュ"
        , toKatakana "syuukyou" `shouldEqual` "シュウキョウ"
        , toKatakana "shuukyou" `shouldEqual` "シュウキョウ"
        , toKatakana "saichuu" `shouldEqual` "サイチュウ"
        , toKatakana "saityuu" `shouldEqual` "サイチュウ"
        , toKatakana "cheri-" `shouldEqual` "チェリー"
        , toKatakana "tyeri-" `shouldEqual` "チェリー"
        , toKatakana "shinrai" `shouldEqual` "シンライ"
        , toKatakana "sinrai" `shouldEqual` "シンライ"
        , toKatakana "hannnou" `shouldEqual` "ハンノウ"
        , toKatakana "han'nou" `shouldEqual` "ハンノウ"
        , toKatakana "wo" `shouldEqual` "ヲ"
        , toKatakana "we" `shouldEqual` "ウェ"
        , toKatakana "du" `shouldEqual` "ヅ"
        , toKatakana "she" `shouldEqual` "シェ"
        , toKatakana "di" `shouldEqual` "ヂ"
        , toKatakana "fu" `shouldEqual` "フ"
        , toKatakana "ti" `shouldEqual` "チ"
        , toKatakana "wi" `shouldEqual` "ウィ"
        , toKatakana "je" `shouldEqual` "ジェ"
        , toKatakana "e-jento" `shouldEqual` "エージェント"
        ]
    , describe "toHiragana"
        [ toHiragana "kanji" `shouldEqual` "かんじ"
        , toHiragana "kanzi" `shouldEqual` "かんじ"
        , toHiragana "kannji" `shouldEqual` "かんじ"
        , toHiragana "chie" `shouldEqual` "ちえ"
        , toHiragana "tie" `shouldEqual` "ちえ"
        , toHiragana "kyouju" `shouldEqual` "きょうじゅ"
        , toHiragana "syuukyou" `shouldEqual` "しゅうきょう"
        , toHiragana "shuukyou" `shouldEqual` "しゅうきょう"
        , toHiragana "saichuu" `shouldEqual` "さいちゅう"
        , toHiragana "saityuu" `shouldEqual` "さいちゅう"
        , toHiragana "cheri-" `shouldEqual` "ちぇりー"
        , toHiragana "tyeri-" `shouldEqual` "ちぇりー"
        , toHiragana "shinrai" `shouldEqual` "しんらい"
        , toHiragana "sinrai" `shouldEqual` "しんらい"
        , toHiragana "hannnou" `shouldEqual` "はんのう"
        , toHiragana "han'nou" `shouldEqual` "はんのう"
        ]
    , describe "toHiragana"
        [ toKana "kanji" `shouldEqual` "カンジ"
        , toKana "kanzi" `shouldEqual` "カンジ"
        , toKana "kannji" `shouldEqual` "カンジ"
        , toKana "chie" `shouldEqual` "チエ"
        , toKana "tie" `shouldEqual` "チエ"
        , toKana "kyouju" `shouldEqual` "キョウジュ"
        , toKana "syuukyou" `shouldEqual` "シュウキョウ"
        , toKana "shuukyou" `shouldEqual` "シュウキョウ"
        , toKana "saichuu" `shouldEqual` "サイチュウ"
        , toKana "saityuu" `shouldEqual` "サイチュウ"
        , toKana "cheri-" `shouldEqual` "チェリー"
        , toKana "tyeri-" `shouldEqual` "チェリー"
        , toKana "shinrai" `shouldEqual` "シンライ"
        , toKana "sinrai" `shouldEqual` "シンライ"
        , toKana "hannnou" `shouldEqual` "ハンノウ"
        , toKana "han'nou" `shouldEqual` "ハンノウ"

        , toKana "wo" `shouldEqual` "ヲ"
        , toKana "we" `shouldEqual` "ウェ"
        , toKana "du" `shouldEqual` "ヅ"
        , toKana "she" `shouldEqual` "シェ"
        , toKana "di" `shouldEqual` "ヂ"
        , toKana "fu" `shouldEqual` "フ"
        , toKana "ti" `shouldEqual` "チ"
        , toKana "wi" `shouldEqual` "ウィ"

        , toKana "je" `shouldEqual` "ジェ"
        , toKana "e-jento" `shouldEqual` "エージェント"
        ]
    , describe "toRoma"
        [ toRoma "カンジ" `shouldEqual` "kanji"
        , toRoma "チャウ" `shouldEqual` "chau"
        , toRoma "ハンノウ" `shouldEqual` "han'nou"
        , toRoma "かんじ" `shouldEqual` "kanji"
        , toRoma "ちゃう" `shouldEqual` "chau"
        , toRoma "はんのう" `shouldEqual` "han'nou"
        ]
    , describe "kunreiToHepburn"
        [ kunreiToHepburn "kannzi" `shouldEqual` "kanji"
        , kunreiToHepburn "tie" `shouldEqual` "chie"
        , kunreiToHepburn "KANNZI" `shouldEqual` "kanji"
        , kunreiToHepburn "TIE" `shouldEqual` "chie"
        ]
    , describe "hepburnToKunrei"
        [ hepburnToKunrei "kanji" `shouldEqual` "kanzi"
        , hepburnToKunrei "chie" `shouldEqual` "tie"
        , hepburnToKunrei "KANJI" `shouldEqual` "kanzi"
        , hepburnToKunrei "CHIE" `shouldEqual` "tie"
        ]
    ]

shouldEqual : String -> String -> Test
shouldEqual actual expected =
  test ("should equal " ++ expected) <|
    \() -> Expect.equal expected actual
