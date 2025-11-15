module Test.Example.CapitalizeTwoWords (capitalizeTwoWords_ast, capitalizeTwoWords_text) where

import Data.Text (Text, pack)
import JS.Parse
  ( function,
  )
import JS.Syntax
  ( Fn (..),
  )
import Test.Util.Parse
  ( runParser,
  )

capitalizeTwoWords_ast :: Fn
capitalizeTwoWords_ast = runParser function capitalizeTwoWords_text

capitalizeTwoWords_text :: Text
capitalizeTwoWords_text =
  pack
    "function capitalizeTwoWords(string) {\n\
    \    const words = string.split(' ');\n\
    \    const word1 = words[0];\n\
    \    const word2 = words[1];\n\
    \    const capitalizedWord1 = capitalizeWord(word1);\n\
    \    const capitalizedWord2 = capitalizeWord(word2);\n\
    \    return capitalizedWord1 + ' ' + capitalizedWord2;\n\
    \}"