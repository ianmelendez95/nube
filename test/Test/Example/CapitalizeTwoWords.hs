module Test.Example.CapitalizeTwoWords (capitalizeTwoWords_fn_ast, capitalizeTwoWords_fn_text) where

import Data.Text (Text, pack)
import Nube.Parse
  ( function,
  )
import Nube.Syntax
  ( Fn (..),
  )
import Test.Util.Parse
  ( runParser,
  )

capitalizeTwoWords_fn_ast :: Fn
capitalizeTwoWords_fn_ast = runParser function capitalizeTwoWords_fn_text

capitalizeTwoWords_fn_text :: Text
capitalizeTwoWords_fn_text =
  pack
    "function capitalizeTwoWords(string) {\n\
    \    const words = string.split(' ');\n\
    \    const word1 = words[0];\n\
    \    const word2 = words[1];\n\
    \    const capitalizedWord1 = capitalizeWord(word1);\n\
    \    const capitalizedWord2 = capitalizeWord(word2);\n\
    \    return capitalizedWord1 + ' ' + capitalizedWord2;\n\
    \}"

capitalizeTwoWords_fn_cont0_text :: Text
capitalizeTwoWords_fn_cont0_text = 
  pack 
    "function capitalizeTwoWords(_ctx) {"