{
module Marginal.Parse.TwoPhase.Parser
  (
    parse
  )
where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Foldable (foldMap)

import           Marginal.Parse.Type
import           Marginal.Parse.TwoPhase.Lexer
}

%name parse
%tokentype                          { Token                         }
%error                              { parseError                    }

%token
  space                             { Token _ TSpace                }
  tab                               { Token _ TTab                  }
  newline                           { Token _ TNewline              }

%%


Ops :: { [Instruction] }
  : space space Number    Ops       { Push $3 : $4                  }
  | space newline space   Ops       { Dup  : $4                     }
  | space newline tab     Ops       { Swap : $4                     }
  | space newline newline Ops       { Drop : $4                     }

  | tab space space space Ops       { Add  : $5                     }
  | tab space space tab   Ops       { Sub  : $5                     }
  | tab space space newline Ops     { Mult : $5                     }
  | tab space tab space Ops         { Div  : $5                     }
  | tab space tab tab Ops           { Mod  : $5                     }

  | tab tab space Ops               { Store    : $4                 }
  | tab tab tab   Ops               { Retrieve : $4                 }

  | newline space space   Label Ops { labelToInstr Mark $4     : $5 }
  | newline space tab     Label Ops { labelToInstr Func $4     : $5 }
  | newline space newline Label Ops { labelToInstr Jump $4     : $5 }
  | newline tab space     Label Ops { labelToInstr JumpZero $4 : $5 }
  | newline tab tab       Label Ops { labelToInstr JumpNeg $4  : $5 }
  | newline tab newline         Ops { Return                   : $4 }
  | newline newline newline     Ops { Exit                     : $4 }

  | tab newline space space   Ops   { PrintChar : $5                }
  | tab newline space tab     Ops   { PrintNum  : $5                }
  | tab newline tab space     Ops   { ReadChar  : $5                }
  | tab newline tab tab       Ops   { ReadNum   : $5                }

  | {- empty -}                     { []                            }

Number :: { Number }
  : space Label                     { tokensToNumber 1 $2           }
  | tab   Label                     { tokensToNumber (-1) $2        }

Label :: { [Token] }
  : space Label                     { $1 : $2                       }
  | tab   Label                     { $1 : $2                       }
  | newline                         { [$1]                          }


{
parseError :: [Token] -> a
parseError ((Token (AlexPn abs ln col) _):_) =
  error ("Parse Error at: " ++ pos) where
  pos = "Character: " ++ show abs
    ++ ", Line: " ++ show ln
    ++ ", Col: " ++ show col

-- not a compact representation, but this is done to be
-- equivalent with the other parser and use the common Instruction type
labelToInstr :: (Label -> Instruction) -> [Token] -> Instruction
labelToInstr f tokens = f (Label bytes) where
  bytes = foldMap (tokenToBytes . mtoken) . init $ tokens
  tokenToBytes TSpace   = BS.singleton ' '
  tokenToBytes TTab     = BS.singleton '\t'
  tokenToBytes TNewline = error "Internal parse error (parsing Label)"

type Sign = Integer

tokensToNumber :: Sign -> [Token] -> Number
tokensToNumber sign tokens = (Number $ sign * num) where
  digit TSpace = 0
  digit TTab   = 1
  digit TNewline = error "Internal parse error (parsing Number)"
  digits = dropWhile (== 0) . fmap (digit . mtoken) . init $ tokens
  num = foldl combine 0 digits
  combine acc c = acc * 2 + c
}
