{

module Marginal.Lexer (runAlex) where

import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monadUserState-bytestring"

@digit  = \ | \t
@number = @digit+\n
@label  = @number

-- All Instruction Modification Parameters
@stack  = " "
@arith  = "\t "
@heap   = "\t\t"
@flow   = "\n"
@io     = "\t\n"

tokens :-

  ^(\ |\t|\n)           ;

  @stack" "@digit+      { TPush      }
  @stack"\n "           { TDup       }
  @stack"\n\t"          { TSwap      }
  @stack"\n\n"          { TDrop      }

  @arith"  "            { TAdd       }
  @arith" \t"           { TSub       }
  @arith" \n"           { TMult      }
  @arith"\t "           { TDiv       }
  @arith"\t\t"          { TMod       }

  @heap" "              { TStore     }
  @heap"\t"             { TRetrieve  }

  @flow"  "@label       { TMark      }
  @flow" \t"@label      { TFunc      }
  @flow" \n"@label      { TJump      }
  @flow"\t "@label      { TJumpZero  }
  @flow"\t\t"@label     { TJumpNeg   }
  @flow"\t\n"           { TReturn    }
  @flow"\n\n"           { TExit      }

  @io"  "               { TPrintChar }
  @io" \n"              { TPrintNum  }
  @io"\t "              { TReadChar  }
  @io"\t\t"             { TReadNum   }
