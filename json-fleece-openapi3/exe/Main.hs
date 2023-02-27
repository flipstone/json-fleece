module Main
  ( main
  ) where

import qualified Fleece.CodeGenUtil.Executable as CGUE
import qualified Fleece.OpenApi3 as FOA3

main :: IO ()
main = CGUE.codeGenMain FOA3.generateOpenApiFleeceCode
