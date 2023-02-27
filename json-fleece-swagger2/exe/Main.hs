module Main
  ( main
  ) where

import qualified Fleece.CodeGenUtil.Executable as CGUE
import qualified Fleece.Swagger2 as FS2

main :: IO ()
main = CGUE.codeGenMain FS2.generateSwaggerFleeceCode
