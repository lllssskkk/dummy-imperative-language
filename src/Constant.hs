module Constant (Name, Program) where

import Control.Monad.Writer (
    Writer,
 )

import Syntax (Statement)

type Name = String

type Program = Writer Statement ()
