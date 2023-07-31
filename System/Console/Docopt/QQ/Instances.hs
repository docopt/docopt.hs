{-# LANGUAGE DeriveLift#-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- Needed to compile under current GHC but deprecated. To fix this in future, see
-- https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-class-extensions.html#instance-overlap
{-# LANGUAGE OverlappingInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module System.Console.Docopt.QQ.Instances where

import System.Console.Docopt.Types
import Language.Haskell.TH.Syntax (Lift)
import Data.Map.Internal (Map(..))

deriving instance Lift (Map Option OptionInfo)
deriving instance Lift (Docopt)
