{-# LANGUAGE DeriveLift#-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module System.Console.Docopt.QQ.Instances where

import System.Console.Docopt.Types
import Language.Haskell.TH.Syntax (Lift)
import Data.Map.Internal (Map(..))

deriving instance Lift (Map Option OptionInfo)
deriving instance Lift (Docopt)
