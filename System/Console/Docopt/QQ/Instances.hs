{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module System.Console.Docopt.QQ.Instances where

import System.Console.Docopt.Types
import Language.Haskell.TH.Syntax (Lift)

#if !MIN_VERSION_containers(0,6,5)
import Data.Map.Internal (Map(..))

deriving instance Lift (Map Option OptionInfo)
#endif

deriving instance Lift (Docopt)
