{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, prune #-}
module System.Console.Docopt.QQ.Instances where

import System.Console.Docopt.Types
import Language.Haskell.TH.Lift

import qualified Data.Map as M

instance (Lift k, Lift v) => Lift (M.Map k v) where
    lift m = [| M.fromList assoc |]
        where assoc = M.toList m

$(deriveLiftMany [ ''Option
                 , ''Pattern
                 , ''OptionInfo
                 , ''Docopt
                 ])
