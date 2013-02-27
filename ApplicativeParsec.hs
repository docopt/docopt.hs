{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module ApplicativeParsec
    (
      module Control.Applicative
    , module Text.ParserCombinators.Parsec
    ) where

import Control.Applicative hiding (optional, (<|>))
import Control.Monad (MonadPlus(..), ap)
-- Hide a few names that are provided by Applicative.
import Text.ParserCombinators.Parsec hiding (many)

-- The Applicative instance for every Monad looks like this.
--instance Applicative (GenParser s a) where
--    pure  = return
--    (<*>) = ap

-- The Alternative instance for every MonadPlus looks like this.
--instance Alternative (GenParser s a) where
--    empty = mzero
--    (<|>) = mplus