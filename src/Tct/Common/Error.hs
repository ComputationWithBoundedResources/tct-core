-- | This module re-exports 
--   <http://hackage.haskell.org/package/mtl Control.Monad.Error>
--   and provides types for custom error handling.
{-# LANGUAGE ScopedTypeVariables #-}
module Tct.Common.Error
  (
    module Control.Monad.Error
  , TctError (..)
  , ErroneousIO
  , runErroneousIO
  , tryIO
  , liftEither
  , liftMaybe
  , hush
  , note
  ) where

import Control.Exception   (IOException, try)
import Control.Monad.Error (Error, ErrorT (..), liftIO, runErrorT)

-- | Custom error type.
data TctError
  = TctDyreError String
  | TctParseError String
  | TctIOError String
  deriving Show

instance Error TctError where

-- | Wraps 'IO' into an erroneous compuation for custom error handling.
type ErroneousIO e = ErrorT e IO

-- | Executes an erroneous computation.
--   Returns @'Left' e@ if if an error occured, and  @'Right' a@ if the computation was successfull.
runErroneousIO :: ErroneousIO e a -> IO (Either e a)
runErroneousIO = runErrorT

-- | Lifts an 'IO' computation to 'ErroneousIO'.
--   Returns 'TctIOError' if an error occured.
tryIO :: IO a -> ErroneousIO TctError a
tryIO io = ErrorT . liftIO $ do
  e :: Either IOException a <- try io
  return $ either (Left . TctIOError . show) Right e

-- | Lifts 'Either' to 'ErroneousIO'. 
--   Indicates an error @e@ if the first argument is @'Left' e@.
liftEither :: Either e a -> ErroneousIO e a
liftEither = ErrorT . return

-- | Lifts 'Maybe' to 'ErroneousIO'.
--   Indicates an error @e@ if the second argument is 'Nothing'.
liftMaybe :: e -> Maybe a  -> ErroneousIO e a
liftMaybe e =  liftEither . note e

-- | Transforms 'Either' to 'Maybe'.
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-- | Transforms 'Maybe' to 'Either'.
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

