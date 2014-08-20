module Tct.Error 
  (module Control.Monad.Error
  , TctError (..)
  , runErroneousIO
  , liftEither
  , liftMaybe
  , hush
  , note
  ) where

import Control.Monad.Error (Error, ErrorT (..), runErrorT, liftIO)

data TctError
  = TctDyreError String
  | TctParseError String
  deriving Show

instance Error TctError where

type ErroneousIO e = ErrorT e IO

runErroneousIO :: ErroneousIO e a -> IO (Either e a)
runErroneousIO = runErrorT

liftEither :: Either e a -> ErroneousIO e a
liftEither = ErrorT . return

liftMaybe :: e -> Maybe a  -> ErroneousIO e a
liftMaybe e =  liftEither . note e

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

