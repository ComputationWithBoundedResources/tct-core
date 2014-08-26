{-# LANGUAGE ScopedTypeVariables #-}
module Tct.Common.Error
  (
    module Control.Monad.Error
  , TctError (..)
  , runErroneousIO
  , tryIO
  , liftEither
  , liftMaybe
  , hush
  , note
  ) where

import Control.Exception   (IOException, try)
import Control.Monad.Error (Error, ErrorT (..), liftIO, runErrorT)

data TctError
  = TctDyreError String
  | TctParseError String
  | TctIOError String
  deriving Show

instance Error TctError where

type ErroneousIO e = ErrorT e IO

runErroneousIO :: ErroneousIO e a -> IO (Either e a)
runErroneousIO = runErrorT

tryIO :: IO a -> ErroneousIO TctError a
tryIO io = ErrorT . liftIO $ do
  e :: Either IOException a <- try io
  return $ either (Left . TctIOError . show) Right e

liftEither :: Either e a -> ErroneousIO e a
liftEither = ErrorT . return

liftMaybe :: e -> Maybe a  -> ErroneousIO e a
liftMaybe e =  liftEither . note e

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

