module Util.QObject (QObject (..)) where

class QObject a where
    render :: a -> String
