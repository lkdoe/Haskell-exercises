module Homework02 where

data MessageType
    = Info
    | Warning
    | Error Int
    deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)
