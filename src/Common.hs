module Common where

type VarName = String
type ClassName = String

data StackValue = IntV Int | StringV String | BoolV Bool | NullV | Location Int
    deriving Eq

type Environment = [(VarName, StackValue)] -- TODO Map

data HeapValue = Object ClassName Environment

type Store = [HeapValue] -- TODO Map
type State = (Environment, Store)

instance Show StackValue where
    show (IntV i) = show i
    show (StringV s) = show s
    show (BoolV b) = show b
    show NullV = "null"


