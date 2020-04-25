{-# LANGUAGE OverloadedStrings #-}

module Language.FJ.Type
  ( Program,
    ClassTable,
    Term (..),
    ClassDef (..),
    ConstDef (..),
    MethodDef (..),
    mkMethodDef,
    Class,
    mkClass,
    getClassName,
    Method,
    mkMethod,
    getMethodName,
    Field,
    mkField,
    getFieldName,
    Var,
    mkVar,
    getVarName,
  )
where

import Data.Text.Prettyprint.Doc
import RIO

type Program = (ClassTable, Term)

-- | Class Table
type ClassTable = Class -> ClassDef

-- | Class Declaration
data ClassDef
  = CL
      Class -- Self class name
      Class -- Super class name
      [(Class, Field)] -- Field declarations
      ConstDef -- Constructor declarations
      [MethodDef] -- Method declarations
  deriving stock (Eq, Show)

-- | Constructor Declarations
data ConstDef
  = K
      Class -- Constructor name
      [(Class, Field)] -- The fields used to initialize the fields of the instance. The first part contains the fields for the superclass
      [Field]
      [(Field, Field)]
  deriving stock (Eq, Show)

-- | Method Declarations
data MethodDef
  = M
      Class -- The name of the return type (Class)
      Method -- method name
      [(Class, Var)] -- Method arguments (Argument type and argument variable name)
      Term -- Method body
  deriving stock (Eq, Show)

mkMethodDef :: Class -> Method -> [(Class, Var)] -> Term -> MethodDef
mkMethodDef = M

data Term
  = -- | Variable
    TmVar Var
  | -- | field access
    TmFieldRef Term Field
  | -- | method invocation
    TmMethodInv Term Method [Term]
  | -- | object creation
    TmNew Class [Term]
  | -- | cast
    TmCast Class Term
  deriving stock (Eq, Show)

instance Pretty Term where
  pretty = \case
    TmVar var -> pretty var
    TmFieldRef t@TmCast {} field ->
      parens (pretty t) <> dot <> pretty field
    TmFieldRef t field -> pretty t <> "." <> pretty field
    TmMethodInv t method args ->
      pretty t <> "." <> pretty method <> tupled (map pretty args)
    TmNew cls args -> "new" <+> pretty cls <> tupled (map pretty args)
    TmCast cls t@TmFieldRef {} -> parens (pretty cls) <> parens (pretty t)
    TmCast cls t -> parens (pretty cls) <> pretty t

-- | It can be just a String, but it seems to be wrong, so I chose newtype.
newtype Class = CN {getClassName :: Text}
  deriving stock (Eq, Show)
  deriving (Pretty) via Text

newtype Method = MN {getMethodName :: Text}
  deriving stock (Eq, Show)
  deriving (Pretty) via Text

newtype Field = FN {getFieldName :: Text}
  deriving stock (Eq, Show)
  deriving (Pretty) via Text

newtype Var = VN {getVarName :: Text}
  deriving stock (Eq, Show)
  deriving (Pretty) via Text

mkClass :: Text -> Class
mkClass = CN

mkMethod :: Text -> Method
mkMethod = MN

mkField :: Text -> Field
mkField = FN

mkVar :: Text -> Var
mkVar = VN
