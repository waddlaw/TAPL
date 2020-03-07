module Language.FJ.Type
  ( Program
  , ClassTable
  , Term (..)
  , ClassDef (..)
  , ConstDef (..)
  , MethodDef (..)
  , Class, mkClass, getClassName
  , Method, mkMethod, getMethodName
  , Field, mkField, getFieldName
  , Var, mkVar, getVarName
  )
where

import RIO

type Program = (ClassTable, Term)

-- | Class Table
type ClassTable = Class -> ClassDef

-- | Class Declaration
data ClassDef = CL
  Class             -- ^ Self class name
  Class             -- ^ Super class name
  [(Class, Field)]  -- ^ Field declarations
  ConstDef          -- ^ Constructor declarations
  [MethodDef]       -- ^ Method declarations
  deriving stock (Eq, Show)

-- | Constructor Declarations
data ConstDef = K
  Class            -- ^ Constructor name
  [(Class, Field)] -- ^ The fields used to initialize the fields of the instance. The first part contains the fields for the superclass
  deriving stock (Eq, Show)

-- | Method Declarations
data MethodDef = M
  Class          -- ^ The name of the return type (Class)
  Method         -- ^ method name
  [(Class, Var)] -- ^ Method arguments (Argument type and argument variable name)
  Term           -- ^ Method body
  deriving stock (Eq, Show)

data Term
  = TmVar Var                      -- ^ Variable
  | TmFieldRef Term Field          -- ^ field access
  | TmMethodInv Term Method [Term] -- ^ method invocation
  | TmNew Class [Term]             -- ^ object creation
  | TmCast Class Term              -- ^ cast
  deriving stock (Eq, Show)

-- | It can be just a String, but it seems to be wrong, so I chose newtype.
newtype Class  = CN { getClassName  :: Text } deriving stock (Eq, Show)
newtype Method = MN { getMethodName :: Text } deriving stock (Eq, Show)
newtype Field  = FN { getFieldName  :: Text } deriving stock (Eq, Show)
newtype Var    = VN { getVarName    :: Text } deriving stock (Eq, Show)

mkClass :: Text -> Class
mkClass = CN

mkMethod :: Text -> Method
mkMethod = MN

mkField :: Text -> Field
mkField = FN

mkVar :: Text -> Var
mkVar = VN