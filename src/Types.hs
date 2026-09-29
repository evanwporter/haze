{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Hashable (Hashable)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | value_change_dump_definitions ::=
--    { declaration_command }{ simulation_command }
data ValueChangeDumpDefinitions = ValueChangeDumpDefinitions
  { declarations :: [DeclarationCommand],
    simulations :: [SimulationCommand]
  }
  deriving (Eq, Show)

-- | declaration_command ::=
--    $comment [ comment_text ] $end
--    | $date [ date_text ] $end
--    | $enddefinitions $end
--    | $scope [ scope_type scope_identifier ] $end
--    | $timescale [ time_number time_unit ] $end
--    | $upscope $end
--    | $var [ var_type size identifier_code reference ] $end
--    | $version [ version_text ] $end
data DeclarationCommand
  = Comment CommentText
  | Date DateText
  | EndDefinitions
  | Scope ScopeType ScopeIdentifier
  | TimeScale TimeNumber TimeUnit
  | Upscope
  | Var VarType Size IdentifierCode Reference
  | Version VersionText
  deriving (Eq, Show)

{- FOURMOLU_DISABLE -}
-- Input starts with...   Parse as...                 Constructor
--

-- $dumpall               dump command               DumpAll [...]

-- $dumpoff               dump command               DumpOff [...]

-- $dumpon                dump command               DumpOn [...]

-- $dumpvars              dump command               DumpVars [...]

-- $comment               comment                    SimComment ...
--
-- #                      simulation time             SimTime ...
--
-- 0,1,x,X,z,Z            scalar value change        SimValueChange ...
-- b,B                    binary vector change       SimValueChange ...
-- r,R                    real vector change         SimValueChange ...

{- FOURMOLU_ENABLE -}

-- | simulation_command ::=
--    $dumpall { value_change } $end
--    | $dumpoff { value_change } $end
--    | $dumpon { value_change } $end
--    | $dumpvars { value_change } $end
--    | $comment [ comment_text ] $end
--    | simulation_time
--    | value_change
data SimulationCommand
  = DumpAll [ValueChange]
  | DumpOff [ValueChange]
  | DumpOn [ValueChange]
  | DumpVars [ValueChange]
  | SimComment CommentText
  | SimTime SimulationTime
  | SimValueChange ValueChange
  deriving (Eq, Show)

-- | scope_type ::=
--    begin
--    | fork
--    | function
--    | module
--    | task
data ScopeType = Begin | Fork | Function | Module | Task
  deriving (Eq, Show)

-- | time_number ::= 1 | 10 | 100
data TimeNumber = T1 | T10 | T100
  deriving (Eq, Show)

-- | time_unit ::= s | ms | us | ns | ps | fs
data TimeUnit = Seconds | MilliSeconds | MicroSeconds | NanoSeconds | PicoSeconds | FemtoSeconds
  deriving (Eq, Show)

-- | var_type ::=
--    event | integer | parameter | real | realtime | reg | supply0 | supply1 | time
--    | tri | triand | trior | trireg | tri0 | tri1 | wand | wire | wor
data VarType
  = Event
  | Integer
  | Parameter
  | Real
  | Realtime
  | Reg
  | Supply0
  | Supply1
  | Time
  | Tri
  | Triand
  | Trior
  | Trireg
  | Tri0
  | Tri1
  | Wand
  | Wire
  | Wor
  deriving (Eq, Show)

-- | simulation_time ::= # decimal_number
newtype SimulationTime = SimulationTime Int
  deriving (Eq, Show, Ord)

-- | value_change ::=
--    scalar_value_change
--    | vector_value_change
data ValueChange = ScalarChange ScalarValueChange | VectorChange VectorValueChange
  deriving (Eq, Show)

-- | scalar_value_change ::= value identifier_code
data ScalarValueChange = ScalarValueChange Value IdentifierCode
  deriving (Eq, Show)

-- | value ::= 0 | 1 | x | X | z | Z
data Value = V0 | V1 | Vx | VX | Vz | VZ
  deriving (Eq, Show)

-- | vector_value_change ::=
--    b binary_number identifier_code
--    | B binary_number identifier_code
--    | r real_number identifier_code
--    | R real_number identifier_code
data VectorValueChange
  = BinaryLower T.Text IdentifierCode
  | BinaryUpper T.Text IdentifierCode
  | RealLower Double IdentifierCode
  | RealUpper Double IdentifierCode
  deriving (Eq, Show)

-- | identifier_code ::= { ASCII character }
newtype IdentifierCode = IdentifierCode T.Text
  deriving (Eq, Show, Generic, Hashable, Ord)

-- | size ::= decimal_number
newtype Size = Size Int
  deriving (Eq, Show)

-- | reference ::=
--    identifier
--    | identifier [ bit_select_index ]
--    | identifier [ msb_index : lsb_index ]
data Reference
  = Identifier T.Text
  | BitSelect T.Text Index
  | RangeSelect T.Text Index Index
  deriving (Eq, Show)

-- | index ::= decimal_number
newtype Index = Index Int
  deriving (Eq, Show)

-- | scope_identifier ::= { ASCII character }
newtype ScopeIdentifier = ScopeIdentifier T.Text
  deriving (Eq, Show)

-- | comment_text ::= { ASCII character }
newtype CommentText = CommentText T.Text
  deriving (Eq, Show)

-- | date_text ::= { ASCII character }
newtype DateText = DateText T.Text
  deriving (Eq, Show)

-- | version_text ::= { ASCII character }
newtype VersionText = VersionText T.Text
  deriving (Eq, Show)
