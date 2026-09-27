module Types where

import qualified Data.Text as T

{- | value_change_dump_definitions ::=
    { declaration_command }{ simulation_command }
-}
data ValueChangeDumpDefinitions = ValueChangeDumpDefinitions
    { declarations :: [DeclarationCommand]
    , simulations :: [SimulationCommand]
    }

{- | declaration_command ::=
    $comment [ comment_text ] $end
    | $date [ date_text ] $end
    | $enddefinitions $end
    | $scope [ scope_type scope_identifier ] $end
    | $timescale [ time_number time_unit ] $end
    | $upscope $end
    | $var [ var_type size identifier_code reference ] $end
    | $version [ version_text system_task ] $end
-}
data DeclarationCommand
    = Comment CommentText
    | DateCmd Date
    | EndDefinitions
    | Scope ScopeType ScopeIdentifier
    | Upscope
    | Var VarType Size IdentifierCode Reference
    | VersionCmd Version SystemTask

{- | simulation_command ::=
    $dumpall { value_change } $end
    | $dumpoff { value_change } $end
    | $dumpon { value_change } $end
    | $dumpvars { value_change } $end
    | $comment [ comment_text ] $end
    | simulation_time
    | value_change
-}
data SimulationCommand
    = DumpAll [ValueChange]
    | DumpOff [ValueChange]
    | DumpOn [ValueChange]
    | DumpVars [ValueChange]
    | SimComment CommentText
    | SimTime SimulationTime
    | SimValueChange ValueChange

{- | scope_type ::=
    begin
    | fork
    | function
    | module
    | task
-}
data ScopeType = Begin | Fork | Function | Module | Task

-- | time_number ::= 1 | 10 | 100
data TimeNumber = T1 | T10 | T100

-- | time_unit ::= s | ms | us | ns | ps | fs
data TimeUnit = Seconds | MilliSeconds | MicroSeconds | NanoSeconds | PicoSeconds | FemtoSeconds

{- | var_type ::=
    event | integer | parameter | real | realtime | reg | supply0 | supply1 | time
    | tri | triand | trior | trireg | tri0 | tri1 | wand | wire | wor
-}
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

-- | simulation_time ::= # decimal_number
newtype SimulationTime = SimulationTime Int

{- | value_change ::=
    scalar_value_change
    | vector_value_change
-}
data ValueChange = ScalarChange ScalarValueChange | VectorChange VectorValueChange

-- | scalar_value_change ::= value identifier_code
data ScalarValueChange = ScalarValueChange Value IdentifierCode

-- | value ::= 0 | 1 | x | X | z | Z
data Value = V0 | V1 | Vx | VX | Vz | VZ
    deriving (Eq, Show)

{- | vector_value_change ::=
    b binary_number identifier_code
    | B binary_number identifier_code
    | r real_number identifier_code
    | R real_number identifier_code
-}
data VectorValueChange
    = BinaryLower T.Text IdentifierCode
    | BinaryUpper T.Text IdentifierCode
    | RealLower Double IdentifierCode
    | RealUpper Double IdentifierCode

-- | identifier_code ::= { ASCII character }
newtype IdentifierCode = IdentifierCode T.Text

-- | size ::= decimal_number
newtype Size = Size Int

{- | reference ::=
    identifier
    | identifier [ bit_select_index ]
    | identifier [ msb_index : lsb_index ]
-}
data Reference
    = Identifier T.Text
    | BitSelect T.Text Index
    | RangeSelect T.Text Index Index

-- | index ::= decimal_number
newtype Index = Index Int

-- | scope_identifier ::= { ASCII character }
newtype ScopeIdentifier = ScopeIdentifier T.Text

-- | comment_text ::= { ASCII character }
newtype CommentText = CommentText T.Text

-- | date_text ::= { ASCII character }
newtype Date = Date T.Text

-- | version_text ::= { ASCII character }
newtype Version = Version T.Text

-- | system_task ::= ${ASCII character}
newtype SystemTask = SystemTask T.Text
