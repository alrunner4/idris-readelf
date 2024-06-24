module ReadElf
import Data.String.Hex
import Data.String.Parser

public export data SymbolBind = Local | Global | Weak
public export data SymbolType = NoType | Func | Object | File
public export data SymbolVisibility = Default | Hidden | Protected

public export
SectionType: Type
SectionType = String

public export
record Section where
    constructor ReadElfSection
    name: String
    type: SectionType
    address: Integer
    offset:  Integer
    size:    Integer

public export
record Table where
    constructor ReadElfTable
    value: Integer
    size:  Integer
    type: SymbolType
    bind: SymbolBind
    visibility: SymbolVisibility
    name: String

public export
record Document where
    constructor ReadElfDocument
    sections: List Section
    tables: List Table

failNothing: Monad m => ParseT m (Maybe a) -> ParseT m a
failNothing p = p >>= \case
    Nothing => fail "parsed Nothing where disallowed"
    Just x  => pure x

export
hex: Parser Integer
hex = failNothing$ map (hexToInteger . fastPack)$ lexeme$ some$ satisfy$ \c =>
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

export
hexPrefixed: Parser Integer
hexPrefixed = string "0x" *> hex

export
natOrHex: Parser Integer
natOrHex = hexPrefixed <|> map natToInteger natural

export
symbolBind: Parser SymbolBind
symbolBind =
        string "LOCAL"  *> pure Local
    <|> string "GLOBAL" *> pure Global
    <|> string "WEAK"   *> pure Weak

export
Show SymbolBind where
    show = \case
        Local  => "LOCAL"
        Global => "GLOBAL"
        Weak   => "WEAK"

export
symbolVisibility: Parser SymbolVisibility
symbolVisibility =
        string "DEFAULT"   *> pure Default
    <|> string "HIDDEN"    *> pure Hidden
    <|> string "PROTECTED" *> pure Protected

export
Show SymbolVisibility where
    show = \case
        Default   => "DEFAULT"
        Hidden    => "HIDDEN"
        Protected => "PROTECTED"

export
symbolType: Parser SymbolType
symbolType =
        string "NOTYPE" *> pure NoType
    <|> string "FUNC"   *> pure Func
    <|> string "OBJECT" *> pure Object
    <|> string "FILE"   *> pure File

export
Show SymbolType where
    show = \case
        NoType => "NOTYPE"
        Func   => "FUNC"
        Object => "OBJECT"
        File   => "FILE"

export
section: Parser Section
section = do
    spaces
    ignore$ char '['
    spaces
    ignore natural
    ignore$ char ']'
    spaces
    name <- map fastPack$ lexeme$ some$ satisfy$ \c => c == '.' || isAlphaNum c
    type <- map fastPack$ lexeme$ some$ satisfy$ \c => c == '_' || isAlphaNum c
    address <- hex
    offset  <- hex
    size    <- hex
    pure$ ReadElfSection { name, type, address, offset, size }

export
Show Section where
    show s = "\{s.name} \{s.type} \{show s.address} \{show s.offset} \{show s.size}"

export
table: Parser Table
table = do
    spaces
    ignore natural -- "num"
    ignore$ char ':'
    spaces
    value <- lexeme hex
    size <- lexeme natOrHex
    type <- lexeme symbolType
    bind <- lexeme symbolBind
    visibility <- lexeme symbolVisibility
    ignore$ lexeme$ map show natural <|> string "UND" <|> string "ABS" -- "ndx"
    name <- map fastPack$ many$ satisfy (/= '\n')
    ignore$ char '\n'
    pure$ ReadElfTable { value, size, type, bind, visibility, name }

export
Show Table where
    show s = "\{show s.value} \{show s.size} \{show s.type} \{show s.bind} \{show s.visibility} \{s.name}"

