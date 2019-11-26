module TypeChecking where

data Type
    = BigIntT -- 32-bit integers
    | SmallIntT -- 32-bit integeres
    | TrileanT -- 3-booleans
    | FloatT
    | CharT
    | StringT
    | ArrayT Type
    | SetT Type
    | EnumT [String]
    | RecordT [Type]
    | UnionT [Type]
    | PointerT [Type]
    | TypeError
