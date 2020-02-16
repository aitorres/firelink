module FireLink.FrontEnd.Utils where

import           Data.List (intercalate)

joinWithCommas :: (Show a) => [a] -> String
joinWithCommas = intercalate ", " . map show

red :: String
red = "\x1b[31m"

bold :: String
bold = "\x1b[1m"

nocolor :: String
nocolor = "\x1b[0m"

magenta :: String
magenta = "\x1b[35m"

blue :: String
blue = "\x1b[94m"

italic ::String
italic = "\x1b[3m"

dim :: String
dim = "\x1b[2m"

cyan :: String
cyan = "\x1b[36m"

brightCyan :: String
brightCyan = "\x1b[96m"

brightMagenta :: String
brightMagenta = "\x1b[95m"

underline :: String
underline = "\x1b[4m"

brightRed :: String
brightRed = "\x1b[91m"

green :: String
green = "\x1b[32m"

yellow :: String
yellow = "\x1b[33m"

data Position = Position
    { row :: !Int -- ^ Row position of error
    , column :: !Int -- ^ Column position of error
    }
    deriving (Eq, Show)
