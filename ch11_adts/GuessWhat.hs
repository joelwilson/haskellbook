-- from 11.1

data GuessWhat =
  ChickenButt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst  :: a
                , psecond :: b }
                deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

-- Sheep can produce between 2 and 30 lbs of wool per year...

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- or

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

idInt :: Id Integer
idInt = MkId 20302

idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

type Awesome = Bool

person :: Product Name Awesome
person = Product "Joel" True

data Twitter =
  Twitter' deriving (Eq, Show)

data AskFm =
  AskFm' deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter'

data SocialNetwork =
    Twitter
  | AskFm
  deriving (Eq, Show)

type Twitter'' = String
type AskFm'' = String

twitter :: Sum Twitter'' AskFm''
twitter = First "Twitter"

-- It has no way of knowing we made a
-- mistake b/c both values are just Strings.
askfm :: Sum Twitter'' AskFm''
askfm = First "AskFm" -- that isn't Twitter!
                      -- but it is Twitter''!

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.000001

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct { pfirst = 42
                          , psecond = 0.000001 }

-- more compelling with domain-specific names:

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

-- reorder stuff when using record syntax
feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

-- Exercise: Programmers
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems,
                                       lang <- allLanguages]
