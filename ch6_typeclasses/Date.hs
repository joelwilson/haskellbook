module Date where

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

type DayOfMonth = Int

-- day of week and numerical day of month
data Date =
  Date DayOfWeek DayOfMonth

instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'