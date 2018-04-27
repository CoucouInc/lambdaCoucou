{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.FrenchCalendar
    (showR, g2r, RMonth, getFrenchDate)
    where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.DateTime as DT
import Control.Monad.IO.Class (liftIO)
import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T

data RMonth
    = Vnd
    | Bru
    | Fri
    | Niv
    | Plu
    | Vnt
    | Ger
    | Flo
    | Pra
    | Mes
    | The
    | Fru
    | SC
    deriving (Eq)

instance Show RMonth where
    show Vnd = "Vendémiaire"
    show Bru = "Brumaire"
    show Fri = "Frimaire"
    show Niv = "Nivôse"
    show Plu = "Pluviôse"
    show Vnt = "Ventôse"
    show Ger = "Germinal"
    show Flo = "Floréal"
    show Pra = "Prairial"
    show Mes = "Messidor"
    show The = "Thermidor"
    show Fru = "Fructidor"
    show SC  = "Sans-Culottides"


type Rule = (Int -> Int, RMonth, Int -> Int)
type Year = Int
type Month = Int
type Day = Int


addThird :: t -> (a, b) -> (a, b, t)
addThird f (a, b) = (a, b, f)

y1792, y1791 :: (a, b) -> (a, b, Int -> Int)
y1792 = addThird (subtract 1792)
y1791 = addThird (subtract 1791)

ruleset :: Month -> Char -> [Rule]
ruleset 1 'a' = y1792 <$> [((+9) , Niv), (subtract 21, Plu)]
ruleset 1 'b' = y1792 <$> [((+10), Niv), (subtract 20, Plu)]
ruleset 1 'c' = y1792 <$> [((+11), Niv), (subtract 19, Plu)]
ruleset 1 'd' = y1792 <$> [((+12), Niv), (subtract 18, Plu)]

ruleset 2 'a' = y1792 <$> [((+10), Plu), (subtract 20, Vnt)]
ruleset 2 'b' = y1792 <$> [((+11), Plu), (subtract 19, Vnt)]
ruleset 2 'c' = y1792 <$> [((+12), Plu), (subtract 18, Vnt)]
ruleset 2 'd' = y1792 <$> [((+13), Plu), (subtract 17, Vnt)]

ruleset 3 'e' = y1792 <$> [((+9) , Vnt), (subtract 21, Ger)]
ruleset 3 'f' = y1792 <$> [((+10), Vnt), (subtract 20, Ger)]
ruleset 3 'g' = y1792 <$> [((+11), Vnt), (subtract 19, Ger)]
ruleset 3 'h' = y1792 <$> [((+12), Vnt), (subtract 18, Ger)]

ruleset 4 'e' = y1792 <$> [((+10), Ger), (subtract 20, Flo)]
ruleset 4 'f' = y1792 <$> [((+11), Ger), (subtract 19, Flo)]
ruleset 4 'g' = y1792 <$> [((+12), Ger), (subtract 18, Flo)]
ruleset 4 'h' = y1792 <$> [((+13), Ger), (subtract 17, Flo)]

ruleset 5 'e' = y1792 <$> [((+10), Flo), (subtract 20, Pra)]
ruleset 5 'f' = y1792 <$> [((+11), Flo), (subtract 19, Pra)]
ruleset 5 'g' = y1792 <$> [((+12), Flo), (subtract 18, Pra)]
ruleset 5 'h' = y1792 <$> [((+13), Flo), (subtract 17, Pra)]

ruleset 6 'e' = y1792 <$> [((+11), Pra), (subtract 19, Mes)]
ruleset 6 'f' = y1792 <$> [((+12), Pra), (subtract 18, Mes)]
ruleset 6 'g' = y1792 <$> [((+13), Pra), (subtract 17, Mes)]
ruleset 6 'h' = y1792 <$> [((+14), Pra), (subtract 16, Mes)]

ruleset 7 'e' = y1792 <$> [((+11), Mes), (subtract 19, The)]
ruleset 7 'f' = y1792 <$> [((+12), Mes), (subtract 18, The)]
ruleset 7 'g' = y1792 <$> [((+13), Mes), (subtract 17, The)]
ruleset 7 'h' = y1792 <$> [((+14), Mes), (subtract 16, The)]

ruleset 8 'e' = y1792 <$> [((+12), The), (subtract 18, Fru)]
ruleset 8 'f' = y1792 <$> [((+13), The), (subtract 17, Fru)]
ruleset 8 'g' = y1792 <$> [((+14), The), (subtract 16, Fru)]
ruleset 8 'h' = y1792 <$> [((+15), The), (subtract 15, Fru)]

ruleset 9 'e' = y1792 <$> [((+13), Fru), (subtract 17, SC)]
ruleset 9 'f' = y1792 <$> [((+14), Fru), (subtract 16, SC)]
ruleset 9 'g' = y1792 <$> [((+15), Fru), (subtract 15, SC)]
ruleset 9 'h' = y1792 <$> [((+16), Fru), (subtract 14, SC)]
ruleset 9 'j' = y1791 <$> [(subtract 23, Vnd)]
ruleset 9 'k' = y1791 <$> [(subtract 22, Vnd)]
ruleset 9 'l' = y1791 <$> [(subtract 21, Vnd)]
ruleset 9 'm' = y1791 <$> [(subtract 20, Vnd)]


ruleset 10 'j' = y1791 <$> [((+7),  Vnd), (subtract 23, Bru)]
ruleset 10 'k' = y1791 <$> [((+8),  Vnd), (subtract 22, Bru)]
ruleset 10 'l' = y1791 <$> [((+9),  Vnd), (subtract 21, Bru)]
ruleset 10 'm' = y1791 <$> [((+10), Vnd), (subtract 20, Bru)]

ruleset 11 'j' = y1791 <$> [((+8),  Bru), (subtract 22, Fri)]
ruleset 11 'k' = y1791 <$> [((+9),  Bru), (subtract 21, Fri)]
ruleset 11 'l' = y1791 <$> [((+10), Bru), (subtract 20, Fri)]
ruleset 11 'm' = y1791 <$> [((+11), Bru), (subtract 19, Fri)]

ruleset 12 'j' = y1791 <$> [((+8),  Fri), (subtract 22, Niv)]
ruleset 12 'k' = y1791 <$> [((+9),  Fri), (subtract 21, Niv)]
ruleset 12 'l' = y1791 <$> [((+10), Fri), (subtract 20, Niv)]
ruleset 12 'm' = y1791 <$> [((+11), Fri), (subtract 19, Niv)]

ruleset _ _ = []

yearRules :: Year -> String
yearRules y
      | y `within` (1792, 1799) = let r | m == 0 = "bfl"
                                        | m == 1 = "cfl"
                                        | m == 2 = "cfl"
                                        | m == 3 = "cfk"
                                        | m == 4 = "l"
                                        | m == 5 = "cfl"
                                        | m == 6 = "cfl"
                                        | m == 7 = "cfk"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (1800, 1807) = let r | m == 0 = "bek"
                                        | m == 1 = "bek"
                                        | m == 2 = "bek"
                                        | m == 3 = "bej"
                                        | m == 4 = "aek"
                                        | m == 5 = "bek"
                                        | m == 6 = "bek"
                                        | m == 7 = "bej"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (1808, 1815) = let r | m == 0 = "aek"
                                        | m == 1 = "bek"
                                        | m == 2 = "bek"
                                        | m == 3 = "bek"
                                        | m == 4 = "bfk"
                                        | m == 5 = "bek"
                                        | m == 6 = "bek"
                                        | m == 7 = "bek"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (1816, 1891) = let r | m == 0 = "bfk"
                                        | m == 1 = "bek"
                                        | m == 2 = "bek"
                                        | m == 3 = "bek"
                                        | m == 4 = "bfk"
                                        | m == 5 = "bek"
                                        | m == 6 = "bek"
                                        | m == 7 = "bek"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (1892, 1899) = let r | m == 0 = "cgl"
                                        | m == 1 = "cfl"
                                        | m == 2 = "cfl"
                                        | m == 3 = "cfl"
                                        | m == 4 = "bfl"
                                        | m == 5 = "cfl"
                                        | m == 6 = "cfl"
                                        | m == 7 = "cfl"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (1900, 1907) = let r | m == 0 = "cfk"
                                        | m == 1 = "bek"
                                        | m == 2 = "bek"
                                        | m == 3 = "bek"
                                        | m == 4 = "bfk"
                                        | m == 5 = "bek"
                                        | m == 6 = "bek"
                                        | m == 7 = "bek"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (1908, 1991) = let r | m == 0 = "bfk"
                                        | m == 1 = "bek"
                                        | m == 2 = "bek"
                                        | m == 3 = "bek"
                                        | m == 4 = "bfk"
                                        | m == 5 = "bek"
                                        | m == 6 = "bek"
                                        | m == 7 = "bek"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (1992, 2003) = let r | m == 0 = "cgl"
                                        | m == 1 = "cfl"
                                        | m == 2 = "cfl"
                                        | m == 3 = "cfl"
                                        | m == 4 = "bfl"
                                        | m == 5 = "cfl"
                                        | m == 6 = "cfl"
                                        | m == 7 = "cfl"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (2004, 2091) = let r | m == 0 = "cgl"
                                        | m == 1 = "cfl"
                                        | m == 2 = "cfl"
                                        | m == 3 = "cfl"
                                        | m == 4 = "cgl"
                                        | m == 5 = "cfl"
                                        | m == 6 = "cfl"
                                        | m == 7 = "cfl"
                                        | otherwise = error "impossible"
                                   in r
      | y `within` (2092, 2099) = let r | m == 0 = "dhm"
                                        | m == 1 = "dgm"
                                        | m == 2 = "dgm"
                                        | m == 3 = "dgm"
                                        | m == 4 = "cgm"
                                        | m == 5 = "dgm"
                                        | m == 6 = "dgm"
                                        | m == 7 = "dgm"
                                        | otherwise = error "impossible"
                                   in r
      | otherwise = ""
  where
      m = y `mod` 8
      n `within` (a, b) = n >= a && n <= b


rDayName :: Int -> String
rDayName d | m == 1    = "Primedi"
           | m == 2    = "Duodi"
           | m == 3    = "Tridi"
           | m == 4    = "Quartidi"
           | m == 5    = "Quintidi"
           | m == 6    = "Sextidi"
           | m == 7    = "Septidi"
           | m == 8    = "Octidi"
           | m == 9    = "Nonidi"
           | m == 0    = "Décadi"
           | otherwise = error "impossible"
    where m = d `mod` 10


applyRule :: (Year, Month, Day) -> Rule -> (Year, RMonth, Day)
applyRule (y, _, d) (fd, rm, fy) = (fy y, rm, fd d)

isValid :: (Year, RMonth, Day) -> Bool
isValid (_, _, n) = n > 0 && n < 31

g2r :: (Year, Month, Day) -> Maybe (Year, RMonth, Day)
g2r x@(y, m, _) =
    let rs    = concatMap (ruleset m) $ yearRules y
        final = filter isValid $ fmap (applyRule x) rs
    in  case final of
            []  -> Nothing
            [r] -> Just r
            _   -> Nothing

showR :: (Year, RMonth, Day) -> String
showR (y, m, d) = rDayName d ++ " " ++ show d ++ " " ++ show m ++ " " ++ show y


getFrenchDate :: IRC.StatefulIRC T.BotState (Maybe Text)
getFrenchDate = do
    (y, m, d) <- liftIO $ DT.toGregorian' <$> DT.getCurrentTime
    let rd     = g2r (fromIntegral y, m, d)
    let result = maybe "Oops, something went wrong" showR rd
    pure $ Just $ T.pack result
