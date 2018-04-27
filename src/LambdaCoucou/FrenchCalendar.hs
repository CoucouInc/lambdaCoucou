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
showR (y, m, d) = show d ++ " " ++ show m ++ " " ++ show y


getFrenchDate :: IRC.StatefulIRC T.BotState (Maybe Text)
getFrenchDate = do
    (y, m, d) <- liftIO $ DT.toGregorian' <$> DT.getCurrentTime
    let rd = g2r (fromIntegral y, m, d)
    let result = maybe
            "Oops, something went wrong"
            ( \r@(_, rm, day) ->
                "Nous sommes aujourd'hui le "
                    ++ showR r
                    ++ " − jour " ++ dayName (rm, day) ++ " − "
                    ++ "et c'est un "
                    ++ rDayName day
            )
            rd
    pure $ Just $ T.pack result





dayName :: (RMonth, Day) -> String
dayName (Niv, 1 ) = "de la tourbe"
dayName (Niv, 2 ) = "de la houille"
dayName (Niv, 3 ) = "du bitume"
dayName (Niv, 4 ) = "du soufre"
dayName (Niv, 5 ) = "du chien"
dayName (Niv, 6 ) = "de la lave"
dayName (Niv, 7 ) = "de la terre végétale"
dayName (Niv, 8 ) = "du fumier"
dayName (Niv, 9 ) = "du salpêtre"
dayName (Niv, 10) = "du fléau"
dayName (Niv, 11) = "du granit"
dayName (Niv, 12) = "de l'argile"
dayName (Niv, 13) = "de l'ardoise"
dayName (Niv, 14) = "du grès"
dayName (Niv, 15) = "du lapin"
dayName (Niv, 16) = "du silex"
dayName (Niv, 17) = "de la marne"
dayName (Niv, 18) = "de la pierre à chaux"
dayName (Niv, 19) = "du marbre"
dayName (Niv, 20) = "du van"
dayName (Niv, 21) = "de la pierre à plâtre"
dayName (Niv, 22) = "du sel"
dayName (Niv, 23) = "du fer"
dayName (Niv, 24) = "du cuivre"
dayName (Niv, 25) = "du chat"
dayName (Niv, 26) = "de l'étain"
dayName (Niv, 27) = "du plomb"
dayName (Niv, 28) = "du zinc"
dayName (Niv, 29) = "du mercure"
dayName (Niv, 30) = "du crible"

dayName (Plu, 1 ) = "de lauréole"
dayName (Plu, 2 ) = "de la mousse"
dayName (Plu, 3 ) = "du fragon"
dayName (Plu, 4 ) = "du perce-neige"
dayName (Plu, 5 ) = "du taureau"
dayName (Plu, 6 ) = "du laurier-tin"
dayName (Plu, 7 ) = "de l'amadouvier"
dayName (Plu, 8 ) = "du mézéréon"
dayName (Plu, 9 ) = "du peuplier"
dayName (Plu, 10) = "de la cognée"
dayName (Plu, 11) = "de l'ellébore"
dayName (Plu, 12) = "du brocoli"
dayName (Plu, 13) = "du laurier"
dayName (Plu, 14) = "de l'avelinier"
dayName (Plu, 15) = "de la vache"
dayName (Plu, 16) = "du buis"
dayName (Plu, 17) = "du lichen"
dayName (Plu, 18) = "de l'if"
dayName (Plu, 19) = "de la pulmonaire"
dayName (Plu, 20) = "de la serpette"
dayName (Plu, 21) = "de la thlaspi"
dayName (Plu, 22) = "du thimele"
dayName (Plu, 23) = "du chiendent"
dayName (Plu, 24) = "de la trainasse"
dayName (Plu, 25) = "du lièvre"
dayName (Plu, 26) = "du guède"
dayName (Plu, 27) = "du noisetier"
dayName (Plu, 28) = "de la cyclamen"
dayName (Plu, 29) = "de la chélidoine"
dayName (Plu, 30) = "du traîneau"

dayName (Vnt, 1 ) = "du tussilage"
dayName (Vnt, 2 ) = "du cornouiller"
dayName (Vnt, 3 ) = "du violier"
dayName (Vnt, 4 ) = "du troène"
dayName (Vnt, 5 ) = "du bouc"
dayName (Vnt, 6 ) = "des asaret"
dayName (Vnt, 7 ) = "de l'alaterne"
dayName (Vnt, 8 ) = "de la violette"
dayName (Vnt, 9 ) = "du marceau"
dayName (Vnt, 10) = "de la bêche"
dayName (Vnt, 11) = "de la narcisse"
dayName (Vnt, 12) = "de l'orme"
dayName (Vnt, 13) = "des fumeterre"
dayName (Vnt, 14) = "de la vélar"
dayName (Vnt, 15) = "de la chèvre"
dayName (Vnt, 16) = "de l'épinard"
dayName (Vnt, 17) = "de la doronic"
dayName (Vnt, 18) = "du mouron"
dayName (Vnt, 19) = "du cerfeuil"
dayName (Vnt, 20) = "du cordeau"
dayName (Vnt, 21) = "de la mandragore"
dayName (Vnt, 22) = "du persil"
dayName (Vnt, 23) = "du cochléaria"
dayName (Vnt, 24) = "de la pâquerette"
dayName (Vnt, 25) = "du thon"
dayName (Vnt, 26) = "du pissenlit"
dayName (Vnt, 27) = "de la sylvie"
dayName (Vnt, 28) = "de la capillaire"
dayName (Vnt, 29) = "du frêne"
dayName (Vnt, 30) = "du plantoir"

dayName (Ger, 1 ) = "de la primevère"
dayName (Ger, 2 ) = "du platane"
dayName (Ger, 3 ) = "de l'asperge"
dayName (Ger, 4 ) = "de la tulipe"
dayName (Ger, 5 ) = "de la poule"
dayName (Ger, 6 ) = "de la bette"
dayName (Ger, 7 ) = "du bouleau"
dayName (Ger, 8 ) = "de la jonquille"
dayName (Ger, 9 ) = "de l'aulne"
dayName (Ger, 10) = "du couvoir"
dayName (Ger, 11) = "de la pervenche"
dayName (Ger, 12) = "du charme"
dayName (Ger, 13) = "de la morille"
dayName (Ger, 14) = "de l'hêtre"
dayName (Ger, 15) = "de l'abeille"
dayName (Ger, 16) = "de la laitue"
dayName (Ger, 17) = "du mélèze"
dayName (Ger, 18) = "de la ciguë"
dayName (Ger, 19) = "du radis"
dayName (Ger, 20) = "de la ruche"
dayName (Ger, 21) = "du gainier"
dayName (Ger, 22) = "de la romaine"
dayName (Ger, 23) = "du marronnier"
dayName (Ger, 24) = "de la roquette"
dayName (Ger, 25) = "du pigeon"
dayName (Ger, 26) = "du lilas"
dayName (Ger, 27) = "de l'anémone"
dayName (Ger, 28) = "de la pensée"
dayName (Ger, 29) = "de la myrtille"
dayName (Ger, 30) = "du greffoir"

dayName (Flo, 1 ) = "de la rose"
dayName (Flo, 2 ) = "du chêne"
dayName (Flo, 3 ) = "de la fougère"
dayName (Flo, 4 ) = "de l'aubépine"
dayName (Flo, 5 ) = "du rossignol"
dayName (Flo, 6 ) = "de l'ancolie"
dayName (Flo, 7 ) = "du muguet"
dayName (Flo, 8 ) = "du champignon"
dayName (Flo, 9 ) = "de la hyacinthe"
dayName (Flo, 10) = "du râteau"
dayName (Flo, 11) = "de la rhubarbe"
dayName (Flo, 12) = "du sainfoin"
dayName (Flo, 13) = "du bâton-d'or"
dayName (Flo, 14) = "du chamérisier"
dayName (Flo, 15) = "du ver à soie"
dayName (Flo, 16) = "de la consoude"
dayName (Flo, 17) = "de la pimprenelle"
dayName (Flo, 18) = "de la corbeille d'or"
dayName (Flo, 19) = "de l'arroche"
dayName (Flo, 20) = "du sarcloir"
dayName (Flo, 21) = "de la statice"
dayName (Flo, 22) = "de la fritillaire"
dayName (Flo, 23) = "de la bourrache"
dayName (Flo, 24) = "de la valériane"
dayName (Flo, 25) = "de la carpe"
dayName (Flo, 26) = "du fusain"
dayName (Flo, 27) = "de la civette"
dayName (Flo, 28) = "de la buglosse"
dayName (Flo, 29) = "de la sénevé"
dayName (Flo, 30) = "de la houlette"

dayName (Pra, 1 ) = "de la luzerne"
dayName (Pra, 2 ) = "de l'hémérocalle"
dayName (Pra, 3 ) = "du trèfle"
dayName (Pra, 4 ) = "de l'angélique"
dayName (Pra, 5 ) = "du canard"
dayName (Pra, 6 ) = "de la mélisse"
dayName (Pra, 7 ) = "du fromental"
dayName (Pra, 8 ) = "du martagon"
dayName (Pra, 9 ) = "du serpolet"
dayName (Pra, 10) = "de la faux"
dayName (Pra, 11) = "de la fraise"
dayName (Pra, 12) = "de la bétoine"
dayName (Pra, 13) = "du pois"
dayName (Pra, 14) = "de l'acacia"
dayName (Pra, 15) = "de la caille"
dayName (Pra, 16) = "de l'œillet"
dayName (Pra, 17) = "du sureau"
dayName (Pra, 18) = "du pavot"
dayName (Pra, 19) = "du tilleul"
dayName (Pra, 20) = "de la fourche"
dayName (Pra, 21) = "du barbeau"
dayName (Pra, 22) = "de la camomille"
dayName (Pra, 23) = "du chèvrefeuille"
dayName (Pra, 24) = "de la caille-lait"
dayName (Pra, 25) = "de la tanche"
dayName (Pra, 26) = "du jasmin"
dayName (Pra, 27) = "de la verveine"
dayName (Pra, 28) = "du thym"
dayName (Pra, 29) = "de la pivoine"
dayName (Pra, 30) = "du chariot"

dayName (Mes, 1 ) = "du seigle"
dayName (Mes, 2 ) = "de l'avoine"
dayName (Mes, 3 ) = "de l'oignon"
dayName (Mes, 4 ) = "de la véronique"
dayName (Mes, 5 ) = "du mulet"
dayName (Mes, 6 ) = "du romarin"
dayName (Mes, 7 ) = "du concombre"
dayName (Mes, 8 ) = "de l'échalote"
dayName (Mes, 9 ) = "de l'absinthe"
dayName (Mes, 10) = "de la faucille"
dayName (Mes, 11) = "de la coriandre"
dayName (Mes, 12) = "de l'artichaut"
dayName (Mes, 13) = "de la giroflée"
dayName (Mes, 14) = "de la lavande"
dayName (Mes, 15) = "du chamois"
dayName (Mes, 16) = "du tabac"
dayName (Mes, 17) = "de la groseille"
dayName (Mes, 18) = "de la gesse"
dayName (Mes, 19) = "cde la erise"
dayName (Mes, 20) = "du parc"
dayName (Mes, 21) = "de la menthe"
dayName (Mes, 22) = "du cumin"
dayName (Mes, 23) = "du haricot"
dayName (Mes, 24) = "de l'orcanète"
dayName (Mes, 25) = "de la pintade"
dayName (Mes, 26) = "de la sauge"
dayName (Mes, 27) = "de l'ail"
dayName (Mes, 28) = "de la vesce"
dayName (Mes, 29) = "du blé"
dayName (Mes, 30) = "de la chalemie"

dayName (The, 1 ) = "de l'épeautre"
dayName (The, 2 ) = "du bouillon-blanc"
dayName (The, 3 ) = "du melon"
dayName (The, 4 ) = "de l'ivraie"
dayName (The, 5 ) = "du bélier"
dayName (The, 6 ) = "de la prêle"
dayName (The, 7 ) = "de l'armoise"
dayName (The, 8 ) = "de la carthame"
dayName (The, 9 ) = "de la mûre"
dayName (The, 10) = "de l'arrosoir"
dayName (The, 11) = "de la panic"
dayName (The, 12) = "de la salicorne"
dayName (The, 13) = "de l'abricot"
dayName (The, 14) = "du basilic"
dayName (The, 15) = "de la brebis"
dayName (The, 16) = "de la guimauve"
dayName (The, 17) = "du lin"
dayName (The, 18) = "de l'amande"
dayName (The, 19) = "de la gentiane"
dayName (The, 20) = "de l'écluse"
dayName (The, 21) = "de la carline"
dayName (The, 22) = "du câprier"
dayName (The, 23) = "de la lentille"
dayName (The, 24) = "de l'aunée"
dayName (The, 25) = "de la loutre"
dayName (The, 26) = "de la myrte"
dayName (The, 27) = "du colza"
dayName (The, 28) = "du lupin"
dayName (The, 29) = "du coton"
dayName (The, 30) = "du moulin"

dayName (Fru, 1 ) = "de la prune"
dayName (Fru, 2 ) = "du millet"
dayName (Fru, 3 ) = "du lycoperdon"
dayName (Fru, 4 ) = "de l'escourgeon"
dayName (Fru, 5 ) = "du saumon"
dayName (Fru, 6 ) = "de la tubéreuse"
dayName (Fru, 7 ) = "du sucrion"
dayName (Fru, 8 ) = "de l'apocyn"
dayName (Fru, 9 ) = "de la réglisse"
dayName (Fru, 10) = "de l'échelle"
dayName (Fru, 11) = "de la pastèque"
dayName (Fru, 12) = "du fenouil"
dayName (Fru, 13) = "de l'épine-vinette"
dayName (Fru, 14) = "de la noix"
dayName (Fru, 15) = "de la truite"
dayName (Fru, 16) = "du citron"
dayName (Fru, 17) = "de la cardère"
dayName (Fru, 18) = "du nerprun"
dayName (Fru, 19) = "de la tagette"
dayName (Fru, 20) = "de la hotte"
dayName (Fru, 21) = "de l'églantier"
dayName (Fru, 22) = "de la noisette"
dayName (Fru, 23) = "du houblon"
dayName (Fru, 24) = "du sorgho"
dayName (Fru, 25) = "de l'écrevisse"
dayName (Fru, 26) = "de la bigarade"
dayName (Fru, 27) = "de la verge d'or"
dayName (Fru, 28) = "du maïs"
dayName (Fru, 29) = "du marron"
dayName (Fru, 30) = "du panier"

dayName (SC, _) = "du coucou" -- lambdacoucou specific
dayName _ = "ERROR"
