module LambdaCoucou.RepublicanCalendar
  -- ( gregorian2republican
  -- , prettyDate
  -- , showR
  -- )
  where

import RIO
import qualified RIO.Text as T

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
ruleset m c = case (m, c) of
  (1, 'a') -> y1792 <$> [((+9) , Niv), (subtract 21, Plu)]
  (1, 'b') -> y1792 <$> [((+10), Niv), (subtract 20, Plu)]
  (1, 'c') -> y1792 <$> [((+11), Niv), (subtract 19, Plu)]
  (1, 'd') -> y1792 <$> [((+12), Niv), (subtract 18, Plu)]

  (2, 'a') -> y1792 <$> [((+10), Plu), (subtract 20, Vnt)]
  (2, 'b') -> y1792 <$> [((+11), Plu), (subtract 19, Vnt)]
  (2, 'c') -> y1792 <$> [((+12), Plu), (subtract 18, Vnt)]
  (2, 'd') -> y1792 <$> [((+13), Plu), (subtract 17, Vnt)]

  (3, 'e') -> y1792 <$> [((+9) , Vnt), (subtract 21, Ger)]
  (3, 'f') -> y1792 <$> [((+10), Vnt), (subtract 20, Ger)]
  (3, 'g') -> y1792 <$> [((+11), Vnt), (subtract 19, Ger)]
  (3, 'h') -> y1792 <$> [((+12), Vnt), (subtract 18, Ger)]

  (4, 'e') -> y1792 <$> [((+10), Ger), (subtract 20, Flo)]
  (4, 'f') -> y1792 <$> [((+11), Ger), (subtract 19, Flo)]
  (4, 'g') -> y1792 <$> [((+12), Ger), (subtract 18, Flo)]
  (4, 'h') -> y1792 <$> [((+13), Ger), (subtract 17, Flo)]

  (5, 'e') -> y1792 <$> [((+10), Flo), (subtract 20, Pra)]
  (5, 'f') -> y1792 <$> [((+11), Flo), (subtract 19, Pra)]
  (5, 'g') -> y1792 <$> [((+12), Flo), (subtract 18, Pra)]
  (5, 'h') -> y1792 <$> [((+13), Flo), (subtract 17, Pra)]

  (6, 'e') -> y1792 <$> [((+11), Pra), (subtract 19, Mes)]
  (6, 'f') -> y1792 <$> [((+12), Pra), (subtract 18, Mes)]
  (6, 'g') -> y1792 <$> [((+13), Pra), (subtract 17, Mes)]
  (6, 'h') -> y1792 <$> [((+14), Pra), (subtract 16, Mes)]

  (7, 'e') -> y1792 <$> [((+11), Mes), (subtract 19, The)]
  (7, 'f') -> y1792 <$> [((+12), Mes), (subtract 18, The)]
  (7, 'g') -> y1792 <$> [((+13), Mes), (subtract 17, The)]
  (7, 'h') -> y1792 <$> [((+14), Mes), (subtract 16, The)]

  (8, 'e') -> y1792 <$> [((+12), The), (subtract 18, Fru)]
  (8, 'f') -> y1792 <$> [((+13), The), (subtract 17, Fru)]
  (8, 'g') -> y1792 <$> [((+14), The), (subtract 16, Fru)]
  (8, 'h') -> y1792 <$> [((+15), The), (subtract 15, Fru)]

  (9, 'e') -> y1792 <$> [((+13), Fru), (subtract 17, SC)]
  (9, 'f') -> y1792 <$> [((+14), Fru), (subtract 16, SC)]
  (9, 'g') -> y1792 <$> [((+15), Fru), (subtract 15, SC)]
  (9, 'h') -> y1792 <$> [((+16), Fru), (subtract 14, SC)]
  (9, 'j') -> y1791 <$> [(subtract 23, Vnd)]
  (9, 'k') -> y1791 <$> [(subtract 22, Vnd)]
  (9, 'l') -> y1791 <$> [(subtract 21, Vnd)]
  (9, 'm') -> y1791 <$> [(subtract 20, Vnd)]

  (10, 'j') -> y1791 <$> [((+7),  Vnd), (subtract 23, Bru)]
  (10, 'k') -> y1791 <$> [((+8),  Vnd), (subtract 22, Bru)]
  (10, 'l') -> y1791 <$> [((+9),  Vnd), (subtract 21, Bru)]
  (10, 'm') -> y1791 <$> [((+10), Vnd), (subtract 20, Bru)]

  (11, 'j') -> y1791 <$> [((+8),  Bru), (subtract 22, Fri)]
  (11, 'k') -> y1791 <$> [((+9),  Bru), (subtract 21, Fri)]
  (11, 'l') -> y1791 <$> [((+10), Bru), (subtract 20, Fri)]
  (11, 'm') -> y1791 <$> [((+11), Bru), (subtract 19, Fri)]

  (12, 'j') -> y1791 <$> [((+8),  Fri), (subtract 22, Niv)]
  (12, 'k') -> y1791 <$> [((+9),  Fri), (subtract 21, Niv)]
  (12, 'l') -> y1791 <$> [((+10), Fri), (subtract 20, Niv)]
  (12, 'm') -> y1791 <$> [((+11), Fri), (subtract 19, Niv)]

  _ -> []

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


rDayName :: Int -> Text
rDayName d
  | m == 1    = "Primedi"
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
isValid (_, SC, n) = n > 0 && n < 6
isValid (_, _, n) = n > 0 && n < 31

gregorian2republican :: (Int, Month, Day) -> Maybe (Year, RMonth, Day)
gregorian2republican x@(y, m, _) =
  let rs    = concatMap (ruleset m) $ yearRules y
      final = filter isValid $ fmap (applyRule x) rs
  in  case final of
        []  -> Nothing
        [r] -> Just r
        _   -> Nothing

showR :: (Year, RMonth, Day) -> Text
showR (y, m, d) = T.pack $ show d <> " " <> show m <> " " <> show y


prettyDate :: (Year, RMonth, Day) -> Text
prettyDate r@(_y, rm, day)
  = "Nous sommes aujourd'hui le "
  <> showR r
  <> " − jour "
  <> dayName (rm, day)
  <> " − "
  <> "et c'est un "
  <> rDayName day
  <> "."

dayName :: (RMonth, Day) -> Text

dayName = \case
  (Vnd, 1 ) -> "du raisin"
  (Vnd, 2 ) -> "du safran"
  (Vnd, 3 ) -> "de la châtaigne"
  (Vnd, 4 ) -> "de la colchique"
  (Vnd, 5 ) -> "du cheval"
  (Vnd, 6 ) -> "de la balsamine"
  (Vnd, 7 ) -> "de la carotte"
  (Vnd, 8 ) -> "de l'amaranthe"
  (Vnd, 9 ) -> "du panais"
  (Vnd, 10) -> "de la cuve"
  (Vnd, 11) -> "de la pomme de terre"
  (Vnd, 12) -> "de l'immortelle"
  (Vnd, 13) -> "du potiron"
  (Vnd, 14) -> "de la réséda"
  (Vnd, 15) -> "de l'âne"
  (Vnd, 16) -> "de la belle de nuit"
  (Vnd, 17) -> "de la citrouille"
  (Vnd, 18) -> "du sarrasin"
  (Vnd, 19) -> "du tournesol"
  (Vnd, 20) -> "du pressoir"
  (Vnd, 21) -> "du chanvre"
  (Vnd, 22) -> "de la pêche"
  (Vnd, 23) -> "du navet"
  (Vnd, 24) -> "de l'amaryllis"
  (Vnd, 25) -> "du bœuf"
  (Vnd, 26) -> "de l'aubergine"
  (Vnd, 27) -> "du piment"
  (Vnd, 28) -> "de la tomate"
  (Vnd, 29) -> "de l'orge"
  (Vnd, 30) -> "du tonneau"

  (Bru, 1 ) -> "de la pomme"
  (Bru, 2 ) -> "du céleri"
  (Bru, 3 ) -> "de la poire"
  (Bru, 4 ) -> "de la betterave"
  (Bru, 5 ) -> "de l'oie"
  (Bru, 6 ) -> "de l'héliotrope"
  (Bru, 7 ) -> "de la figue"
  (Bru, 8 ) -> "de la scorsonère"
  (Bru, 9 ) -> "de l'alisier"
  (Bru, 10) -> "de la charrue"
  (Bru, 11) -> "du salsifis"
  (Bru, 12) -> "de la mâcre"
  (Bru, 13) -> "du topinambour"
  (Bru, 14) -> "de l'andive"
  (Bru, 15) -> "du dindon"
  (Bru, 16) -> "du chervis"
  (Bru, 17) -> "du cresson"
  (Bru, 18) -> "de la dentelaire"
  (Bru, 19) -> "de la grenade"
  (Bru, 20) -> "de la herse"
  (Bru, 21) -> "de la bacchante"
  (Bru, 22) -> "de l'azerole"
  (Bru, 23) -> "de la garance"
  (Bru, 24) -> "de l'orange"
  (Bru, 25) -> "du faisan"
  (Bru, 26) -> "de la pistache"
  (Bru, 27) -> "du macjonc"
  (Bru, 28) -> "du coing"
  (Bru, 29) -> "du cormier"
  (Bru, 30) -> "du rouleau"

  (Fri, 1 ) -> "de la raiponce"
  (Fri, 2 ) -> "du turneps"
  (Fri, 3 ) -> "de la chicorée"
  (Fri, 4 ) -> "du nèfle"
  (Fri, 5 ) -> "du cochon"
  (Fri, 6 ) -> "de la mâche"
  (Fri, 7 ) -> "du chou-fleur"
  (Fri, 8 ) -> "du miel"
  (Fri, 9 ) -> "de la genièvre"
  (Fri, 10) -> "de la pioche"
  (Fri, 11) -> "de la cire"
  (Fri, 12) -> "de la cire"
  (Fri, 13) -> "du cèdre"
  (Fri, 14) -> "du sapin"
  (Fri, 15) -> "du chevreuil"
  (Fri, 16) -> "de l'ajonc"
  (Fri, 17) -> "du cyprès"
  (Fri, 18) -> "du lierre"
  (Fri, 19) -> "de la sabine"
  (Fri, 20) -> "du hoyau"
  (Fri, 21) -> "de l'érable à sucre"
  (Fri, 22) -> "de la bruyère"
  (Fri, 23) -> "du roseau"
  (Fri, 24) -> "de l'oseille"
  (Fri, 25) -> "du grillon"
  (Fri, 26) -> "du pignon"
  (Fri, 27) -> "de la liège"
  (Fri, 28) -> "de la truffe"
  (Fri, 29) -> "de l'olive"
  (Fri, 30) -> "de la pelle"

  (Niv, 1 ) -> "de la tourbe"
  (Niv, 2 ) -> "de la houille"
  (Niv, 3 ) -> "du bitume"
  (Niv, 4 ) -> "du soufre"
  (Niv, 5 ) -> "du chien"
  (Niv, 6 ) -> "de la lave"
  (Niv, 7 ) -> "de la terre végétale"
  (Niv, 8 ) -> "du fumier"
  (Niv, 9 ) -> "du salpêtre"
  (Niv, 10) -> "du fléau"
  (Niv, 11) -> "du granit"
  (Niv, 12) -> "de l'argile"
  (Niv, 13) -> "de l'ardoise"
  (Niv, 14) -> "du grès"
  (Niv, 15) -> "du lapin"
  (Niv, 16) -> "du silex"
  (Niv, 17) -> "de la marne"
  (Niv, 18) -> "de la pierre à chaux"
  (Niv, 19) -> "du marbre"
  (Niv, 20) -> "du van"
  (Niv, 21) -> "de la pierre à plâtre"
  (Niv, 22) -> "du sel"
  (Niv, 23) -> "du fer"
  (Niv, 24) -> "du cuivre"
  (Niv, 25) -> "du chat"
  (Niv, 26) -> "de l'étain"
  (Niv, 27) -> "du plomb"
  (Niv, 28) -> "du zinc"
  (Niv, 29) -> "du mercure"
  (Niv, 30) -> "du crible"

  (Plu, 1 ) -> "de lauréole"
  (Plu, 2 ) -> "de la mousse"
  (Plu, 3 ) -> "du fragon"
  (Plu, 4 ) -> "du perce-neige"
  (Plu, 5 ) -> "du taureau"
  (Plu, 6 ) -> "du laurier-tin"
  (Plu, 7 ) -> "de l'amadouvier"
  (Plu, 8 ) -> "du mézéréon"
  (Plu, 9 ) -> "du peuplier"
  (Plu, 10) -> "de la cognée"
  (Plu, 11) -> "de l'ellébore"
  (Plu, 12) -> "du brocoli"
  (Plu, 13) -> "du laurier"
  (Plu, 14) -> "de l'avelinier"
  (Plu, 15) -> "de la vache"
  (Plu, 16) -> "du buis"
  (Plu, 17) -> "du lichen"
  (Plu, 18) -> "de l'if"
  (Plu, 19) -> "de la pulmonaire"
  (Plu, 20) -> "de la serpette"
  (Plu, 21) -> "de la thlaspi"
  (Plu, 22) -> "du thimele"
  (Plu, 23) -> "du chiendent"
  (Plu, 24) -> "de la trainasse"
  (Plu, 25) -> "du lièvre"
  (Plu, 26) -> "du guède"
  (Plu, 27) -> "du noisetier"
  (Plu, 28) -> "de la cyclamen"
  (Plu, 29) -> "de la chélidoine"
  (Plu, 30) -> "du traîneau"

  (Vnt, 1 ) -> "du tussilage"
  (Vnt, 2 ) -> "du cornouiller"
  (Vnt, 3 ) -> "du violier"
  (Vnt, 4 ) -> "du troène"
  (Vnt, 5 ) -> "du bouc"
  (Vnt, 6 ) -> "des asaret"
  (Vnt, 7 ) -> "de l'alaterne"
  (Vnt, 8 ) -> "de la violette"
  (Vnt, 9 ) -> "du marceau"
  (Vnt, 10) -> "de la bêche"
  (Vnt, 11) -> "de la narcisse"
  (Vnt, 12) -> "de l'orme"
  (Vnt, 13) -> "des fumeterre"
  (Vnt, 14) -> "de la vélar"
  (Vnt, 15) -> "de la chèvre"
  (Vnt, 16) -> "de l'épinard"
  (Vnt, 17) -> "de la doronic"
  (Vnt, 18) -> "du mouron"
  (Vnt, 19) -> "du cerfeuil"
  (Vnt, 20) -> "du cordeau"
  (Vnt, 21) -> "de la mandragore"
  (Vnt, 22) -> "du persil"
  (Vnt, 23) -> "du cochléaria"
  (Vnt, 24) -> "de la pâquerette"
  (Vnt, 25) -> "du thon"
  (Vnt, 26) -> "du pissenlit"
  (Vnt, 27) -> "de la sylvie"
  (Vnt, 28) -> "de la capillaire"
  (Vnt, 29) -> "du frêne"
  (Vnt, 30) -> "du plantoir"

  (Ger, 1 ) -> "de la primevère"
  (Ger, 2 ) -> "du platane"
  (Ger, 3 ) -> "de l'asperge"
  (Ger, 4 ) -> "de la tulipe"
  (Ger, 5 ) -> "de la poule"
  (Ger, 6 ) -> "de la bette"
  (Ger, 7 ) -> "du bouleau"
  (Ger, 8 ) -> "de la jonquille"
  (Ger, 9 ) -> "de l'aulne"
  (Ger, 10) -> "du couvoir"
  (Ger, 11) -> "de la pervenche"
  (Ger, 12) -> "du charme"
  (Ger, 13) -> "de la morille"
  (Ger, 14) -> "de l'hêtre"
  (Ger, 15) -> "de l'abeille"
  (Ger, 16) -> "de la laitue"
  (Ger, 17) -> "du mélèze"
  (Ger, 18) -> "de la ciguë"
  (Ger, 19) -> "du radis"
  (Ger, 20) -> "de la ruche"
  (Ger, 21) -> "du gainier"
  (Ger, 22) -> "de la romaine"
  (Ger, 23) -> "du marronnier"
  (Ger, 24) -> "de la roquette"
  (Ger, 25) -> "du pigeon"
  (Ger, 26) -> "du lilas"
  (Ger, 27) -> "de l'anémone"
  (Ger, 28) -> "de la pensée"
  (Ger, 29) -> "de la myrtille"
  (Ger, 30) -> "du greffoir"

  (Flo, 1 ) -> "de la rose"
  (Flo, 2 ) -> "du chêne"
  (Flo, 3 ) -> "de la fougère"
  (Flo, 4 ) -> "de l'aubépine"
  (Flo, 5 ) -> "du rossignol"
  (Flo, 6 ) -> "de l'ancolie"
  (Flo, 7 ) -> "du muguet"
  (Flo, 8 ) -> "du champignon"
  (Flo, 9 ) -> "de la hyacinthe"
  (Flo, 10) -> "du râteau"
  (Flo, 11) -> "de la rhubarbe"
  (Flo, 12) -> "du sainfoin"
  (Flo, 13) -> "du bâton-d'or"
  (Flo, 14) -> "du chamérisier"
  (Flo, 15) -> "du ver à soie"
  (Flo, 16) -> "de la consoude"
  (Flo, 17) -> "de la pimprenelle"
  (Flo, 18) -> "de la corbeille d'or"
  (Flo, 19) -> "de l'arroche"
  (Flo, 20) -> "du sarcloir"
  (Flo, 21) -> "de la statice"
  (Flo, 22) -> "de la fritillaire"
  (Flo, 23) -> "de la bourrache"
  (Flo, 24) -> "de la valériane"
  (Flo, 25) -> "de la carpe"
  (Flo, 26) -> "du fusain"
  (Flo, 27) -> "de la civette"
  (Flo, 28) -> "de la buglosse"
  (Flo, 29) -> "de la sénevé"
  (Flo, 30) -> "de la houlette"

  (Pra, 1 ) -> "de la luzerne"
  (Pra, 2 ) -> "de l'hémérocalle"
  (Pra, 3 ) -> "du trèfle"
  (Pra, 4 ) -> "de l'angélique"
  (Pra, 5 ) -> "du canard"
  (Pra, 6 ) -> "de la mélisse"
  (Pra, 7 ) -> "du fromental"
  (Pra, 8 ) -> "du martagon"
  (Pra, 9 ) -> "du serpolet"
  (Pra, 10) -> "de la faux"
  (Pra, 11) -> "de la fraise"
  (Pra, 12) -> "de la bétoine"
  (Pra, 13) -> "du pois"
  (Pra, 14) -> "de l'acacia"
  (Pra, 15) -> "de la caille"
  (Pra, 16) -> "de l'œillet"
  (Pra, 17) -> "du sureau"
  (Pra, 18) -> "du pavot"
  (Pra, 19) -> "du tilleul"
  (Pra, 20) -> "de la fourche"
  (Pra, 21) -> "du barbeau"
  (Pra, 22) -> "de la camomille"
  (Pra, 23) -> "du chèvrefeuille"
  (Pra, 24) -> "de la caille-lait"
  (Pra, 25) -> "de la tanche"
  (Pra, 26) -> "du jasmin"
  (Pra, 27) -> "de la verveine"
  (Pra, 28) -> "du thym"
  (Pra, 29) -> "de la pivoine"
  (Pra, 30) -> "du chariot"

  (Mes, 1 ) -> "du seigle"
  (Mes, 2 ) -> "de l'avoine"
  (Mes, 3 ) -> "de l'oignon"
  (Mes, 4 ) -> "de la véronique"
  (Mes, 5 ) -> "du mulet"
  (Mes, 6 ) -> "du romarin"
  (Mes, 7 ) -> "du concombre"
  (Mes, 8 ) -> "de l'échalote"
  (Mes, 9 ) -> "de l'absinthe"
  (Mes, 10) -> "de la faucille"
  (Mes, 11) -> "de la coriandre"
  (Mes, 12) -> "de l'artichaut"
  (Mes, 13) -> "de la giroflée"
  (Mes, 14) -> "de la lavande"
  (Mes, 15) -> "du chamois"
  (Mes, 16) -> "du tabac"
  (Mes, 17) -> "de la groseille"
  (Mes, 18) -> "de la gesse"
  (Mes, 19) -> "cde la erise"
  (Mes, 20) -> "du parc"
  (Mes, 21) -> "de la menthe"
  (Mes, 22) -> "du cumin"
  (Mes, 23) -> "du haricot"
  (Mes, 24) -> "de l'orcanète"
  (Mes, 25) -> "de la pintade"
  (Mes, 26) -> "de la sauge"
  (Mes, 27) -> "de l'ail"
  (Mes, 28) -> "de la vesce"
  (Mes, 29) -> "du blé"
  (Mes, 30) -> "de la chalemie"

  (The, 1 ) -> "de l'épeautre"
  (The, 2 ) -> "du bouillon-blanc"
  (The, 3 ) -> "du melon"
  (The, 4 ) -> "de l'ivraie"
  (The, 5 ) -> "du bélier"
  (The, 6 ) -> "de la prêle"
  (The, 7 ) -> "de l'armoise"
  (The, 8 ) -> "de la carthame"
  (The, 9 ) -> "de la mûre"
  (The, 10) -> "de l'arrosoir"
  (The, 11) -> "de la panic"
  (The, 12) -> "de la salicorne"
  (The, 13) -> "de l'abricot"
  (The, 14) -> "du basilic"
  (The, 15) -> "de la brebis"
  (The, 16) -> "de la guimauve"
  (The, 17) -> "du lin"
  (The, 18) -> "de l'amande"
  (The, 19) -> "de la gentiane"
  (The, 20) -> "de l'écluse"
  (The, 21) -> "de la carline"
  (The, 22) -> "du câprier"
  (The, 23) -> "de la lentille"
  (The, 24) -> "de l'aunée"
  (The, 25) -> "de la loutre"
  (The, 26) -> "de la myrte"
  (The, 27) -> "du colza"
  (The, 28) -> "du lupin"
  (The, 29) -> "du coton"
  (The, 30) -> "du moulin"

  (Fru, 1 ) -> "de la prune"
  (Fru, 2 ) -> "du millet"
  (Fru, 3 ) -> "du lycoperdon"
  (Fru, 4 ) -> "de l'escourgeon"
  (Fru, 5 ) -> "du saumon"
  (Fru, 6 ) -> "de la tubéreuse"
  (Fru, 7 ) -> "du sucrion"
  (Fru, 8 ) -> "de l'apocyn"
  (Fru, 9 ) -> "de la réglisse"
  (Fru, 10) -> "de l'échelle"
  (Fru, 11) -> "de la pastèque"
  (Fru, 12) -> "du fenouil"
  (Fru, 13) -> "de l'épine-vinette"
  (Fru, 14) -> "de la noix"
  (Fru, 15) -> "de la truite"
  (Fru, 16) -> "du citron"
  (Fru, 17) -> "de la cardère"
  (Fru, 18) -> "du nerprun"
  (Fru, 19) -> "de la tagette"
  (Fru, 20) -> "de la hotte"
  (Fru, 21) -> "de l'églantier"
  (Fru, 22) -> "de la noisette"
  (Fru, 23) -> "du houblon"
  (Fru, 24) -> "du sorgho"
  (Fru, 25) -> "de l'écrevisse"
  (Fru, 26) -> "de la bigarade"
  (Fru, 27) -> "de la verge d'or"
  (Fru, 28) -> "du maïs"
  (Fru, 29) -> "du marron"
  (Fru, 30) -> "du panier"

  (SC, 1) -> "de la vertu"
  (SC, 2) -> "du génie"
  (SC, 3) -> "du travail"
  (SC, 4) -> "de l'opinion"
  (SC, 5) -> "des récompenses"

  _ -> "ERROR"

