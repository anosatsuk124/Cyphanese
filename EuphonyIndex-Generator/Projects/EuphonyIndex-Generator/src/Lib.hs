module Lib
    ( someFunc
    ) where

import System.IO
import GHC.IO.Encoding (utf8)
import System.Random
import qualified Data.List as L
import qualified Data.Map as Map
import Text.Show.Unicode

randIO :: Int -> Int -> IO Int
randIO x y = randomRIO (x, y) :: IO Int

fact 0 = 1
fact n = fact(n-1) * n

{- 死骸、いつかのために残す
select :: Int -> Int -> [Int] -> IO [Int]
select range randrange selects = do
    rand <- randIO 0 randrange
    case range of
        0 -> return selects
        otherwise -> select (range - 1) randrange (rand:selects)
-}

--母音表

vowels :: Int -> [String] -> [String] -> IO [String]
vowels kinds vows void = do --母音の種類と母音表と空のリスト
    select <- randIO 1 (length vows)
    select2 <- randIO 1 (length vows)
    vowslength <- randIO 1 2 --二重母音までにしました。疲れたので三重母音とか言わないで、てか日本語話者は使えないよ
    if kinds == length void
        then return void
        else if kinds > length void && vowslength == 1 && notElem (vows!!(select-1)) void
            then vowels kinds vows ((vows!!(select-1)):void)
        else if kinds > length void && vowslength == 2 && notElem ((vows!!(select-1))++(vows!!(select2-1))) void && (vows!!(select-1)) /= (vows!!(select2-1))
            then vowels kinds vows (((vows!!(select-1))++(vows!!(select2-1))):void)
        else vowels kinds vows void

--子音表
consonants :: Int -> [String] -> [String] -> IO [String]
consonants kinds con void = do
    select <- randIO 1 (length con)
    if kinds == length void
        then return void
        else if kinds > length void && notElem (con!!(select-1)) void
           then consonants kinds con ((con!!(select-1)):void)
        else consonants kinds con void

--音節構造自動生成器(ランダム)
sylgen :: Int -> [String] -> IO [String]
sylgen maxsyllable void = do
    let symbol = ["C", "V"]
    select <- randIO 0 1
    if length void == 0
        then do
            max <- randIO 1 maxsyllable --音節構造の大きさの範囲を指定するとこ
            sylgen max ((symbol!!select):void)
        else if (length (head void)) == maxsyllable then syl maxsyllable void
            else if (head void)!!0 == 'V' then sylgen maxsyllable ((init (head (((symbol!!0) ++ head void):(init void)))):(init void))
            else case select of
                    0 -> sylgen maxsyllable (((symbol!!0) ++ head void):(init void))
                    1 -> sylgen maxsyllable (((symbol!!1) ++ head void):(init void))

syl :: Int -> [String] -> IO [String]
syl maxsyllable input = if (elem 'V' (input!!(length input - 1)) && elem 'C' (input!!(length input - 1))) || elem 'V' (input!!(length input -1)) then return input else sylgen maxsyllable []

sylsets :: Int -> Int -> [String] -> IO [String]
sylsets kinds maxsyllable void = do
        sylgen <- sylgen maxsyllable []
        if length void == kinds then return void
            else if notElem (head sylgen) void then sylsets kinds maxsyllable (sylgen ++ void)
            else sylsets kinds (maxsyllable+1) void

--単語の雛形

prewordgen :: Int -> [String] -> [String] -> IO [String]
prewordgen syllableculster syllable void = do
    select <- randIO 1 (length syllable)
    if length void == 0 then prewordgen syllableculster syllable (syllable!!(select-1):void)
        else if length void == syllableculster then preword void []
        else prewordgen syllableculster syllable (syllable!!(select-1):void)

preword :: [String] -> [String] -> IO [String]
preword input output = if length input == 0 then return output
                    else if length output == 0 then preword (tail input) ((head input):output) else preword (tail input) ((head output ++ head input):(init output))

prewordsets :: Int -> Int -> [String] -> [String] -> IO [String]
prewordsets kinds syllablerange syllable void = do
    syllableculster <- randIO 1 syllablerange
    prewordgen <- prewordgen syllableculster syllable []
    if length void == kinds then return void
        else prewordsets kinds syllablerange syllable (prewordgen ++ void)

--単語生成

wordgen :: [String] -> [String] -> [String] -> [String] -> IO [String]
wordgen preword vowel consonant void = do
    selectcon <- randIO 0 (length consonant-1)
    selectvow <- randIO 0 (length vowel-1)
    let con = (consonant!!selectcon)
    let vow = (vowel!!selectvow)
    if length void == length (head preword) then word void []
    else case length void of
            0 -> case (head preword)!!0 of
                    'V' -> wordgen preword vowel consonant (vow:void)
                    'C' -> wordgen preword vowel consonant (con:void)
            _ -> case (head preword)!!(length void) of
                    'V' -> wordgen preword vowel consonant (void ++ (vow:[]))
                    'C' -> if (last void) == con then wordgen preword vowel consonant void else wordgen preword vowel consonant (void ++ (con:[]))

word :: [String] -> [String] -> IO [String]
word input output = if length input == 0 then return output
                    else if  length output == 0 then word (tail input) ((head input):output) else word (tail input) ((head output ++ head input):(init output))

wordsets :: [String] -> [String] -> [String] -> [String] -> IO [String]
wordsets prewords vowel consonant void =
    if length void == length prewords then return void
        else do
            let preword = (prewords!!(length void)):[]
            wordgen <- wordgen preword vowel consonant []
            wordsets prewords vowel consonant (void ++ wordgen)

--文の生成
sentgen :: [String] -> [String] -> String
sentgen wordlist void
        | length wordlist == 0 = head void
        | length void == 0 = sentgen (tail wordlist) ((head wordlist):void)
        | otherwise = sentgen (tail wordlist) ((head wordlist ++ " " ++ head void):init void)

--ユーフォニー指数

euphony :: Double -> Double -> Double -> Double -> Int -> Double
euphony alpha beta gamma delta epsilon
        | epsilon == 2 = -0.04*(e0**2) + 1.4*e0
        | otherwise = e0
    where
        e0 = euphony0 alpha beta gamma delta epsilon

euphony0 :: Double -> Double -> Double -> Double -> Int -> Double
euphony0 alpha beta gamma delta epsilon = 0.5 * (1 + (1 / (1 + exp (0.5*alpha - 7)))) * (100 / (1 + exp (-2.26*alpha - 0.0693*beta + 0.0112*gamma + 0.388*delta - 11.9)))

{-
euphony :: Double -> Double -> Double -> Double -> Double
euphony alpha beta gamma delta = 100/(1+exp(-( -0.001484*alpha+0.039190*beta-0.046390*gamma-0.014107*delta)))
-}
wordsize :: [String] -> [Int] -> [Int]
wordsize wordlist void
    | length wordlist == 0 = void
    | otherwise = wordsize (tail wordlist) (void ++ (length (head wordlist)):[])

mean :: [Double] -> Double --算術平均
mean ns = (sum ns) / (fromIntegral $ length ns)

mode :: [Int] -> [Int] --最頻値
mode [] = []
mode ns =
    let l = Map.fromListWith (\n m -> n + m) $ map (\x -> (x, (1::Double))) ns
        a = foldr1 (\x acc -> if x > acc then x else acc) $ Map.elems l
    in Map.keys $ Map.filter (==a) l

alpha :: [String] -> Double
alpha wordlist = mean (map fromIntegral $ wordsize wordlist [])

beta0 :: [String] -> [String] -> Int
beta0 prewordlist void
    | length prewordlist == 0 = length void
    | length (head prewordlist) == 1 = beta0 (tail prewordlist) void
    | ((head prewordlist)!!0) == 'C' && ((head prewordlist)!!1) == 'C' = beta0 (tail prewordlist) ((head prewordlist):void)
    | otherwise = beta0 (tail prewordlist) void

beta :: [String] -> Double
beta prewordlist = (fromIntegral $ beta0 prewordlist []) / (fromIntegral $ length prewordlist)

gamma0 :: [String] -> [String] -> [String] -> Int -> Int
gamma0 wordlist prewordlist void1 void2
    | length wordlist == 0 = length void1
    | length (head wordlist) < 3 = gamma0 (tail wordlist) prewordlist void1 void2
    | elem (head wordlist) ("s":[]) == False = gamma0 (tail wordlist) prewordlist (void1 ++ ((head wordlist):[])) void2
    | otherwise = if ((head wordlist)!!void2) == 's'
                    then
                        if ((head prewordlist)!!(void2+1)) /= 'C' then gamma0 (tail wordlist) prewordlist void1 void2
                           else if ((head wordlist)!!(void2+2)) == 'r' || ((head wordlist)!!(void2+2)) == 'l' || ((head wordlist)!!(void2+2)) == 'h' then gamma0 (tail wordlist) prewordlist void1 0
                                else gamma0 (tail wordlist) prewordlist (void1 ++ (head wordlist):[]) 0
                    else gamma0 wordlist prewordlist void1 (void2+1)

gamma :: [String] -> [String] -> Double
gamma wordlist prewordlist = (((fromIntegral (gamma0 wordlist prewordlist [] 0)) / (fromIntegral $ length wordlist)) * (beta prewordlist))

delta0 :: [String] -> [String] -> String
delta0 wordlist void
        | length wordlist == 0 = head void
        | length void == 0 = delta0 (tail wordlist) ((head wordlist):void)
        | otherwise = delta0 (tail wordlist) ((head wordlist ++ head void):init void)

delta1 :: String -> Int -> Int
delta1 word void
    | length word == 0 = void
    | head word == 'C' = delta1 (tail word) void+1
    | otherwise = delta1 (tail word) void

delta :: [String] -> Double
delta syllablelist = (fromIntegral (delta1 (delta0 syllablelist []) 0)) / (fromIntegral (length (delta0 syllablelist [])))

epsilon0 :: String -> Int -> [Int] -> Int
epsilon0 word void1 void2
    | length word == 0 = head (mode void2)
    | head word == 'C' = epsilon0 (tail word) (void1+1) void2
    | otherwise = epsilon0 (tail word) 0 (void1:void2)

epsilon :: [String] -> Int
epsilon word = epsilon0 (delta0 word []) 0 []

zeta0 :: String -> Int
zeta0 syllable
    | (head syllable) == 'C' = 1
    | otherwise = 0 --  音節頭に子音が出現する割合

zeta1 :: [String] -> Int -> Int
zeta1 syllset void
    | length syllset == 0 = void
    | otherwise = zeta1 (tail syllset) (void + zeta0 (head syllset))

zeta :: [String] -> Int --自動生成だけ
zeta syllablelist
    | fromIntegral (zeta1 syllablelist 0) / fromIntegral (length syllablelist) == 1 = 1
    | otherwise = 0

eta :: [String] -> Int --VCが表層形の音節構造か否か自動生成だけ
eta syllablelist
    | length syllablelist == 1 && (head syllablelist) == "VC" = 1
    | otherwise = 0

theta0 :: String -> [Int] -> [Int]
theta0 preword void--母音クラスタの平均値
    | length preword == 0 = void
    | length void == 0 = theta0 preword (0:void)
    | (head preword) == 'V' = theta0 (tail preword) (((head void)+1):(tail void))
    | otherwise = theta0 (tail preword) (0:void)

theta1 :: [String] -> [Int] -> [Int]
theta1 prewordlist void
    | length prewordlist == 0 = void
    | otherwise = theta1 (tail prewordlist) (void ++ (theta0 (head prewordlist) []))

theta :: [String] -> Double
theta prewordlist = mean (map fromIntegral (theta1 prewordlist []))


--自然言語処理
natural :: String -> [String] -> [String] -> String -> String -> String
natural sentence vowels consonants void1 void2
    | length sentence == 0 = void2
    | (head sentence) == ' ' && elem 'C' void1 = natural (tail sentence) vowels consonants "" (void2 ++ "C" ++ " ")
    | (head sentence) == ' ' && elem 'V' void1 = natural (tail sentence) vowels consonants "" (void2 ++ "V" ++ " ")
    | (head sentence) == ' ' = natural (tail sentence) vowels consonants "" (void2 ++ " ")
    | elem ((head sentence):[]) vowels == True = natural (tail sentence) vowels consonants "" (void2 ++ "V")
    | elem ((head sentence):[]) consonants == True = natural (tail sentence) vowels consonants "" (void2 ++ "C")
    | elem (void1 ++ ((head sentence):[])) consonants && elem (void1 ++ ((head sentence):[]) ++ ((sentence!!1):[])) consonants == True = natural (tail sentence) vowels consonants "" (void2 ++ "C")
    | elem (void1 ++ ((head sentence):[])) vowels && elem (void1 ++ ((head sentence):[]) ++ ((sentence!!1):[])) vowels == True = natural (tail sentence) vowels consonants "" (void2 ++ "V")
    | elem (void1 ++ ((head sentence):[])) consonants == True = natural (tail sentence) vowels consonants "" (void2 ++ "C")
    | elem (void1 ++ ((head sentence):[])) vowels == True = natural (tail sentence) vowels consonants "" (void2 ++ "V")
    | elem ((head sentence):[]) vowels == False = natural (tail sentence) vowels consonants (void1 ++ ((head sentence):[])) void2
    | elem ((head sentence):[]) consonants == False = natural (tail sentence) vowels consonants (void1 ++ ((head sentence):[])) void2
    | otherwise = natural (tail sentence) vowels consonants "" (void2 ++ "?")

naturallist :: String -> [String] -> [String] -> String -> [String] -> [String]
naturallist sentence vowels consonants void1 void2
    | length sentence == 0 = (void2 ++ (void1:[]))
    | elem ((head sentence):[]) vowels == True = naturallist (tail sentence) vowels consonants (void1 ++ ((head sentence):[])) void2
    | elem ((head sentence):[]) consonants == True = naturallist (tail sentence) vowels consonants (void1 ++ ((head sentence):[])) void2
    | otherwise = naturallist (tail sentence) vowels consonants "" (void2 ++ (void1:[]))

--IPAfile生成器
ipagen0 :: [String] -> String -> Int -> Int -> [String]
ipagen0 ipa supply void ipalength
    | ipalength == 0 = ipagen0 ipa supply void (length ipa)
    | void == ipalength = ipa
    | otherwise = ipagen0 ((tail ipa) ++ (((head ipa) ++ ((last supply):[])):[])) supply (void+1) ipalength

ipagen :: [String] -> [String] -> Int -> [String] -> [String]
ipagen ipa supply void output--IPAと補助記号を引数に取ると全組み合わせ作ってくれる
    | void == length supply = output
    | otherwise = ipagen ipa supply (void+1) (output ++ (ipagen0 ipa (supply!!void) 0 0))

euphonyindexgenerator1 :: String -> IO()
euphonyindexgenerator1 path = do
    input <- readFile ("src/IPA-consonants/affricates.txt")
    input2 <- readFile ("src/IPA-consonants/approximants.txt")
    input13 <- readFile ("src/IPA-consonants/clicks.txt")
    input14 <- readFile ("src/IPA-consonants/ejectives.txt")
    input5 <- readFile ("src/IPA-consonants/flaps.txt")
    input6 <- readFile ("src/IPA-consonants/fricatives.txt")
    input7 <- readFile ("src/IPA-consonants/implosives.txt")
    input8 <- readFile ("src/IPA-consonants/nasals.txt")
    input9 <- readFile ("src/IPA-consonants/plosives.txt")
    input10 <- readFile ("src/IPA-consonants/trills.txt")
    let consonant = (L.words input) ++ (L.words input2) ++ (L.words input13) ++ (L.words input14) ++ (L.words input5) ++ (L.words input6) ++ (L.words input7) ++ (L.words input8) ++ (L.words input9) ++ (L.words input10)
    input1 <- readFile("src/IPA-vowels/vowels.txt")
    input3 <- readFile("src/IPA-vowels/long-vowels.txt")
    let vowel = (L.words input1) ++ (L.words input3)
    vows <- randIO 1 (length vowel)
    vowels <- vowels vows vowel []
    cons <- randIO 2 (length consonant)
    consonants <- consonants cons consonant []
    kind <- randIO 1 50
    max <- randIO 1 3
    sylsets <- sylsets kind max []
    prewordgen <- prewordgen 3 sylsets []
    syllablerange <- randIO 1 6
    prewordsets <- prewordsets 30 syllablerange  sylsets []
    wordgen <- wordgen prewordgen vowels consonants []
    wordsets <- wordsets prewordsets vowels consonants []
    let sentence = sentgen wordsets []
    let alph = alpha prewordsets
    let bet = beta prewordsets
    let gam = gamma wordsets prewordsets
    let del = delta prewordsets
    let ep = epsilon prewordsets
    let zt = zeta sylsets
    let et = eta sylsets
    let thet = theta prewordsets
    let euphonyindex = euphony alph bet gam del ep
    appendFile path (sentence ++ "\t" ++ (show euphonyindex) ++ "\t" ++ (show alph) ++ "\t" ++ (show bet) ++ "\t" ++ (show gam) ++ "\t" ++ (show del) ++ "\t" ++ (show ep) ++ "\t" ++ (show zt) ++ (show et) ++ "\t" ++ (show thet) ++ "\t 0" ++ "\r\n")

euphonyindexgenerator0 :: String -> String -> Int -> Int -> IO()
euphonyindexgenerator0 header path x x2
    | x == x2 = do
        appendFile path header
        euphonyindexgenerator0 header path x (x2+1)
    | x == 0 = appendFile path "\r\n"
    | otherwise = do
        euphonyindexgenerator1 path
        euphonyindexgenerator0 header path (x-1) x2

euphonyindexgenerator :: String -> String -> Int -> IO()
euphonyindexgenerator header path x = euphonyindexgenerator0 header path x x

ipaconIO :: IO [String]
ipaconIO = do
    input <- readFile ("src/IPA-consonants/affricates-supply.txt")
    input2 <- readFile ("src/IPA-consonants/approximants-supply.txt")
    input3 <- readFile ("src/IPA-consonants/clicks-supply.txt")
    input4 <- readFile ("src/IPA-consonants/ejectives-supply.txt")
    input5 <- readFile ("src/IPA-consonants/flaps-supply.txt")
    input6 <- readFile ("src/IPA-consonants/fricatives-supply.txt")
    input7 <- readFile ("src/IPA-consonants/implosives-supply.txt")
    input8 <- readFile ("src/IPA-consonants/nasals-supply.txt")
    input9 <- readFile ("src/IPA-consonants/plosives-supply.txt")
    input10 <- readFile ("src/IPA-consonants/trills-supply.txt")
    let consonants = (L.words input) ++ (L.words input2) ++ (L.words input3) ++ (L.words input4) ++ (L.words input5) ++ (L.words input6) ++ (L.words input7) ++ (L.words input8) ++ (L.words input9) ++ (L.words input10)
    return consonants

natgen :: [String] -> IO()
natgen inp
    | length inp == 0 = print "finish"
    | otherwise = do
        let input = (head inp)
        input2 <- readFile("src/IPA-vowels/vowels-supply.txt")
        input3 <- readFile("src/IPA-vowels/syllablic-consonants.txt")
        input4 <- readFile("src/IPA-vowels/long-vowels.txt")
        input5 <- ipaconIO
        let vowel = (L.words input2) ++ (L.words input3) ++ (L.words input4)
        let consonant = input5
        let nat = natural input vowel consonant "" ""
        let natwordset = naturallist input vowel consonant "" []
        let pre = L.words nat
        let alph = alpha pre
        let bet = beta pre
        let gam = gamma natwordset pre
        let del = delta pre
        let ep = epsilon pre
        let thet = theta pre
        let euphonyindex = euphony alph bet gam del ep
        appendFile "asIPAnatural2.txt" (input ++ "\t" ++ (show euphonyindex) ++ "\t" ++ (show alph) ++ "\t" ++ (show bet) ++ "\t" ++ (show gam) ++ "\t" ++ (show del) ++ "\t" ++ (show ep) ++ "\t 0" ++ "\t 0" ++ (show thet) ++ "\t 1" ++ "\r\n")
        natgen (tail inp)

someFunc :: IO()
someFunc = do
    hSetBinaryMode stdout True
    hSetEncoding stdout utf8
    --euphonyindexgenerator "sentence \t E \t alpha \t beta \t gamma \t delta \t epsilon \t zeta \t eta \t theta \t natorcong \r\n" "asIPA2.txt" 200
    langset <- readFile("languageset.txt")
    natgen (lines langset)
    {-
    putStrLn "文: "
    input <- getLine
    input2 <- readFile("src/IPA-vowels/vowels-supply.txt")
    input3 <- readFile("src/IPA-vowels/syllablic-consonants.txt")
    input4 <- readFile("src/IPA-vowels/long-vowels.txt")
    input5 <- ipaconIO
    let vowel = (L.words input2) ++ (L.words input3) ++ (L.words input4)
    let consonant = input5
    let nat = natural input vowel consonant "" ""
    let natwordset = naturallist input vowel consonant "" []
    let pre = L.words nat
    putStrLn "音節構造: "
    print pre
    putStrLn "ユーフォニー指数(Euphony Index;)"
    putStr "alpha:"
    let alph = alpha pre
    print $ alph
    putStr "beta:"
    let bet = 100 * (beta pre)
    print $ bet
    putStr "gamma:"
    let gam = 100 * (gamma natwordset pre)
    print $ gam
    putStr "delta:"
    let del = 100 * (delta pre)
    print $ del
    putStr "epsilon:"
    let ep = epsilon pre
    print $ ep
    putStr "E = "
    let euphonyindex = euphony alph bet gam del ep
    --appendFile "asIPAnatural.txt" (input ++ "\t" ++ (show euphonyindex) ++ "\t" ++ (show alph) ++ "\t" ++ (show bet) ++ "\t" ++ (show gam) ++ "\t" ++ (show del) ++ "\t" ++ (show ep) ++ "\t 1" ++ "\r\n")
    print $ euphonyindex
    -}
    {-
    input1 <- readFile("src/IPA-vowels/vowels.txt")
    input3 <- readFile("src/IPA-vowels/long-vowels.txt")
    let vowel = (L.words input1) ++ (L.words input3)
    vows <- randIO 1 (length vowel)
    vowels <- vowels vows vowel []
    input <- readFile ("src/IPA-consonants/affricates.txt")
    input2 <- readFile ("src/IPA-consonants/approximants.txt")
    input13 <- readFile ("src/IPA-consonants/clicks.txt")
    input14 <- readFile ("src/IPA-consonants/ejectives.txt")
    input5 <- readFile ("src/IPA-consonants/flaps.txt")
    input6 <- readFile ("src/IPA-consonants/fricatives.txt")
    input7 <- readFile ("src/IPA-consonants/implosives.txt")
    input8 <- readFile ("src/IPA-consonants/nasals.txt")
    input9 <- readFile ("src/IPA-consonants/plosives.txt")
    input10 <- readFile ("src/IPA-consonants/trills.txt")
    let consonant = (L.words input) ++ (L.words input2) ++ (L.words input13) ++ (L.words input14) ++ (L.words input5) ++ (L.words input6) ++ (L.words input7) ++ (L.words input8) ++ (L.words input9) ++ (L.words input10)
    cons <- randIO 2 (length consonant)
    consonants <- consonants cons consonant []
    putStrLn "音節構造一覧"
    kind <- randIO 1 50
    max <- randIO 1 3
    sylsets <- sylsets kind max []
    print $ sylsets
    putStrLn "単語の雛形(語の音節構造)"
    prewordgen <- prewordgen 3 sylsets []
    print $ prewordgen
    putStrLn "単語の雛形(語の音節構造)一覧"
    syllablerange <- randIO 1 6
    prewordsets <- prewordsets 30 syllablerange  sylsets []
    print $ prewordsets
    putStrLn "単語"
    wordgen <- wordgen prewordgen vowels consonants []
    uprint $ wordgen
    putStrLn "単語一覧"
    wordsets <- wordsets prewordsets vowels consonants []
    uprint $ wordsets
    putStrLn "文の生成"
    let sentence = sentgen wordsets []
    uprint $ sentence
    putStrLn "ユーフォニー指数(Euphony Index;)"
    putStr "alpha:"
    let alph = alpha prewordsets
    print $ alph
    putStr "beta:"
    let bet = 100 * (beta prewordsets)
    print $ bet
    putStr "gamma:"
    let gam = 100 * (gamma wordsets prewordsets)
    print $ gam
    putStr "delta:"
    let del = 100 * (delta prewordsets)
    print $ del
    putStr "epsilon:"
    let ep = epsilon prewordsets
    print $ ep
    putStr "E = "
    let euphonyindex = euphony alph bet gam del ep
    print $ euphonyindex 
    --euphonyindexgenerator "sentence \t E \t alpha \t beta \t gamma \t delta \t epsilon \n" 500
    euphonyindexgenerator "" "./asIPA1.txt" 50
    -}