import System.IO
import Data.Char
import Data.Maybe
import Debug.Trace

data Packet = Packet { version :: Int
                     , typeId :: Int
                     , value :: String
                     , subpackets :: [Packet]
                     } deriving (Show)

main = do
    s <- readFile "input.txt"
    let (packet, _) = parsePacket $ hexStrToBitStr s
    print $ problem1 packet

problem1 :: Packet -> Int
problem1 packet = calculateVersionSum [packet]

calculateVersionSum :: [Packet] -> Int
calculateVersionSum [] = 0
calculateVersionSum packets =
    sum $ map (\p -> version p + (calculateVersionSum $ subpackets p)) packets

parsePacket :: String -> (Packet, String)
parsePacket s =
    let
        version = bitStrToNumber $ take 3 s
        typeId = bitStrToNumber $ take 3 $ drop 3 s
        payload = drop 6 s
    in
        if typeId == 4
            then parseLiteralValuePacket version typeId payload
            else parseOperatorPacket version typeId payload

parseLiteralValuePacket :: Int -> Int -> String -> (Packet, String)
parseLiteralValuePacket version typeId payload =
    let
        (value, rest) = parseVarInt payload
    in
        (Packet version typeId value [], rest)

parseOperatorPacket :: Int -> Int -> String -> (Packet, String)
parseOperatorPacket version typeId (lengthTypeId:rest)=
    let
        (subpackets, rest') = parseSubPackets lengthTypeId rest
    in
        ((Packet version typeId "" subpackets), rest')

parseSubPackets :: Char -> String -> ([Packet], String)
parseSubPackets '1' rest =
    let
        numSubpackets = bitStrToNumber $ take 11 rest
    in
        foldl (\a _ -> expandPacketList a) ([], drop 11 rest) [1..numSubpackets]

parseSubPackets '0' rest =
    let
        lenSubpackets = bitStrToNumber $ take 15 rest
        subpacketStr = take lenSubpackets $ drop 15 rest
        rest' = drop (15 + lenSubpackets) rest
    in
        (parseSubPackets' subpacketStr, rest')

parseSubPackets' :: String -> [Packet]
parseSubPackets' [] = []
parseSubPackets' s =
    let
        (p, rest) = parsePacket s
    in
        (p:parseSubPackets' rest)

expandPacketList :: ([Packet], String) -> ([Packet], String)
expandPacketList (packets, rest) =
    let
        (subpacket, rest') = parsePacket rest
    in
        (packets ++ [subpacket], rest')

parseVarInt :: String -> (String, String)
parseVarInt ('0':xs) = (take 4 xs, drop 4 xs)
parseVarInt ('1':xs) =
    let (rest, s) = parseVarInt (drop 4 xs)
    in ((take 4 xs) ++ rest, s)

bitStrToNumber :: [Char] -> Int
bitStrToNumber s = bitStrToNumber' $ reverse s

bitStrToNumber' :: String -> Int
bitStrToNumber' [] = 0
bitStrToNumber' (x:xs) = (digitToInt x) + 2 * bitStrToNumber' xs

hexStrToBitStr :: String -> String
hexStrToBitStr s = concat $ map hexDigitToBitStr s

hexDigitToBitStr :: Char -> String
hexDigitToBitStr c
    | c == '0' = "0000"
    | c == '1' = "0001"
    | c == '2' = "0010"
    | c == '3' = "0011"
    | c == '4' = "0100"
    | c == '5' = "0101"
    | c == '6' = "0110"
    | c == '7' = "0111"
    | c == '8' = "1000"
    | c == '9' = "1001"
    | c == 'A' = "1010"
    | c == 'B' = "1011"
    | c == 'C' = "1100"
    | c == 'D' = "1101"
    | c == 'E' = "1110"
    | c == 'F' = "1111"
