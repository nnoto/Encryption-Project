--Name: Nicholas Noto
--CruzID: nnoto

import Data.Array
import Data.Char 
import Data.Bits 
import Numeric 

-- Encrypt/Decrypt using RC4 keystream
encrypt :: String -> String -> String
encrypt key plaintext = chrList (xorLists (ordList (initializePrga (initializeKsa (key)))) (ordList plaintext))

-- Convert character list to integer list, helper function to make encrypt easier to read 
ordList :: [Char] -> [Int]
ordList x = map ord x

-- Convert integer list to character list, helper function to make encrypt easier to read
chrList :: [Int] -> [Char]
chrList x = map chr x

-- XOR two lists until second list runs out
xorLists :: [Int] -> [Int] -> [Int]
xorLists x y = zipWith xor x y


-- Pseudo-random generation algorithm                          
initializePrga :: Array Int Int -> String
initializePrga = prga 0 0 
  where prga i j s = let i' = mod (i + 1) 256
                         j' = mod (s ! i' + j) 256
                         s' = swap s i' j' 
                         t = s' ! (mod (s ! i' + s ! j') 256)
                      in (chr t):(prga i' j' s')

-- Initialize state array for KSA
initializeArray :: Array Int Int
initializeArray  = array (0,255) (zip [0..255] [0..255])

-- Key Scheduling Algorithm 
initializeKsa :: String -> Array Int Int
initializeKsa key = ksa 0 0 initializeArray --initialize state array
  where ksa 256 j s = s --end condition for recursion, loop 0-255
        ksa i j s = let k = mod (j + s ! i + ord (key !! (mod i (length key)))) 256 --ord converts char to int
                     in ksa (i + 1) k (swap s k i ) --recurse and increment i

-- Swap the values of S[i] and S[j], // is the incremental array update infix operator
swap :: Array Int Int -> Int -> Int -> Array Int Int
swap s index1 index2 | index1 == index2 = s | otherwise = (s // [(index1, s ! index2), (index2, s ! index1)])


-- Convert to hex values
printHex :: String -> String	
printHex = concat . map showHex' . ordList
			where showHex' i = showHex i ""

main :: IO ()
main = do
	--Test 1
	let en1 = encrypt "Key" "Plaintext"
	let keyStr1 = initializePrga (initializeKsa "Key")
	let keyStr1' = printHex keyStr1
	let en1' = printHex en1
	let de1 = encrypt "Key" en1
	--Test 2
	let en2 = encrypt "Wiki" "pedia"
	let key2 = initializePrga (initializeKsa "Wiki")
	let key2' = printHex key2
	let en2' = printHex en2
	let de2 = encrypt "Wiki" en2
	--Test 3
	let en3 = encrypt "Secret" "Attack at dawn"
	let key3 = initializePrga (initializeKsa "Secret")
	let key3' = printHex key3
	let en3' = printHex en3
	let de3 = encrypt "Secret" en3
	--Test 4 with user input
	putStr "Select Key: "
	n <- getLine
	putStr "Text to encrypt: "
	m <- getLine
	let en4 = encrypt n m
	let keyStr4 = initializePrga (initializeKsa n)
	let keyStr4' = printHex keyStr4
	let en4' = printHex en4
	let de4 = encrypt n en4
	--Test 1 output
	putStrLn ("Key: Key\nText to encrypt: Plaintext")
	putStrLn ("Keystream: " ++ take 16 keyStr1')
	putStrLn ("Cipher: " ++ en1')
	putStrLn ("Decrypted: " ++ de1 ++ "\n")
	--Test 2 output
	putStrLn ("Key: Wiki\nText to encrypt: pedia")
	putStrLn ("Keystream: " ++ take 16 key2')
	putStrLn ("Cipher: " ++ en2')
	putStrLn ("Decrypted: " ++ de2 ++ "\n")
	--Test 3 output
	putStrLn ("Key: Secret\nText to encrypt: Attack at dawn")
	putStrLn ("Keystream: " ++ take 16 key3')
	putStrLn ("Cipher: " ++ en3')
	putStrLn ("Decrypted: " ++ de3)
		--Test 4 output
	putStrLn ("\nKey: " ++ n ++ "\nText to encrypt: " ++ m)
	putStrLn ("Keystream: " ++ take 16 keyStr4')
	putStrLn ("Cipher: " ++ en4')
	putStrLn ("Decrypted: " ++ de4)
	
	
