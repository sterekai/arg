module ARG.Crypto where
	import Data.Bits (xor)
	import Data.Char (ord)

	crypt :: String -> String -> [Int]
	crypt message key =
		map (\(index, char) ->
			let key_char = (key !! (rem index (length key))) in
			xor (ord char) (ord key_char)
		) (zip [0..] message)
