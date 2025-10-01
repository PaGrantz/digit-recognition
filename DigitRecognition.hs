module DigitRecognition where
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import Data.Ratio (numerator, denominator)
import Data.Ratio ((%))
import Debug.Trace

--                                          Type Aliases
-- These type aliases help to abstract our code. You will use them extensively in DigitRecognition.hs
--
type PixelImage = [[Bool]] 
-- A pixel image is a two-dimensional list of booleans.
-- False represents an empty pixel, and True a grey or black pixel. Each pixel image will be 28x28.
type Feature = Int
-- Each pixel location is considered a separate feature for classification. Because the image is
-- 28x28, there are 784 total features.
type Digit = Integer
-- Each image is of a specific digit, 0 through 9. To distinguish the labels or guesses from
-- other numbers, we use a type alias.

-- A corpus is an association list between digits and the images that are labeled with that
-- digit. By storing the information this way, we avoid the frequent computation of which images
-- are labeled with which digit. 
type Corpus = [(Digit, [PixelImage])]


--                                      Primitive Functions - Given by Professor
--
--hasFeature checks if an image has a specific feature: i.e. if that pixel is white or blank.
--
--This encapsulates the ugliness of storing images as nested lists. Notice the normally
--forbidden use of !!. This suggests that there should be a better way to handle and process
--images. For the purposes of this project we will accept this.  We can take reassurance that
--lists of length 28 are so short that better storage methods are probably unnecessary.
hasFeature :: PixelImage -> Feature -> Bool
hasFeature img ftr = 
    let dim = length img
        row = img !! (ftr `div` dim)
        pixel = row !! (ftr `mod` dim)
    in pixel

--outOf wraps around Haskell's built-in Rational data type. Rationals store fractional values
--precisely, with no possibility of underflow. Internally, the numerator and denominator are
--kept as Integers, which have no maximum outside the space limitations of computer memory. You
--will use this function to return an estimated probability. 
outOf :: Int -> Int -> Rational
outOf a b =  (fromIntegral a) % (fromIntegral b)
   

--                                       Project Functions - Written by me

-- Creates a list of all possible digit labels. 
allDigits :: [Digit]
allDigits = [0..9]


-- Creates a list of all possible features (pixels), starting at 0.
allFeatures :: [Feature]
allFeatures = [0..783]

-- showPixelImage takes a PixelImage and turns it into a single string.
-- The string will have '#' for black pixels, and ' ' for white pixels.
--
-- This uses a helper function that takes an individual row of a pixel image and turns it into a string.
-- 
-- Example: showPixelImage [[True, True], [True, False]]
--          "##\n# \n"
showPixelImage :: PixelImage -> String
showPixelImage img = [ char | lsts <- img, char <- (lineAsString lsts) ++ ['\n'] ]

lineAsString :: [Bool] -> String
lineAsString boollst = [char | bol <- boollst, let char = if bol==True then '#' else ' ']

-- lookupVal takes a key of type a, an association list from a to b, and returns the hopefully
-- unique value associated with the key. If lst contains the tuple (k, v), then 
-- lookupVal k lst should return v. If there are more or less than expected results, it throws an error.
--
-- This uses a helper function lstOfVals which first accumulates all the tuples with the given key.
--
-- Example: lookupVal 'b' [('a', 8), ('b', 7), ('c', 9)]
--          7
lookupVal :: Eq a => a -> [(a, b)] -> b
lookupVal key lst = if (length (possibles) <1) then error "No matches for the given key in the list" else
                    if (length (possibles) >1) then error "More than one match for the given key in the list" 
                    else head (possibles)
                    where possibles = (lstOfVals key lst)

lstOfVals :: Eq a => a -> [(a, b)] -> [b]
lstOfVals key lst = [ x | tps <- lst, fst tps == key, let x = snd tps]

-- buildCorpus takes the list of tuples and separates it into sub-lists for each label.
--
-- This uses helper functions buildAll and lstOfOneDigit. buildAll creates the sub-lists for all labels,
-- which uses lstOfOneDigit to accumulate a list of all the images with a given label. buildCorpus then
-- removes any unnecessary lists for a digit that don't have any associated images.
-- 
-- Example:  
--imgA = [[True, False]]
--imgB = [[False, False]]
--imgC = [[False, True]]
--imgLbls = [(imgA, 9), (imgB, 2), (imgC, 9)]
--sol = [(9, [imgA, imgC]), (2, [imgB])] 
-- buildCorpus imgLbls 
--           [(9, [ [[True, False]], [[False, True]] ]), (2, [[[False, False]]])]
buildCorpus :: [(PixelImage, Digit)] -> Corpus
buildCorpus imgLbls = [ (a,b) | (a,b)<-(buildAll imgLbls), length b /=0]
    
buildAll :: [(PixelImage, Digit)] -> Corpus 
buildAll imgLbls = [ (a,b) | a <- allDigits, let b = (lstOfOneDigit imgLbls a)]

lstOfOneDigit :: [(PixelImage, Digit)] -> Digit -> [PixelImage]
lstOfOneDigit imgLbls a = [ img | tps <- imgLbls, (snd tps) == a, let img = (fst tps) ]



-- Given a corpus and a specific digit Y, probOfDigit estimates P(Y). This is the fraction
-- of the images in the corpus that are labeled with Y.  This uses `outOf` to create
-- the fraction.
-- Example: probOfDigit corpus 9
--         2 % 3
probOfDigit :: Corpus -> Digit -> Rational
probOfDigit corpus digit = (length (lookupVal digit corpus)) `outOf` (sum [ x | tps<-corpus, let x=(length (snd tps)) ] )

-- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
-- probOfFeature imgs ftr estimates the probability P(ftr=Black | Y).
probOfFeature :: [PixelImage] -> Feature -> Rational
probOfFeature imgs ftr = ( length [ y | y<-imgs, y `hasFeature` ftr] ) `outOf` (length imgs)

-- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
-- probOfNoFeature imgs ftr estimates the probability P(ftr=White | Y).
probOfNoFeature :: [PixelImage] -> Feature -> Rational
probOfNoFeature imgs ftr = ( length [ y | y<-imgs, not (y `hasFeature` ftr)] ) `outOf` (length imgs)

-- rankOfDigit estimates the rank of a given digit for a given instance.
rankOfDigit :: Corpus -> Digit -> PixelImage -> Rational
rankOfDigit corpus digit newImg = ( product [ y | x<-allFeatures, let y = (if (newImg `hasFeature` x)
                                                                           then (probOfFeature lst x)
                                                                           else (probOfNoFeature lst x))])
                                  where lst = (lookupVal digit corpus)

-- classifyImage return sthe most likely digit, based on the rank computed by rankOfDigit.
classifyImage :: Corpus -> PixelImage -> Digit
classifyImage corpus newImg = snd (maximum [ (a,b) | b<-allDigits, let a = (rankOfDigit corpus b newImg) ])

--                                  Optional Helpful Functions - Given by Professor

-- valueOfRank takes a rank and turns it into a somewhat reasonable integer, suitable for
-- printing. The ranks may be negative, that's perfectly fine.
valueOfRank :: Rational -> Int
valueOfRank r = 350 + ratLog r 
    where numDigits x = length $ show x
          ratLog r = (numDigits $ numerator r) - (numDigits $ denominator r)


-- rankImage is similar to classify image, but instead of returning the best digit, it should
-- return the list of digits and their ranks. Used by the --ranking flag.
-- It is helpful, but not necessary, to sort the list.
rankImage :: Corpus -> PixelImage -> [(Digit, Int)]
rankImage corpus newImg = 
    undefined
