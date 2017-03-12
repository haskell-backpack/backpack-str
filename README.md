# backpack-str

                | S | T |TL | B |BC |BL |BLC
----------------|---|---|---|---|---|---|---
Str             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
Chr             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
Index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
empty           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
singleton       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
pack            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unpack          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
cons            | ✔ |   |   |   |   | ✔ | ✔
cons'           |   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
snoc            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
append          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
head            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
uncons          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unsnoc          | ✔ |   |   | ✔ | ✔ | ✔ | ✔
last            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
tail            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
init            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
null            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
length          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
compareLength   | ✔ | ✔ | ✔ |   |   |   |  
map             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
reverse         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
intersperse     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
intercalate     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
transpose       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
replace         |   | ✔ | ✔ |   |   |   |  
toCaseFold      |   | ✔ | ✔ |   |   |   |  
toLower         |   | ✔ | ✔ |   |   |   |  
toUpper         |   | ✔ | ✔ |   |   |   |  
toTitle         |   | ✔ | ✔ |   |   |   |  
justifyLeft     |   | ✔ | ✔ |   |   |   |  
justifyRight    |   | ✔ | ✔ |   |   |   |  
center          |   | ✔ | ✔ |   |   |   |  
foldl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl'          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl1'         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldr           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldr'          | ✔ |   |   | ✔ | ✔ |   |  
foldr1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldr1'         |   |   |   | ✔ | ✔ |   |  
concat          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
concatMap       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
any             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
all             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
maximum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
minimum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
scanl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
scanl1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |  
scanr           | ✔ | ✔ | ✔ | ✔ | ✔ |   |  
scanr1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |  
mapAccumL       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
mapAccumR       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
repeat          | ✔ |   | ✔ |   |   | ✔ | ✔
cycle           | ✔ |   | ✔ |   |   | ✔ | ✔
iterate         | ✔ |   | ✔ |   |   | ✔ | ✔
replicate       | ✔ |   |   | ✔ | ✔ | ✔ | ✔
concatReplicate | ✔ | ✔ | ✔ |   |   |   |  
unfoldr         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unfoldrN        |   |   |   | ✔ | ✔ |   |  
take            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeEnd         |   | ✔ | ✔ |   |   |   |  
drop            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
dropEnd         |   | ✔ | ✔ |   |   |   |  
splitAt         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeWhile       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeWhileEnd    |   | ✔ | ✔ |   |   |   |  
dropWhile       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
dropWhileEnd    |   | ✔ | ✔ |   |   |   |  
stripStart      |   | ✔ | ✔ |   |   |   |  
stripEnd        |   | ✔ | ✔ |   |   |   |  
strip           |   | ✔ | ✔ |   |   |   |  
span            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
spanEnd         |   |   |   | ✔ | ✔ |   |  
break           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
breakEnd        |   |   |   | ✔ | ✔ |   |  
breakOn         |   | ✔ | ✔ | ✔ | ✔ |   |  
breakOnEnd      |   | ✔ | ✔ |   |   |   |  
group           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
groupBy         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
inits           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
tails           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
stripPrefix     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
stripSuffix     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
splitOn         |   | ✔ | ✔ |   |   |   |  
splitWhen       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
chunksOf        |   | ✔ | ✔ |   |   |   |  
lines           | ✔ | ✔ | ✔ |   | ✔ |   | ✔
unlines         | ✔ | ✔ | ✔ |   | ✔ |   | ✔
words           | ✔ | ✔ | ✔ |   | ✔ |   | ✔
unwords         | ✔ | ✔ | ✔ |   | ✔ |   | ✔
isPrefixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
isSuffixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
isInfixOf       | ✔ | ✔ | ✔ | ✔ | ✔ |   |  
commonPrefixes  |   | ✔ | ✔ |   |   |   |  
breakSubstring  |   |   |   | ✔ | ✔ |   |  
findSubstring   |   |   |   | ✔ | ✔ |   |  
findSubstrings  |   |   |   | ✔ | ✔ |   |  
elem            | ✔ |   |   | ✔ | ✔ | ✔ | ✔
notElem         | ✔ |   |   | ✔ | ✔ | ✔ | ✔
find            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
filter          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
partition       | ✔ | ✔ | ✔ | ✔ |   | ✔ |  
breakOnAll      |   | ✔ | ✔ |   |   |   |  
index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
elemIndex       | ✔ |   |   | ✔ | ✔ | ✔ | ✔
elemIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔
elemIndexEnd    |   |   |   | ✔ | ✔ | ✔ |  
elemCount       | ✔ |   |   | ✔ | ✔ | ✔ | ✔
substringCount  |   | ✔ | ✔ |   |   |   |  
findIndex       | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔
findIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔
zip             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
zipWith         | ✔ |   |   | ✔ | ✔ | ✔ | ✔
packZipWith     | ✔ | ✔ | ✔ |   |   |   |  
unzip           | ✔ |   |   | ✔ | ✔ | ✔ |  
sort            | ✔ |   |   | ✔ | ✔ |   |  
copy            | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔
packCString     | ✔ |   |   | ✔ | ✔ |   |  
packCStringLen  | ✔ |   |   | ✔ | ✔ |   |  
useAsOSString   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
newOSString     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
packOSString    | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
readInt         |   |   |   |   | ✔ |   | ✔
readInteger     |   |   |   |   | ✔ |   | ✔
