# backpack-str

**String types**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
Str             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
Chr             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
Index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Introducing and eliminating 'Str'**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
empty           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
singleton       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
pack            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unpack          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Basic interface**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
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

**Transforming strings**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
map             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
reverse         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
intersperse     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
intercalate     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
transpose       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
replace         |   | ✔ | ✔ |   |   |   |  

**Case conversion**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
toCaseFold      |   | ✔ | ✔ |   |   |   |  
toLower         |   | ✔ | ✔ |   |   |   |  
toUpper         |   | ✔ | ✔ |   |   |   |  
toTitle         |   | ✔ | ✔ |   |   |   |  

**Justification**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
justifyLeft     |   | ✔ | ✔ |   |   |   |  
justifyRight    |   | ✔ | ✔ |   |   |   |  
center          |   | ✔ | ✔ |   |   |   |  

**Reducing strings (folds)**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
foldl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl'          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl1'         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldr           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldr'          | ✔ |   |   | ✔ | ✔ |   |  
foldr1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldr1'         |   |   |   | ✔ | ✔ |   |  

**Special folds**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
concat          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
concatMap       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
any             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
all             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
maximum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
minimum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Building 'Str's**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
scanl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
scanl1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |  
scanr           | ✔ | ✔ | ✔ | ✔ | ✔ |   |  
scanr1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |  

**Accumulating maps**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
mapAccumL       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
mapAccumR       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Infinite strings**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
repeat          | ✔ |   | ✔ |   |   | ✔ | ✔
cycle           | ✔ |   | ✔ |   |   | ✔ | ✔
iterate         | ✔ |   | ✔ |   |   | ✔ | ✔

**Unfolds and replicates**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
replicate       | ✔ |   |   | ✔ | ✔ | ✔ | ✔
concatReplicate | ✔ | ✔ | ✔ |   |   |   |  
unfoldr         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unfoldrN        |   |   |   | ✔ | ✔ |   |  

**Substrings: Breaking strings**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
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

**Substrings: Breaking into many substrings**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
splitOn         |   | ✔ | ✔ |   |   |   |  
splitWhen       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
chunksOf        |   | ✔ | ✔ |   |   |   |  

**Breaking into lines and words**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
lines           | ✔ | ✔ | ✔ |   | ✔ |   | ✔
unlines         | ✔ | ✔ | ✔ |   | ✔ |   | ✔
words           | ✔ | ✔ | ✔ |   | ✔ |   | ✔
unwords         | ✔ | ✔ | ✔ |   | ✔ |   | ✔

**Predicates**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
isPrefixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
isSuffixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
isInfixOf       | ✔ | ✔ | ✔ | ✔ | ✔ |   |  

**View patterns**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
commonPrefixes  |   | ✔ | ✔ |   |   |   |  

**Search for arbitrary substrings**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
breakSubstring  |   |   |   | ✔ | ✔ |   |  
findSubstring   |   |   |   | ✔ | ✔ |   |  
findSubstrings  |   |   |   | ✔ | ✔ |   |  

**Searching by equality**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
elem            | ✔ |   |   | ✔ | ✔ | ✔ | ✔
notElem         | ✔ |   |   | ✔ | ✔ | ✔ | ✔

**Searching with a predicate**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
find            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
filter          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
partition       | ✔ | ✔ | ✔ | ✔ |   | ✔ |  
breakOnAll      |   | ✔ | ✔ |   |   |   |  

**Indexing 'Str's**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
elemIndex       | ✔ |   |   | ✔ | ✔ | ✔ | ✔
elemIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔
elemIndexEnd    |   |   |   | ✔ | ✔ | ✔ |  
elemCount       | ✔ |   |   | ✔ | ✔ | ✔ | ✔
substringCount  |   | ✔ | ✔ |   |   |   |  
findIndex       | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔
findIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔

**Zipping and unzipping**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
zip             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
zipWith         | ✔ |   |   | ✔ | ✔ | ✔ | ✔
packZipWith     | ✔ | ✔ | ✔ |   |   |   |  
unzip           | ✔ |   |   | ✔ | ✔ | ✔ |  

**Ordered Strs**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
sort            | ✔ |   |   | ✔ | ✔ |   |  

**Copying Strs**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
copy            | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔

**Using Str as CString**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
packCString     | ✔ |   |   | ✔ | ✔ |   |  
packCStringLen  | ✔ |   |   | ✔ | ✔ |   |  

**Using Str as operating system string**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
useAsOSString   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
newOSString     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
packOSString    | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Reading from Str**

                | S | T | TL| B | BC| BL|BLC
----------------|---|---|---|---|---|---|---
readInt         |   |   |   |   | ✔ |   | ✔
readInteger     |   |   |   |   | ✔ |   | ✔
