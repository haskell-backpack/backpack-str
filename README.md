# backpack-str

## Feature matrix

Key | Module name
----|--------------------------
S   | Str.String
T   | Str.Text
TL  | Str.Text.Lazy
B   | Str.ByteString
BC  | Str.ByteString.Char8
BL  | Str.ByteString.Lazy
BLC | Str.ByteString.Lazy.Char8
F   | Str.Foundation

**String types**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
Str             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
Chr             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
Index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Introducing and eliminating strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
empty           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
singleton       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
pack            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unpack          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Basic interface**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
cons            | ✔ |   |   |   |   | ✔ | ✔ |  
cons'           |   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
snoc            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
append          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
head            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
uncons          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
unsnoc          | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
last            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
tail            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
init            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
null            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
length          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
compareLength   | ✔ | ✔ | ✔ |   |   |   |   |  

**Transforming strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
map             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
reverse         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
intersperse     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
intercalate     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
transpose       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
replace         |   | ✔ | ✔ |   |   |   |   |  

**Case conversion**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
toCaseFold      |   | ✔ | ✔ |   |   |   |   |  
toLower         |   | ✔ | ✔ |   |   |   |   |  
toUpper         |   | ✔ | ✔ |   |   |   |   |  
toTitle         |   | ✔ | ✔ |   |   |   |   |  

**Justification**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
justifyLeft     |   | ✔ | ✔ |   |   |   |   |  
justifyRight    |   | ✔ | ✔ |   |   |   |   |  
center          |   | ✔ | ✔ |   |   |   |   |  

**Reducing strings (folds)**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
foldl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl'          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldl1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
foldl1'         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
foldr           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
foldr'          | ✔ |   |   | ✔ | ✔ |   |   | ✔
foldr1          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
foldr1'         |   |   |   | ✔ | ✔ |   |   |  

**Special folds**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
concat          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
concatMap       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
any             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
all             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
maximum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
minimum         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Building strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
scanl           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
scanl1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |  
scanr           | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |  
scanr1          | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |  

**Accumulating maps**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
mapAccumL       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
mapAccumR       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  

**Infinite strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
repeat          | ✔ |   | ✔ |   |   | ✔ | ✔ |  
cycle           | ✔ |   | ✔ |   |   | ✔ | ✔ |  
iterate         | ✔ |   | ✔ |   |   | ✔ | ✔ |  

**Unfolds and replicates**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
replicate       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ |  
concatReplicate | ✔ | ✔ | ✔ |   |   |   |   |  
unfoldr         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
unfoldrN        |   |   |   | ✔ | ✔ |   |   |  

**Substrings: Breaking strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
take            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeEnd         |   | ✔ | ✔ |   |   |   |   |  
drop            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
dropEnd         |   | ✔ | ✔ |   |   |   |   |  
splitAt         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeWhile       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
takeWhileEnd    |   | ✔ | ✔ |   |   |   |   |  
dropWhile       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
dropWhileEnd    |   | ✔ | ✔ |   |   |   |   |  
stripStart      |   | ✔ | ✔ |   |   |   |   |  
stripEnd        |   | ✔ | ✔ |   |   |   |   |  
strip           |   | ✔ | ✔ |   |   |   |   |  
span            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
spanEnd         |   |   |   | ✔ | ✔ |   |   |  
break           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
breakEnd        |   |   |   | ✔ | ✔ |   |   |  
breakOn         |   | ✔ | ✔ | ✔ | ✔ |   |   |  
breakOnEnd      |   | ✔ | ✔ |   |   |   |   |  
group           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
groupBy         | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
inits           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  
tails           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ |  

**Substrings: Breaking into many substrings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
splitOn         |   | ✔ | ✔ |   |   |   |   |  
splitWhen       | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
chunksOf        |   | ✔ | ✔ |   |   |   |   |  

**Breaking into lines and words**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
lines           | ✔ | ✔ | ✔ |   | ✔ |   | ✔ | ✔
unlines         | ✔ | ✔ | ✔ |   | ✔ |   | ✔ |  
words           | ✔ | ✔ | ✔ |   | ✔ |   | ✔ | ✔
unwords         | ✔ | ✔ | ✔ |   | ✔ |   | ✔ |  

**Predicates**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
isPrefixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
isSuffixOf      | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
isInfixOf       | ✔ | ✔ | ✔ | ✔ | ✔ |   |   |  

**View patterns**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
stripPrefix     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
stripSuffix     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
commonPrefixes  |   | ✔ | ✔ |   |   |   |   |  

**Search for arbitrary substrings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
breakSubstring  |   |   |   | ✔ | ✔ |   |   |  
findSubstring   |   |   |   | ✔ | ✔ |   |   |  
findSubstrings  |   |   |   | ✔ | ✔ |   |   |  

**Searching by equality**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
elem            | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
notElem         | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔

**Searching with a predicate**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
find            | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
filter          | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
partition       | ✔ | ✔ | ✔ | ✔ |   | ✔ |   | ✔
breakOnAll      |   | ✔ | ✔ |   |   |   |   |  

**Indexing strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
index           | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
elemIndex       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
elemIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
elemIndexEnd    |   |   |   | ✔ | ✔ | ✔ |   |  
elemCount       | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
substringCount  |   | ✔ | ✔ |   |   |   |   |  
findIndex       | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔ | ✔
findIndices     | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔

**Zipping and unzipping**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
zip             | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
zipWith         | ✔ |   |   | ✔ | ✔ | ✔ | ✔ | ✔
packZipWith     | ✔ | ✔ | ✔ |   |   |   |   |  
unzip           | ✔ |   |   | ✔ | ✔ | ✔ |   |  

**Ordered Strs**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
sort            | ✔ |   |   | ✔ | ✔ |   |   |  

**Copying strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
copy            | ✔ | ✔ |   | ✔ | ✔ | ✔ | ✔ | ✔

**Using as CString**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
packCString     | ✔ |   |   | ✔ | ✔ |   |   |  
packCStringLen  | ✔ |   |   | ✔ | ✔ |   |   |  

**Using as operating system string**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
useAsOSString   | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
newOSString     | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔
packOSString    | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔ | ✔

**Reading integers from strings**

                | S | T | TL| B | BC| BL|BLC| F 
----------------|---|---|---|---|---|---|---|---
readInt         |   |   |   |   | ✔ |   | ✔ |  
readInteger     |   |   |   |   | ✔ |   | ✔ |  
