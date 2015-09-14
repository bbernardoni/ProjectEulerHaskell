import Data.Numbers.Primes
import Data.List
import Data.Char
import Data.Array
import Data.Ord
import Data.Maybe
import Numeric

-- helper math functions
divBy n d = mod n d == 0
primeFacts = primeFacts' 0
primeFacts' p f
    | curP*curP>f  = [f]
    | divBy f curP = curP:primeFacts' (p) (div f curP)
    | otherwise    = primeFacts' (p+1) f
        where curP = primes!!p
isPalindrome xs = xs == reverse xs
numOfDivs = product.map((+1).length).group.primeFacts
sumOfDivs 1 = 1
sumOfDivs n = (product.map sumOfDivs'.group.primeFacts$n)-n
sumOfDivs' n@(p:_) = (p^(length(n)+1) - 1)`div`(p - 1)
zipLeftover [] [] = []
zipLeftover xs [] = xs
zipLeftover [] ys = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys
takeLast n xs = zipLeftover (drop n xs) xs
factorial n = product [1..n]
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 
sumDigits = sum.map digitToInt.show
delim xs = filter (not.null).lines.foldr (\x a->if x`elem`xs then '\n':a else x:a) []
toDigits = unfoldr(\x->if x==0 then Nothing else Just(x`mod`10,x`div`10))
toDigits' = reverse.toDigits
fromDigits = foldr(\x a->a*10+x)0
--fromDigits' = foldl(\a x->a*10+x)0
rotate (x:xs) = xs++[x]
rotations xs = init(zipWith(++)(tails xs)(inits xs))
toBinaryStr n = showIntAtBase 2 intToDigit n ""
noDups n = n==nub n
isPandigital n = noDups n && length n==9 && notElem '0' n
isPandigital' w n = (length.intersect['1'..intToDigit w]$pan)==w && length pan==w
    where pan = show n
coPrime a = null.intersect(primeFacts a).primeFacts
infiniteElem x = (==x).head.dropWhile(<x)
isSquare n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double)
isPentagonal n = sq * sq == num && sq`mod`6==5
    where   num = 24*n+1
            sq = floor $ sqrt $ (fromIntegral num::Double)
infIntersect3 [] _ _ = []
infIntersect3 _ [] _ = []
infIntersect3 _ _ [] = []
infIntersect3 xa@(x:xs) ya@(y:ys) za@(z:zs)
    | x==y && x==z = x:infIntersect3 xs ys zs
    | x>y = infIntersect3 xa ys za
    | x>z = infIntersect3 xa ya zs
    | otherwise = infIntersect3 xs ya za
isPermutation a b
    | length(show a)/=length(show b) = False
    | otherwise = show a\\show b==""
 
-- common sequences
fibs = 1:2:zipWith (+) fibs (tail fibs)
primesUS = sieve [2..]
    where sieve (p:xs) = p:sieve[x|x<-xs,x `mod` p > 0]
tri = 1:zipWith (+) [2..] tri
tri2 = scanl1 (+) [1..]
polyNum n = scanl1(+)[1,n-1..]
pascal = iterate pascal'$repeat 1
pascal' all@(x:xs) = 1:zipWith (+) (pascal' all) xs

-- problems
problem1a = sum.filter(\x->divBy x 3||divBy x 5)$[1..999]
problem1b = sum[n|n<-[1..999],divBy n 5||divBy n 3]
problem1c = sum[3,6..999]+sum[5,10..999]-sum[15,30..999] 
problem2 = sum.filter even.takeWhile(<4000000)$fibs
problem3 = last.primeFacts$600851475143
problem4a = maximum [x*y|x<-[999,998..100],y<-[999,998..100],isPalindrome.show$x*y]
problem4b = maximum [x*y|x<-[999,998..100],y<-[999,998..x],isPalindrome.show$x*y]
problem5 = foldl lcm 1 [1..20]
problem6a = (sum[1..100])^2-sum(map (^2) [1..100])
problem6b = (sum[1..100])^2-sum[n^2|n<-[1..100]]
problem7 = primes!!10000
problem8a = help8 problem8D
problem8b = maximum.map(product.take 13).tails$problem8D
problem9a = head[a*b*c|c<-[5..],b<-[(1+div c 2)..c-1],let a=1000-b-c,a<b,a^2+b^2==c^2]
problem9b = head[a*b*c|c<-[5..],b<-[1..c-1],a<-[1000-b-c],a<b,a^2+b^2==c^2]
problem9c = head[a*b*c|c<-[5..],b<-[4..c-1],a<-[1000-b-c],a^2+b^2==c^2]
problem10 = sum.takeWhile(<2000000)$primes
problem11 = maximum$map($problem11D)[help11r, help11c, help11b, help11f]
problem12a = head.dropWhile((<500).numOfDivs)$tri
problem12b = head[div (n*(n+1)) 2|n<-[1..],help12 n>500]
problem13 = take 10.show.sum$problem13D
problem14 = snd.foldl1' max.map(\n->(help14 n,n))$[1..1000000]
problem15a = pascal!!20!!20
problem15b = choose 40 20
problem16 = sumDigits$2^1000
problem17 = help17t + 11
problem18 = help18 problem18D
problem19 = length.filter (==0)$help19a
problem20 = sumDigits.factorial$100
problem21 = foldl help21 0 [2..10000]
problem22a = sum.map help22.zip [1..].sort.delim "\","
problem22b = sum.zipWith(*)[1..].map(sum.map(\c->ord c-64)).sort.delim "\","
problem23a = sum.filter help23$[1..28123]
problem23b = sum.filter help23b$[1..28123]
problem24 = (sort.permutations$"0123456789")!!999999
problem25 = findIndex((1000<=).length.show)fibs
problem26a = snd.maximum.map(\n->(help26 9 n,n)).filter isPrime$[7..999]
problem26b = snd.maximum$[(help26 9 n,n)|n<-[7..999],isPrime n]
problem27 = snd.maximum$[(length.takeWhile(\x->isPrime$x^2+a*x+b)$[0..],a*b)|a<-[-999..999],b<-[2..999],isPrime b]
problem28 = 1+sum[16*n^2+4*n+4|n<-[1..500]]
problem29 = length.nub$[a^b|a<-[2..100],b<-[2..100]]
problem30a = sum[n|n<-[10..299999],(sum.map(^5).toDigits$n)==n]
problem30b = sum.filter(\n->(sum.map(^5).toDigits$n)==n)$[10..299999]
problem31 = help31 7 200
problem32a = sum.nub$[a*b|a<-[1..9],b<-[1234..9876],a*b<9877,isPandigital$show a++show b++show(a*b)]++[a*b|a<-[12..98],b<-[123..987],a*b<9877,isPandigital$show a++show b++show(a*b)]
problem32b = sum.nub$[a*b|a<-[1..98],b<-[123..9876],a*b<9877,isPandigital$show a++show b++show(a*b)]
problem33a = let prod=help33 in snd prod`div`(fst prod`gcd`snd prod)
problem33b = let prod=foldl(\(a,b)(c,d)->(a*c,b*d))(1,1)$[(10*x+y,10*y+z)|x<-[1..9],y<-[1..9],z<-[1..9],x/=y,9*x*z+y*z==10*x*y] in snd prod`div`(fst prod`gcd`snd prod)
problem34 = sum[n|n<-[10..2999999],(sum.map factorial.toDigits$n)==n]
problem35a = length.filter (all(isPrime.fromDigits).tail.rotations.toDigits).takeWhile(<1000000)$primes
problem35b = length[p|p<-takeWhile(<1000000)primes,all(isPrime.fromDigits).tail.rotations.toDigits$p]
problem36 = sum[n|n<-[1..999999],isPalindrome.show$n,isPalindrome.toBinaryStr$n]
problem37 = sum.take 11$[p|p<-drop 4 primes,all(isPrime.read).tail.inits.show$p,all(isPrime.read).init.tails.show$p]
problem38a = maximum[n*100002|n<-[9182..9876],noDups.show$n,isPandigital.concatMap show$[n,2*n]]
problem38b = maximum[n*100002|n<-[9182..9876],isPandigital.concatMap show$[n,2*n]]
problem39 = head.maximumBy(comparing length).group.sort.concatMap(takeWhile(<=1000).zipWith(*)[1..].repeat)$[p|m<-[1..21],n<-[1..m-1],coPrime m n,odd(m-n),let p=2*m*(m+n),p<=1000]
problem40a = product.map digitToInt.take 7.zipWith(!!)(repeat.concatMap show$[0..]).iterate(*10)$1
problem40b = product$map(\x->digitToInt$concat[show x|x<-[0..]]!!(10^x))[0..6]
problem41 = head.filter(isPrime.read).concatMap(sortBy(flip compare).permutations).reverse.inits$['1'..'9']
problem42 = length.filter((flip infiniteElem)tri.sum.map(\c->ord c-64)).delim "\","
problem43 = sum.map read.filter(and.zipWith(flip divBy)primes.take 7.map(read.take 3).tail.tails).permutations$['0'..'9']
problem44 = head[p1-p2|p1<-polyNum 5,p2<-takeWhile(<p1)(polyNum 5),isPentagonal(p1+p2),isPentagonal(p1-p2)]
problem45 = head.drop 2$infIntersect3(polyNum 3)(polyNum 5)(polyNum 6)
problem46 = head[g|g<-[9,11..],not.isPrime$g,all(\s->not.isPrime$g-s).takeWhile(<g)$[x^2*2|x<-[1..]]]
problem47 = head[n|n<-[9..],let pf=map(group.primeFacts)[n..n+3],all(\x->length x==4)pf,noDups.concat$pf]
problem48 = reverse.take 10.reverse.show.sum$[x^x|x<-[1..1000]]
problem49 = head$tail[show n++show m++show o|n<-takeWhile(<9999)$dropWhile(<999)primes,m<-takeWhile(<9999)$dropWhile(<=n)primes,isPermutation n m,let o=2*m-n,isPrime o,isPermutation n o]
problem50 = help50 primes.last.takeWhile(<999999)$scanl1(+)primes
problem51 = head[n|n<-dropWhile(<999)primes,let ni=elemIndices '1'$reverse$show n,length ni>2,let a=sum$map(10^)$take 3 ni,and$tail$sort$map isPrime$take 9$iterate(+a)n]
problem52 = head[n|n<-[1..],all(isPermutation n)$map(*n)[2..6]]
problem53 = length[n|n<-[9..100],r<-[1..n],choose n r>999999]
problem54 = length.filter((\(x:y:ys)->x>y).map(help54.sort).(\x->[take 5 x,drop 5 x]).map(\(x:y:ys)->(fromJust$elemIndex x "AKQJT98765432",fromJust$elemIndex y "CSHD")).words).lines
problem55 = length$filter(not.any(isPalindrome.show).take 50.tail.iterate(\x->x+(read.reverse.show$x)))[5..9999]
problem56 = maximum[sumDigits(a^b)|a<-[9..99],b<-[9..99]]
problem57 = length$filter(\(a,b)->help57 a/=help57 b)$take 1000$iterate help57b(3,2)
problem58 = help58 3 5
problem59 = 0

-- main
main = print problem58
--main = problem54io

-- helper problem functions
help58 p t
    | (fromIntegral p)/(fromIntegral t) < 0.1 = s
    | otherwise = help58 (p+ps) (t+4)
    where ps = length.filter isPrime.take 3.tail.iterate(\x->x+1-s)$s^2
          s = (t+5)`div`2
help57 = floor.logBase 10
help57b (a,b)
    | a>1000 = ((a+2*b)/1000,(a+b)/1000)
    | otherwise = (a+2*b,a+b)
help54 ((0,a):(1,b):(2,c):(3,d):(4,e):xs)|all(==a)[b,c,d,e] = 900
help54 ((v,a):(w,b):(x,c):(y,d):(z,e):xs)|fl&&st = 812-v
                                         |all(==v)[w,x,y]||all(==z)[w,x,y] = 712-x
                                         |(v==w&&v==x&&y==z)||(v==w&&x==z&&y==z) = 612-x
                                         |fl = 512-v
                                         |st = 412-v
                                         |(v==w&&v==x)||(x==z&&y==z)||(w==x&&x==y) = 312-x
                                         |(v==w&&x==y)||(v==w&&y==z)||(w==x&&y==z) = 212-w
                                         |v==w||w==x = 112-w
                                         |x==y||y==z = 112-y
                                         |otherwise = 12-v
    where st = all(==z)[v+4,w+3,x+2,y+1]
          fl = all(==a)[b,c,d,e]
help50 (x:xs) n
    | isPrime n = n
    | otherwise = help50 xs (n-x)
help33 = foldl(\(a,b)(c,d)->(a*c,b*d))(1,1)$[(a,b)|a<-[11..98],not.divBy a$10,b<-[a+1..99],help33b a b]
help33b a b
    | null istr = False
    | length istr > 1 = False
    | otherwise = (a*b2==b*a2)
    where astr = show a
          bstr = show b
          istr = intersect astr bstr
          a2 = read(astr\\istr)
          b2 = read(bstr\\istr)
help31c = [1,2,5,10,20,50,100,200]
help31 c m
    | c==0 = 1
    | coin==m = nextC+1
    | coin<m  = nextC+help31 c (m-coin)
    | coin>m  = nextC
    where nextC = help31(c-1)m
          coin = help31c!!c
help26 n d
    | divBy n d = length.show$n
    | otherwise = help26 (n*10+9) d
help23a = filter (\x->sumOfDivs x>x) [12..28123]
help23aa = listArray(1,28123).map(\x->sumOfDivs x>x)$[1..28123]
help23a2 = filter (help23aa!) [1..28123]
help23 x = null.take 1.filter(\y->(x-y)`elem`help23a).takeWhile(<=x`div`2)$help23a
help23b x = not.any(help23aa!).map(x-).takeWhile(<=x`div`2)$help23a2
help22 (x, xs) = x*(sum.map (\c->ord c-64))xs
help21 a x = a + if divSum>x then if sumOfDivs divSum==x then x+divSum else 0 else 0
    where divSum = sumOfDivs x
help19y = [31,28,31,30,31,30,31,31,30,31,30,31]
help19l = [31,29,31,30,31,30,31,31,30,31,30,31]
help19m = concat.take 100.cycle$[help19y,help19y,help19y,help19l]
help19a = foldl help19b [2] help19m
help19b a@(x:xs) y = ((x+y)`mod`7):a
help18 [xs] = xs
help18 (xs:xss) = let cs=help18 xss in zipWith(+)xs.zipWith max cs.tail$cs
help17f = sum[3,3,5,4,4,3,5,5,4]
help17s = sum[3,6,6,8,8,7,7,9,8,8] + 10*sum[6,6,5,5,5,7,6,6] + 9*help17f
help17t = 100*help17f + 10*900 - 9*3 + 10*help17s
help14 1 = 1
help14 x 
    | even x    = 1 + help14 (div x 2)
    | otherwise = 1 + help14 (3*x + 1)
help12 n
    | even n = numOfDivs(div n 2)*numOfDivs(n+1)
    | otherwise = numOfDivs n*numOfDivs(div(n+1) 2)
help11r = maximum.map(maximum.map(product.take 4).tails)
help11c = help11r.transpose
help11b = help11r.transpose.zipWith drop [0..]
help11f = help11b.map reverse
help8 [] = 0
help8 (x:xs) = max(help8 xs) (product(take 13 xs))

-- file io
problem22aio = do
    contents <- readFile "p022_names.txt"
    print$problem22a contents
problem22bio = do
    contents <- readFile "p022_names.txt"
    print$problem22b contents
problem42io = do
    contents <- readFile "p042_words.txt"
    print$problem42 contents
problem54io = do
    contents <- readFile "p054_poker.txt"
    print$problem54 contents

-- data
problem8D = 
   [7,3,1,6,7,1,7,6,5,3,1,3,3,0,6,2,4,9,1,9,2,2,5,1,1,9,6,7,4,4,2,6,5,7,4,7,4,2,3,5,5,3,4,9,1,9,4,9,3,4,
    9,6,9,8,3,5,2,0,3,1,2,7,7,4,5,0,6,3,2,6,2,3,9,5,7,8,3,1,8,0,1,6,9,8,4,8,0,1,8,6,9,4,7,8,8,5,1,8,4,3,
    8,5,8,6,1,5,6,0,7,8,9,1,1,2,9,4,9,4,9,5,4,5,9,5,0,1,7,3,7,9,5,8,3,3,1,9,5,2,8,5,3,2,0,8,8,0,5,5,1,1,
    1,2,5,4,0,6,9,8,7,4,7,1,5,8,5,2,3,8,6,3,0,5,0,7,1,5,6,9,3,2,9,0,9,6,3,2,9,5,2,2,7,4,4,3,0,4,3,5,5,7,
    6,6,8,9,6,6,4,8,9,5,0,4,4,5,2,4,4,5,2,3,1,6,1,7,3,1,8,5,6,4,0,3,0,9,8,7,1,1,1,2,1,7,2,2,3,8,3,1,1,3,
    6,2,2,2,9,8,9,3,4,2,3,3,8,0,3,0,8,1,3,5,3,3,6,2,7,6,6,1,4,2,8,2,8,0,6,4,4,4,4,8,6,6,4,5,2,3,8,7,4,9,
    3,0,3,5,8,9,0,7,2,9,6,2,9,0,4,9,1,5,6,0,4,4,0,7,7,2,3,9,0,7,1,3,8,1,0,5,1,5,8,5,9,3,0,7,9,6,0,8,6,6,
    7,0,1,7,2,4,2,7,1,2,1,8,8,3,9,9,8,7,9,7,9,0,8,7,9,2,2,7,4,9,2,1,9,0,1,6,9,9,7,2,0,8,8,8,0,9,3,7,7,6,
    6,5,7,2,7,3,3,3,0,0,1,0,5,3,3,6,7,8,8,1,2,2,0,2,3,5,4,2,1,8,0,9,7,5,1,2,5,4,5,4,0,5,9,4,7,5,2,2,4,3,
    5,2,5,8,4,9,0,7,7,1,1,6,7,0,5,5,6,0,1,3,6,0,4,8,3,9,5,8,6,4,4,6,7,0,6,3,2,4,4,1,5,7,2,2,1,5,5,3,9,7,
    5,3,6,9,7,8,1,7,9,7,7,8,4,6,1,7,4,0,6,4,9,5,5,1,4,9,2,9,0,8,6,2,5,6,9,3,2,1,9,7,8,4,6,8,6,2,2,4,8,2,
    8,3,9,7,2,2,4,1,3,7,5,6,5,7,0,5,6,0,5,7,4,9,0,2,6,1,4,0,7,9,7,2,9,6,8,6,5,2,4,1,4,5,3,5,1,0,0,4,7,4,
    8,2,1,6,6,3,7,0,4,8,4,4,0,3,1,9,9,8,9,0,0,0,8,8,9,5,2,4,3,4,5,0,6,5,8,5,4,1,2,2,7,5,8,8,6,6,6,8,8,1,
    1,6,4,2,7,1,7,1,4,7,9,9,2,4,4,4,2,9,2,8,2,3,0,8,6,3,4,6,5,6,7,4,8,1,3,9,1,9,1,2,3,1,6,2,8,2,4,5,8,6,
    1,7,8,6,6,4,5,8,3,5,9,1,2,4,5,6,6,5,2,9,4,7,6,5,4,5,6,8,2,8,4,8,9,1,2,8,8,3,1,4,2,6,0,7,6,9,0,0,4,2,
    2,4,2,1,9,0,2,2,6,7,1,0,5,5,6,2,6,3,2,1,1,1,1,1,0,9,3,7,0,5,4,4,2,1,7,5,0,6,9,4,1,6,5,8,9,6,0,4,0,8,
    0,7,1,9,8,4,0,3,8,5,0,9,6,2,4,5,5,4,4,4,3,6,2,9,8,1,2,3,0,9,8,7,8,7,9,9,2,7,2,4,4,2,8,4,9,0,9,1,8,8,
    8,4,5,8,0,1,5,6,1,6,6,0,9,7,9,1,9,1,3,3,8,7,5,4,9,9,2,0,0,5,2,4,0,6,3,6,8,9,9,1,2,5,6,0,7,1,7,6,0,6,
    0,5,8,8,6,1,1,6,4,6,7,1,0,9,4,0,5,0,7,7,5,4,1,0,0,2,2,5,6,9,8,3,1,5,5,2,0,0,0,5,5,9,3,5,7,2,9,7,2,5,
    7,1,6,3,6,2,6,9,5,6,1,8,8,2,6,7,0,4,2,8,2,5,2,4,8,3,6,0,0,8,2,3,2,5,7,5,3,0,4,2,0,7,5,2,9,6,3,4,5,0]
problem11D = 
   [[08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08],
    [49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00],
    [81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65],
    [52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91],
    [22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80],
    [24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50],
    [32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70],
    [67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21],
    [24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72],
    [21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95],
    [78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92],
    [16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57],
    [86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58],
    [19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40],
    [04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66],
    [88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69],
    [04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36],
    [20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16],
    [20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54],
    [01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]]
problem13D :: [Integer]
problem13D = 
   [37107287533902102798797998220837590246510135740250,
    46376937677490009712648124896970078050417018260538,
    74324986199524741059474233309513058123726617309629,
    91942213363574161572522430563301811072406154908250,
    23067588207539346171171980310421047513778063246676,
    89261670696623633820136378418383684178734361726757,
    28112879812849979408065481931592621691275889832738,
    44274228917432520321923589422876796487670272189318,
    47451445736001306439091167216856844588711603153276,
    70386486105843025439939619828917593665686757934951,
    62176457141856560629502157223196586755079324193331,
    64906352462741904929101432445813822663347944758178,
    92575867718337217661963751590579239728245598838407,
    58203565325359399008402633568948830189458628227828,
    80181199384826282014278194139940567587151170094390,
    35398664372827112653829987240784473053190104293586,
    86515506006295864861532075273371959191420517255829,
    71693888707715466499115593487603532921714970056938,
    54370070576826684624621495650076471787294438377604,
    53282654108756828443191190634694037855217779295145,
    36123272525000296071075082563815656710885258350721,
    45876576172410976447339110607218265236877223636045,
    17423706905851860660448207621209813287860733969412,
    81142660418086830619328460811191061556940512689692,
    51934325451728388641918047049293215058642563049483,
    62467221648435076201727918039944693004732956340691,
    15732444386908125794514089057706229429197107928209,
    55037687525678773091862540744969844508330393682126,
    18336384825330154686196124348767681297534375946515,
    80386287592878490201521685554828717201219257766954,
    78182833757993103614740356856449095527097864797581,
    16726320100436897842553539920931837441497806860984,
    48403098129077791799088218795327364475675590848030,
    87086987551392711854517078544161852424320693150332,
    59959406895756536782107074926966537676326235447210,
    69793950679652694742597709739166693763042633987085,
    41052684708299085211399427365734116182760315001271,
    65378607361501080857009149939512557028198746004375,
    35829035317434717326932123578154982629742552737307,
    94953759765105305946966067683156574377167401875275,
    88902802571733229619176668713819931811048770190271,
    25267680276078003013678680992525463401061632866526,
    36270218540497705585629946580636237993140746255962,
    24074486908231174977792365466257246923322810917141,
    91430288197103288597806669760892938638285025333403,
    34413065578016127815921815005561868836468420090470,
    23053081172816430487623791969842487255036638784583,
    11487696932154902810424020138335124462181441773470,
    63783299490636259666498587618221225225512486764533,
    67720186971698544312419572409913959008952310058822,
    95548255300263520781532296796249481641953868218774,
    76085327132285723110424803456124867697064507995236,
    37774242535411291684276865538926205024910326572967,
    23701913275725675285653248258265463092207058596522,
    29798860272258331913126375147341994889534765745501,
    18495701454879288984856827726077713721403798879715,
    38298203783031473527721580348144513491373226651381,
    34829543829199918180278916522431027392251122869539,
    40957953066405232632538044100059654939159879593635,
    29746152185502371307642255121183693803580388584903,
    41698116222072977186158236678424689157993532961922,
    62467957194401269043877107275048102390895523597457,
    23189706772547915061505504953922979530901129967519,
    86188088225875314529584099251203829009407770775672,
    11306739708304724483816533873502340845647058077308,
    82959174767140363198008187129011875491310547126581,
    97623331044818386269515456334926366572897563400500,
    42846280183517070527831839425882145521227251250327,
    55121603546981200581762165212827652751691296897789,
    32238195734329339946437501907836945765883352399886,
    75506164965184775180738168837861091527357929701337,
    62177842752192623401942399639168044983993173312731,
    32924185707147349566916674687634660915035914677504,
    99518671430235219628894890102423325116913619626622,
    73267460800591547471830798392868535206946944540724,
    76841822524674417161514036427982273348055556214818,
    97142617910342598647204516893989422179826088076852,
    87783646182799346313767754307809363333018982642090,
    10848802521674670883215120185883543223812876952786,
    71329612474782464538636993009049310363619763878039,
    62184073572399794223406235393808339651327408011116,
    66627891981488087797941876876144230030984490851411,
    60661826293682836764744779239180335110989069790714,
    85786944089552990653640447425576083659976645795096,
    66024396409905389607120198219976047599490197230297,
    64913982680032973156037120041377903785566085089252,
    16730939319872750275468906903707539413042652315011,
    94809377245048795150954100921645863754710598436791,
    78639167021187492431995700641917969777599028300699,
    15368713711936614952811305876380278410754449733078,
    40789923115535562561142322423255033685442488917353,
    44889911501440648020369068063960672322193204149535,
    41503128880339536053299340368006977710650566631954,
    81234880673210146739058568557934581403627822703280,
    82616570773948327592232845941706525094512325230608,
    22918802058777319719839450180888072429661980811197,
    77158542502016545090413245809786882778948721859617,
    72107838435069186155435662884062257473692284509516,
    20849603980134001723930671666823555245252804609722,
    53503534226472524250874054075591789781264330331690]
problem18D :: [[Int]]
problem18D = 
    [[75],
    [95,64],
    [17,47,82],
    [18,35,87,10],
    [20,04,82,47,65],
    [19,01,23,75,03,34],
    [88,02,77,73,07,63,67],
    [99,65,04,28,06,16,70,92],
    [41,41,26,56,83,40,80,70,33],
    [41,48,72,33,47,32,37,16,94,29],
    [53,71,44,65,25,43,91,52,97,51,14],
    [70,11,33,28,77,73,17,78,39,68,17,57],
    [91,71,52,38,17,14,91,43,58,50,27,29,48],
    [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
    [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]