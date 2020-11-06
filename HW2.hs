module HW2 (
    parse, -- reexport for easy terminal use
    foldAndPropagateConstants,
    assignCommonSubexprs,
    reducePoly
) where

import Expression
import Parser
import Data.List
-- Do not change the module definition and imports above! Feel free
-- to modify the parser, but please remember that it is just a helper
-- module and is not related to your grade. You should only submit
-- this file. Feel free to import other modules, especially Data.List!

findVariable :: [(String, ExprV)]->String ->[(String, ExprV)]
findVariable  [] _  = []
findVariable ll var = if (fst (head(reverse(ll))))==var then
                            ll
                        else
                             findVariable (reverse(tail(reverse ll))) var

isLeaf :: ExprV ->Bool
isLeaf (Leaf (Constant x)) = True
isLeaf _ = False

isin :: [(String, ExprV)] -> String -> (Bool,ExprV)
isin [] str1 = (False,Leaf(Constant 1))
isin (aa:ss) (str1) = if (fst(aa)==str1)
                        then (True,snd(aa))
                        else
                            isin ss str1 

calculate ::  [(String, ExprV)] -> ExprV  -> ExprV
calculate ll (Leaf(Constant a)) = (Leaf(Constant a))    
calculate ll (Leaf(Variable a)) = if (fst(isin ll a)) then 
                                    if isLeaf(calculate ll (snd(isin ll a)))then
                                        (calculate ll (snd(isin ll a)))
                                    else
                                        (Leaf(Variable a))
                                else
                                    (Leaf(Variable a))

calculate ll ( BinaryOperation op (Leaf (Constant a))(Leaf (Constant b))) = if op==Plus then
                                                                                        (Leaf (Constant (a+b)))
                                                                                    else
                                                                                        (Leaf (Constant (a*b)))
                                                                                        
calculate ll (UnaryOperation op (Leaf (Constant a))) = (Leaf (Constant (-a)))
calculate ll (UnaryOperation op (Leaf (Variable a))) = if (fst(isin ll a)) then 
                                                        calculate ll (UnaryOperation op (calculate ll (snd(isin ll a))))
                                                      else
                                                        (UnaryOperation op (Leaf (Variable a)))

calculate ll (UnaryOperation op (ex))= if (calculate ll (ex)==ex) then
                                        UnaryOperation op (calculate ll (ex))
                                    else
                                        calculate ll (UnaryOperation op (calculate ll (ex)))

calculate ll (BinaryOperation op (ex) (ex1)) = if ((calculate ll (ex) ==ex) && (calculate ll (ex1)  ==ex1)) then
                                                (BinaryOperation op(calculate ll (ex ))(calculate ll (ex1 )))
                                           else
                                                calculate ll (BinaryOperation op(calculate ll (ex))(calculate ll (ex1)))

calculateV1 :: [(String, ExprV)] -> [(String, ExprV)]->[(String, ExprV)]
calculateV1 [] _ = []
calculateV1 _ [] = []
calculateV1 ll ll1 = ((fst(head(ll1))),(calculate (findVariable ll (fst(head(ll1)))) (snd(head(ll1))))):(calculateV1 ll (tail(ll1)))


foldAndPropagateConstants :: [(String, ExprV)] -> [(String, ExprV)]
foldAndPropagateConstants [] = []
foldAndPropagateConstants ll = calculateV1 ll ll 

duplicated :: [ExprV] -> [ExprV]
duplicated [] = []

duplicated (aa:[]) = []

duplicated (aa:xs) = if (elem aa xs) then
                            aa: duplicated xs 
                        else
                            duplicated xs 

everycal :: ExprV -> [ExprV] ->[ExprV]
everycal (Leaf a ) ll = [] 
everycal (BinaryOperation op (Leaf a)(Leaf b )) ll = (BinaryOperation op (Leaf a)(Leaf b )):ll
everycal (UnaryOperation op (Leaf w)) ll = (UnaryOperation op (Leaf w)):ll
everycal (BinaryOperation a (ex1)(ex2)) ll = ((everycal ex1 ll)++(everycal ex2 ll))
everycal (UnaryOperation a (ex1)) ll = everycal ex1 ll

repating :: ExprV -> [ExprV]
repating aa = nub(duplicated (everycal aa []))

-- repeating gives the changeit's ll 
numgen :: Int -> String 
numgen nump = "$"++(show(nump))

ekle :: [ExprV] -> [(String, ExprV)] -> Int ->[(String, ExprV)] 
ekle [] ll nump = []
ekle (x:xs) ll nump = (ll++[(numgen nump,x)]) ++ (ekle xs) ([]) (nump+1)

findandpaste :: ExprV ->[(String, ExprV)] -> String 
findandpaste _ [] = "**"
findandpaste aa (x:xs) = if (snd x)==aa then
                            (fst x)
                        else
                            findandpaste aa xs

changeit :: ExprV -> [ExprV] ->[(String, ExprV)]->ExprV
changeit (Leaf a ) ll finll = (Leaf a)
changeit (BinaryOperation op (Leaf a)(Leaf b )) ll finll= if (elem (BinaryOperation op (Leaf a)(Leaf b )) ll ) then
                                                            Leaf (Variable (findandpaste (BinaryOperation op (Leaf a)(Leaf b )) finll ))

                                                    else
                                                        (BinaryOperation op (Leaf a)(Leaf b ))
changeit (UnaryOperation op (Leaf w)) ll finll= if (elem (UnaryOperation op (Leaf w)) ll) then

                                                    Leaf (Variable (findandpaste (UnaryOperation op (Leaf w)) finll ))

                                            else
                                                (UnaryOperation op (Leaf w))
changeit (BinaryOperation a (ex1)(ex2)) ll finll= (BinaryOperation a (changeit ex1 ll finll)(changeit ex2 ll finll))
changeit (UnaryOperation a (ex1)) ll finll= (UnaryOperation a (changeit ex1 ll finll))

asign :: ExprV -> [(String, ExprV)] ->([(String, ExprV)], ExprV)
asign ex finll = if (repating ex)==[] then 
                    (finll,ex)
              else
                  asign (changeit (ex) (repating ex) (ekle (repating ex) (finll) (length finll) )) (ekle (repating ex) (finll)(length finll))


assignCommonSubexprs :: ExprV -> ([(String, ExprV)], ExprV)
assignCommonSubexprs aa = asign aa []  

timesV1 :: Int-> [Int]->[Int]->[Int]->[Int]
timesV1 a [] ll2 analiste = analiste
timesV1 a (x:xs) ll2 analiste = timesV1 (a+1) xs ll2 (times x a 0 ll2 analiste)

times :: Int -> Int ->Int ->[Int]->[Int]->[Int]
times a b c [] ll2 = ll2
times sayi uzeri1 uzeri2 (x:xs) analiste = if((x*sayi)==0)then
                                                times sayi uzeri1 (uzeri2+1) xs analiste
                                            else
                                                times sayi uzeri1 (uzeri2+1) xs ((fst(splitAt (uzeri1+uzeri2) analiste))++ [(head(snd (splitAt (uzeri1+uzeri2) analiste))+(x*sayi))] ++(tail(snd (splitAt (uzeri1+uzeri2) analiste))))

changePoly :: ExprV -> [Int] ->[Int] -> [Int]
changePoly (( Leaf (Variable b) ) ) ll finll= (fst(splitAt 1 ll))++ [(head(snd (splitAt 1 ll))+1)] ++(tail(snd (splitAt 1 ll)))
changePoly (( Leaf (Constant b) ) ) ll finll= (fst(splitAt 0 ll))++ [(head(snd (splitAt 0 ll))+b)] ++(tail(snd (splitAt 0 ll)))

changePoly (UnaryOperation op (Leaf a )) ll finll = map negate (changePoly (Leaf a)ll finll )

changePoly ( BinaryOperation op ( Leaf a ) ( Leaf b ) ) ll  finll= if (op == Plus) then
                                                                    (zipWith (+) (changePoly ( Leaf a ) ll finll) (changePoly (Leaf b) ll finll) )
                                                                else
                                                                    (timesV1 0 (changePoly (Leaf(a)) (ll) finll) (changePoly (Leaf b) (ll) finll) (finll) )

changePoly (BinaryOperation op (ex1) (ex2) ) ll finll = if (op == Plus) then
                                                            (zipWith (+) (changePoly (ex1) ll finll) (changePoly (ex2) ll finll) )
                                                        else
                                                            (timesV1 0 (changePoly (ex1) (ll) finll) (changePoly (ex2) (ll) finll) (finll)) 
changePoly (UnaryOperation op (ex1)) ll finll = (map negate (changePoly (ex1) (ll) (finll)))

minpoly :: [Int]->[Int]
minpoly [] =[]
--ll'i reverse gönder
minpoly ll = if (head ll)==0 then
                minpoly (tail ll)
            else
                (reverse ll)

findString:: ExprV -> String 
findString (Leaf(Variable a))= a
findString (BinaryOperation op ex1 ex2)= if findString ex1 /= "My code Fails :( " then
                                            findString ex1
                                        else if findString ex2 /= "My code Fails :( " then
                                            findString ex2
                                        else
                                            "My code Fails :( "
findString (UnaryOperation op ex1)= findString ex1
findString (Leaf (Constant b)) = "My code Fails :( "

kackere :: Int-> Int-> Int ->ExprV -> String-> ExprV
kackere sayi ilkmi howmany  ex1 isim = if ilkmi==0 then
                                            if howmany==0 then
                                                (Leaf(Constant sayi))
                                            else
                                                if sayi==1 then
                                                    kackere sayi (ilkmi+1) (howmany) (Leaf (Variable isim)) isim
                                                else if sayi ==(-1) then
                                                    kackere sayi (ilkmi+1) (howmany) (UnaryOperation Minus (ex1)) isim
                                                else
                                                    kackere sayi (ilkmi+1) (howmany) (BinaryOperation Times (Leaf(Constant sayi)) (ex1)) isim 
    
                                        else
                                            if howmany>1 then 
                                                kackere sayi (ilkmi) (howmany-1) (BinaryOperation Times (ex1) (Leaf (Variable isim))) isim
                                            else 
                                            ex1


aa:: Int ->ExprV->Int-> Int->[Int] -> ExprV -> ExprV
aa enilk anaexp sayi len (x:[]) exp = if enilk ==0 then
                                            (Leaf (Constant x))
                                        else
                                            if (exp==(Leaf (Constant 348))) then
                                                (kackere x 0 (len-sayi) (Leaf(Variable (findString anaexp))) (findString anaexp))
                                            else
                                                (BinaryOperation Plus (exp) (kackere x 0 (len-sayi) (Leaf(Variable (findString anaexp))) (findString anaexp)))

aa enilk anaexp sayi len (x:xs) exp = if sayi>0 then
                                        if enilk==0 then
                                            if sayi==1 then
                                                (Leaf (Constant x))
                                            else
                                                if x==0 then
                                                    aa (enilk+1) anaexp (sayi-1) (len) (xs) (Leaf (Constant 348))
                                                else
                                                    aa (enilk+1) anaexp (sayi-1) (len) (xs) (Leaf (Constant x))
                                        else
                                            if x==0 then
                                                aa enilk anaexp (sayi-1) (len) (xs) (exp)
                                            else
                                                if (exp==(Leaf (Constant 348))) then
                                                    aa enilk anaexp (sayi-1) (len) (xs) (kackere x 0 (len-sayi) (Leaf(Variable (findString anaexp))) (findString anaexp))
                                                else
                                                    aa enilk anaexp (sayi-1) (len) (xs) (BinaryOperation Plus (exp) (kackere x 0 (len-sayi) (Leaf(Variable (findString anaexp))) (findString anaexp)))
                                    else
                                        exp


reducePoly :: ExprV -> ExprV
reducePoly exp11 = (aa 0 (exp11) (length(minpoly(reverse(changePoly exp11 (take 30 [0,0..])(take 60 [0,0..]))))) (length(minpoly(reverse(changePoly exp11 (take 30 [0,0..])(take 60 [0,0..]))))) (minpoly(reverse(changePoly exp11 (take 30 [0,0..])(take 60 [0,0..])))) (Leaf (Constant 9999) )) 

