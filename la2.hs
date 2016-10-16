module PolyLA2 where  -- defines the module name so you can import from another haskell file

type Coeff=Int
type Exp=Int
type Polynomial=[(Coeff, Exp)] -- a list of terms in a polynomial formula


-- call looks like this: addpoly [(3, 2), (4, 1)] [(2, 3), (6, 2)] for (3x^2 + 4x) + (2x^3 + 6x^2)
addpoly::Polynomial->Polynomial->Polynomial
-- return if either poly is empty
addpoly xs [] = xs
addpoly [] ys = ys
addpoly ((i, j):xs) ((k, l):ys)
-- if i is 0 advance past it, dont add anything to the answer
    | i == 0 = addpoly xs ((k, l):ys)
-- same thing for k
    | k == 0 = addpoly ((i, j):xs) ys
-- if the two exponents are equal, add the coef,
-- advance both and prepend the addition to the answer
    | j == l = ((i + k, j):(addpoly xs ys))
-- if the expression in xs is higher: dont advance ys, prepend xs to the answer 
-- since there is nothing to add to it
    | j > l  = (i, j):(addpoly xs ((k, l):ys))
-- vice versa for ys
    | l > j  = (k, l):(addpoly ((i, j):xs) ys)

addpolys::[Polynomial]->Polynomial
addpolys (p:ps) 
	| length ps == 1 = addpoly p (head ps)
	| length ps >  1 = addpolys ((addpoly p (head ps)):(tail ps))


-- TODO: implement multpoly to multiply two polynomials
multpoly::Polynomial->Polynomial->Polynomial
multpoly xs [] = []
multpoly [] ys = []
multpoly (x:xs) ys = addpoly (multterm x ys) (multpoly xs ys)


multterm::(Coeff, Exp)->Polynomial->Polynomial
multterm x ys
	| length ys == 1 = [b]
	| otherwise = b:multterm x (tail ys)
	where b = (fst x * (fst (head ys)), snd x + (snd (head ys)))



