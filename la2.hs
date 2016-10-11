module PolyLA2 where  -- defines the module name so you can import from another haskell file

type Coeff=Int
type Exp=Int
type Polynomial=[(Coeff, Exp)] -- a list of terms in a polynomial formula


-- TODO: implement addpoly to add two polynomials
addpoly::Polynomial->Polynomial->Polynomial



-- TODO: implement multpoly to multiply two polynomials
multpoly::Polynomial->Polynomial->Polynomial


