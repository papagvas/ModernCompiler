type Id = String

data Binop = Plus | Minux | Times | Div deriving Show

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
         deriving Show

data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp Binop Exp
         | EseqExp Stm Exp
         deriving Show

aux :: Exp -> Int
aux (OpExp l op r) = max (aux l) (aux r)
aux (EseqExp s e) = max (maxargs s) (aux e) 
aux _ = 0

maxargs :: Stm -> Int
maxargs (CompoundStm l r) = max (maxargs l) (maxargs r)
maxargs (AssignStm n e) = aux e
maxargs (PrintStm es) = max (length es) (maximum (map aux es)) 
