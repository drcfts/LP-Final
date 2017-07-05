module LFCFDTypes where 

type Id = String

type Gamma= [(Id, Tipo)]

data Tipo = TInt | TBool | TFuncao Tipo Tipo 
 deriving(Show, Eq)

data Expressao = ValorI Int
               | ValorB Bool
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao 
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao 
               | Let Id Expressao Expressao       
               | Ref Id
               | Lambda (Id, Tipo) Tipo Expressao
               | Aplicacao Expressao Expressao
               | If Expressao Expressao Expressao
 deriving(Show, Eq)


verificarTipos :: Expressao -> Gamma -> Maybe Tipo
verificarTipos (ValorI n) _   = return TInt
verificarTipos (ValorB b) _   = return TBool
verificarTipos (Lambda (v, t1) t2 exp) g = return (TFuncao t1 t2)
verificarTipos (Soma l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing
verificarTipos (Subtracao l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing
verificarTipos (Divisao l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing  
verificarTipos (Multiplicacao l r) gamma  =
  verificarTipos l gamma >>= \lt ->
  verificarTipos r gamma >>= \rt ->
  if lt == TInt && rt == TInt then return TInt else Nothing
verificarTipos (Let v e c) gamma =
  verificarTipos e gamma >>= \t -> 
  verificarTipos c ((v, t):gamma) 
verificarTipos (Ref id) gamma = pesquisarTipos id gamma
verificarTipos (If c t e) gamma  =
  verificarTipos c gamma >>= \c ->
  verificarTipos t gamma >>= \t ->
  verificarTipos e gamma >>= \e ->
  if c == TBool && t == e then return t else Nothing
verificarTipos (Aplicacao lambda valor) gamma  =
  let
  tipoLambda = verificarTipos lambda gamma
  tipoValor = verificarTipos valor gamma
  in case tipoLambda of
    Just (TFuncao t1 t2) -> if tipoValor == Just t1 then Just t2 else Nothing
    otherwise -> Nothing



pesquisarTipos :: Id -> Gamma -> Maybe Tipo
pesquisarTipos id [] = Nothing
pesquisarTipos id ((i,tipo):xs)
 | id == i = Just tipo
 | otherwise = pesquisarTipos id xs
  
