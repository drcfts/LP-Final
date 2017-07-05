-- | A linguagem LFCFD suporta tanto 
-- expressoes identificadas (LET) quanto 
-- identificadores e funcoes de alta ordem
-- (com o mecanismo de expressoes lambda).
-- As substituicoes sao postergadas. 


-- Linguagem que implementa, alem de laziness,
-- a recursao de funcoes
module LFCFDLRecursion where 

type Id = String

-- o ambiente de substituicoes
-- postergadas (Deferred Substitutions).
-- nesse caso, o ambiente corresponde a uma
-- lista entre Identificadores e Valores, onde
-- um valor eh uma expressao com valor inteiro
-- ou uma expressao lambda.

type Env = [(Id, ValorE)]
 
-- Nessa versao da linguagem, o interpretador precisa
-- retornar valores de um tipo especial, ValorE, que
-- podem ser ou um valor inteiro (VInt Int) ou um
-- closure, que mantem um ambiente de substituicoes
-- postergadas no escopo de uma expressao lambda. Ou
-- seja, avaliar uma expressao lambda no contexto:
--
-- let x = 5 in (\y -> x + y) deve retornar o
-- closure Closure y (x + y) [(x,5)]
 
-- Adicionar um campo Box para aplicar sharing
data ValorE = VInt Int                
            | FClosure Id Expressao Env
            | EClosure Expressao Env 
 deriving(Show, Eq)
 
-- A principal diferenca do Let recursivo para o 
-- let anterior esta no escopo do ambiente de substituicoes.
-- Deve-se criar, assim, ambientes ciclicos

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao 
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao 
               | Let Id Expressao Expressao
               | Ref Id
               | Lambda Id Expressao
               | Aplicacao Expressao Expressao   
               | If0 Expressao Expressao Expressao
               | LetR Id Expressao Expressao        -- Let modificado pra recursao 
 deriving(Show, Eq)

-- | O interpretador da linguagem LFCFD
-- (funcao 'avaliar') precisa ser ajustado, uma vez que
-- o tipo de retorno nao pode ser simplesmente
-- um inteiro ou uma expressao. Com substituicoes postergadas,
-- nessa linguagem o retorno precisa ser um ValorE, conforme 
-- discutido anteriormente. 

avaliar :: Expressao -> Env -> ValorE
avaliar (Valor n)            _ = VInt n
avaliar (Soma e d)          env = avaliarExpBin e d (+) env
avaliar (Subtracao e d)     env = avaliarExpBin e d (-) env 
avaliar (Multiplicacao e d) env = avaliarExpBin e d (*) env
avaliar (Divisao e d)       env = avaliarExpBin e d div env
avaliar (Let id expN c)     env = avaliar (Aplicacao (Lambda id c) expN) env
avaliar (Ref v)             env = pesquisar v env
avaliar (Lambda a c)        env = FClosure a c env
avaliar (If0 cond t e) env
 | (avaliar cond env) == (VInt 0) = avaliar t env
 | otherwise = avaliar e env 
avaliar (Aplicacao e1 e2)   env =
  let
    v = avaliacaoStrict (avaliar e1 env)
    e = EClosure e2 env 
  in case v of
     (FClosure a c env') -> avaliar c ((a, e):env')
     otherwise -> error "Tentando aplicar uma expressao que nao eh uma funcao anonima"

-- Avaliar do let recursivo com laziness	 
avaliar (LetR id expN corpo)    env = 
  let
    e = EClosure expN env 
    env2 = (avaliarCiclico id corpo (env))
    v = avaliacaoStrict (avaliar corpo env)
  in case v of
     (FClosure a c env') -> avaliar c (((a, e):env2))
     (VInt n) -> (VInt n)
     otherwise -> error "Tentando subsituir algo desconhecido"

-- Funcoes acrescentadas para linguagem recursiva

-- Avaliacao que associa a variavel associada ao ambiente
-- de closure definida pela funcao
-- Expressao que recebe eh o lambda
avaliarCiclico :: Id -> Expressao -> Env -> Env
avaliarCiclico id exp env =
 let
   valueHolder = VInt 1          -- Valor "lixo" para inicializar o escopo da lista
   newEnv = (id, valueHolder):env
   valExpNomeada = avaliar exp env  -- Esse sera o valor valido na lista que devera ser usado
 in
   setBox id valExpNomeada newEnv
   
-- Muda a lista para ter o valor necessario
setBox :: Id -> ValorE -> Env -> Env
setBox id valor [] = [(id, valor)] ++ []
setBox id valor ((i,v): xs) 
 | id == i = ((i,valor): xs)       -- Atualiza o valor da variavel pela recursao 
 | otherwise = setBox id valor xs

 
 
-- Forca avaliacao da linguagem lazy
avaliacaoStrict :: ValorE -> ValorE     
avaliacaoStrict (EClosure e env) = avaliacaoStrict (avaliar e env)
avaliacaoStrict e = e


-- | Realiza uma pesquisa por uma determinada
-- variaval no ambiente de substituicoes postergadas. 
pesquisar :: Id -> Env -> ValorE
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
 | v == i = avaliacaoStrict e -- Forca avaliacao do ValorE para nao causar problemas no avaliarExpBin
 | otherwise = pesquisar v xs
  
-- | Avalia uma expressao binaria.
avaliarExpBin :: Expressao -> Expressao -> (Int -> Int -> Int) -> Env -> ValorE
avaliarExpBin e d op env = VInt (op ve vd)
 where
  (VInt ve) = avaliar e env 
  (VInt vd) = avaliar d env'
  env' = ((x, VInt ve):env)       -- Lista da direita possui associacao da referencia com seu valor
  Ref x = e
  
