module LFCFDTestes where

import LFCFDLazy

import Test.HUnit

v5 = Valor 5

let1 = Let "x" (Valor 5) (Lambda "y" (Soma (Ref "x") (Ref "y")))

aplicacao = Let "x" (Valor 5) (Aplicacao (Lambda "y" (Soma (Ref "x") (Ref "y"))) (Valor 3))

let2 = Let "x" (Valor 3) (Soma(Ref "x")(Valor 1))

let3 = Let "x" (Soma(Valor 4)(Valor 5)) (Let "y" (Soma(Ref "x")(Ref "x")) (Let "z" (Ref "y") (Let "x" (Valor 4) (Ref "z"))))

--  Let x = 3 in x
let4 = Let "x" (Valor 3) (Ref "x")

--  Let x = 4 in x+x
let5 = Let "x" (Valor 4) (Soma (Ref "x")(Ref "x"))

--  Let x = 3+4 in x+x
let6 = Let "x" (Soma(Valor 3)(Valor 4)) ((Soma(Ref"x")(Ref"x")))

--  Let x = 2 in Let y 4 + x in x
let7 = Let "x" (Valor 2) (Let "y" (Soma(Valor 4)(Ref "x")) (Ref "x"))

--  Let x = 4-1 in Let y x*x in x*y
let8 = Let "x" (Subtracao (Valor 4)(Valor 1)) (Let "y" (Multiplicacao(Ref "x")(Ref "x")) (Ref "y"))

-------------------------------------
(avaliado1, env1) = avaliar v5 []
(avaliado2, env2) = avaliar aplicacao []
(avaliado3, env3) = avaliar let2 []
(avaliado4, env4) = avaliar let3 []
(avaliado5, env5) = avaliar let4 []
(avaliado6, env6) = avaliar let5 []
(avaliado7, env7) = avaliar let6 []
(avaliado8, env8) = avaliar let7 []
(avaliado9, env9) = avaliar let8 []

-------------------------------------
teste1 = TestCase (assertEqual "avaliar 5" (VInt 5) avaliado1)

teste2 = TestCase (assertEqual "avaliar Let x = 5 in (\\y -> x + y) 3" (VInt 8) avaliado2)


--------------------------------------------
-- Questoes propostas no capitulo 8 do livro
--------------------------------------------

teste3 = TestCase (assertEqual "avaliar Let x = 3 in x+1" (VInt 4) avaliado3)

teste4 = TestCase (assertEqual "avaliar Let x = 4+5 in Let y = x+x in Let z = y in Let x = 4 in z" (VInt 18) avaliado4)

teste5 = TestCase (assertEqual "avaliar Let x= 3 in x" (VInt 3) avaliado5)

teste6 = TestCase (assertEqual "avaliar Let x=4 in x+x" (VInt 8) avaliado6)

teste7 = TestCase (assertEqual "avaliar Let x=3+4 in x+x" (VInt 14) avaliado7 )

teste8 = TestCase (assertEqual "avaliar Let x=2 in Let y = 4 + x in x" (VInt 2) avaliado8 )

teste9 = TestCase (assertEqual "avaliar Let x=4-1 in Let y=x*x in y" (VInt 9) avaliado9 )

todosOsTestes = TestList [ teste1
                         , teste2
                         , teste3
                         , teste4
                         , teste5
                         , teste6
                         , teste7
                         , teste8
                         , teste9
                         ]

executarTestes = runTestTT todosOsTestes
