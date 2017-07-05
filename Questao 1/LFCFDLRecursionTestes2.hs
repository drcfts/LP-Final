module LFCFDTestes2 where 

import LFCFDLRecursion2

import Test.HUnit

v5 = Valor 5

let0 = LetR "x" (Valor 5) (Valor 2)

let1 = LetR "x" (Valor 5) (Lambda "y" (Soma (Ref "x") (Ref "y")))

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
let8 = LetR "x" (Subtracao (Valor 4)(Valor 1)) (LetR "y" (Multiplicacao(Ref "x")(Ref "x")) (Ref "y"))

teste1 = TestCase (assertEqual "avaliar 5" (VInt 5) (avaliar v5 []))

teste2 = TestCase (assertEqual "avaliar Let x = 5 in (\\y -> x + y) 3" (VInt 8) (avaliar aplicacao []))

-------
-- Testes de Recursao
-------

-- Funcao fatorial
letR1 = LetR "fac" (Lambda "x" (If0 (Ref "x") (Valor 1) (Multiplicacao (Ref "x") (Aplicacao (Ref "fac") (Subtracao (Ref "x") (Valor 1)))))) (Aplicacao (Ref "fac") (Valor 5)) 
letR2 = LetR "fac" (Valor 7) (Lambda "x" (If0 (Ref "x") (Valor 1) (Multiplicacao (Ref "x") (LetR "fac" (Subtracao (Ref "x") (Valor 1))(Ref "fac") )))) 

-- Funcao de soma recursiva (if x == 0 -> 0
--                           else x + soma(x -1)
letR3 = LetR "soma" (Valor 10)(Lambda "x" (If0 (Ref "x") (Valor 0) (Soma (Ref "x") (LetR "soma" (Subtracao (Ref "x") (Valor 1))(Ref "soma") )))) 
letR4 = LetR "soma" (Valor 0) (Lambda "x" (If0 (Ref "x") (Valor 0) (Soma (Ref "x") (LetR "soma" (Subtracao (Ref "x") (Valor 1))(Ref "soma") )))) 
letR5 = LetR "soma" (Valor 1) (Lambda "x" (If0 (Ref "x") (Valor 0) (Soma (Ref "x") (LetR "soma" (Subtracao (Ref "x") (Valor 1))(Ref "soma") )))) 


--------------------------------------------
-- Questoes propostas no capitulo 8 do livro (Lazy)
--------------------------------------------
teste3 = TestCase (assertEqual "avaliar Let x = 3 in x+1" (VInt 4) (avaliar let2 []))

teste4 = TestCase (assertEqual "avaliar Let x = 4+5 in Let y = x+x in Let z = y in Let x = 4 in z" (VInt 18) (avaliar let3 []))

teste5 = TestCase (assertEqual "avaliar Let x= 3 in x" (VInt 3) (avaliar let4 []))

teste6 = TestCase (assertEqual "avaliar Let x=4 in x+x" (VInt 8) (avaliar let5 []))

teste7 = TestCase (assertEqual "avaliar Let x=3+4 in x+x" (VInt 14) (avaliar let6 []) )

teste8 = TestCase (assertEqual "avaliar Let x=2 in Let y = 4 + x in x" (VInt 2) (avaliar let7 []) )

teste9 = TestCase (assertEqual "avaliar Let x=4-1 in Let y=x*x in y" (VInt 9) (avaliar let8 []) )

--------------------------------------------
-- Questoes propostas no capitulo 10 do livro (Recursao)
--------------------------------------------
teste15 = TestCase (assertEqual "avaliar Let x = 5 in 2" (VInt 2) (avaliar let0 []))


teste10 = TestCase (assertEqual "teste fatorial de 5" (VInt 120) (avaliar letR1 []))
teste11 = TestCase (assertEqual "teste fatorial de 7" (VInt 5040) (avaliar letR2 []))

teste12 = TestCase (assertEqual "teste soma recursiva de 10" (VInt 55) (avaliar letR3 []))
teste13 = TestCase (assertEqual "teste soma recursiva de 0" (VInt 0) (avaliar letR4 []))
teste14 = TestCase (assertEqual "teste soma recursiva de 1" (VInt 1) (avaliar letR5 []))


todosOsTestes = TestList [ teste1
                         , teste2
                         , teste3
                         , teste4
                         , teste5
                         , teste6
                         , teste7
                         , teste8
                        -- , teste9
                         , teste10
                         , teste11
                         , teste12
                         , teste13
                         , teste14
                         , teste15
                         ]

executarTestes = runTestTT todosOsTestes
