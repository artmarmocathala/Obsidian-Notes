Em matemática, a composição de duas funções $f$ e $g$, denotada por $(f \circ g)$, é uma função $h$ tal que $h(x) = f(g(x))$. Ou seja, o resultado da aplicação de $g$ a $x$ é então passado como argumento para $f$.

Em programação funcional, é comum implementar funcionalidades através de uma cadeia de chamadas de funções, onde o resultado da aplicação da função anterior ao parâmetro da chamada é o parâmetro de entrada da próxima função na cadeia.

### Operador de Composição em Haskell (`.`)

Em Haskell, funções podem ser compostas por meio do operador ponto final (`.`).
-   Ele é um operador infixo como os demais operadores da linguagem.
-   É associativo à direita.
-   Tem nível 9 de precedência (alta precedência).

**Tipo do operador `.`:**
```haskell
ghci> :info (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
-- infixr 9 .
```
Isso significa que `(.)` recebe duas funções:
1.  Uma função de `b` para `c` (vamos chamá-la de `f`).
2.  Uma função de `a` para `b` (vamos chamá-la de `g`).
E retorna uma nova função de `a` para `c`.
Então, `(f . g) x` é equivalente a `f (g x)`.

### Exemplo de Uso

Relembrando a função `suffixes` de [[PF - Pattern Matching#Padrão como as-pattern|Padrão 'como']]:
```haskell
suffixesOriginal :: String -> [String]
suffixesOriginal [] = []
suffixesOriginal xs@(_:xs') = xs : suffixesOriginal xs'
```
A função `tails` do módulo `Data.List` tem comportamento quase idêntico, mas inclui o sufixo vazio no final:
```haskell
ghci> :module +Data.List
ghci> tails "Teste"
["Teste","este","ste","te","e",""]
```
A função `init` remove o último elemento de uma lista.
Podemos reimplementar `suffixes` utilizando `tails` e `init` através da composição:
```haskell
suffixes :: String -> [String]
suffixes xs = init (tails xs)
```
Usando o operador de composição, isso se torna mais conciso:
```haskell
suffixesNew :: String -> [String]
suffixesNew = init . tails
```
Aqui, `suffixesNew` é uma nova função criada pela composição de `init` e `tails`. Quando `suffixesNew` recebe um argumento `xs`, `xs` é primeiro passado para `tails`, e o resultado de `tails xs` é então passado para `init`.

**Outro Exemplo:**
Contabilizar o número de palavras de uma string que começam em maiúscula:
```haskell
-- Precisamos de:
-- words :: String -> [String] (quebra string em palavras)
-- head :: [a] -> a (pega o primeiro elemento)
-- isUpper :: Char -> Bool (verifica se é maiúscula, de Data.Char)
-- filter :: (a -> Bool) -> [a] -> [a] (filtra a lista)
-- length :: [a] -> Int (conta elementos)

-- Sem composição:
capCountVerbose :: String -> Int
capCountVerbose str = length (filter (\word -> isUpper (head word)) (words str))

-- Com composição:
-- :module +Data.Char
capCount :: String -> Int
capCount = length . filter (isUpper . head) . words

main = do
  print (capCount "Paradigmas de Programação Funcional") -- Saída: 3
  -- (Paradigmas, Programação, Funcional)
```
A expressão `(isUpper . head)` cria uma nova função anônima que primeiro aplica `head` a uma palavra e depois `isUpper` ao resultado. Esta nova função é então passada para `filter`.