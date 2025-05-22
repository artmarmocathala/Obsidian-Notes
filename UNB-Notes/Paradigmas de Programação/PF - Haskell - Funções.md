## Aplicação de Funções

-   Haskell não utiliza parêntesis para delimitar os parâmetros de uma função em uma chamada (a menos que sejam necessários para agrupar expressões ou resolver ambiguidades).
-   A sintaxe para a chamada de funções é simplesmente o nome da função seguido por seus argumentos, separados por espaços:
    `nome_da_funcao param1 param2 ... paramN`
-   **Exemplo:** Comparar dois inteiros:
    ```haskell
    ghci> compare 5 3
    GT  -- Significa "Greater Than" (Maior Que)
    ```
-   Os parêntesis são utilizados para:
    -   Resolver ambiguidades de precedência de operadores.
    -   Clarificar o significado de expressões complexas.
    -   Agrupar um argumento que é ele mesmo uma expressão complexa.
    **Exemplo:** Comparar as raízes quadradas de dois inteiros:
    ```haskell
    ghci> compare (sqrt 5) (sqrt 3)
    GT
    -- Sem parêntesis, `compare sqrt 5 sqrt 3` seria interpretado como
    -- `(((compare sqrt) 5) sqrt) 3`, o que não faz sentido.
    ```

## Exemplos de Funções que Agem em Listas (da Biblioteca Padrão)

Muitas funções úteis para listas estão no módulo `Prelude` (importado por padrão) ou `Data.List`.

1.  **`head`**: Retorna o primeiro elemento de uma lista. Lança um erro se a lista estiver vazia.
    ```haskell
    ghci> head [2..5]
    2
    ghci> :type head
    head :: [a] -> a
    ```
2.  **`tail`**: Retorna todos os elementos da lista, exceto o primeiro. Lança um erro se a lista estiver vazia.
    ```haskell
    ghci> tail [2..5]
    [3,4,5]
    ghci> :type tail
    tail :: [a] -> [a]
    ```
3.  **`last`**: Retorna o último elemento da lista. Lança um erro se a lista estiver vazia.
    ```haskell
    ghci> last "ABC"
    'C'
    ghci> :type last
    last :: [a] -> a
    ```
4.  **`init`**: Retorna todos os elementos da lista, exceto o último. Lança um erro se a lista estiver vazia.
    ```haskell
    ghci> init "ABCDE"
    "ABCD"
    ghci> :type init
    init :: [a] -> [a]
    ```
5.  **`take`**: `take n xs` retorna os primeiros `n` elementos da lista `xs`. Se `n` for maior que o tamanho da lista, retorna a lista inteira. Se `n <= 0`, retorna `[]`.
    ```haskell
    ghci> take 5 "Hello World!"
    "Hello"
    ghci> :type take
    take :: Int -> [a] -> [a]
    ```
6.  **`drop`**: `drop n xs` retorna a lista `xs` sem seus primeiros `n` elementos. Se `n` for maior que o tamanho da lista, retorna `[]`. Se `n <= 0`, retorna `xs`.
    ```haskell
    ghci> drop 6 "Hello World!"
    "World!"
    ghci> :type drop
    drop :: Int -> [a] -> [a]
    ```
7.  **`length`**: Retorna o número de elementos da lista.
    ```haskell
    ghci> length [53..278]
    226
    ghci> :type length
    length :: [a] -> Int  -- Na verdade é Foldable t => t a -> Int
    ```
8.  **`null`**: Retorna `True` se a lista está vazia; e `False`, caso contrário.
    ```haskell
    ghci> null []
    True
    ghci> null [1,2]
    False
    ghci> :type null
    null :: [a] -> Bool -- Na verdade é Foldable t => t a -> Bool
    ```
9.  **`lines`**: Recebe uma string e retorna uma lista de strings, onde cada string corresponde a uma linha da string original (quebrada em `\n`).
    ```haskell
    ghci> :type lines
    lines :: String -> [String]
    ghci> lines "Hello\nWorld"
    ["Hello","World"]
    ```
10. **`unlines`**: É sua inversa: recebe uma lista de strings e une todas elas em uma única string, adicionando o terminador de linha `\n` entre elas (e no final).
    ```haskell
    ghci> :type unlines
    unlines :: [String] -> String
    ghci> unlines ["a", "b", "c"]
    "a\nb\nc\n"
    ```
11. **`concat`**: Generaliza `++`. Recebe uma lista de listas e as concatena em uma única lista.
    ```haskell
    ghci> :type concat
    concat :: [[a]] -> [a] -- Na verdade é Foldable t => t [a] -> [a]
    ghci> concat ["um", "dois", "tres"]
    "umdoistres"
    ghci> concat [[1,2],[3],[4,5,6]]
    [1,2,3,4,5,6]
    ```
12. **`reverse`**: Recebe uma lista `xs` e retorna uma nova lista, com todos os elementos de `xs` em ordem inversa.
    ```haskell
    ghci> reverse [1..5]
    [5,4,3,2,1]
    ```
13. **`and`**, **`or`**: Aplicam as operações lógicas binárias `&&` e `||` respectivamente, a todos os elementos de uma lista de booleanos, até que reste apenas um elemento.
    ```haskell
    ghci> and [True, False, True]
    False
    ghci> or [True, False, True]
    True
    ```
14. **`splitAt`**: `splitAt i xs` recebe um inteiro `i` e uma lista `xs`, e retorna um par de listas `(ys, zs)` onde `ys` são os primeiros `i` elementos de `xs` (equivalente a `take i xs`) e `zs` é o restante (equivalente a `drop i xs`).
    ```haskell
    ghci> splitAt 3 [1..5]
    ([1,2,3],[4,5])
    ghci> :type splitAt
    splitAt :: Int -> [a] -> ([a], [a])
    ```
15. **`zip`**: Recebe duas listas `xs` e `ys` e gera uma lista de pares `zs`, cujo tamanho é o mesmo da menor dentre as duas. Os elementos $(x_i, y_i)$ são oriundos destas listas, nesta ordem.
    ```haskell
    ghci> :type zip
    zip :: [a] -> [b] -> [(a, b)]
    ghci> zip [1..] "Teste"
    [(1,'T'),(2,'e'),(3,'s'),(4,'t'),(5,'e')]
    ```
    Existem também `zip3`, `zip4`, ..., `zip7` para zunir múltiplas listas.
16. **`words`**: Quebra uma string em uma lista de palavras, delimitadas por qualquer caractere que corresponda a espaços em branco (conforme `Data.Char.isSpace`).
    ```haskell
    ghci> :type words
    words :: String -> [String]
    ghci> words "A B\tC\nD\rE"
    ["A","B","C","D","E"]
    ```

## Funções Infixadas

-   Haskell utiliza, por padrão, a notação prefixada para funções.
    `z = f x y` (o nome da função `f` precede os argumentos `x` e `y`).
-   Se uma função recebe dois ou mais argumentos, é possível que a notação infixada traga uma melhor compreensão e leitura.
-   Para utilizar a notação infixada, basta colocar o nome da função entre **crases** (``` ` ```), tanto em uma definição quanto em uma chamada.
-   Ambas as formas (prefixada e infixada com crases) são intercambiáveis para funções binárias.

**Exemplo:**
```haskell
import Data.Bits ((.|.)) -- Operador OU bit-a-bit

bitwise_or :: Int -> Int -> Int
bitwise_or a b = a .|. b -- Definição usando operador infixo .|.

main = do
  let x = bitwise_or 1 2 -- Chamada prefixada
  let y = 3 `bitwise_or` 5 -- Chamada infixada
  print (x, y) -- Saída: (3,7)
```

**Exemplos de funções frequentemente usadas como infixas:**

1.  **`elem`**: `x \`elem\` xs` retorna `True` se o elemento `x` pertence à lista `xs`.
    ```haskell
    ghci> :type elem
    elem :: (Eq a, Foldable t) => a -> t a -> Bool
    ghci> 'x' `elem` "Teste"
    False
    ghci> 3 `elem` [1,2,3,4]
    True
    ```
2.  **`notElem`**: Negação de `elem`. `x \`notElem\` xs` retorna `True` se `x` não pertence a `xs`.
    ```haskell
    ghci> 'x' `notElem` "Teste"
    True
    ```
3.  **`isPrefixOf`** (do módulo `Data.List`): `prefix \`isPrefixOf\` list` retorna `True` se `prefix` é um prefixo de `list`.
    ```haskell
    ghci> :m +Data.List
    ghci> "He" `isPrefixOf` "Hello"
    True
    ```
4.  **`isInfixOf`**, **`isSuffixOf`** (do módulo `Data.List`): Comportamento semelhante para verificar se uma lista é sublista ou sufixo de outra, respectivamente.

## Definindo Novas Funções

-   A sintaxe para a definição de uma nova função é:
    `nome_da_funcao param1 param2 ... paramN = expressao`
-   O nome da função deve iniciar em minúsculas.
-   Os parâmetros devem ser separados por um espaço em branco.
-   O valor da `expressao` será o retorno da função.
-   A **declaração do tipo** da função (assinatura de tipo) é opcional, mas altamente recomendada para clareza e para ajudar o compilador (e o programador).
    ```haskell
    -- Declaração de tipo (opcional, mas boa prática)
    areaCircle :: Double -> Double
    -- Definição da função
    areaCircle r = pi * r ^ 2
    ```
-   A declaração de tipo é útil para definir o tipo da função em caso de ambiguidade (ex: com classes de tipos numéricos) e para documentar a intenção.
-   Lembre-se que, devido ao [[PF - Currying|currying]], uma função de múltiplos parâmetros é na verdade uma sequência de funções de um parâmetro.
    ```haskell
    -- Os parêntesis na declaração de tipo são opcionais para o último retorno
    -- areaTriangle :: Double -> (Double -> Double) é o mesmo que:
    areaTriangle :: Double -> Double -> Double
    areaTriangle b h = (b + h) / 2.0 -- Exemplo simples, não é a área real do triângulo
    ```

## O Símbolo `$` (Operador de Aplicação de Função)

-   O símbolo `$` é um operador infixo em Haskell com um comportamento que pode não ser óbvio à primeira vista.
-   **Tipo do operador `$ `:**
    ```haskell
    ghci> :info ($)
    ($) :: (a -> b) -> a -> b
    -- Defined in ‘GHC.Base’
    infixr 0 $ -- Associatividade à direita, precedência 0 (a mais baixa)
    ```
-   Ele recebe uma função `f :: a -> b` à sua esquerda e um parâmetro `a` à sua direita, e retorna `b = f(a)`.
-   Este é o mesmo comportamento da aplicação de função normal (espaço em branco):
    `f $ x` é semanticamente equivalente a `f x`.

-   **Importância:** A importância deste operador vem de sua **baixa precedência (0) e associatividade à direita (`infixr 0`)**.
    -   A aplicação de função normal (espaço) tem a maior precedência e é associativa à esquerda.
    -   O `$` serve para mudar a associatividade e precedência da aplicação de funções, permitindo **evitar o uso excessivo de parêntesis**.

-   **Exemplo:**
    A expressão `sqrt head [2..5]` é inválida, pois a aplicação normal (espaço) é associativa à esquerda e de alta precedência, fazendo com que seja interpretada como `(sqrt head) [2..5]`, o que é um erro de tipo (`sqrt` espera um número, não uma função `head`).

    Para calcular a raiz quadrada da cabeça da lista, precisaríamos de parêntesis:
    `sqrt (head [2..5])`

    Já com o uso do operador dólar, temos:
    ```haskell
    ghci> sqrt $ head [2..5]
    1.4142135623730951 -- sqrt (head [2..5]) == sqrt 2
    ```
    Como `$` tem a menor precedência, a expressão `head [2..5]` é avaliada primeiro, e seu resultado (2) é então passado para `sqrt`.
    `f $ g $ h x` é `f (g (h x))`.
    `f (g (h x))` é equivalente a `f $ g $ h x`.
    `putStrLn $ "Nome: " ++ studentName s` é `putStrLn ("Nome: " ++ studentName s)`.