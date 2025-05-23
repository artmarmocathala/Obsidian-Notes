Haskell possui um conjunto de tipos de dados e operadores básicos que formam a fundação para construções mais complexas.

## Operadores Aritméticos

-   Em Haskell, expressões utilizando os operadores aritméticos binários podem ser escritas tanto em forma **prefixada** (colocando o operador entre parêntesis) quanto na forma **infixada** (padrão).
    ```haskell
    ghci> (+) 6 3   -- Forma prefixada da expressão 6 + 3
    9
    ghci> 6 + 3     -- Forma infixada
    9
    ```
-   **Operadores disponíveis:**
    -   Adição: `+`
    -   Subtração: `-`
    -   Multiplicação: `*`
    -   Divisão: `/` (representa a divisão em ponto flutuante, não inteira)
        ```haskell
        ghci> (-) 6 3
        3
        ghci> (*) 6 3
        18
        ghci> (/) 6 3
        2.0
        ```
    -   Exponenciação:
        -   `^`: Para expoentes inteiros não negativos. Retorna um número do mesmo tipo da base.
        -   `**`: Para expoentes fracionários ou bases fracionárias. Retorna um número de ponto flutuante.
        ```haskell
        ghci> 2 ^ 70
        1180591620717411303424
        ghci> 2.0 ** 70
        1.1805916207174113e21
        ```
-   **Aritmética de Precisão Arbitrária:** Os resultados para inteiros (`Integer`) não geram *overflow*, pois a aritmética estendida é implementada nativamente.
-   **Números Negativos:** Devem vir entre parêntesis em algumas expressões para evitar ambiguidades com o operador de subtração.
    ```haskell
    ghci> 6 - (-3) -- 6 + 3 = 9
    9
    ```

## Operadores Lógicos e Relacionais

-   **Valores Booleanos:** `True` e `False` (note a primeira letra maiúscula).
-   **Operadores Lógicos:**
    -   E: `&&`
    -   Ou: `||`
    -   Não: `not` (função prefixada)
    ```haskell
    ghci> True && False
    False
    ghci> True || False
    True
    ghci> not True
    False
    ```
-   **Operadores Relacionais:**
    -   Igual: `==`
    -   Diferente: `/=`
    -   Menor: `<`
    -   Menor ou igual: `<=`
    -   Maior: `>`
    -   Maior ou igual: `>=`
    ```haskell
    ghci> 5 == 5
    True
    ghci> 5 /= 3
    True
    ghci> "hello" < "world"
    True
    ```
-   **Precedência de Operadores:**
    -   No GHCi, a precedência e associatividade dos operadores podem ser consultadas com o comando `:info` (ou `:i`).
    -   O valor de precedência varia de 0 (menor) a 9 (maior).
    -   `infixl` indica associatividade à esquerda, `infixr` à direita, `infix` não associativo.
    ```haskell
    ghci> :info (+)
    class Num a where
      (+) :: a -> a -> a
      ...
    	-- Defined in ‘GHC.Num’
    infixl 6 + -- Associatividade à esquerda, precedência 6

    ghci> :info (*)
    class Num a where
      (*) :: a -> a -> a
      ...
    	-- Defined in ‘GHC.Num’
    infixl 7 * -- Associatividade à esquerda, precedência 7

    ghci> :info (^)
    (^) :: (Num a, Integral b) => a -> b -> a
    	-- Defined in ‘GHC.Real’
    infixr 8 ^ -- Associatividade à direita, precedência 8
    ```

## Listas

-   As listas são um dos tipos de estrutura de dados mais fundamentais em Haskell. São coleções homogêneas (todos os elementos devem ser do mesmo tipo).
-   São delimitadas por colchetes `[]` e seus elementos são separados por vírgulas.
    ```haskell
    ghci> [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    ```
-   A **lista vazia** é representada por `[]`.
-   **Enumeração (Progressões Aritméticas):** Haskell oferece uma notação concisa para criar listas com elementos em sequência.
    ```haskell
    ghci> [1..5]        -- [1, 2, 3, 4, 5]
    ghci> [2, 5..18]    -- [2, 5, 8, 11, 14, 17] (deduz o passo a partir dos dois primeiros)
    ghci> [1.0, 1.5 .. 3.0] -- [1.0,1.5,2.0,2.5,3.0]
    ghci> ['a'..'e']    -- ['a','b','c','d','e']
    ```
    Podem ser usadas para criar listas infinitas: `[1..]` representa todos os inteiros a partir de 1.
-   **Concatenação:** O operador `++` concatena duas listas.
    ```haskell
    ghci> [5..6] ++ [1..4]
    [5,6,1,2,3,4]
    ```
-   **Operador `cons` (`:`):** Adiciona um elemento ao *início* de uma lista. É o construtor fundamental de listas.
    `x : xs` cria uma nova lista com `x` como cabeça e `xs` como cauda.
    ```haskell
    ghci> 1 : [2,3,4]
    [1,2,3,4]
    ghci> 'a' : "bc" -- Equivalente a 'a' : ['b', 'c']
    "abc"
    ```
    Toda lista não vazia pode ser vista como `cabeça : cauda`. Ex: `[1,2,3]` é `1 : (2 : (3 : []))`.

## Caracteres e Strings

-   Um **caractere** (`Char`) é delimitado por aspas simples.
    ```haskell
    ghci> 'a'
    'a'
    ghci> :type 'a'
    'a' :: Char
    ```
-   Uma **string** (`String`) é, na verdade, uma lista de caracteres (`[Char]`). É delimitada por aspas duplas.
    ```haskell
    ghci> "Exemplo de string"
    "Exemplo de string"
    ghci> :type "Exemplo de string"
    "Exemplo de string" :: String
    -- String é um sinônimo de tipo para [Char]
    -- type String = [Char]
    ```
-   **Equivalência:**
    ```haskell
    ghci> "ABC" == ['A', 'B', 'C']
    True
    ghci> "" == []
    True
    ```
-   **Tipos Distintos:** Note que `"A"` (uma string/lista de um caractere) e `'A'` (um único caractere) têm tipos distintos.
-   **Enumeração de Strings:** Como strings são listas, a notação de enumeração pode ser utilizada:
    ```haskell
    ghci> ['a'..'z']
    "abcdefghijklmnopqrstuvwxyz"
    ghci> ['a'..'z'] == "abcdefghijklmnopqrstuvwxyz"
    True
    ```