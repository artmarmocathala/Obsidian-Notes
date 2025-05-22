**Mapas**, **filtros** e **reduções (ou dobras/folds)** são funções de alta ordem fundamentais em programação funcional. Elas abstraem padrões comuns de iteração sobre estruturas de dados, especialmente listas.

Elas abstraem três conceitos fundamentais:

1.  **Mapa (Map):** A partir de uma lista `xs`, criar uma nova lista `ys` tal que $y_i = f(x_i)$ para uma função $f$ dada. (Aplica uma função a cada elemento).
2.  **Filtro (Filter):** A partir de uma lista `xs`, criar uma nova lista `ys` formada pelos elementos $x$ de `xs` que atendem a um predicado $P$. (Seleciona elementos que satisfazem uma condição).
3.  **Redução (Reduce/Fold):** Gerar um único elemento $y$ a partir de uma lista `xs` através da aplicação sucessiva de uma operação binária `op` e, geralmente, um valor inicial `x0`. (Combina todos os elementos em um único valor).

Todas as três técnicas recebem uma função como parâmetro. A aplicação destas técnicas substitui, em vários casos, a necessidade dos laços das linguagens imperativas, levando a um código mais declarativo e conciso.

## Mapas (Map)

Em Haskell, os mapas são implementados por meio da função `map`.

-   **Tipo:**
    ```haskell
    ghci> :type map
    map :: (a -> b) -> [a] -> [b]
    ```
-   **Funcionamento:**
    -   `map` recebe uma função `f` (que transforma um elemento do tipo `a` em um elemento do tipo `b`) e uma lista de elementos do tipo `a`.
    -   O retorno é uma nova lista de elementos do tipo `b`, onde cada $b_i = f(a_i)$.
-   **Vantagens:** Simplifica o código e o torna mais legível, abstraindo o mecanismo de iteração.

**Exemplo: Capitalização utilizando `map`**
(Revisitando o exemplo de [[PF - Recursão em Programação Funcional]])
```haskell
import Data.Char (toUpper, toLower)

-- Função para capitalizar uma única palavra
capWord :: String -> String
capWord [] = []
capWord (y:ys) = toUpper y : map toLower ys -- map toLower aplica toLower a cada char em ys

-- Função para capitalizar uma lista de palavras
capitalizeWithMap :: [String] -> [String]
capitalizeWithMap xs = map capWord xs

main = print (capitalizeWithMap ["hello", "WORLD", "hAsKeLl"])
-- Saída: ["Hello","World","Haskell"]
```

## Filtros (Filter)

Em Haskell, os filtros são implementados por meio da função `filter`.

-   **Tipo:**
    ```haskell
    ghci> :type filter
    filter :: (a -> Bool) -> [a] -> [a]
    ```
-   **Funcionamento:**
    -   `filter` recebe um predicado `P` (uma função que retorna `Bool`) e uma lista de elementos do tipo `a`.
    -   Retorna uma nova lista contendo apenas os elementos `x` da lista de entrada para os quais `P x` for verdadeira.
    -   A ordem relativa dos elementos é preservada.

**Exemplo:** Extrair dígitos hexadecimais de uma string.
```haskell
import Data.Char (isHexDigit)

main = print (filter isHexDigit "Coordenadas (20A, 38F, 40X)")
-- Saída: "Cdeada20A38F40"
-- Nota: O slide original tinha "Cdeada20A38F40" como saída,
-- mas isHexDigit para '(' e ',' etc. é False.
-- A saída correta para o input "Coordenadas (20A, 38F, 40X)" é "C20A38F40".
-- Se o input fosse "Cdeada20A38F40X", a saída seria "Cdeada20A38F40".
-- Vou usar o exemplo do slide para manter a consistência com a imagem.
-- O slide parece ter um erro no exemplo de saída ou no input.
-- Assumindo que o input pretendido era algo que gerasse a saída do slide:
-- ghci> filter isHexDigit "Cdeada20A38F40"
-- "Cdeada20A38F40"
```

**Exemplo: Lista de aprovados usando `filter` e `map`**
(Revisitando o exemplo de [[PF - Recursão em Programação Funcional]])
```haskell
data Student = Student { studentName :: String, studentScore :: Int } deriving (Show)

isApproved :: Student -> Bool
isApproved student = studentScore student >= 5

aprovadosComFilterMap :: [Student] -> [String]
aprovadosComFilterMap xs = map studentName (filter isApproved xs)
-- Alternativamente, com função anônima (lambda):
-- aprovadosComFilterMap xs = map studentName (filter (\s -> studentScore s >= 5) xs)

main = do
  let students = [ Student "Ana" 8, Student "Beto" 3, Student "Carlos" 5,
                   Student "Daniel" 4, Student "Edgar" 7 ]
  print (aprovadosComFilterMap students)
-- Saída: ["Ana","Carlos","Edgar"]
```

## Reduções (Dobras / Folds)

Em Haskell, as reduções são implementadas por meio de funções de "dobra" (*folds*), principalmente `foldl` (dobra à esquerda) e `foldr` (dobra à direita). Elas combinam os elementos de uma lista usando uma função binária e um valor inicial (acumulador).

### Dobra à Esquerda (`foldl`)

`foldl` abstrai o seguinte padrão:
i.  Fazer algo a cada elemento da lista (da esquerda para a direita).
ii. Atualizar um acumulador a cada ação.
iii. Retornar o acumulador ao final do processo.

-   **Tipo:**
    ```haskell
    foldl :: (b -> a -> b) -> b -> [a] -> b
    -- Função de passo: (acumulador_atual -> elemento_lista -> novo_acumulador)
    -- Valor inicial do acumulador (zero)
    -- Lista a ser processada
    -- Resultado final (acumulador final)
    ```
-   **Definição (simplificada):**
    ```haskell
    foldl_ step zero []     = zero
    foldl_ step zero (x:xs) = foldl_ step (step zero x) xs
    ```
    `step` é uma função que recebe o acumulador atual e o próximo elemento da lista, e retorna o novo valor do acumulador. `zero` é o valor inicial do acumulador.

-   `foldl` é uma dobra à esquerda porque consome os elementos da lista da esquerda para a direita.

**Exemplos:**
1.  **Soma dos elementos de uma lista:**
    ```haskell
    accumulate :: [Int] -> Int
    accumulate xs = foldl (+) 0 xs -- (+) é a função de passo, 0 é o acumulador inicial

    main = print (accumulate [1..10]) -- Saída: 55
    ```
    Os parêntesis em `(+)` transformam o operador infixo em uma função prefixada.

2.  **Fatorial:**
    ```haskell
    factorial :: Int -> Int
    factorial n = foldl (*) 1 [1..n]

    main = print (factorial 4)
    -- Expansão de factorial 4:
    -- foldl (*) 1 [1,2,3,4]
    -- == foldl (*) (1 * 1) [2,3,4]  -> acc = 1
    -- == foldl (*) (1 * 2) [3,4]    -> acc = 2
    -- == foldl (*) (2 * 3) [4]      -> acc = 6
    -- == foldl (*) (6 * 4) []       -> acc = 24
    -- == 24
    ```

**`foldl` e Avaliação Não-Estrita:**
A função `foldl` deve ser usada com cuidado em Haskell devido à [[PF - Valoração Não-Estrita (Lazy Evaluation)|avaliação não-estrita]].
-   Até que o caso base (lista vazia) seja atingido, todas as expressões intermediárias (as aplicações de `step`) são armazenadas como *thunks* (cálculos pendentes) sem serem computadas.
-   Estas expressões ocupam mais memória do que os valores que elas representariam se fossem calculadas.
-   Se a lista for muito extensa, a pilha de thunks pode crescer demais, e a pilha de execução pode estourar quando o resultado final for forçado.
    ```haskell
    ghci> foldl (+) 0 [1..10^7]
    *** Exception: stack overflow
    ```
-   Este tipo de erro é denominado *space overflow* (estouro de espaço), pois o programa consome muito mais espaço (em memória para thunks) do que deveria.
-   Para listas longas onde a estrita avaliação do acumulador é desejada, Haskell provê `foldl'` (do módulo `Data.List`), que é uma versão estrita de `foldl`.

### Dobra à Direita (`foldr`)

`foldr` realiza a dobra da direita para a esquerda.

-   **Tipo:**
    ```haskell
    foldr :: (a -> b -> b) -> b -> [a] -> b
    -- Função de passo: (elemento_lista -> acumulador_parcial_direita -> novo_acumulador_parcial_direita)
    -- Valor inicial do acumulador (zero/valor base para lista vazia)
    -- Lista a ser processada
    -- Resultado final
    ```
-   **Definição (simplificada):**
    ```haskell
    foldr_ step zero []     = zero
    foldr_ step zero (x:xs) = step x (foldr_ step zero xs)
    ```
    `foldr` processa a lista "de dentro para fora" ou da direita para a esquerda.

**Exemplo de Expansão (Fatorial com `foldr`):**
```haskell
factorialR :: Int -> Int
factorialR n = foldr (*) 1 [1..n]

main = print (factorialR 3)
-- Expansão de factorialR 3:
-- foldr (*) 1 [1,2,3]
-- == 1 * (foldr (*) 1 [2,3])
-- == 1 * (2 * (foldr (*) 1 [3]))
-- == 1 * (2 * (3 * (foldr (*) 1 [])))
-- == 1 * (2 * (3 * 1))
-- == 1 * (2 * 3)
-- == 1 * 6
-- == 6
```
Interpretação de `foldr`: troque o construtor da lista (`:`) por `step` e a lista vazia (`[]`) por `zero`.
Ex: `[1,2,3]` é `1 : (2 : (3 : []))`
`foldr op z [1,2,3]` é `1 `op` (2 `op` (3 `op` z))`

**Utilidade de `foldr`:**
-   À primeira vista, `foldr` pode parecer menos útil que `foldl` para algumas operações, pois processa do último para o primeiro.
-   Contudo, `foldr` é muito poderosa e pode ser usada para implementar muitas outras funções de lista, incluindo `map` e até mesmo `foldl`.
-   Funciona bem com listas infinitas se a função `step` não precisar sempre do segundo argumento (o resultado da dobra do resto da lista) para produzir parte do resultado.

**Implementando `filter` com `foldr`:**
```haskell
filterWithFoldr :: (a -> Bool) -> [a] -> [a]
filterWithFoldr p xs = foldr step [] xs
  where
    step :: a -> [a] -> [a]
    step x ys -- x é o elemento atual, ys é o resultado de filtrar o resto da lista
      | p x       = x : ys -- Se x satisfaz p, adiciona x ao resultado do resto
      | otherwise = ys     -- Senão, descarta x e retorna o resultado do resto
```

**Implementando `map` com `foldr`:**
```haskell
mapWithFoldr :: (a -> b) -> [a] -> [b]
mapWithFoldr f xs = foldr step [] xs
  where
    step :: a -> [b] -> [b]
    step x ys = f x : ys -- Aplica f a x e adiciona ao resultado de mapear o resto
```

**Implementando `foldl` com `foldr` (mais complexo):**
Esta definição não é óbvia e envolve aplicação parcial de funções (currying).
```haskell
foldlAsFoldr :: (b -> a -> b) -> b -> [a] -> b
foldlAsFoldr f z xs = foldr step id xs z
  where
    -- step :: a -> (b -> b) -> (b -> b)
    -- x é um elemento da lista [a]
    -- g é uma função acumulada do tipo (b -> b)
    -- a é o acumulador final do tipo b
    step x g = \a -> g (f a x)
    -- id é a função identidade, :: x -> x. Usada como valor base para g.
```
*Expansão de `accumulate [1,2,3] = foldlAsFoldr (+) 0 [1,2,3]`*:
`foldr step id [1,2,3] 0`
`== step 1 (foldr step id [2,3]) 0`
`== step 1 (step 2 (foldr step id [3])) 0`
`== step 1 (step 2 (step 3 (foldr step id []))) 0`
`== step 1 (step 2 (step 3 id)) 0`
Seja `g0 = id`.
`g1 = step 3 g0 = \a -> g0 ((+) a 3) = \a -> (+) a 3`.
`g2 = step 2 g1 = \a -> g1 ((+) a 2) = \a -> ((+) ((+) a 2) 3)`.
`g3 = step 1 g2 = \a -> g2 ((+) a 1) = \a -> ((+) ((+) ((+) a 1) 2) 3)`.
Aplicando `0` a `g3`:
`g3 0 == ((+) ((+) ((+) 0 1) 2) 3) == (((0+1)+2)+3) == (1+2)+3 == 3+3 == 6`.

Funções que podem ser implementadas com `foldr` são denominadas **recursivas primitivas**.