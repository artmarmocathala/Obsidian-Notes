**Currying** (nomeado em homenagem ao lógico Haskell Curry, que também deu nome à linguagem Haskell) é uma técnica de transformação que converte uma função que aceita múltiplos parâmetros em uma sequência de funções, cada uma com um único parâmetro. O retorno de cada uma dessas funções é a próxima função na sequência, até que o último parâmetro seja fornecido e o resultado final seja produzido.

Por exemplo, considere uma função $f$ que soma dois inteiros:
$f : \mathbb{Z} \times \mathbb{Z} \rightarrow \mathbb{Z}$
$(x, y) \mapsto f(x, y) = x + y$

A aplicação do currying em $f$ leva a uma função $g$ (e subsequentemente $h_x$):
$g : \mathbb{Z} \rightarrow (\mathbb{Z} \rightarrow \mathbb{Z})$
$x \mapsto g(x) = h_x$
onde $h_x$ é uma nova função:
$h_x : \mathbb{Z} \rightarrow \mathbb{Z}$
$y \mapsto h_x(y) = x + y$

Então, $f(x,y)$ se torna $(g(x))(y)$ ou, de forma mais concisa, $g\;x\;y$.

### Currying em Haskell

Em Haskell, todas as funções são curried por padrão. Isso significa que toda função em Haskell tecnicamente aceita apenas um argumento. Se uma função parece aceitar múltiplos argumentos, ela é na verdade uma função que aceita o primeiro argumento e retorna uma nova função que aceita o segundo argumento, e assim por diante.

-   Na sintaxe de tipo de uma função, as setas (`->`) indicam essa sequência de aplicações parciais.
    Por exemplo, o tipo da função `take`:
    ```haskell
    ghci> :type take
    take :: Int -> [a] -> [a]
    ```
    Uma leitura possível deste tipo seria: "a função `take` recebe dois parâmetros – um `Int` e uma lista de elementos do tipo `a`".
    Porém, efetivamente este tipo significa que a função `take`, ao receber como parâmetro um número inteiro (`Int`), retorna uma *nova função* `f` cujo tipo é `[a] -> [a]` (uma função que espera uma lista e retorna uma lista).

-   **Aplicação Parcial:**
    Como as funções são curried, podemos realizar a **aplicação parcial**, que é fornecer menos argumentos do que a função "espera" no total. O resultado é uma nova função que espera os argumentos restantes.

    **Exemplo:**
    ```haskell
    ghci> let take3 = take 3 -- Aplicação parcial de take com o primeiro argumento 3
    ghci> :type take3
    take3 :: [a] -> [a]     -- take3 é uma nova função que espera uma lista
    ghci> take3 [1..10]
    [1,2,3]
    ```

-   **Interpretação:**
    -   De fato, em Haskell, todas as funções recebem um único parâmetro.
    -   A cada aplicação de um parâmetro, o retorno é uma nova função, que recebe "um parâmetro a menos" que a anterior (em termos de quantos faltam para o resultado final).
    -   Outra forma de interpretar este comportamento é pensar que, se $f$ é uma função com múltiplos parâmetros, a cada aplicação um dos parâmetros tem seu valor fixado, e os parâmetros ainda não definidos são os parâmetros da função resultante.

-   **Benefícios:**
    A aplicação parcial permite a definição concisa de novas funções baseadas em funções preexistentes, simplificando o código e facilitando a leitura.

    **Exemplo:** Definindo `head` em termos de `take`:
    ```haskell
    head' :: [a] -> a -- (Nota: head real lança erro em lista vazia, esta é simplificada)
    -- head' xs = take 1 xs  -- Forma mais explícita
    ```
    De fato, o parâmetro `xs` pode ser omitido desta definição (eta-redução), resultando em:
    ```haskell
    head'' = take 1 -- head'' agora é uma função que espera uma lista
    ```
    (Para que `head''` realmente retorne o elemento e não uma lista unitária, seria `head'' = (!!0) . take 1` ou algo mais complexo para lidar com a extração do elemento da lista unitária e o caso da lista vazia). A ideia principal é a fixação do primeiro argumento de `take`.

    Outro exemplo de função definida por meio de aplicação parcial:
    ```haskell
    -- retorna apenas os elementos ímpares da lista
    odds :: [Int] -> [Int]
    odds = filter odd -- 'odd' é uma função :: Int -> Bool
                     -- 'filter' é :: (a -> Bool) -> [a] -> [a]
                     -- 'filter odd' é :: [Int] -> [Int]
    ```

### Seções

Haskell tem uma sintaxe especial para aplicação parcial de operadores infixos, denominada **seção**. Para tal, basta envolver o operador entre parêntesis e fornecer o operando da esquerda ou da direita.

-   `(x op)` é uma função que espera o operando da direita. Ex: `(2*)` é uma função que multiplica seu argumento por 2.
-   `(op y)` é uma função que espera o operando da esquerda. Ex: `(^2)` é uma função que eleva seu argumento ao quadrado.

**Exemplos:**
```haskell
ghci> let doubles = map (2*)
ghci> doubles [1..5]
[2,4,6,8,10]

ghci> let squares = map (^2)
ghci> squares [1..5]
[1,4,9,16,25]

ghci> let hasx = ('x' `elem`) -- 'elem' é infixo com crases
ghci> :type hasx
hasx :: [Char] -> Bool
ghci> hasx "texto com x"
True

ghci> let suffixOfTest = (`isSuffixOf` "Test") -- isSuffixOf é do Data.List
ghci> :module +Data.List
ghci> suffixOfTest "est"
True
```