Haskell, sendo uma linguagem declarativa, não possui estruturas de controle imperativas tradicionais como laços `for` ou `while`. Em vez disso, o fluxo de controle é gerenciado principalmente através de:
-   [[PF - Recursão em Programação Funcional|Recursão]]
-   [[PF - Pattern Matching|Pattern Matching]] (incluindo em definições de função e expressões `case`)
-   Guardas
-   Expressões `if-then-else`
-   [[PF - Mapas, Filtros e Reduções|Funções de alta ordem]] como `map`, `filter`, `fold`

## Condicionais (`if-then-else`)

-   Haskell tem uma variante do construto `if-then-else`.
-   **Sintaxe:**
    `if condicao then valor_se_verdadeiro else valor_se_falso`
-   **Características:**
    1.  A `condicao` deve ser uma expressão do tipo `Bool`.
    2.  Os dois valores (`valor_se_verdadeiro` e `valor_se_falso`) DEVEM ter o **mesmo tipo**.
    3.  Ao contrário de muitas linguagens imperativas, a cláusula `else` é **obrigatória** em Haskell. Isso ocorre porque `if-then-else` é uma *expressão* que deve sempre resultar em um valor.
    4.  Se indentação for utilizada, ela deve ser consistente, pois faz parte da estrutura do construto (regras de layout).
    5.  Diferentemente das linguagens imperativas, o uso direto de `if-then-else` não é tão frequente, sendo muitas vezes preterido por [[PF - Pattern Matching|pattern matching]] ou guardas, que podem ser mais expressivos e legíveis para múltiplos casos.

**Exemplo de Condicionais em Haskell (Sequência de Collatz):**
A sequência de Collatz é definida como: $c(1) = 1$; se $n$ é par, $c(n) = n/2$; se $n$ é ímpar, $c(n) = 3n+1$. A função abaixo gera a sequência a partir de `n` até chegar a 1.
```haskell
-- Este arquivo pode ser importado no GHCi com o comando :load collatz.hs
-- Após a importação a função collatz estará disponível para uso

-- Sequência de Collatz: c(1) = 1, c(n) = n/2, se n é par,
-- c(n) = 3*n + 1, se n é ímpar
collatz :: Int -> [Int]
collatz n = n : if n == 1            -- Adiciona n à lista e então checa a condição
                then []              -- Caso base: se n é 1, termina com lista vazia
                else if even n       -- Senão, checa se n é par
                       then collatz (div n 2) -- Se par, continua com n/2
                       else collatz (3*n + 1) -- Se ímpar, continua com 3n+1

main = print (collatz 6)
-- Saída: [6,3,10,5,16,8,4,2,1]
```

## Expressões `case`

Como visto em [[PF - Pattern Matching#Expressões case|Pattern Matching]], as expressões `case` permitem confrontar uma expressão com vários padrões.

**Sintaxe:**
```haskell
case expressao of
  padrao1 -> valor1
  padrao2 -> valor2
  ...
  padraoN -> valorN
```
-   O valor da `expressao` é confrontado com cada um dos padrões, na ordem descrita.
-   Caso um casamento seja válido, o valor da expressão `case` será o valor indicado após a seta (`->`).
-   Também é possível usar o wildcard `_`.

**Exemplo (revisitado): `fromMaybe`**
```haskell
-- Esta função está disponível na biblioteca Data.Maybe.
myFromMaybe :: a -> Maybe a -> a
myFromMaybe defval wrapped =
  case wrapped of
    Nothing    -> defval
    Just value -> value
```

## Guardas

Como visto em [[PF - Pattern Matching#Guardas|Pattern Matching]], guardas fornecem uma maneira de adicionar condições booleanas a padrões.

-   O uso de pattern matching é feito apenas com a estrutura dos valores.
-   Para testes mais elaborados, com condicionais, Haskell disponibiliza as guardas.
-   Cada padrão a ser casado pode ser seguido por uma ou mais guardas, sendo elas expressões do tipo `Bool`.
-   Cada guarda é introduzida pelo símbolo `|` e seguida de um `=` (ou `->`, no caso de expressões `case`), e então a expressão a ser retornada se a guarda for `True`.
-   Após o símbolo, segue a expressão a ser utilizada caso o padrão seja casado e a guarda verdadeira.
-   A expressão `otherwise` é atada ao valor `True`, de modo que é uma guarda sempre verdadeira, o que melhora a legibilidade do código para o último caso.

**Exemplo (revisitado): `roots`**
```haskell
roots :: Double -> Double -> Double -> [Double]
roots a b c
  | delta < 0  = []
  | delta == 0 = [-b / (2*a)]
  | otherwise  = [(-b + sqrtDelta) / (2*a), (-b - sqrtDelta) / (2*a)]
  where
    delta = b^2 - 4*a*c
    sqrtDelta = sqrt delta
```