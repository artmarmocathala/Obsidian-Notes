## Funções de Primeira Classe

**Funções de primeira classe** são funções que recebem o mesmo tratamento que os tipos primitivos da linguagem (como inteiros ou strings).
Isso significa que elas podem ser:
-   Passadas como parâmetros para outras funções.
-   Retornadas como resultado de outras funções.
-   Armazenadas em variáveis ou estruturas de dados.

## Funções de Alta Ordem

**Funções de alta ordem** são uma consequência direta de se ter funções de primeira classe. Uma função é de alta ordem se:
-   Recebe uma ou mais funções como parâmetros, OU
-   Retorna uma função como seu resultado.

Funções que *não* são de alta ordem (ou seja, operam apenas sobre dados não-funcionais e retornam dados não-funcionais) são denominadas **funções de primeira ordem**.

No [[Cálculo Lambda]], todas as funções são inerentemente de alta ordem.

### Exemplo: `map`

Um exemplo típico de função de alta ordem é a função `map`. Ela geralmente recebe:
1.  Uma função $f$.
2.  Uma lista $xs$.
E retorna uma nova lista cujo $i$-ésimo elemento é $f(x_i)$, onde $x_i$ é o $i$-ésimo elemento de $xs$.

**Exemplo em Haskell:**
A função `sqrt` (raiz quadrada) é aplicada a cada elemento da lista `[1, 2, 3, 4, 5]`.
```haskell
-- Saída: [1.0,1.4142135623730951,1.7320508075688772,2.0,2.23606797749979]
main = print xs
  where
    xs = map sqrt [1, 2, 3, 4, 5]
```

### Outros Exemplos em Haskell

Haskell possui muitas funções de alta ordem em sua biblioteca padrão.

-   **`break`**: Recebe um predicado `P` e uma lista `xs`. Retorna um par de listas `(ys, zs)` onde `xs = ys ++ zs`, e `zs` tem início no primeiro elemento `x` de `xs` tal que a expressão `P x` é verdadeira.
    ```haskell
    ghci> :type break
    break :: (a -> Bool) -> [a] -> ([a], [a])
    ghci> break even [1, 1, 2, 3, 5, 8]
    ([1,1],[2,3,5,8])
    ```

-   **`all`**: Recebe um predicado `P` e uma lista `xs`. Retorna `True` se `P x` é verdadeira para todos os `x` em `xs`.
    ```haskell
    ghci> all odd [1,3,5]
    True
    ghci> all (>0) [1,2,-1,3]
    False
    ```

-   **`any`**: Recebe um predicado `P` e uma lista `xs`. Retorna `True` se `P x` é verdadeira para ao menos um elemento `x` de `xs`.
    ```haskell
    ghci> any even [1,3,5]
    False
    ghci> any (>0) [-1,-2,1,-3]
    True
    ```

-   **`takeWhile`**: Recebe um predicado `P` e uma lista `xs`. Retorna uma lista `ys` cujos elementos são todos dentre os primeiros elementos `x` de `xs` tais que `P x` é verdadeira.
    ```haskell
    Prelude Data.Char> takeWhile isUpper "FGAmaDF"
    "FGA"
    ```

-   **`dropWhile`**: Sua complementar. Recebe os mesmos parâmetros e retorna uma lista `ys` cujo primeiro elemento é o primeiro elemento `x` de `xs` para o qual a expressão `P x` é falsa.
    ```haskell
    Prelude Data.Char> dropWhile isUpper "FGAmaDF"
    "maDF"
    ```

-   **`span`**: Retorna um par de listas com as duas partes resultantes da chamada de `takeWhile` e `dropWhile` com o mesmo predicado e lista.
    `span p xs` é equivalente a `(takeWhile p xs, dropWhile p xs)`
    ```haskell
    Prelude Data.Char> span isUpper "FGAmaDF"
    ("FGA","maDF")
    ```