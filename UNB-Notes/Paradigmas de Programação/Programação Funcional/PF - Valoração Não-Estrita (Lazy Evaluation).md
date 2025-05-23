No contexto de implementação de linguagens de programação, **valoração** (ou avaliação) é o processo de computar o valor de uma expressão.

Existem duas estratégias principais de valoração:

1.  **Valoração Estrita (Eager Evaluation):**
    -   Significa que as expressões são valoradas *imediatamente* e atribuídas à variável que conterá o retorno tão logo esta última seja definida.
    -   Em chamadas de função, a valoração estrita computa o valor de *todos os parâmetros antes* da execução do bloco da função.
    -   A maioria das linguagens imperativas (C, Java, Python) usa valoração estrita por padrão.

2.  **Valoração Não-Estrita (Lazy Evaluation):**
    -   Não computa o valor de uma variável ou expressão, mesmo em sua definição, postergando este cálculo para o seu *primeiro uso efetivo*.
    -   Se um valor nunca é usado, ele nunca é computado.
    -   Se um valor é usado múltiplas vezes, ele é computado na primeira vez e o resultado é guardado (memoizado) para usos subsequentes (esta forma específica é chamada *call-by-need*, que é o que Haskell implementa).
    -   Haskell é a linguagem mais conhecida que usa valoração não-estrita por padrão.

### Vantagens da Valoração Não-Estrita:

-   **Evita cálculos desnecessários:** Se o resultado de uma computação complexa não é necessário, ele não é realizado, economizando tempo.
-   **Permite trabalhar com estruturas de dados infinitas:** É possível definir e manipular listas ou outras estruturas que são conceitualmente infinitas, pois apenas as partes que são realmente acessadas serão computadas.
    ```haskell
    -- A saída será [1, 2, 3, 4, 5]
    main = print xs
      where
        infiniteList = [1..] -- Lista de todos os números naturais a partir de 1
        xs = take 5 infiniteList -- Só os 5 primeiros são computados
    ```
-   **Melhora a modularidade e composição:** Permite definir componentes que podem ser combinados de formas flexíveis, sem se preocupar se um componente intermediário produzirá dados demais.

### Desvantagens da Valoração Não-Estrita:

-   **Maior tempo de processamento e memória em alguns casos:** É preciso manter o registro das expressões não avaliadas (chamadas *thunks*). Um *thunk* é essencialmente uma promessa de calcular um valor quando necessário. Isso pode adicionar overhead.
-   **Raciocínio sobre performance pode ser mais complexo:** Prever quando e quantas vezes algo será avaliado pode ser menos intuitivo do que na valoração estrita.
-   **Problemas de "space leaks":** Se thunks não intencionais são mantidos por muito tempo, podem consumir muita memória (ex: `foldl` em listas longas, ver [[PF - Mapas, Filtros e Reduções]]).

### Exemplo de Diferença: Valoração Estrita vs. Não-Estrita

Considere a expressão:
`(sum [1..10] > 50) || (length [1..] > 1000)`

**Valoração Estrita:**
1.  Computa `sum [1..10]`: `sum ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])` -> `55`.
2.  Computa `55 > 50`: `True`.
3.  *Em muitas linguagens estritas, o operador `||` (OU lógico) faria curto-circuito aqui, pois o primeiro operando é `True`, então o resultado é `True` sem avaliar o segundo. Contudo, se a linguagem não fizesse curto-circuito ou se o operador fosse diferente:*
4.  Tentaria computar `length [1..]`: `length [1, 2, 3, ...]` -> Isso entraria em um **laço infinito** porque `[1..]` é uma lista infinita.

**Valoração Não-Estrita (Haskell):**
1.  A expressão é `(sum [1..10] > 50) || (length [1..] > 1000)`.
2.  O operador `||` precisa do valor do primeiro operando.
3.  Para avaliar `(sum [1..10] > 50)`, precisa de `sum [1..10]`.
4.  `sum [1..10]` é avaliado: `55`.
5.  Agora avalia `55 > 50`: `True`.
6.  Como o primeiro operando de `||` é `True`, o resultado da expressão `||` é `True` **sem precisar avaliar o segundo operando** `(length [1..] > 1000)`.
7.  A parte `(length [1..] > 1000)` nunca é computada. O resultado final é `True`.

A estratégia de avaliação não-estrita está embutida na linguagem Haskell, sem a necessidade de nenhum indicativo ou marcação especial nos programas.