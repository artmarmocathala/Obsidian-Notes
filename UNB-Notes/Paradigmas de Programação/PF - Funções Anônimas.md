As funções geralmente contêm quatro partes: nome, lista de parâmetros, corpo e retorno.
**Funções anônimas** são funções que não possuem um nome.

Existem dois tipos principais ou conceitos relacionados a funções anônimas:

1.  **Funções Lambda (λ-funções):**
    São a forma mais direta de funções anônimas. No [[Cálculo Lambda]], todas as funções são essencialmente anônimas (lambdas). Em linguagens de programação, elas são tipicamente compostas por:
    -   Uma lista de parâmetros.
    -   Um corpo (a expressão que define o que a função faz).
    -   Um valor de retorno implícito (o resultado da avaliação do corpo).

    Em geral, funções anônimas (lambda) são utilizadas como:
    -   Parâmetros para [[PF - Funções de Primeira Classe e Alta Ordem|funções de alta ordem]] (ex: `map`, `filter`).
    -   Para a construção concisa de funções que são retornadas por outras funções.

    **Sintaxe em Haskell:**
    A sintaxe para a definição de uma função lambda é:
    `\var1 var2 ... varN -> expression`

    -   A lista de parâmetros `var1, var2, ..., varN` pode conter casamentos de padrões.
    -   A `expression` (corpo), contudo, não pode conter guardas diretamente na sintaxe lambda (guardas seriam usadas com `if-then-else` ou `case` dentro da expressão).
    -   A depender do contexto, pode ser necessário usar parêntesis para delimitar o corpo da função lambda.

    **Exemplo em Haskell:**
    A função abaixo imprime os inteiros de 1 a `n`, substituindo os múltiplos de `m` pela palavra "Pim".
    ```haskell
    -- Abaixo um exemplo para n = 10 e m = 3
    -- A função anônima é: \x -> if mod x m == 0 then "Pim" else show x
    pim n m = map (\x -> if mod x m == 0 then "Pim" else show x) [1..n]

    main = putStr $ unlines $ pim 10 3
    -- Saída:
    -- 1
    -- 2
    -- Pim
    -- 4
    -- 5
    -- Pim
    -- 7
    -- 8
    -- Pim
    -- 10
    ```

2.  **Closures:**
    Closures são um conceito intimamente relacionado e frequentemente implementado usando funções anônimas.
    -   São semelhantes às funções lambda, mas com uma característica crucial: elas **referenciam variáveis que estavam no escopo onde a closure foi definida, mesmo que esse escopo não exista mais quando a closure é executada** (variáveis "livres" ou "capturadas").
    -   Em geral, os closures são implementados como estruturas de dados que contêm:
        -   Um ponteiro para o código da função.
        -   Um ambiente que armazena as referências (ou valores) das variáveis capturadas necessárias para sua execução.

    Haskell, devido à sua pureza e avaliação lazy, lida com escopos e referências de maneira que o conceito de closure é naturalmente integrado. Qualquer função em Haskell pode ser uma closure se ela se refere a variáveis de um escopo externo.