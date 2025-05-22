Em Haskell, há duas formas principais de introduzir definições locais (variáveis ou funções auxiliares) dentro do escopo de uma função: expressões `let` e cláusulas `where`.

## Expressão `let ... in ...`

-   **Sintaxe:**
    ```haskell
    nome_da_funcao par1 ... parN = let var1 = value1
                                       var2 = value2
                                       ...
                                       varM = valueM
                                   in expressao_da_funcao_usando_vars
    ```
    Ou, de forma mais geral, `let <bindings> in <expression>`.

-   **Funcionamento:**
    -   A parte `let` introduz um conjunto de *bindings* (definições) locais. Cada linha define uma nova variável (ou função local).
    -   A palavra-chave `in` separa os bindings da expressão principal onde esses bindings são visíveis.
    -   O escopo destas variáveis locais é a `expressao_da_funcao_usando_vars` e também as definições de variáveis subsequentes dentro do mesmo bloco `let` (permitindo que `var2` use `var1`, por exemplo).
    -   `let ... in ...` é uma *expressão*, o que significa que ela mesma tem um valor (o valor da `expressao_da_funcao_usando_vars`). Isso permite aninhar expressões `let`.

-   **Shadowing:** Se o nome de uma das variáveis locais coincidir com o nome de um dos parâmetros da função (ou uma variável de um escopo mais externo), prevalecerá a variável local (ocorre *shadowing*). Isso pode levar a expressões confusas ou bugs se não for intencional.

## Cláusula `where`

-   **Sintaxe:**
    ```haskell
    nome_da_funcao par1 ... parN = expressao_da_funcao_usando_vars
      where
        var1 = value1
        var2 = value2
        ...
        varM = valueM
    ```

-   **Funcionamento:**
    -   A cláusula `where` é anexada a uma definição de função (ou a um caso em pattern matching).
    -   Ela introduz bindings locais que são visíveis em toda a `expressao_da_funcao_usando_vars` que a precede (e em todas as guardas associadas a essa definição de função).
    -   O escopo das variáveis definidas na expressão `where` é a expressão da função que precede o bloco, e o próprio bloco `where` (permitindo definições mútuas).

## Diferenças e Escolha

-   **Prioridade Visual:** A principal diferença estilística é que `let ... in ...` coloca as definições locais *antes* da expressão principal, enquanto `where` as coloca *depois*.
    -   `let` é útil quando as definições locais são pequenas e ajudam a entender a expressão que se segue.
    -   `where` é frequentemente preferida quando a expressão principal é o foco, e as definições locais são detalhes de implementação ou valores auxiliares.
-   **Escopo:**
    -   Variáveis em `let` são visíveis na expressão `in` e nos bindings subsequentes do `let`.
    -   Variáveis em `where` são visíveis em toda a definição da função à qual o `where` está anexado (incluindo todas as guardas dessa definição).
-   **Funções Locais:** Além de variáveis locais, é possível definir funções locais (auxiliares) tanto em expressões `let` quanto em cláusulas `where`.
-   **Estilo:** A escolha entre `let` e `where` é, em muitos casos, uma questão de estilo e legibilidade.

**Exemplo de Uso de Variáveis Locais (Função `roots` com `where`):**
```haskell
-- p(x) = c
roots :: Double -> Double -> Double -> [Double]
roots 0 0 c = []

-- p(x) = bx + c
roots 0 b c = [-c/b]

-- p(x) = ax^2 + bx + c
roots a b c = if delta >= 0
                then [(-b + sqrt delta)/(2*a), (-b - sqrt delta)/(2*a)]
                else []
  where
    delta = b^2 - 4*a*c -- delta é local para a definição de 'roots a b c'

main = print $ roots 1 (-5) 6
-- Saída: [3.0,2.0]
```

**Exemplo com `let` (dentro de um bloco `do` para I/O):**
```haskell
main = do
  putStrLn "Digite um número:"
  inputStr <- getLine
  let num = read inputStr :: Int -- 'num' é local para o resto do bloco 'do'
  putStrLn ("O dobro de " ++ show num ++ " é " ++ show (num * 2))
```