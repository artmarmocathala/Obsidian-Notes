## Indentação (Layout Rule)

Em Haskell, a indentação e os espaços em branco são significativos e são utilizados para delimitar blocos de código. Isso é conhecido como a **regra de layout** (ou *layout rule*).

-   **Início de Programa/Módulo:** A primeira expressão de um programa ou módulo pode começar em qualquer coluna do texto.
-   **Mesmo Nível:** As demais expressões de mesmo nível (ex: múltiplas definições de função em um módulo, múltiplas equações para a mesma função) devem começar nesta mesma coluna.
-   **Continuação de Expressão:**
    -   Se uma expressão é seguida de uma linha em branco, a próxima linha não em branco começa um novo item de layout.
    -   Se uma expressão é seguida por uma expressão em uma coluna *mais à direita*, ela é considerada uma continuação da expressão anterior.
-   **Blocos `let`, `where`, `do`, `case ... of`:**
    -   Após uma palavra-chave como `let`, `where`, `do`, ou `of` (em uma expressão `case`), o compilador memorizará a posição (coluna) do próximo *token* significativo.
    -   Expressões nas próximas linhas que comecem na *mesma posição* serão consideradas novas entradas (bindings, ações, alternativas de case) destes blocos.
    -   Se uma linha subsequente começar *mais à direita*, ela é uma continuação da entrada anterior.
    -   Se uma linha subsequente começar *mais à esquerda* (ou na mesma coluna de um contexto de layout mais externo), ela fecha o bloco atual.

-   **Espaços vs. Tabulações:** Por conta destas regras, é **altamente indicado o uso de espaços, e não de tabulações**, na indentação do código Haskell. Tabulações podem ter larguras diferentes em editores diferentes, quebrando o layout.
-   **Delimitação Explícita com Chaves:**
    -   Embora não seja comum para a maior parte do código Haskell (idiomático), os blocos podem ser delimitados explicitamente por chaves (`{` e `}`).
    -   Neste caso, as regras de layout acima podem ser violadas, e as entradas dentro do bloco devem ser separadas por ponto-e-vírgula (`;`).
    ```haskell
    -- Exemplo com chaves (menos idiomático)
    foo x y = let { a = x+1; b = y+1 } in a*b
    ```

## Comentários

-   **Comentários de Linha Única:** Iniciam com dois traços (`--`) e continuam até o final da linha.
    ```haskell
    -- Este é um comentário de linha única
    x = 5 -- Isto também é um comentário
    ```
-   **Comentários de Múltiplas Linhas (Bloco):** São delimitados por `{-` e `-}`. Podem ser aninhados.
    ```haskell
    {-
      Este é um comentário
      de múltiplas linhas.
      {- Pode até ter comentários aninhados! -}
    -}
    main = print "Hello"
    ```

## Estrutura de um Módulo (Arquivo `.hs`)

Um arquivo Haskell (`.hs`) tipicamente define um **módulo**.
-   **Declaração de Módulo (Opcional para `Main`):**
    `module NomeDoModulo (listaDeExportacao) where`
    -   `NomeDoModulo` deve coincidir com o nome do arquivo (ex: `MyModule.hs` define `module MyModule ...`).
    -   `listaDeExportacao` especifica quais funções, tipos, etc., definidos neste módulo são visíveis para outros módulos que o importam. Se omitida, tudo é exportado.
    -   O módulo principal que contém a função `main` é geralmente chamado `Main` (ou o nome do arquivo é `Main.hs`, ou o compilador é instruído a usar um arquivo específico como principal).
-   **Importações:**
    `import NomeDoOutroModulo`
    `import OutroModulo (funcaoEspecifica, TipoEspecifico(..))`
    `import qualified YetAnotherModule as YAM`
-   **Definições:** Funções, tipos de dados, classes de tipos, instâncias, etc.