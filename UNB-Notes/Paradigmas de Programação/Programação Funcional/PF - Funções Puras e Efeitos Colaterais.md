## Efeitos Colaterais

Uma função ou expressão tem um **efeito colateral** se, além de computar e retornar um valor, ela também:
-   Modifica algum estado fora de seu escopo local, ou
-   Tem uma interação observável com o "mundo exterior" além de retornar um valor.

Os efeitos colaterais mais comuns são:
-   Alteração de variáveis globais ou estáticas.
-   Modificação de parâmetros passados por referência (se a linguagem permitir).
-   Escrita ou leitura em periféricos (ex: console, arquivos, rede).
-   Invocação de outras funções que tenham efeitos colaterais.

## Transparência Referencial

A **transparência referencial** é uma propriedade de expressões em que uma expressão pode ser substituída pelo seu valor sem mudar o comportamento do programa.
-   Se uma função é referencialmente transparente, para um mesmo conjunto de entradas, ela *sempre* produzirá a mesma saída, e sua única contribuição para o programa é o valor que ela retorna.
-   A ausência de efeitos colaterais é uma condição necessária (mas não sempre suficiente, dependendo da definição exata) para a transparência referencial.

## Funções Puras

**Funções puras** são aquelas que:
1.  **Não têm efeitos colaterais:** Elas não modificam nenhum estado fora de seu próprio escopo.
2.  **São determinísticas (para transparência referencial):** Seu valor de retorno depende única e exclusivamente de seus parâmetros de entrada. Para a mesma entrada, sempre produzirão a mesma saída.

Em outras palavras, o retorno de uma função pura depende única e exclusivamente dos parâmetros.

### Implicações em Haskell

-   Haskell assume, por padrão, que todas as funções são puras. Efeitos colaterais (como I/O) são gerenciados de forma explícita e separada do código puro, tipicamente usando o tipo `IO` (ver [[PF - Haskell - I_O (Entrada e Saída)]]).
-   Esta característica tem profundas implicações na linguagem e na forma de programar:
    -   **Modularidade:** Toda função é auto-contida e tem uma interface bem definida. O comportamento de uma função pode ser entendido apenas olhando para seus inputs e outputs, sem se preocupar com estados ocultos.
    -   **Testabilidade:** Facilita o teste unitário de cada função, pois não há dependências de estado externo.
    -   **Raciocínio e Verificação:** Torna mais fácil raciocinar sobre o comportamento dos programas e provar sua correção.
    -   **Otimização:** O compilador tem mais liberdade para otimizar código puro (ex: reordenar execuções, memoization, paralelização).
    -   **Concorrência:** Código puro é inerentemente seguro para concorrência, pois não há risco de race conditions por modificação de dados compartilhados.

**Exemplo de Comportamento de Função Pura em Haskell:**
Considere que a função `f` tenha o tipo `f :: Bool -> Bool`.
Como `f` é pura em Haskell, ela só pode ter um dos seguintes comportamentos:
i.  Ignorar seu argumento e retornar sempre `True`.
ii. Ignorar seu argumento e retornar sempre `False`.
iii. Retornar seu argumento sem modificações (função identidade para `Bool`).
iv. Negar seu argumento (função `not`).

Não pode, por exemplo, imprimir algo no console ou ler um arquivo, pois isso seria um efeito colateral.

### Código Puro vs. Código Impuro

-   Haskell distingue e separa os trechos de código puro e impuro.
-   **Código puro:** Não tem efeitos colaterais, não altera estados e tem mesmo retorno para o mesmo conjunto de parâmetros.
-   **Código impuro:** Não tem garantias sobre o retorno (para as mesmas entradas, pode retornar valores diferentes devido a interações com o sistema), pode interagir com o sistema, alterando seu estado, e ter efeitos colaterais.
-   Código impuro *pode* invocar código puro.
-   Código puro *não pode* invocar diretamente código impuro (a impureza "contaminaria" a pureza). A interação é gerenciada através do sistema de tipos de Haskell (ex: mônada `IO`).
-   As garantias dadas por um código puro trazem vantagens, como a possibilidade de paralelismo automático e otimizações mais agressivas pelo compilador.
-   Em Haskell, a filosofia é manter a maior parte do programa como código puro, e isolar o código impuro (necessário para interações com o mundo) o máximo possível, tornando-o a parte mais simples e controlada do sistema.