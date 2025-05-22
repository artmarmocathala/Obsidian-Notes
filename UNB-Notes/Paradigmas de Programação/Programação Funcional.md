Paradigma de programação que trata a computação como a valoração de funções matemáticas e que evita estados e dados mutáveis.

É fundamentada no [[Cálculo Lambda]], de modo que seus programas consistem em expressões, característica de um paradigma declarativo e não imperativo.

O conceito principal é de que o retorno de uma função depende única e exclusivamente de seus parâmetros (ou argumentos). Isto significa que uma função chamada duas ou mais vezes, com os mesmos argumentos, terá sempre o mesmo retorno (propriedade conhecida como [[PF - Funções Puras e Efeitos Colaterais#Transparência Referencial|transparência referencial]]), o que não necessariamente ocorre em paradigmas imperativos.

Exemplo em C de uma mesma função retornando valores diferentes com a mesma entrada (devido a efeitos colaterais):
```c
#include <stdio.h>

int y = 0; // Variável global, estado externo à função f

int f(int x) {
    return x + y++; // y++ modifica o estado global
}

int main() {
    printf("f(2) = %d\n", f(2));     // Saída: f(2) = 2 (x=2, y era 0, torna-se 1)
    printf("f(2) = %d\n", f(2));     // Saída: f(2) = 3 (x=2, y era 1, torna-se 2)
    return 0;
}

```
---
## Tópicos Principais

-   [[PF - Histórico]]
-   [[PF - Características]]
-   [[PF - Variáveis Imutáveis]]
-   [[PF - Funções de Primeira Classe e Alta Ordem]]
-   [[PF - Funções Anônimas]]
-   [[PF - Funções Puras e Efeitos Colaterais]]
-   [[PF - Currying]]
-   [[PF - Valoração Não-Estrita (Lazy Evaluation)]]
-   [[PF - Pattern Matching]]
-   [[PF - Composição de Funções]]
-   [[PF - Recursão em Programação Funcional]]
    -   [[PF - Recursão de Cauda]]
-   [[PF - Mapas, Filtros e Reduções]]
-   [[PF - Haskell - Introdução]]
    -   [[PF - Haskell - Tipos e Operadores Primitivos]]
    -   [[PF - Haskell - Funções]]
    -   [[PF - Haskell - Sistema de Tipos]]
    -   [[PF - Haskell - Tipos de Dados de Usuário]]
    -   [[PF - Haskell - Estruturas de Controle]]
    -   [[PF - Haskell - Variáveis Locais]]
    -   [[PF - Haskell - Sintaxe e Estrutura]]
    -   [[PF - Haskell - I_O (Entrada e Saída)]]
-   [[PF - Referências]]