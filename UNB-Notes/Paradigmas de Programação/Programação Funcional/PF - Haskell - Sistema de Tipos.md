O sistema de tipos é uma parte crucial da linguagem Haskell, contribuindo para sua segurança, expressividade e robustez.

## Características Gerais

-   **Tipagem Estática:** Os tipos de todas as expressões são verificados em **tempo de compilação**. Isso significa que muitos erros relacionados a tipos são detectados antes mesmo do programa ser executado.
-   **Tipagem Forte:** As regras de identificação, conversão e validação dos tipos são estritas. Haskell não realiza muitas conversões implícitas de tipo que são comuns em outras linguagens. Se uma expressão violar as regras de tipagem, ela será considerada malformada e levará a um erro de tipo em tempo de compilação.
    -   Não há promoção de tipos automática (ex: `Int` para `Double`) em expressões mistas sem conversão explícita.
-   **Inferência de Tipos:** Embora Haskell seja estaticamente tipado, na maioria dos casos, o programador não precisa declarar explicitamente os tipos das funções ou variáveis. O compilador Haskell (GHC) é capaz de **inferir** os tipos mais gerais possíveis para as expressões automaticamente.
    -   Declarações de tipo explícitas (assinaturas de tipo) são, no entanto, consideradas boa prática para documentação, clareza e para ajudar o compilador a fornecer mensagens de erro mais precisas.
-   **Segurança de Tipos:** A combinação de tipagem forte e estática faz com que os erros de tipos em tempo de execução sejam virtualmente eliminados em código Haskell bem tipado. Se um programa Haskell compila, é muito menos provável que ele falhe em tempo de execução devido a incompatibilidades de tipo.
-   **Concisão:** A inferência de tipos contribui para a concisão do código Haskell.

## O que é um Tipo?

O **tipo** de um dado é uma abstração sobre a cadeia de bytes que armazena o valor da variável ou constante. Ele define o conjunto de valores que uma expressão pode tomar e as operações que podem ser aplicadas a ela.

## Convenções de Nomenclatura

-   **Nomes de Tipos e Construtores de Tipos/Valores:** Por convenção, iniciam com letra **maiúscula** (ex: `Int`, `Bool`, `String`, `Maybe`, `StudentInfo`).
-   **Nomes de Variáveis e Funções:** Por convenção, iniciam com letra **minúscula** (ex: `x`, `name`, `areaCircle`, `studentName`).
-   **Variáveis de Tipo (em polimorfismo):** Também iniciam com letra minúscula (ex: `a`, `b` na assinatura `map :: (a -> b) -> [a] -> [b]`).

## Assinatura de Tipo

A assinatura de um tipo em Haskell é declarada usando `::` (lê-se "tem o tipo de").
`expression :: Type`

**Exemplos:**
```haskell
'a' :: Char
True :: Bool
[1,2,3] :: [Int]
areaCircle :: Double -> Double
```

## Determinando Tipos no GHCi

No GHCi, o tipo de uma expressão pode ser determinado por meio do comando `:type` (ou sua abreviação `:t`).
```haskell
ghci> :type 'H'
'H' :: Char
ghci> :t True
True :: Bool
ghci> :t [1,2,3]
[1,2,3] :: Num a => [a]  -- Lista de algum tipo numérico 'a'
ghci> :t ("Hello", 1)
("Hello", 1) :: Num b => ([Char], b) -- Tupla com String e algum tipo numérico 'b'
ghci> :t map
map :: (a -> b) -> [a] -> [b]
```

## Conversões entre Tipos

-   Conversões entre tipos em Haskell geralmente envolvem a criação de um novo valor (cópia) e devem ser explícitas.
-   Isso pode ter impacto na performance se cópias frequentes de grandes estruturas de dados forem realizadas, mas garante a integridade do sistema de tipos.

## Polimorfismo

Haskell suporta **polimorfismo**, o que permite escrever funções e definir tipos de dados que podem operar com uma variedade de tipos de forma genérica.

-   **Listas são tipos polimórficos:** A notação `[a]` significa "uma lista de elementos do tipo `a`". Aqui, `a` é uma **variável de tipo**. Ela pode ser instanciada para qualquer tipo concreto (ex: `[Int]`, `[String]`, `[Bool]`).
-   **Variáveis de tipo:** Começam com letra minúscula (ex: `a`, `b`, `item`).
-   **Inferência de tipos polimórficos:** O compilador tenta inferir o tipo polimórfico mais geral.
    ```haskell
    ghci> :t head
    head :: [a] -> a -- 'head' funciona para listas de qualquer tipo 'a'
    ```
-   **Exemplo: `fst`**
    A função `fst` retorna o primeiro elemento de uma tupla de dois elementos.
    ```haskell
    ghci> :t fst
    fst :: (a, b) -> a
    ```
    Aqui, `a` e `b` representam tipos, possivelmente distintos. Pelo tipo da função `fst :: (a,b) -> a`, o único comportamento possível que ela pode ter (sem recorrer a "trapaças" como `unsafeCoerce` ou erros) é retornar o primeiro elemento da tupla.
    Esta é uma característica importante do Haskell: o tipo de uma função pode dar pistas fortes sobre (ou possivelmente determinar unicamente) o comportamento de uma função, especialmente para funções polimórficas puras.

## Classes de Tipos (Type Classes)

Embora não explicitamente detalhado nos slides fornecidos até agora, as classes de tipos são um conceito central em Haskell, relacionado ao polimorfismo. Elas permitem **polimorfismo ad-hoc (ou restrito)**.

-   Uma classe de tipos define um conjunto de funções (operações) que devem ser implementadas por tipos que são instâncias dessa classe.
-   Exemplos comuns:
    -   `Eq`: Para tipos cujos valores podem ser comparados por igualdade (`==`, `/=`).
    -   `Ord`: Para tipos cujos valores podem ser ordenados (`<`, `<=`, `>`, `>=`). Requer `Eq`.
    -   `Show`: Para tipos cujos valores podem ser convertidos para uma representação em `String` (usado por `print`).
    -   `Read`: Para tipos cujos valores podem ser lidos a partir de uma `String`.
    -   `Num`: Para tipos numéricos que suportam operações como `+`, `-`, `*`, `abs`, `signum`, `fromInteger`.

-   **Restrições de Contexto em Assinaturas de Tipo:**
    Quando você vê algo como `Num a => a -> a -> a` (o tipo de `+`), a parte `Num a =>` é uma restrição de contexto. Ela lê-se como: "Para qualquer tipo `a` que seja uma instância da classe de tipos `Num`...".
    ```haskell
    ghci> :t (+)
    (+) :: Num a => a -> a -> a
    ghci> :t (==)
    (==) :: Eq a => a -> a -> Bool
    ```
    Isso significa que `+` pode operar sobre quaisquer dois valores do mesmo tipo `a`, desde que esse tipo `a` pertença à classe `Num` (ex: `Int`, `Double`, `Integer`). Similarmente, `==` funciona para qualquer tipo `a` que seja uma instância de `Eq`.