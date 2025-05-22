Além dos tipos primitivos e polimórficos fornecidos pela linguagem, Haskell permite que os programadores definam seus próprios tipos de dados. Isso é feito principalmente usando a palavra-chave `data` e, para sinônimos, `type`.

## Sinônimos de Tipo (`type`)

É possível definir um **sinônimo** para um tipo já existente com o intuito de ampliar o entendimento do código por meio do uso de nomes mais descritivos ou para simplificar assinaturas de tipo complexas.
Isto pode ser feito por meio da palavra-reservada `type`.

-   Um sinônimo de tipo não cria um novo tipo; ele apenas fornece um nome alternativo para um tipo existente.
-   A definição de sinônimos é similar à feita em C/C++ por meio da palavra-reservada `typedef`.

**Exemplo:**
```haskell
-- Sinônimos para os campos de um estudante
type Name = String
type UID = Int
type Score = Double

-- Usando os sinônimos na definição de um novo tipo de dado (ver abaixo)
-- ou em assinaturas de função:
getName :: Student -> Name
getName (Student n _ _) = n

-- Mesmo tipo anterior (StudentInfo do slide), porém com nomes de campos mais descritivos
-- usando os sinônimos na definição do tipo Student.
data Student = Student Name UID Score deriving (Show)
```

## Definição de Novos Tipos de Dados (`data`)

É possível introduzir novos tipos de dados por meio da palavra reservada `data`.

**Sintaxe básica:**
`data ConstrutorDoTipo = ConstrutorDeValor tipo1 tipo2 ... tipoN`

-   **`ConstrutorDoTipo`**: O nome do novo tipo que está sendo definido (ex: `StudentInfo`, `Point2D`, `BloodGroup`, `Shape`). Deve começar com letra maiúscula.
-   **`ConstrutorDeValor`**: O nome usado para criar valores (instâncias) deste tipo (ex: `Student`, `Point2D`, `A`, `Circle`). Também deve começar com letra maiúscula.
    -   Um tipo pode ter um ou mais construtores de valor (ver [[PF - Haskell - Tipos de Dados de Usuário#Tipos de Dados Algébricos|Tipos de Dados Algébricos]]).
-   **`tipo1 tipo2 ... tipoN`**: São os tipos dos campos (membros) que o `ConstrutorDeValor` armazena. Podem ser zero ou mais.

**Características:**
-   O nome do tipo e de seus construtores de valor são, em princípio, independentes, embora frequentemente sejam iguais quando há apenas um construtor de valor.
-   Os nomes dos tipos (Construtores de Tipo) são usados exclusivamente em suas definições e em assinaturas de tipo.
-   Os construtores de valores são utilizados no programa para criar variáveis (instâncias) do tipo definido. O construtor de valor pode ser entendido como uma função que recebe os tipos dos campos como argumentos e retorna um valor do novo tipo.
-   Quando não há ambiguidade (geralmente quando um tipo tem apenas um construtor de valor), os nomes do tipo e do construtor de valor podem ser iguais. Esta prática é normal e legal.
    ```haskell
    data Point2D = Point2D Double Double -- ConstrutorDoTipo e ConstrutorDeValor são ambos "Point2D"
    ```

**Exemplo Simples:**
```haskell
-- Definição do novo tipo StudentInfo
data StudentInfo = Student String Int Double deriving (Show)
-- StudentInfo é o ConstrutorDoTipo
-- Student é o ConstrutorDeValor
-- String, Int, Double são os tipos dos campos

-- Nova variável do tipo recém-criado
newStudent :: StudentInfo
newStudent = Student "Fulano de Tal" 20 1.77
-- `Student "Fulano de Tal" 20 1.77` cria um valor do tipo StudentInfo
```
A cláusula `deriving (Show)` permite que valores do tipo `StudentInfo` sejam convertidos para String e impressos (ex: com `print`).

**Distinção de Tipos:**
Haskell não permite a mistura de tipos de dados que são estruturalmente idênticos, mas têm nomes diferentes. Eles são considerados tipos distintos.
```haskell
data Point2D = Point2D Double Double deriving (Show)
data Polar   = Polar   Double Double deriving (Show)

x :: Point2D
x = Point2D 1.0 2.0

y :: Polar
y = Polar 1.0 2.0

-- x e y não são comparáveis diretamente (ex: com == sem uma instância de Eq para ambos)
-- e não podem ser usados intercambiavelmente, pois têm tipos distintos,
-- mesmo que ambos armazenem dois Doubles.
```

## Tipos de Dados Algébricos (ADTs)

Um tipo de dados definido com `data` pode ter **mais de um construtor de valor**. Esses são chamados Tipos de Dados Algébricos. Os diferentes construtores são separados por `|` (pipe).

-   Eles também são denominados **tipos enumeráveis** quando os construtores não têm parâmetros (campos).
    ```haskell
    -- Eq permite que tipos sejam comparáveis por igualdade
    data BloodGroup = A | B | AB | O deriving (Eq, Show)
    -- A, B, AB, O são construtores de valor sem parâmetros para o tipo BloodGroup.
    -- let myBloodType = A
    ```

-   Cada um dos construtores pode ter zero ou mais parâmetros (campos), e os parâmetros podem ser de tipos diferentes para construtores diferentes dentro do mesmo tipo.
    ```haskell
    type Radius = Double
    type Base   = Double
    type Height = Double -- Corrigido de Heigth para Height

    data Shape = Circle Radius
               | Triangle Base Height
               deriving (Eq, Show)

    -- Circle é um construtor de valor que toma um Radius
    -- Triangle é um construtor de valor que toma uma Base e uma Height
    -- let c = Circle 5.0
    -- let t = Triangle 3.0 4.0
    ```

**Analogias com C/C++:**
-   Com um **único construtor de valor**, o tipo de dado algébrico equivale a uma `struct` em C.
-   Com **dois ou mais construtores, todos sem parâmetros**, corresponde a uma `enum` em C.
-   Nos **demais casos** (múltiplos construtores, alguns ou todos com parâmetros), pode ser visto como uma `union` "taggeada" (tagged union ou discriminated union) de C/C++, onde o construtor de valor atua como a "tag" que discrimina qual variante da união está ativa.

## Notação de Registro (Record Syntax)

Escrever funções de acesso para os membros (campos) de um tipo de dado (como `studentName`, `studentUID` em exemplos anteriores) pode ser tedioso e propenso a erros, especialmente se a ordem dos campos mudar.
Haskell oferece uma definição alternativa, chamada **notação de registro**, que permite nomear os campos e automaticamente gera funções de acesso para eles no momento da definição do novo tipo de dado.

**Sintaxe:**
```haskell
data ConstrutorDoTipo = ConstrutorDeValor {
  nomeDoCampo1 :: TipoDoCampo1,
  nomeDoCampo2 :: TipoDoCampo2,
  ...
  nomeDoCampoN :: TipoDoCampoN
} deriving (...)
```

**Exemplo:** O tipo `Student` usando notação de registro:
```haskell
type Name = String
type UID = Int
type Score = Double

data StudentRec = StudentRec {
  studentName :: Name,  -- studentName é agora uma função :: StudentRec -> Name
  studentUID  :: UID,   -- studentUID  é agora uma função :: StudentRec -> UID
  studentScore:: Score  -- studentScore é agora uma função :: StudentRec -> Score
} deriving (Show)

-- Criando uma variável com notação de registro (a ordem dos campos não importa na criação):
s :: StudentRec
s = StudentRec { studentName = "Fulano de Tal",
                 studentUID  = 12345,
                 studentScore = 8.7 }

s' :: StudentRec
s' = StudentRec { studentScore = 9.0, studentName = "Ciclana", studentUID = 67890 }


main = do
  print s
  print (studentName s, studentScore s') -- Usando as funções de acesso geradas
  -- Saída de print s (com deriving Show):
  -- StudentRec {studentName = "Fulano de Tal", studentUID = 12345, studentScore = 8.7}
```
-   Esta notação também pode ser utilizada para criar variáveis deste novo tipo, e a ordem dos campos na criação não importa se os nomes forem usados.
-   A impressão padrão herdada de `Show` também fica mais elaborada, mostrando os nomes dos campos.
-   Permite atualização de campos de forma funcional: `s { studentScore = 9.5 }` cria um *novo* registro `StudentRec` igual a `s` mas com `studentScore` modificado.

## Tipos Parametrizáveis

Em Haskell é possível criar tipos de dados parametrizados, por meio da introdução de uma **variável de tipo** na definição de tipo. Isso é semelhante ao polimorfismo para funções, mas aplicado a definições de tipos.

-   Na biblioteca padrão (`Prelude`), o tipo parametrizado `Maybe` é um exemplo fundamental:
    ```haskell
    data Maybe a = Just a | Nothing
    -- 'a' é uma variável de tipo.
    ```
-   A variável `a` nesta definição significa "um tipo de dado qualquer". `Maybe a` pode ser `Maybe Int`, `Maybe String`, etc.
-   Este tipo de dado é utilizado para retornos de funções que podem estar ausentes (ou seja, funções que podem não conseguir produzir um valor do tipo esperado).
    -   `Just valor` encapsula um valor existente do tipo `a`.
    -   `Nothing` representa a ausência de um valor.

-   **Exemplo:** A função `head` padrão lança um erro quando aplicada à lista vazia.
    ```haskell
    ghci> head []
    *** Exception: Prelude.head: empty list
    ```
    Uma versão "segura" de `head` pode ser implementada usando `Maybe` para retornar `Nothing` no caso de lista vazia:
    ```haskell
    safeHead :: [a] -> Maybe a
    safeHead []    = Nothing
    safeHead (x:_) = Just x

    main = do
      print (safeHead [1,2,3]) -- Saída: Just 1
      print (safeHead [])      -- Saída: Nothing
    ```

## Tipos Recursivos

Um tipo é dito **recursivo** se ele for definido em termos de si próprio.

-   **Exemplo:** Uma lista parametrizada pode ser definida recursivamente:
    ```haskell
    data List a = Cons a (List a)  -- Construtor recursivo: Cons contém um 'a' e outra 'List a'
                | Null             -- Caso base: construtor não recursivo
                deriving (Show)

    -- Define uma lista do novo tipo: 1 -> 2 -> 3 -> Null
    myList :: List Int
    myList = Cons 1 (Cons 2 (Cons 3 Null))

    -- Converte da lista definida para a lista padrão do Haskell
    toList :: List a -> [a]
    toList Null         = []
    toList (Cons x xs) = x : toList xs

    main = print (myList, toList myList)
    -- Saída: (Cons 1 (Cons 2 (Cons 3 Null)),[1,2,3])
    ```
-   Os tipos recursivos devem ter ao menos um **caso base** (um construtor que não se refere ao próprio tipo, como `Null` no exemplo acima).
-   Assim, eles devem ter, no mínimo, dois construtores distintos (um recursivo e um base, ou múltiplos de cada).

-   **Exemplo: Árvore Binária**
    Uma árvore binária pode ser definida como:
    ```haskell
    data Tree a = Empty                -- Caso base: árvore vazia
                | Node a (Tree a) (Tree a) -- Construtor recursivo: nó com valor e duas sub-árvores
                deriving (Show)

    myTree :: Tree Int
    myTree = Node 10 (Node 5 Empty Empty) (Node 15 Empty (Node 20 Empty Empty))

    main = print myTree
    -- Saída: Node 10 (Node 5 Empty Empty) (Node 15 Empty (Node 20 Empty Empty))
    ```