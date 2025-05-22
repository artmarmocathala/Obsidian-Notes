**Pattern Matching** (Casamento de Padrões) é uma característica fundamental em muitas linguagens funcionais, incluindo Haskell. Diz respeito ao processo de verificar se uma dada estrutura de dados "casa" (corresponde) com um ou mais padrões especificados.

### Utilidades:

1.  **Extrair membros (desconstrução):** Permite extrair componentes de uma estrutura de dados de forma declarativa.
    Por exemplo, para uma tupla `(x,y)`, o padrão `(a,b)` casa com a tupla e "ata" `a` ao valor de `x` e `b` ao valor de `y`.
2.  **Verificar tipos/construtores:** Pode verificar se um objeto pertence a um tipo específico ou foi criado com um construtor de dados particular (especialmente útil com [[PF - Haskell - Tipos de Dados de Usuário#Tipos de Dados Algébricos|Tipos de Dados Algébricos]]).
3.  **Definir funções por casos:** Permite definir diferentes comportamentos para uma função com base na estrutura de seus argumentos.

### Vantagens:

-   **Código mais conciso e legível:** Reduz a necessidade de condicionais aninhados (`if-else`) e atribuições manuais para extrair valores.
-   **Melhor entendimento do código:** A estrutura da função reflete diretamente a estrutura dos dados que ela manipula.

### Funcionamento Geral:

-   São escritos vários padrões possíveis para os argumentos de uma função ou para uma expressão em uma construção `case`.
-   Quando a função é chamada ou a expressão `case` é avaliada, o valor de entrada é confrontado com cada padrão, **na sequência em que foram definidos**.
-   O primeiro padrão que casar com sucesso é escolhido.
-   A expressão associada a esse padrão é então avaliada, e seu resultado se torna o resultado da função ou da expressão `case`.
-   Se nenhum padrão casar, geralmente resulta em um erro em tempo de execução (Haskell tentará avisar sobre padrões não exaustivos em tempo de compilação se possível).

### Wildcard:

-   É comum o uso de um caractere ou palavra-chave **wildcard** (em Haskell, o sublinhado `_`) que corresponde a *qualquer* valor sem atá-lo a uma variável. É útil quando uma parte da estrutura não é relevante para o caso atual.

### Desconstrução:

Em alguns contextos, o pattern matching é também denominado **desconstrução**, pois "desmonta" a estrutura de dados em seus componentes.

### Exemplos em Haskell:

**1. Definindo a função `factorial`:**
```haskell
-- Cada linha abaixo corresponde a um padrão a ser checado
factorial :: Int -> Int
factorial 0 = 1                        -- Padrão 1: casa com o argumento 0
factorial n = n * factorial (n - 1)    -- Padrão 2: casa com qualquer outro Int n
                                       -- (deve vir depois do caso base)
main = print (factorial 5) -- Resultado: 120
```

**2. Definindo funções sobre Tipos de Dados Algébricos (ver [[PF - Haskell - Tipos de Dados de Usuário]]):**
```haskell
type Radius = Double
type Base = Double
type Height = Double

data Shape = Circle Radius
           | Triangle Base Height
           deriving (Eq, Show)

-- A função area() é definida para diferentes padrões de entrada (construtores de Shape)
area :: Shape -> Double
area (Circle r)      = pi * r ^ 2         -- Padrão para Circle, extrai r
area (Triangle b h)  = (b * h) / 2.0      -- Padrão para Triangle, extrai b e h

main = print (area (Circle 2.0), area (Triangle 3.0 4.0))
-- Saída: (12.566370614359172,6.0)
```

**3. Pattern Matching em Listas:**
A lista vazia `[]` e o padrão de construção de lista `(x:xs)` (lê-se "x cons xs", onde `x` é a cabeça e `xs` é a cauda) são comumente usados.
```haskell
-- Verifica se a lista está vazia
sumList :: [Int] -> Int
sumList [] = 0                         -- Padrão para lista vazia
sumList (x:xs) = x + sumList xs        -- Padrão para lista não vazia
                                       -- x é o primeiro elemento, xs é o resto da lista

main = print (sumList [1..100]) -- Resultado: 5050
```
*Convenção de nomenclatura:* Usar uma letra (ex: `x`) para um elemento e a mesma letra seguida de `s` (ex: `xs`) para uma lista desses elementos é uma convenção comum em Haskell, onde `xs` significa "múltiplos `x`" (plural).

**4. Extração de Membros de um Tipo de Dado com Wildcards:**
```haskell
type Name = String
type UID = Int
type Score = Double
data Student = Student Name UID Score deriving (Show)

-- O caractere _ é o wildcard e casa com qualquer padrão sem criar uma ligação de variável
studentName :: Student -> Name
studentName (Student name _ _) = name

studentUID :: Student -> UID
studentUID (Student _ uid _) = uid

studentScore :: Student -> Score
studentScore (Student _ _ score) = score

main = do
    let s = Student "Fulano de Tal" 12345 8.7
    putStrLn $ "Nome: " ++ studentName s ++ ", UID: " ++ show (studentUID s) ++ ", Score: " ++ show (studentScore s)
-- Saída: Nome: Fulano de Tal, UID: 12345, Score: 8.7
```

**5. Padrão 'como' (as-pattern):**
Ao escrever funções que recebem listas como parâmetros, é comum checar dois padrões: a lista vazia (`[]`) e a lista com ao menos um elemento (padrão `(x:xs)`).
Este segundo padrão desconstrói a lista. Se a lista toda for necessária na expressão que se segue, ela precisaria ser reconstruída (`x:xs`).
O **padrão ‘como’** (as-pattern), escrito como `nomeOriginal@padrão`, permite checar o `padrão` e, se casar, também atar o valor original completo a `nomeOriginal`.

**Exemplo:** Listar todos os sufixos não nulos de uma string:
```haskell
suffixes :: String -> [String]
suffixes [] = []
suffixes xs@(_:xs') = xs : suffixes xs' -- xs é a lista original, (_:xs') desconstrói
                                        -- para garantir que não é vazia e para a recursão

main = print (suffixes "Teste")
-- Saída: ["Teste","este","ste","te","e"]
```
Sem o as-pattern, teríamos que reconstruir `xs` se precisássemos dele inteiro após o casamento com `(_:xs')`.

**6. Expressões `case`:**
Permitem confrontar uma expressão com vários padrões em um local que não seja a definição de uma função.
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

**Exemplo:** `fromMaybe` (função que extrai o valor de um `Maybe` ou retorna um valor padrão se for `Nothing`):
```haskell
-- Esta função está disponível na biblioteca Data.Maybe.
-- Para utilizá-la sem precisar da implementação abaixo,
-- importe esta biblioteca com o comando: import Data.Maybe

myFromMaybe :: a -> Maybe a -> a
myFromMaybe defval wrapped =
  case wrapped of
    Nothing    -> defval
    Just value -> value

main = do
  print (myFromMaybe 0 (Just 5))  -- Saída: 5
  print (myFromMaybe 0 Nothing) -- Saída: 0
```

**7. Guardas:**
Para testes mais elaborados que vão além da estrutura (com condicionais booleanas), Haskell disponibiliza as **guardas**.
-   Cada padrão em uma definição de função (ou `case`) pode ser seguido por uma ou mais guardas.
-   Cada guarda é introduzida pelo símbolo `|` (pipe), seguida por uma expressão booleana, e então `=` (ou `->` em `case`), e a expressão a ser retornada se a guarda for `True`.
-   As guardas são avaliadas em ordem para um padrão que casou. A primeira guarda que for `True` tem sua expressão correspondente avaliada.
-   A expressão `otherwise` é frequentemente usada como a última guarda e é simplesmente definida como `True`, servindo como um "caso geral".

**Exemplo:** Função `roots` para equação quadrática, usando guardas:
```haskell
-- p(x) = c
roots :: Double -> Double -> Double -> [Double]
roots 0 0 c = [] -- Não é equação ou é 0=c ou c=0

-- p(x) = bx + c
roots 0 b c = [-c/b]

-- p(x) = ax^2 + bx + c
roots a b c
  | delta < 0  = []             -- Guarda 1: delta negativo
  | delta == 0 = [-b / (2*a)]  -- Guarda 2: delta zero
  | otherwise  = [(-b + sqrtDelta) / (2*a), (-b - sqrtDelta) / (2*a)] -- Guarda 3 (delta > 0)
  where
    delta = b^2 - 4*a*c
    sqrtDelta = sqrt delta

main = print $ roots 1 (-5) 6 -- Saída: [3.0,2.0]
```