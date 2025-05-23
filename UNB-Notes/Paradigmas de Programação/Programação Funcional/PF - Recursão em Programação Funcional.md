Em programação funcional, a **recursão** é a principal técnica utilizada para realizar operações repetitivas, substituindo os laços iterativos (como `for` e `while`) comuns em linguagens imperativas. Uma função recursiva é aquela que se define em termos de si mesma.

### Estrutura de uma Função Recursiva:

Toda função recursiva bem definida deve possuir pelo menos:

1.  **Caso Base (ou Casos Base):** Uma condição (ou condições) que termina a recursão. Sem um caso base, a função se chamaria indefinidamente, levando a um *stack overflow*. O caso base define um resultado direto para uma entrada específica.
2.  **Passo Recursivo (ou Chamada Recursiva):** A parte da função onde ela se chama novamente, mas com um argumento modificado que se aproxima do caso base.

### Laços vs. Recursão

-   **Linguagens Imperativas:** Usam laços (`for`, `while`) que dependem da modificação de variáveis de estado (contadores, acumuladores) para controlar a iteração e acumular resultados.
-   **Linguagens Funcionais Puras:** Como não há [[PF - Variáveis Imutáveis|variáveis mutáveis]] ou efeitos colaterais, a repetição é alcançada passando o estado modificado como argumentos para a próxima chamada recursiva.

### Exemplos de Substituição de Laços por Recursão

Os exemplos a seguir ilustram como problemas tipicamente resolvidos com laços em C++ podem ser implementados usando recursão em Haskell.

#### 1. Capitalização de Palavras

**Problema:** Dada uma lista de palavras (strings), capitalizar cada palavra (primeira letra maiúscula, restante minúscula).

**Implementação em C++ (com laços):**
```cpp
#include <vector>
#include <string>
#include <cctype> // Para toupper, tolower

std::vector<std::string> capitalize_cpp(const std::vector<std::string>& xs) {
    std::vector<std::string> ys;
    for (const auto& x : xs) { // Laço externo para iterar sobre as palavras
        std::string y = x;
        if (!y.empty()) {
            y[0] = std::toupper(x[0]);
            for (size_t i = 1; i < x.size(); ++i) { // Laço interno para caracteres
                y[i] = std::tolower(x[i]);
            }
        }
        ys.push_back(y);
    }
    return ys;
}
```

**Implementação em Haskell (com recursão):**
```haskell
import Data.Char (toUpper, toLower)

capitalize :: [String] -> [String]
capitalize [] = [] -- Caso base: lista de palavras vazia
capitalize (x:xs) = cap x : capitalize xs -- Passo recursivo: capitaliza x, anexa, e chama para o resto (xs)
  where
    cap :: String -> String
    cap [] = [] -- Caso base: palavra vazia
    cap (y:ys) = toUpper y : lower ys -- Passo recursivo para capitalizar uma palavra

    lower :: String -> String
    lower [] = [] -- Caso base: resto da palavra vazia
    lower (z:zs) = toLower z : lower zs -- Passo recursivo para minusculizar o resto
```
*Nota:* Este exemplo pode ser simplificado em Haskell usando [[PF - Mapas, Filtros e Reduções|funções de alta ordem]] como `map`.

#### 2. Lista de Aprovados

**Problema:** Dada uma lista de alunos com seus nomes e notas, gerar uma lista com os nomes dos alunos aprovados (nota >= 5).

**Implementação em C++ (com laço):**
```cpp
#include <vector>
#include <string>

struct Student_cpp {
    std::string name;
    int score;
};

std::vector<std::string> aprovados_cpp(const std::vector<Student_cpp>& xs) {
    std::vector<std::string> ys;
    for (const auto& s : xs) { // Laço para iterar sobre os alunos
        if (s.score >= 5) {
            ys.push_back(s.name);
        }
    }
    return ys;
}
```

**Implementação em Haskell (com recursão e guardas):**
```haskell
data Student = Student String Int deriving (Show)

aprovados :: [Student] -> [String]
aprovados [] = [] -- Caso base: lista de alunos vazia
aprovados (x:xs)
  | score >= 5 = name : aprovados xs -- Guarda: se aprovado, inclui nome e continua
  | otherwise  = aprovados xs       -- Guarda: se não, ignora e continua
  where
    (Student name score) = x -- Pattern matching para extrair nome e nota
```
*Nota:* Este exemplo também pode ser simplificado em Haskell usando `filter` e `map`.

#### 3. Produto Escalar

**Problema:** Calcular o produto escalar de dois vetores (listas de números).
Produto escalar de $\vec{u} = (u_1, ..., u_n)$ e $\vec{v} = (v_1, ..., v_n)$ é $\sum_{i=1}^{n} u_i v_i$.
**Recursão:** $\vec{u} \cdot \vec{v} = u_1 v_1 + \vec{u}' \cdot \vec{v}'$, onde $\vec{u}'$ e $\vec{v}'$ são os vetores sem o primeiro elemento.
**Caso base:** $\vec{u} \cdot \vec{v} = 0$ se um dos vetores for vazio (ou ambos).

**Implementação em C++ (com laço):**
```cpp
#include <vector>
#include <numeric> // Para std::inner_product (alternativa)

double dot_product_cpp(const std::vector<double>& xs, const std::vector<double>& ys) {
    double res = 0.0;
    // Assume xs e ys têm o mesmo tamanho
    for (size_t i = 0; i < xs.size(); ++i) {
        res += xs[i] * ys[i];
    }
    return res;
}
```

**Implementação em Haskell (com recursão):**
```haskell
dotProduct :: [Double] -> [Double] -> Double
dotProduct [] [] = 0.0         -- Caso base: ambas as listas vazias
dotProduct (x:xs) (y:ys) = (x * y) + dotProduct xs ys -- Passo recursivo
dotProduct _ _ = 0.0           -- Caso base: uma lista mais curta que a outra (ou uma vazia)
                               -- Alternativamente, poderia lançar um erro se os tamanhos diferem.
```

#### 4. Verificação de Primalidade

**Problema:** Dado um inteiro positivo $n$, verificar se é primo.
Um número $n$ é primo se não é divisível por nenhum $d$ tal que $1 < d \le \sqrt{n}$.
**Recursão:** Para testar $n$, verificamos a divisibilidade por $d$. Se $d^2 > n$, $n$ é primo. Se $n \pmod d = 0$, $n$ não é primo. Senão, testamos com $d+2$ (se $d$ inicial for 3 e $n$ não for 2 nem par).

**Implementação em C++ (com laço):**
```cpp
bool is_prime_cpp(int n) {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;
    for (int d = 3; d * d <= n; d += 2) {
        if (n % d == 0) return false;
    }
    return true;
}
```

**Implementação em Haskell (com recursão e guardas):**
```haskell
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = test n 3
  where
    test :: Int -> Int -> Bool -- Função auxiliar recursiva
    test num d
      | d * d > num = True        -- Caso base: nenhum divisor encontrado até sqrt(num)
      | mod num d == 0 = False    -- Caso base: divisor encontrado
      | otherwise     = test num (d + 2) -- Passo recursivo: testa próximo divisor ímpar
```

Estes exemplos mostram como a recursão, combinada com pattern matching e guardas, fornece uma maneira poderosa e declarativa de expressar algoritmos repetitivos em Haskell.