Uma função é dita **recursiva de cauda** (*tail recursive*) se a chamada recursiva é a *última operação* realizada na função. Ou seja, nada mais é feito com o resultado da chamada recursiva antes que ele seja retornado pela função atual.

-   **Função recursiva normal (não de cauda):**
    ```haskell
    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n * factorial (n - 1) -- A multiplicação ocorre *após* a chamada recursiva
    ```
    Aqui, após `factorial (n - 1)` retornar, seu resultado ainda precisa ser multiplicado por `n`.

-   **Função recursiva de cauda:**
    Para tornar uma função recursiva de cauda, geralmente se introduz um **acumulador** como parâmetro extra. Este acumulador carrega o resultado parcial da computação.

    ```haskell
    factorialTailRecursive :: Int -> Int -> Int -- O segundo Int é o acumulador
    factorialTailRecursive 0 acc = acc          -- Caso base: retorna o acumulador
    factorialTailRecursive n acc = factorialTailRecursive (n - 1) (n * acc) -- Chamada recursiva é a última coisa

    -- Função wrapper para interface usual
    factorial :: Int -> Int
    factorial n = factorialTailRecursive n 1 -- Inicia acumulador com 1
    ```
    Neste caso, `factorialTailRecursive (n - 1) (n * acc)` é a última operação. O valor de `n * acc` é calculado *antes* da chamada recursiva.

### Otimização de Chamada de Cauda (Tail Call Optimization - TCO)

A principal vantagem da recursão de cauda é que ela permite a **Otimização de Chamada de Cauda (TCO)**.
-   Quando uma chamada recursiva está em posição de cauda, o compilador pode otimizá-la para que não consuma novo espaço na pilha de chamadas (*call stack*).
-   Em vez de criar um novo quadro na pilha para a chamada recursiva, o quadro atual pode ser reutilizado. Essencialmente, a chamada recursiva é transformada em uma espécie de "salto" (goto) para o início da função com novos parâmetros.
-   Isso significa que funções recursivas de cauda podem executar em espaço de pilha constante, assim como os laços em linguagens imperativas, evitando erros de *stack overflow* mesmo para um grande número de "iterações" recursivas.

### Comparação

-   **Nem toda função recursiva é recursiva de cauda.**
    Exemplo: A definição padrão do fatorial `n * factorial (n-1)` não é de cauda.
-   **Toda função recursiva de cauda é recursiva.**

**Exemplo: Fatorial**

Definição não recursiva de cauda:
$n! = \begin{cases} 1, & \text{se } n = 0 \text{ ou } n = 1 \\ n \cdot (n-1)!, & \text{caso contrário} \end{cases}$
O retorno consiste no *produto* da chamada de $(n-1)!$ pelo parâmetro $n$.

Definição recursiva de cauda (usando uma função auxiliar com acumulador $m$):
$f(n, m) = \begin{cases} m, & \text{se } n = 0 \text{ ou } n = 1 \\ f(n-1, n \cdot m), & \text{caso contrário} \end{cases}$
Desde modo, $n! = f(n, 1)$.
Observe que a chamada recursiva $f(n-1, n \cdot m)$ agora consiste *apenas* em uma invocação da função $f$. O parâmetro $m$ é denominado **acumulador**.

A recursão de cauda permite a otimização de chamada de cauda (TCO). Isto porque, neste caso, é possível evitar o uso da pilha de execução, reaproveitando um único registro de ativação a cada chamada.

Compiladores Haskell (como GHC) são bons em realizar TCO.