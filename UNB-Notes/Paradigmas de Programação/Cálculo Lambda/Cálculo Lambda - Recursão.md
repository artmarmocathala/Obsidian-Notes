## Sumário

1.  Recursão
2.  Combinador Y

## Recursão

-   A recursão diz respeito a definição de uma função em termos de si mesma.
-   Ao contrário de outras notações, o cálculo $\lambda$ não permite esta definição diretamente, uma vez que os termos-$\lambda$ são anônimos.
-   Uma maneira de contornar isso é utilizar uma expressão-$\lambda$ que receba a si mesma como argumento.
-   Além disso, é preciso lidar com os dois aspectos fundamentais de uma função recursiva: o(s) caso(s) base(s) e a chamada recursiva.

## Estrutura básica da recursão

$$ \gamma(x) = \begin{cases} g(x), & \text{se } P(x), \\ h(x, \gamma), & \text{caso contrário} \end{cases} $$

-   $P(x)$ é um predicado que retorna verdadeiro se $x$ é o valor que caracteriza um caso base.
-   Se $P(x)$ for verdadeiro, o valor de $\gamma$ em $x$ será dado pela função $g$.
-   Caso contrário, $\gamma(x)$ será dado por $h(x, \gamma)$, onde $h$ é uma função que depende de $x$ e de $\gamma$.

## Representação da estrutura básica da recursão no cálculo-$\lambda$

$$ \Gamma \equiv \lambda \gamma x.(Px)(gx)(h \gamma x) $$
(Assumindo que $h$ é uma função que espera $\gamma$ e depois $x$, então $(h \gamma x)$ é $((h \gamma) x)$.)
Usando o if-then-else $\mathbf{IF} \equiv \lambda ctb.ctb$:
$$ \Gamma \equiv \lambda \gamma x.\mathbf{IF} (Px) (gx) (h \gamma x) $$

-   Observe que na definição da função recursiva $\Gamma$ é utilizado o termo-$\lambda$ $\mathbf{IF}$.
-   Se o predicado $(Px)$ retornar verdadeiro, o retorno será o primeiro parâmetro $(gx)$, que corresponde ao valor de $\Gamma$ para o caso base.
-   Se falso, será avaliada a função $h \gamma x$.
-   Não há garantias, contudo, que $\Gamma = \gamma$, pois no cálculo $\lambda$ os termos são anônimos.
-   É preciso, portanto, definir um termo que garanta esta equivalência.

## Teorema do Ponto Fixo

Para qualquer termo-$\lambda$ $G$ existe um termo $X$ tal que $GX \equiv X$.

**Demonstração:**
Seja $G$ um termo-$\lambda$ qualquer. Defina $W \equiv \lambda x.G(xx)$ e $X \equiv WW$.
Deste modo,
$$ X \equiv WW \equiv (\lambda x.G(xx))W \equiv G(WW) \equiv GX $$

## Combinador Y (Fixed-Point Combinator)

**Proposição (Combinador Y):**
O combinador $Y$
$$ Y \equiv \lambda f.(\lambda x.f(xx))(\lambda x.f(xx)) $$
é um termo-$\lambda$ tal que, para qualquer termo $G$,
$$ YG \equiv G(YG) $$

**Demonstração:**
Seja $G$ um termo-$\lambda$ qualquer. Daí
$$ \begin{aligned} YG &\equiv (\lambda f.(\lambda x.f(xx))(\lambda x.f(xx)))G \\ &\equiv (\lambda x.G(xx))(\lambda x.G(xx)) \quad \text{(β-redução, substituindo f por G)} \\ & \text{Seja } W_G = \lambda x.G(xx) \text{. Então } YG \equiv W_G W_G \\ &\equiv G(W_G W_G) \quad \text{(pela definição de } W_G \text{, aplicando } W_G \text{ a si mesmo)} \\ &\equiv G( (\lambda x.G(xx))(\lambda x.G(xx)) ) \\ &\equiv G(YG) \quad \text{(reconhecendo a expressão de YG dentro de G)} \end{aligned} $$

## Observações sobre o combinador Y

-   Veja que, para qualquer termo-$\lambda$ $G$, $YG$ é um ponto fixo de $G$.
-   Esta propriedade é o que faltava para a definição completa da recursão, pois ao aplicar $(YG)$ ao parâmetro $x$ da recursão, o resultado é
    $$ (YG)x \equiv G(YG)x $$
    ou seja, o termo $G$ é aplicado aos parâmetros $YG$ e $x$, o que permite invocar $G$ novamente quantas vezes forem necessárias.
-   Assim, para definir uma função recursiva $\gamma \equiv Y\Gamma$ no cálculo-$\lambda$, basta determinar o predicado $P$ e as funções $g$ e $h$ que compõem a função $\Gamma$.
    $$ \Gamma \equiv \lambda \gamma' x.(Px)(gx)(h \gamma' x) $$
    Então a função recursiva é $Y\Gamma$.

## Exemplo de recursão: fatorial

$$ !n = \begin{cases} 1, & \text{se } n = 0, \\ n \times !(n-1), & \text{caso contrário} \end{cases} $$

-   A notação está "invertida" para ficar consistente com a notação prefixada do cálculo lambda. (Ex: $\times n (!(P n))$)
-   Na notação de recursão do cálculo lambda,
    -   Predicado $P$: $Z$ (IsZero)
    -   Função caso base $g$: $\lambda x.1$ (retorna o numeral Church 1)
    -   Função passo recursivo $h$: $\lambda \text{fact} n. \times n (\text{fact} (Pn))$
        (onde $\times$ é a multiplicação, $P$ é o predecessor)
-   Deste modo, $! \equiv Y\Gamma_{fact}$, onde
    $$ \Gamma_{fact} \equiv \lambda f n.(Zn)1(\times n (f(Pn))) $$

## Exemplo de aplicação do fatorial

$$ \begin{aligned} !3 &\equiv (Y\Gamma_{fact})3 \\ &\equiv \Gamma_{fact}(Y\Gamma_{fact})3 \\ &\equiv (\lambda f n.(Zn)1(\times n (f(Pn)))) (Y\Gamma_{fact}) 3 \\ &\equiv (Z3)1(\times 3 ((Y\Gamma_{fact})(P3))) \\ &\equiv F1(\times 3 ((Y\Gamma_{fact})2)) \quad \text{(pois Z3 é F, P3 é 2)} \\ &\equiv \times 3 ((Y\Gamma_{fact})2) \\ &\equiv \times 3 (\Gamma_{fact}(Y\Gamma_{fact})2) \\ &\equiv \times 3 ( (Z2)1(\times 2 ((Y\Gamma_{fact})(P2))) ) \\ &\equiv \times 3 ( F1(\times 2 ((Y\Gamma_{fact})1)) ) \\ &\equiv \times 3 (\times 2 ((Y\Gamma_{fact})1)) \\ &\equiv \times 3 (\times 2 (\Gamma_{fact}(Y\Gamma_{fact})1)) \\ &\equiv \times 3 (\times 2 ( (Z1)1(\times 1 ((Y\Gamma_{fact})(P1))) )) \\ &\equiv \times 3 (\times 2 ( F1(\times 1 ((Y\Gamma_{fact})0)) )) \\ &\equiv \times 3 (\times 2 (\times 1 ((Y\Gamma_{fact})0))) \\ &\equiv \times 3 (\times 2 (\times 1 (\Gamma_{fact}(Y\Gamma_{fact})0))) \\ &\equiv \times 3 (\times 2 (\times 1 ( (Z0)1(\times 0 ((Y\Gamma_{fact})(P0))) ))) \\ &\equiv \times 3 (\times 2 (\times 1 ( T1(\times 0 ((Y\Gamma_{fact})0)) ))) \\ &\equiv \times 3 (\times 2 (\times 1 (1))) \\ &\equiv \times 3 (\times 2 1) \\ &\equiv \times 3 2 \\ &\equiv 6 \end{aligned} $$