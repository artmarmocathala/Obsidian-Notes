## Sumário

1.  Números naturais
2.  Relações entre números naturais
3.  Adição e multiplicação

## Contexto

-   É esperado que uma linguagem de programação seja capaz de realizar operações aritméticas com números naturais.
-   Contudo, conforme dito anteriormente, o cálculo $\lambda$ contém apenas dois termos primitivos: o símbolo $\lambda$ e o ponto final.
-   Assim, como no caso dos valores lógicos, é preciso representar os números naturais por meio de expressões-$\lambda$.
-   Como os inteiros são infinitos, é preciso definir uma forma de deduzir todos eles a partir de algum valor inicial.

## Zero

Definição de zero:
O número natural **zero** pode ser representado pelo termo-$\lambda$:
$$ 0 \equiv \lambda sz.z $$
**Observação:** veja que, de acordo com a definição, acima $0 \equiv F$ (o valor lógico falso).

## Sucessor

O termo-$\lambda$
$$ S \equiv \lambda wyx.y(wyx) $$
é denominado função **sucessor**, ou simplesmente, sucessor, de um número natural.

**Observação:** a função sucessor permite a definição de todos os números naturais a partir do zero: $1 \equiv S0, 2 \equiv S1, \dots$

## Definição de 1
$$ \begin{aligned} 1 &\equiv S0 \\ &\equiv (\lambda wyx.y(wyx))(\lambda sz.z) \\ &\equiv (\lambda w.(\lambda yx.y(wyx)))(\lambda sz.z) \\ &\equiv (\lambda yx.y(wyx))[w := (\lambda sz.z)] \\ &\equiv \lambda yx.y((\lambda sz.z)yx) \\ &\equiv \lambda yx.y(x) \\ &\equiv \lambda sz.s(z) \end{aligned} $$
**Observação:** no último passo foram aplicadas duas conversões-α para renomear as variáveis $y$ e $x$ para $s$ e $z$ respectivamente, para manter a consistência com a forma padrão dos numerais de Church.

## Definição de 2
$$ \begin{aligned} 2 &\equiv S1 \\ &\equiv (\lambda wyx.y(wyx))(\lambda sz.s(z)) \\ &\equiv (\lambda w.(\lambda yx.y(wyx)))(\lambda sz.s(z)) \\ &\equiv (\lambda yx.y(wyx))[w := (\lambda sz.s(z))] \\ &\equiv \lambda yx.y((\lambda sz.s(z))yx) \\ &\equiv \lambda yx.y(y(x)) \\ &\equiv \lambda sz.s(s(z)) \end{aligned} $$
**Observação:** a definição dos naturais pode ser interpretada como composições de funções. Se $s$ é uma função, $0$ significa simplesmente retornar o argumento $z$; $1$ significa aplicar a função uma vez (isto é, $s(z)$); $2$ significa aplicar a função duas vezes: $s(s(z)) = s^2(z)$, e assim por diante.
Em geral, um número natural $n$ é representado como $\lambda sz.s^n(z)$.

## Teste Condicional (Função Z - IsZero)

O termo-$\lambda$
$$ Z \equiv \lambda n.n(\mathbf{K}F)T $$
(onde $T \equiv \lambda xy.x$, $F \equiv \lambda xy.y$, e $\mathbf{K} \equiv \lambda ab.a$. Assim, $\mathbf{K}F \equiv \lambda b.F$)
o qual chamaremos **função Z**, retorna verdadeiro ($T$) quando aplicada em $0$, e retorna falso ($F$) para qualquer outro número natural.

## Observações sobre a função Z

Para entender o comportamento da função $Z$:
-   $0yx \equiv (\lambda sz.z)yx \equiv x$.
    Isto é, quando aplicada ao termo $yx$, $0$ ignora o termo $y$ e retorna o argumento $x$.
-   O termo-$\lambda$ $Fz \equiv \lambda y.y \equiv \mathbf{I}$ (a função identidade, se $z$ for o primeiro argumento).
-   O natural $N$ aplica $N$ vezes o termo $y$ ao argumento $x$:
    $Nyx \equiv (\lambda sz.s^N(z))yx \equiv y^N(x) \equiv y(y(\dots y(x)\dots))$

Assim,
$$ \begin{aligned} Z0 &\equiv (\lambda n.n(\mathbf{K}F)T)0 \\ &\equiv 0(\mathbf{K}F)T \\ &\equiv (\lambda sz.z)(\mathbf{K}F)T \\ &\equiv T \end{aligned} $$
Para um natural $N \neq 0$ qualquer,
$$ \begin{aligned} ZN &\equiv (\lambda n.n(\mathbf{K}F)T)N \\ &\equiv N(\mathbf{K}F)T \\ &\equiv (\lambda sz.s^N(z))(\mathbf{K}F)T \\ &\equiv (\mathbf{K}F)^N(T) \\ &\equiv F \quad \text{(desde que N > 0)} \end{aligned} $$

## Pares

No cálculo-$\lambda$, o **par** $(a,b)$ pode ser representado pela expressão-$\lambda$:
$$ (a,b) \equiv \lambda z.zab $$
O **primeiro elemento** do par pode ser extraído a partir da aplicação desta expressão ao termo $T$:
$$ (\lambda z.zab)T \equiv Tab \equiv a $$
O **segundo elemento** é extraído por meio da aplicação da expressão ao termo $F$:
$$ (\lambda z.zab)F \equiv Fab \equiv b $$

## Par sucessor

**Proposição:** O termo-$\lambda$
$$ \Phi \equiv \lambda pz.z(S(pT))(pT) $$
transforma o par $(n, n-1)$ no par $(n+1, n)$. (Assumindo $(n, m) \equiv \lambda z.znm$).

**Demonstração:** De fato, o termo $(pT)$ extrai o primeiro elemento do par $p$, e o termo $S(pT)$ é o sucessor deste elemento. Assim,
Se $p \equiv (k, k') \equiv \lambda z.zk k'$. Então $pT \equiv Tkk' \equiv k$.
$$ \begin{aligned} \Phi((n, n-1)) &\equiv (\lambda pz.z(S(pT))(pT)) (\lambda w.w n (n-1)) \\ &\equiv \lambda z.z (S((\lambda w.w n (n-1))T)) ((\lambda w.w n (n-1))T) \\ &\equiv \lambda z.z (S n) n \\ &\equiv (n+1, n) \end{aligned} $$

## Antecessor

**Proposição:** A expressão-$\lambda$
$$ P \equiv \lambda n.n\Phi(\lambda z.z00)F $$
computa o **antecessor** de qualquer natural $N$ maior que zero, e $P0 = 0$.
O par inicial é $(0,0) \equiv \lambda z.z00$.
$\Phi$ transforma $(k, k')$ em $(k+1, k)$.
Aplicar $\Phi$ $N$ vezes a $(0,0)$ resulta em $(N, N-1)$.
Então $N\Phi(\lambda z.z00)$ resulta no par $(N, N-1)$.
Aplicar $F$ a este par extrai o segundo elemento, $N-1$.

**Demonstração:**
Temos que
$$ P0 \equiv (\lambda n.n\Phi(\lambda z.z00)F)0 \equiv 0\Phi(\lambda z.z00)F \equiv (\lambda z.z00)F \equiv F00 \equiv 0 $$
Seja $N$ um natural maior do que zero. Daí
$$ PN \equiv (\lambda n.n\Phi(\lambda z.z00)F)N \equiv N\Phi(\lambda z.z00)F $$
Isto é, $\Phi$ é aplicado $N$ vezes ao par $(0,0)$.
$\Phi^N(\lambda z.z00)$ resulta no par $(N, N-1) \equiv \lambda z.zN(N-1)$.
Então,
$$ PN \equiv (\lambda z.zN(N-1))F \equiv FN(N-1) \equiv N-1 $$

## Exemplo de antecessor
$$ \begin{aligned} P3 &\equiv ((\lambda n.n\Phi(\lambda z.z00)F))3 \\ &\equiv 3\Phi(\lambda z.z00)F \\ &\equiv \Phi(\Phi(\Phi(\lambda z.z00)))F \\ &\equiv \Phi(\Phi(\lambda z.z10))F \\ &\equiv \Phi(\lambda z.z21)F \\ &\equiv (\lambda z.z32)F \\ &\equiv F32 \equiv 2 \end{aligned} $$

## Desigualdade (Relação maior ou igual que)

Sejam $x$ e $y$ dois números naturais. O termo-$\lambda$
$$ G \equiv \lambda xy.Z(xPy) $$
retorna verdadeiro ($T$) se $x$ é maior ou igual que $y$, ou falso ($F$), caso contrário.
(Lembre-se que $xPy$ calcula $y-x$, resultando em $0$ se $y \le x$).

**Observação:** o termo $G$ pode ser interpretado da seguinte maneira: se o resultado de se aplicar $x$ vezes o antecessor $P$ no natural $y$ é zero, então $x \ge y$.

## Igualdade (Relação igual a)

O termo-$\lambda$
$$ E \equiv \lambda xy. \land (Gxy) (Gyx) $$
(Usando $\land$ definido como $\lambda ab.abF$, então $E \equiv \lambda xy. (Gxy)(Gyx)F$)
retorna verdadeiro ($T$) se $x$ e $y$ são números naturais iguais, e falso ($F$), caso contrário.

**Observação:** $x = y$ se $x \ge y$ e $y \ge x$.

## Adição

Seja $S$ a expressão-$\lambda$ que computa o sucessor de um número natural. O termo-$\lambda$
$$ + \equiv \lambda xy.xSy $$
corresponde à **adição** de números naturais. ($xSy$ aplica $S$, $x$ vezes a $y$).

**Observação:** de acordo com a definição acima, a adição ($+$) é uma operação pré-fixada.

## Exemplo de adição
$$ \begin{aligned} +23 &\equiv (\lambda xy.xSy)23 \\ &\equiv 2S3 \\ &\equiv S(S(3)) \\ &\equiv S(4) \\ &\equiv 5 \end{aligned} $$

## Multiplicação

A expressão-$\lambda$
$$ \times \equiv \lambda xyz.x(yz) $$
corresponde à **multiplicação** de números naturais.
Isto significa que $(\times M N)$ é a função $\lambda z.M(Nz)$.

**Observações:**
(a) Do mesmo modo que foi observado na adição, a multiplicação ($\times$) é uma operação pré-fixada.
(b) A interpretação desta expressão é: $x$ aplica a função $(yz)$ (que é $y$ aplicações de $z$) $x$ vezes. Mais precisamente, $(\times X Y)$ resulta no numeral Church que representa $X \times Y$. A função $Y$ é aplicada $X$ vezes à função $Z$.

## Exemplo de multiplicação

No exemplo a seguir, para calcular $\times 2 3$, a variável $s$ no slide corresponde ao $z$ na definição $\lambda xyz.x(yz)$ quando se espera um numeral de Church como resultado, ou seja, $(\times 2 3)$ deve ser $\lambda sz.s^6z$.
A expressão do slide é $\lambda xys.x(ys)$, que é consistente se $s$ for o primeiro argumento da função resultante (o "sucessor" nos numerais de Church).

Vamos usar a forma do slide $\times \equiv \lambda xys.x(ys)$.
Então $\times 2 3$ é $\lambda s.2(3s)$.
(Nota: $2 \equiv \lambda qz'.q(qz')$, e $3 \equiv \lambda rt.r(r(rt))$).
$$ \begin{aligned} \times 23 &\equiv (\lambda xys.x(ys))23 \\ &\equiv \lambda s.2(3s) \\ &\equiv \lambda s. (\lambda qz'.q(qz')) ((\lambda rt.r(r(rt)))s) \\ &\equiv \lambda s. (\lambda qz'.q(qz')) (\lambda t.s(s(st))) \\ &\equiv \lambda s. (\lambda z'_1. (\lambda t.s(s(st))) ( (\lambda t.s(s(st))) z'_1 ) ) \\ &\equiv \lambda s. \lambda z'_1. (\lambda t_1.s(s(s t_1))) (s(s(s z'_1))) \\ &\equiv \lambda s. \lambda z'_1. s(s(s( s(s(s z'_1)) ))) \\ &\equiv \lambda sz.s(s(s(s(s(s z))))) \equiv 6 \end{aligned} $$