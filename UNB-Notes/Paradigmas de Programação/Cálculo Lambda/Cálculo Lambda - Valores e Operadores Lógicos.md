## Sumário

1.  Valores lógicos
2.  Operadores Lógicos

## Contextualização

-   Sendo originalmente um sistema lógico, o cálculo $\lambda$ possui apenas dois termos primitivos: a letra grega lambda ($\lambda$) e o ponto final (.).
-   Os axiomas de construção de termos-$\lambda$ (expressões, aplicação e abstração) permitem a definição de novos termos a partir destes dois termos primitivos.
-   Deste modo, os valores lógicos da Lógica Proposicional Booleana (verdadeiro e falso) devem ser igualmente definidos como termos-$\lambda$.
-   As operações lógicas, que permitem a construção de proposições compostas, também devem ser definidas como termos-$\lambda$.

## Verdadeiro e Falso

O valor lógico **verdadeiro** pode ser representado pela expressão-$\lambda$:
$$ T \equiv \lambda xy.x $$
e o valor lógico **falso** pode ser representado por:
$$ F \equiv \lambda xy.y $$
**Observação:** veja que $T$ é, de fato, o combinador $\mathbf{K}$ e que $F$ é o combinador $\mathbf{K_*}$, o qual é extensionalmente igual ao combinador $\mathbf{SK}$, pois
$$ \mathbf{SK}xy \equiv \mathbf{K}y(xy) \equiv y \equiv Fxy $$

## if-then-else

Se $p$ é igual a $T$ ou a $F$, a expressão-$\lambda$
$$ \mathbf{IF} \equiv \lambda pab.pab $$
corresponde ao construto if-then-else.

**Observação:** para visualizar esta correspondência, observe que
$$ (\mathbf{IF})Tab \equiv Tab \equiv a $$
e que
$$ (\mathbf{IF})Fab \equiv Fab \equiv b $$

## Operadores Lógicos

Sejam $x, y \in \{T, F\}$. Os operadores da lógica proposicional booleana são:

1.  **conjunção:**
    $$ \land xy \equiv \lambda xy.xyx \equiv (\mathbf{IF})xyx $$
    (Notação comum: $x \land y$)
2.  **disjunção:**
    $$ \lor xy \equiv \lambda xy.xxy \equiv (\mathbf{IF})xxy $$
    (Notação comum: $x \lor y$)
3.  **negação:**
    $$ \neg x \equiv \lambda x.xFT \equiv (\mathbf{IF})xFT $$
    (Notação comum: $\neg x$)

## Operadores Lógicos e Combinadores

As operações lógicas são extensionalmente iguais aos combinadores dados a seguir, de modo que podem ser utilizadas como operações:

1.  **conjunção** (pós-fixada):
    $$ \mathbf{AND} \equiv F \equiv \mathbf{SK} $$
    (Uso: $xy \mathbf{AND}$)
2.  **disjunção** (in-fixada):
    $$ \mathbf{OR} \equiv T \equiv \mathbf{K} $$
    (Uso: $x \mathbf{OR} y$)
    *Nota: A definição original $\lambda xy.xxy$ para disjunção, quando aplicada como $T \mathbf{OR} T$, resultaria em $TTT \equiv T$. Se $x$ é $T$, $xxy \rightarrow TTy \rightarrow T$. Se $x$ é $F$, $xxy \rightarrow FFy \rightarrow y$. Então $x \lor y \equiv Txy \equiv (\mathbf{IF})xTy \equiv xTy$. A slide parece ter um $(\mathbf{IF})xxy$, que é $xxy$. Para $T \lor F \rightarrow TTF \rightarrow T$. Para $F \lor T \rightarrow FFT \rightarrow T$. Para $F \lor F \rightarrow FFF \rightarrow F$. A definição $\mathbf{OR} \equiv T$ não funciona como infixa diretamente sem uma estrutura que pegue o primeiro argumento. A definição $\lambda xy.xxy$ é mais robusta.*
    A slide indica $\mathbf{OR} \equiv T \equiv \mathbf{K}$. Se usarmos $x (\mathbf{OR}) y \equiv x T y$, então:
    $T (\mathbf{OR}) T \equiv TTT \equiv T$
    $T (\mathbf{OR}) F \equiv TTF \equiv T$
    $F (\mathbf{OR}) T \equiv FT T \equiv T$
    $F (\mathbf{OR}) F \equiv FTF \equiv F$
    Isso corresponde à disjunção.

3.  **negação** (pós-fixada):
    $$ \mathbf{NOT} \equiv FT \equiv (\mathbf{SK})\mathbf{K} $$
    (Uso: $x \mathbf{NOT}$)

## Exemplo: tabelas-verdade

**Conjunção:** (Usando $(xy)\mathbf{AND} \equiv xyF$)
$$ (TT)(\mathbf{AND}) \equiv TTF \equiv T(TF) \equiv T $$
$$ (TF)(\mathbf{AND}) \equiv TFF \equiv T(FF) \equiv F $$
$$ (FT)(\mathbf{AND}) \equiv FTF \equiv F(TF) \equiv F $$
$$ (FF)(\mathbf{AND}) \equiv FFF \equiv F(FF) \equiv F $$

**Disjunção:** (Usando $x \mathbf{OR} y \equiv xTy$)
$$ (T)(\mathbf{OR})(T) \equiv TTT \equiv T(TT) \equiv T $$
$$ (T)(\mathbf{OR})(F) \equiv TTF \equiv T(TF) \equiv T $$
$$ (F)(\mathbf{OR})(T) \equiv FTT \equiv F(TT) \equiv T $$
$$ (F)(\mathbf{OR})(F) \equiv FTF \equiv F(TF) \equiv F $$