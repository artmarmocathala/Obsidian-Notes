Variáveis imutáveis são aquelas cujo valor não pode ser modificado após sua inicialização.

-   Em linguagens imperativas, tais variáveis são frequentemente denominadas **constantes**.
-   Variáveis imutáveis são particularmente úteis em ambientes *multithread*, pois eliminam problemas de concorrência relacionados à modificação de dados compartilhados.
-   Em linguagens funcionais, uma variável é "atada" (bound) a um valor ou expressão em sua atribuição inicial. Ao longo do programa, ela não pode ser "desatada" ou reatada a uma outra expressão ou valor.

### Exemplo em Haskell

O código Haskell abaixo gera um erro de compilação, pois a variável `x` é imutável e há uma tentativa de redefini-la:

```haskell
main = print x
  where
    x = 1
    y = 2 * x
    x = 3 * y + 1 -- Erro! Tentativa de redefinir x
```
Neste caso, Haskell interpretaria a segunda definição de `x` como uma nova variável `x` sombreando a anterior dentro de um escopo mais interno, se a sintaxe permitisse, ou mais comumente, como uma definição recursiva mútua se não fosse um erro de redefinição no mesmo escopo `where`. No entanto, a intenção de "modificar" `x` não é permitida.