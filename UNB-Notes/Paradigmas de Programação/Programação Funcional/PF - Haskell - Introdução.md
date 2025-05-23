**Haskell** é uma linguagem de programação moderna, **puramente funcional**, de propósito geral.

### Características Principais:

-   **[[PF - Valoração Não-Estrita (Lazy Evaluation)|Valoração não-estrita (Lazy Evaluation)]]:** Expressões são avaliadas apenas quando seus resultados são necessários. Isso permite, por exemplo, a manipulação de estruturas de dados infinitas.
-   **[[PF - Haskell - Sistema de Tipos|Sistema de tipagem forte e estático]]:** Os tipos são verificados em tempo de compilação, prevenindo muitos erros comuns. Haskell também possui **inferência de tipos**, o que significa que na maioria das vezes não é necessário declarar explicitamente os tipos das funções e variáveis, pois o compilador pode deduzi-los.
-   **[[PF - Haskell - Sistema de Tipos#Polimorfismo|Polimorfismo de Tipos]]:** Permite escrever funções genéricas que operam sobre uma variedade de tipos.
-   **[[PF - Funções de Primeira Classe e Alta Ordem|Funções de alta ordem]]:** Funções podem ser passadas como argumentos, retornadas de outras funções e armazenadas em estruturas de dados.
-   **Pureza:** Funções puras (sem [[PF - Funções Puras e Efeitos Colaterais|efeitos colaterais]]) são o padrão. Efeitos colaterais são gerenciados explicitamente através do sistema de tipos (ex: mônada `IO`).

### Aspectos Numéricos:

-   Oferece inteiros de precisão arbitrária (`Integer`).
-   Números racionais.
-   Números em ponto flutuante (`Float`, `Double`).
-   Variáveis booleanas (`Bool`).

### Origem e Motivações:

-   Foi desenvolvida por um comitê de pesquisadores e acadêmicos. A primeira versão data de 1990.
-   Principais motivações para a criação do Haskell:
    i.  Unificar os esforços de dezenas de diferentes linguagens funcionais existentes na época.
    ii. Ter uma linguagem funcional simples e apropriada para ensino.
    iii. Criar uma linguagem funcional padrão e de código livre (*open source*).

### GHC (Glasgow Haskell Compiler)

**GHC** é o principal compilador e ambiente de desenvolvimento para Haskell. É um projeto de código aberto e inclui:

-   **`ghc`**: O compilador em si, que traduz código Haskell para código de máquina eficiente.
-   **`runghc`**: Um interpretador que permite executar scripts Haskell sem compilação explícita.
-   **`ghci`**: Um ambiente interativo (REPL - Read-Eval-Print Loop) para experimentar com código Haskell, testar funções e carregar módulos.

**Características do GHC:**
-   Bom suporte para paralelismo e programação concorrente, gerando códigos rápidos.
-   Oferece, por padrão, uma vasta gama de bibliotecas.
-   Bibliotecas adicionais podem ser encontradas e instaladas a partir do **Hackage**, o repositório central de pacotes Haskell.

**Instalação (Exemplo em Linux):**
```bash
$ sudo apt-get install ghc
```
(Para outros sistemas, ou para uma instalação mais robusta e gerenciamento de versões, ferramentas como `ghcup` são recomendadas.)

**Usando o GHCi:**
Para rodar o GHCi, use o comando no terminal:
```bash
$ ghci
```
Isso abrirá o prompt do GHCi, geralmente `Prelude>`.
Para mudar o prompt do GHCi, use o comando `:set`:
```haskell
Prelude> :set prompt "ghci> "
ghci>
```
Para sair do GHCi, use `:quit` ou `:q`.