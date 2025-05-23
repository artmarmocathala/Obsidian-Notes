Em uma linguagem puramente funcional como Haskell, lidar com entrada e saída (I/O) – que são inerentemente operações com [[PF - Funções Puras e Efeitos Colaterais|efeitos colaterais]] – requer um tratamento especial para manter a pureza do restante do código.

## Código Puro vs. Código Impuro

-   Haskell determina uma clara separação entre **código puro** e **código impuro**.
    -   **Código puro:** Funções que não têm efeitos colaterais. Para as mesmas entradas, sempre produzem as mesmas saídas. Não interagem com o "mundo externo" (arquivos, console, rede, estado do sistema).
    -   **Código impuro:** Ações que interagem com o mundo externo, podendo produzir efeitos colaterais (ex: ler um arquivo, imprimir no console) ou retornar valores diferentes mesmo com as mesmas "entradas" nominais (ex: `getLine` pode retornar strings diferentes a cada chamada).
-   Esta estratégia permite que o código puro fique isento de efeitos colaterais, facilitando o raciocínio, testes e otimizações (como paralelização automática).
-   Como as rotinas de entrada e saída interagem com o mundo externo, todas elas produzem ou estão suscetíveis a efeitos colaterais, sendo assim, são tratadas como código impuro.

## O Tipo `IO`

Para gerenciar essa separação, Haskell usa o sistema de tipos. Operações de I/O são encapsuladas em valores do tipo `IO a`.
-   Um valor do tipo `IO a` representa uma **ação de I/O** que, quando executada, realizará alguma interação com o mundo externo e (potencialmente) produzirá um valor do tipo `a`.
-   Se a ação não produz um valor significativo (ex: `putStrLn`), o tipo é `IO ()`, onde `()` (lê-se "unit") é a tupla vazia, similar ao `void` em C/C++.

**Exemplos:**
-   `putStrLn :: String -> IO ()` (Recebe uma `String`, realiza a ação de imprimi-la, e não retorna valor útil).
-   `getLine :: IO String` (Realiza a ação de ler uma linha, e o resultado dessa ação é uma `String`).

## Ações de Entrada e Saída

-   A presença de `IO` na assinatura de uma função indica código impuro.
-   Uma ação de I/O (`IO a`) pode ser declarada e armazenada, mas só pode ser *executada* dentro de outra ação de I/O (ou, em última instância, pela função `main`).
-   **Função `main`:** O ponto de entrada de um programa Haskell é a função `main`, que deve ter o tipo `IO ()` (ou algum outro tipo `IO t`). O sistema de execução do Haskell é responsável por executar a ação `IO` definida em `main`.

## Blocos `do`

A palavra reservada `do` é usada para definir uma sequência de ações de I/O (ou outras ações monádicas).
-   Dentro de um bloco `do`, cada linha geralmente representa uma ação de I/O.
-   **Extraindo valores de ações `IO`:** O operador `<-` (seta para a esquerda) é usado dentro de um bloco `do` para executar uma ação `IO a` e "extrair" (atar) seu resultado do tipo `a` a uma variável.
    ```haskell
    main :: IO ()
    main = do
      putStrLn "Qual o seu nome?"
      nome <- getLine  -- getLine :: IO String; nome :: String
      putStrLn ("Olá, " ++ nome ++ "!")
    ```
-   O valor de um bloco `do` é o valor da última ação executada nele.

## `let` em Blocos `do`

A palavra reservada `let` (sem `in`) pode ser usada dentro de um bloco `do` para atar o resultado de **código puro** a uma variável. Essas variáveis podem então ser usadas nas ações de I/O subsequentes no mesmo bloco `do`.
```haskell
import Data.Char (toUpper)

capitalizeString :: String -> String
capitalizeString [] = []
capitalizeString (x:xs) = toUpper x : xs -- Função pura

main :: IO ()
main = do
  putStrLn "Digite uma palavra:"
  input <- getLine
  let capitalized = capitalizeString input -- 'capitalized' é o resultado de código puro
  putStrLn ("Capitalizado: " ++ capitalized)
```

## Leitura e Escrita de Strings em Console

-   **`putStrLn :: String -> IO ()`**: Escreve uma string no console, seguida de uma quebra de linha.
-   **`putStr :: String -> IO ()`**: Escreve uma string no console, sem quebra de linha.
-   **`getLine :: IO String`**: Lê uma string do console até encontrar uma quebra de linha (que é descartada) e a retorna.
-   **`getContents :: IO String`**: Lê toda a entrada do console (lazy) até EOF (End-Of-File) como uma única string.
-   **`readLn :: Read a => IO a`**: Lê uma linha e tenta convertê-la para um tipo `a` que seja instância da classe `Read`.
-   **`print :: Show a => a -> IO ()`**: Converte um valor de qualquer tipo `a` que seja instância de `Show` para uma `String` e a imprime no console, seguida de uma quebra de linha. (Equivalente a `putStrLn . show`).

**Exemplo de Leitura e Escrita:**
```haskell
-- Lê uma string do console e a imprime, sem modificações
-- O operador $ coloca tudo o que se segue entre parêntesis (baixa precedência)
main :: IO ()
main = do
  putStrLn "Insira uma string: "
  s <- getLine
  putStrLn $ "A string inserida foi '" ++ s ++ "'"
```

**Exemplo de Código Puro em um Bloco de I/O:**
```haskell
import Data.Char (toUpper, toLower)

-- Função pura para capitalizar palavras
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : map toLower xs

-- Função pura para formatar um nome completo
formattedName :: String -> String
formattedName name = unwords $ map capitalizeWord $ words name

main :: IO ()
main = do
  putStrLn "Insira seu nome completo: "
  name <- getLine
  let s = formattedName name -- Chamada de código puro
  putStrLn $ "Bom dia, Sr(a) " ++ s
```

## I/O em Arquivos

A biblioteca `System.IO` fornece funções para I/O em arquivos.

-   **`Handle`**: Um `Handle` é um tipo abstrato que representa uma conexão aberta com um arquivo (ou outro dispositivo de I/O).
-   **`openFile :: FilePath -> IOMode -> IO Handle`**: Abre um arquivo.
    -   `FilePath` é um sinônimo para `String` representando o caminho do arquivo.
    -   `IOMode` especifica o modo de abertura:
        -   `ReadMode`: Apenas leitura.
        -   `WriteMode`: Apenas escrita. Cria o arquivo se não existir, trunca se existir.
        -   `AppendMode`: Apenas escrita. Cria se não existir, adiciona ao final se existir.
        -   `ReadWriteMode`: Leitura e escrita.
-   **`hClose :: Handle -> IO ()`**: Fecha um arquivo associado a um `Handle`. É importante fechar os arquivos para liberar recursos e garantir que os dados sejam gravados.
-   **Funções com `h` (para `Handle`):** Muitas funções de I/O têm versões que operam sobre um `Handle` específico, prefixadas com `h`:
    -   `hPutStrLn :: Handle -> String -> IO ()`
    -   `hPutStr :: Handle -> String -> IO ()`
    -   `hGetLine :: Handle -> IO String`
    -   `hGetContents :: Handle -> IO String`
    -   `hIsEOF :: Handle -> IO Bool` (Verifica se o fim do arquivo foi alcançado para o handle).

**Exemplo de Manipulação de Arquivos (leitura e escrita linha a linha):**
```haskell
import System.IO
import Data.Char (toUpper)

-- Função para remover espaços extras e capitalizar a primeira letra de cada palavra
stripped :: String -> String
stripped = unwords . map capitalizeFirst . words
  where
    capitalizeFirst [] = []
    capitalizeFirst (c:cs) = toUpper c : cs

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  mainLoop inh outh
  hClose inh
  hClose outh

mainLoop :: Handle -> Handle -> IO ()
mainLoop inh outh = do
  ineof <- hIsEOF inh
  if ineof
    then return () -- Ação IO que não faz nada, apenas satisfaz o tipo
    else do
      line <- hGetLine inh
      hPutStrLn outh $ stripped line
      mainLoop inh outh
```
*(Suponha que `input.txt` exista e `output.txt` será criado/sobrescrito).*

## `return` em Haskell

-   Em Haskell, a palavra reservada `return` tem um significado distinto do utilizado em linguagens imperativas como C/C++.
-   **`return :: Monad m => a -> m a`**: No contexto de `IO` (que é uma Mônada), `return :: a -> IO a`.
-   `return` **não** encerra prematuramente um bloco ou função.
-   Em vez disso, `return` pega um valor puro `a` e o "injeta" no contexto `IO`, criando uma ação `IO a` que, quando executada, não realiza nenhum efeito colateral, mas simplesmente produz o valor `a`.
-   Ele corresponde, de certa forma, ao inverso do operador `<-`. Enquanto `v <- ioAction` extrai `v` de `ioAction`, `return v` cria uma `ioAction` que produz `v`.
-   No exemplo `mainLoop` acima, `return ()` cria uma ação `IO ()` que não faz nada, servindo para terminar a recursão do loop quando o fim do arquivo é atingido.

## Controlando o Cursor de Leitura/Escrita

-   **`hTell :: Handle -> IO Integer`**: Retorna a posição atual do cursor no arquivo (em número de bytes a partir do início).
-   **`hSeek :: Handle -> SeekMode -> Integer -> IO ()`**: Permite atualizar a posição do cursor.
    -   `SeekMode`:
        -   `AbsoluteSeek`: Posição relativa ao início do arquivo.
        -   `RelativeSeek`: Posição relativa à posição atual.
        -   `SeekFromEnd`: Posição relativa ao fim do arquivo (offset geralmente negativo ou zero).

**Exemplo de Controle do Cursor (calcular tamanho do arquivo):**
```haskell
import System.IO

fileSize :: Handle -> IO Integer
fileSize h = do
  hSeek h SeekFromEnd 0 -- Move o cursor para o final do arquivo (offset 0 do fim)
  size <- hTell h       -- Pega a posição atual (que é o tamanho)
  -- Opcional: retornar o cursor para o início se for continuar lendo
  -- hSeek h AbsoluteSeek 0
  return size

main :: IO ()
main = do
  putStrLn "Insira o nome do arquivo:"
  path <- getLine
  h <- openFile path ReadMode
  size <- fileSize h
  print size
  hClose h
```

## Entrada e Saída Padrão do Sistema (`stdin`, `stdout`, `stderr`)

Haskell provê três `Handle`s pré-definidos, associados à entrada e saída padrão do sistema:
-   **`stdin :: Handle`**: Corresponde à entrada padrão (usualmente o teclado).
-   **`stdout :: Handle`**: Corresponde à saída padrão (usualmente o terminal/console).
-   **`stderr :: Handle`**: Corresponde à saída de erro padrão (usualmente o terminal/console).

As funções de I/O sem o prefixo `h` (como `getLine`, `putStrLn`, `print`) são geralmente definidas em termos destes handles padrão:
```haskell
-- Conceitualmente:
-- getLine  = hGetLine stdin
-- putStrLn = hPutStrLn stdout
-- print    = hPrint stdout -- hPrint é como print mas para um handle específico
```

## Removendo e Renomeando Arquivos

A biblioteca `System.Directory` oferece funções para manipular arquivos e diretórios diretamente no sistema de arquivos.

-   **`removeFile :: FilePath -> IO ()`**: Recebe o caminho para um arquivo e o remove.
    ```haskell
    ghci> :m +System.Directory
    ghci> :type removeFile
    removeFile :: FilePath -> IO ()
    ```
-   **`renameFile :: FilePath -> FilePath -> IO ()`**: Recebe dois parâmetros: o caminho do arquivo original e o caminho do novo nome/destino.
    ```haskell
    ghci> :type renameFile
    renameFile :: FilePath -> FilePath -> IO ()
    ```
    Se o arquivo de destino já existir, ele será removido (ou sobrescrito, dependendo do SO) para que então o arquivo original assuma seu novo caminho/nome.

**Exemplo de Manipulação de Nome de Arquivo (Normalização):**
```haskell
import Data.Char (isSpace, isAlphaNum, toLower)
import Data.List (span)
import System.Directory (renameFile)
import System.IO (putStrLn, getLine) -- Explicitando para clareza

-- Normaliza um nome de arquivo: minúsculas, substitui espaços por '_', remove caracteres inválidos
normalize :: FilePath -> FilePath
normalize infile =
  let (namePart, extPart) = span (\c -> c /= '.' && c /= '/') (reverse infile) -- Pega extensão primeiro
      reversedName = reverse namePart
      reversedExt = reverse extPart
      
      processName [] = []
      processName (c:cs)
        | isSpace c = '_' : processName cs
        | isAlphaNum c || c == '_' || c == '-' = toLower c : processName cs
        | otherwise = processName cs -- descarta outros caracteres

      normalizedName = processName reversedName
  in if null reversedExt then normalizedName else normalizedName ++ "." ++ processName (drop 1 reversedExt)


main :: IO ()
main = do
  putStrLn "Digite o nome do arquivo a normalizar:"
  infile <- getLine
  let outfile = normalize infile
  putStrLn $ "Nome original: " ++ infile
  putStrLn $ "Nome normalizado: " ++ outfile
  -- Exemplo de uso (comentado para não executar renomeação real sem um arquivo)
  -- writeFile infile "conteudo teste" -- Cria um arquivo para testar
  -- renameFile infile outfile
  -- putStrLn "Arquivo renomeado (se existia)."
```
*(O exemplo do slide para `normalize` é um pouco diferente e mais complexo, focando em `span` e `filter` de forma mais genérica. A versão acima é uma tentativa de normalização mais típica de nomes de arquivo).*

## Lazy I/O

Haskell também oferece funções para utilizar [[PF - Valoração Não-Estrita (Lazy Evaluation)|lazy evaluation]] no contexto de I/O, o que pode levar a um estilo de programação mais funcional para processamento de arquivos.

-   **`hGetContents :: Handle -> IO String`**:
    -   Recebe um `Handle` e retorna uma `IO String`.
    -   A `String` retornada contém *todo* o conteúdo do arquivo a partir do ponto onde está localizado o cursor de leitura.
    -   Crucialmente, o acesso a este conteúdo é feito **por demanda (lazy)**. A string não contém, necessariamente, todo o conteúdo do arquivo de uma só vez em memória.
    -   Este acesso é feito de forma transparente ao usuário que, para fins de código, terá uma `String` que pode ser utilizada em código puro.
    -   Isso permite escrever código com I/O mais próximo da abordagem funcional, sem a necessidade de laços explícitos para processamento de blocos de informações, como é feito em linguagens imperativas.

**Exemplo de I/O no Estilo Funcional (usando `hGetContents`):**
```haskell
import System.IO
import Data.Char (toUpper)

-- Função pura para processar o conteúdo
strippedContent :: String -> String
strippedContent = unlines . map (map toUpper . unwords . words) . lines
-- 1. lines: quebra o conteúdo em uma lista de linhas (Strings)
-- 2. map (unwords . words): para cada linha, quebra em palavras, junta de volta com um espaço
--    (isso normaliza múltiplos espaços entre palavras para um único espaço)
-- 3. map (map toUpper): para cada linha processada, converte todos os seus caracteres para maiúsculo
-- 4. unlines: junta as linhas processadas de volta em uma única String com '\n'

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode

  input <- hGetContents inh -- Leitura lazy de todo o conteúdo
  -- 'input' é uma String que pode ser passada para funções puras

  hPutStrLn outh $ strippedContent input -- Processa e escreve

  hClose inh -- Ainda é bom fechar os handles, especialmente o de escrita
  hClose outh
```

### Funções `readFile` e `writeFile`

O idioma de abrir um arquivo, ler seu conteúdo, processar este conteúdo, escrever o resultado em um arquivo de saída e fechar os handles é tão comum que Haskell oferece duas funções que abstraem este processo, utilizando lazy I/O:

-   **`readFile :: FilePath -> IO String`**:
    -   Recebe o caminho do arquivo a ser aberto.
    -   Retorna uma `IO String`, onde a `String` é o conteúdo completo do arquivo, acessado por meio de *lazy evaluation*.
    -   O handle do arquivo é gerenciado internamente (aberto e fechado).

-   **`writeFile :: FilePath -> String -> IO ()`**:
    -   Recebe o caminho do arquivo de saída e a `String` (conteúdo) a ser escrita.
    -   Cria/sobrescreve o arquivo com o conteúdo.
    -   O handle também é gerenciado internamente.

Ambas as funções fazem parte da biblioteca `System.IO`. Estas funções mantêm o handle dos arquivos internamente, dispensando a chamada explícita de `hClose`.

**Exemplo de I/O no Estilo Funcional sem Handles Explícitos:**
```haskell
import System.IO
import Data.Char (toUpper) -- Reutilizando do exemplo anterior

strippedContent :: String -> String
strippedContent = unlines . map (map toUpper . unwords . words) . lines

main :: IO ()
main = do
  input <- readFile "input.txt" -- Leitura lazy
  writeFile "output.txt" $ strippedContent input -- Escrita
  putStrLn "Processamento concluído!"
```

## Interações (`interact`)

Quando os arquivos a serem processados são a entrada e a saída padrão, o idioma abstraído pelas funções `readFile` e `writeFile` pode ser condensado ainda mais em uma única função: `interact`.

-   **`interact :: (String -> String) -> IO ()`**:
    -   Tem como parâmetro uma função pura que recebe uma `String` e retorna uma `String`.
    -   `interact` lê *todo* o conteúdo da entrada padrão (lazy), passa-o como parâmetro para a função fornecida.
    -   O retorno (String) da função é então escrito na saída padrão.
-   Esta função também faz parte da biblioteca `System.IO`.
-   Quando utilizada em conjunto com filtros e outras funções de processamento de strings, `interact` permite escrever programas concisos, com poucas linhas, que processam dados de forma pipeline (estilo Unix).

**Exemplo de Interação com Filtro (remover comentários):**
```haskell
import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhile)
-- import System.IO (interact) -- interact está no Prelude

-- Função pura que processa o conteúdo da string
removeComments :: String -> String
removeComments = unlines . filter (not . isCommentLine) . lines
  where
    isCommentLine :: String -> Bool
    isCommentLine s = "--" `isPrefixOf` (dropWhile isSpace s)

main :: IO ()
main = interact removeComments
-- Para testar:
-- Salve como remove_comments.hs
-- Compile: ghc remove_comments.hs
-- Execute: ./remove_comments < arquivo_com_comentarios.txt > arquivo_sem_comentarios.txt
-- Ou interativamente: ./remove_comments
--   Digite linhas, algumas começando com --
--   Pressione Ctrl-D (Linux/macOS) ou Ctrl-Z Enter (Windows) para indicar EOF.
```
Exemplo do slide:
```haskell
-- Elimina os comentários do arquivo
import Data.Char
import Data.List

main = interact (unlines . f . lines) where
  -- Este comentário também será filtrado se o input vier de um arquivo
  -- que contenha esta linha e for processado por este programa.
  f = filter (not . isPrefixOf "--" . dropWhile isSpace)
```