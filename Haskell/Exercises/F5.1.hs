import System.IO (isEOF)

main ::  IO ()
main = do {
    conteudo <- getContents;
    let linha = length (lines content);
        letra = length (words content);
        bytes = length content;
    putStrLn $ "Linhas: " ++ show numLines;
    putStrLn $ "Palavras: " ++ show numWords;
    putStrLn $ "Bytes: " ++ show numBytes;
}