module MiniProRH where

data Cargo = Estagiario | Programador | Coordenador | Gerente deriving Show

data Pessoa = Pessoa {cargo :: String, nome :: String} deriving Show

verSalario :: Pessoa -> Double
verSalario (Pessoa Estagiario _) = 1500
verSalario (Pessoa Programador _) = 5750.15
verSalario (Pessoa Coordenador _) = 8000
verSalario (Pessoa Gerente _) = 10807.20

verFolha :: Pessoa -> String
verFolha pessoa = "{nome: \"" ++ (nome pessoa) ++
                  "\", cargo: \"" ++ show (cargo pessoa) ++
                  "\", salario: " ++ show (verSalario pessoa) ++ "}"

promover :: Pessoa -> Pessoa
promover (Pessoa Estagiario n) = Pessoa
