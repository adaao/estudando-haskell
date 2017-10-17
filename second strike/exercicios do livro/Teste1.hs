module Teste1 where


data Cargo = Estagiario | Programador | Coordenaador | Gerente deriving Show

data Pessoa = Pessoa {cargo :: Cargo, nome :: String} deriving Show

verSalario :: Pessoa -> Double
verSalario (Pessoa Estagiario _) = 1500
verSalario (Pessoa Programador _) = 5750.15
verSalario (Pessoa Coordenaador _) = 8000
verSalario (Pessoa Gerente _) = 10807.20

verFolha :: Pessoa -> String
verFolha p = "{nome: \"" ++ (nome p) ++
            "\", cargo: " ++ show (cargo p) ++
            "\", salario: " ++ show (verSalario p) ++ "}"

promover :: Pessoa -> Pessoa
promover (Pessoa Estagiario n) = Pessoa Programador n
promover (Pessoa Programador n) = Pessoa Coordenaador n
promover (Pessoa Coordenaador n) = Pessoa Gerente n
promover (Pessoa _ n) = Pessoa Gerente n