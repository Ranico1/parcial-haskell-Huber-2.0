
--PUNTO 1
data Chofer = Chofer {
nombre :: String,
kilometraje :: Int,
viajes :: [Viaje],
condiciones :: Condicion   
}

type Condicion = Viaje -> Bool 

data Viaje = Viaje {
fecha :: Int,
cliente :: Cliente,
costo :: Int    
}

data Cliente = Cliente {
nombreCliente :: String,
direccion :: String
}

-- MODELADO DE CONDICIONES PUNTO 2

ninguna :: Viaje -> Bool 
ninguna _ = True 

viajeCaro :: Viaje -> Bool 
viajeCaro unViaje = 200 < costo unViaje

clienteLargo :: Int -> Viaje -> Bool 
clienteLargo cantlLetras unViaje = longitudCliente (cliente unViaje) > cantlLetras 

longitudCliente :: Cliente -> Int 
longitudCliente unCliente = length (nombreCliente unCliente)

zonaComplicada :: String -> Viaje -> Bool
zonaComplicada zonaDeterminada unViaje = zonaDeterminada /= (direccion.cliente) unViaje 

--PUNTO 3

lucas = Cliente {
nombreCliente = "Lucas",
direccion = "Victoria"
}

daniel = Chofer {
nombre = "Daniel",
kilometraje = 23500,
viajes = [viajeLucas],
condiciones = zonaComplicada "Olivos"
}

viajeLucas = Viaje {
fecha = 20042017,
costo = 150,
cliente = lucas
}

alejandra = Chofer {
nombre = "Alejandra",
kilometraje = 180000,
viajes = [],
condiciones = ninguna
}

--PUNTO 4
tomarViaje :: Viaje -> Chofer -> Bool
tomarViaje unViaje unChofer = condiciones unChofer unViaje 

--PUNTO 5 
saberLiquidacion :: Chofer -> Int 
saberLiquidacion = sumarViajes .viajes 

sumarViajes :: [Viaje] -> Int 
sumarViajes = sum.map costo

--PUNTO 6 


realizarViaje :: [Chofer] -> Viaje -> Chofer
realizarViaje choferes unViaje = (anadirViaje unViaje.queMenosTrabajo.puedeRealizarViaje unViaje) choferes 

queMenosTrabajo :: [Chofer] -> Chofer
queMenosTrabajo = foldl1 ordenarLista 

ordenarLista :: Chofer -> Chofer -> Chofer
ordenarLista unChofer otroChofer 
    | (length.viajes) unChofer < (length.viajes) otroChofer = unChofer
    | otherwise = otroChofer


puedeRealizarViaje :: Viaje -> [Chofer] -> [Chofer]
puedeRealizarViaje unViaje = filter (tomarViaje unViaje) 

anadirViaje :: Viaje -> Chofer -> Chofer 
anadirViaje unViaje = mapViajes (unViaje:) 

mapViajes :: ([Viaje]-> [Viaje]) -> Chofer -> Chofer
mapViajes f unChofer = unChofer {viajes = f $ viajes unChofer}