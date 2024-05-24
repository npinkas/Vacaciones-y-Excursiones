Vacaciones
Una empresa de turismo localizada en una isla que nadie sabe dónde está nos pide construir un software que los ayude a sacar estadísticas de los tours que ofrece a sus clientes: los turistas…
De cada turista nos interesa:
Sus niveles de cansancio y stress
Si está viajando solo
Los idiomas que habla

Para simplificar los problemas numéricos, cuando haya que usar números, pueden usar Int para no tener problemas con Floats y etcéteras.


La isla contiene varias excursiones para los turistas, por ahora nos pidieron modelar estas:

Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 
Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
Paseo en barco: depende de cómo esté la marea
si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
si está moderada, no pasa nada.
si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.
Nos avisaron que es común que, cada cierto tiempo, se vayan actualizando las excursiones que ofrecen, en base a las nuevas demandas que surgen en el mercado turístico. 
Se pide
Crear un modelo para los turistas y crear los siguientes tres ejemplos:
Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress.

____ Duda
Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga falta modificar las funciones existentes. 
___ 

Además:
Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.
Dada la función


deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
Por ejemplo, si “stress” es la función que me da el stress de un turista:
> deltaExcursionSegun stress ana irALaPlaya
-3     -- porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20)
Usar la función anterior para resolver cada uno de estos puntos:
Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.


Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones llamados tours. Un tour se compone por una serie de excursiones.
Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, y finaliza con una salida con gente local que habla "melmacquiano".
Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra isla: si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.
Modelar los tours para:
Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades como cantidad de excursiones tenga el tour, y luego realizar las excursiones en orden.

Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla.
Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour. 
La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.


Implementar y contestar en modo de comentarios o pruebas por consola
Construir un tour donde se visiten infinitas playas.
¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.
