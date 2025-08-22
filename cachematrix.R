## makeCacheMatrix: Esta función crea un objeto de tipo "matriz" especial que puede almacenar en caché su inversa.
## El objeto es en realidad una lista de cuatro funciones:
## 1. set: para establecer la matriz
## 2. get: para obtener la matriz
## 3. setinverse: para establecer el valor de la inversa en caché
## 4. getinverse: para obtener el valor de la inversa de la caché

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL  # 'm_inv' será donde se almacene la inversa en caché
  
  # Función para establecer la matriz. Si la matriz cambia, la inversa se reinicia a NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  
  # Función para obtener la matriz
  get <- function() x
  
  # Función para establecer el valor de la inversa
  setinverse <- function(inverse) m_inv <<- inverse
  
  # Función para obtener el valor de la inversa
  getinverse <- function() m_inv
  
  # Devuelve una lista de las funciones
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: Esta función calcula la inversa de la matriz especial devuelta por makeCacheMatrix.
## Primero, verifica si la inversa ya ha sido calculada y si la matriz no ha cambiado.
## Si la inversa está en caché, la recupera y la devuelve. De lo contrario, la calcula y la almacena en caché.

cacheSolve <- function(x, ...) {
  m_inv <- x$getinverse()
  
  # Si 'm_inv' no es NULL, la inversa ya está en caché
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  
  # Si no está en caché, la calcula
  data <- x$get()
  m_inv <- solve(data, ...)
  
  # La guarda en caché usando la función 'setinverse'
  x$setinverse(m_inv)
  
  # Devuelve el valor calculado
  m_inv
}

git add cachematrix.R
git commit -m "Solucion a la tarea de caching de matrices en R"
git push



