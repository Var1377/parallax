pub enum Result<T, E> {
    Ok(T),
    Err(E)
} 


impl<T, E> Result<T, E> {
    fn map(self, f: fn(T) -> U) -> Result<U, E> = match self {
        Ok(value) => Ok(f(value)),
        Err(error) => Err(error),
    };

    fn map_err(self, f: fn(E) -> F) -> Result<T, F> = match self {
        Ok(value) => Ok(value),
        Err(error) => Err(f(error)),
    };
    
    fn and_then(self, f: fn(T) -> Result<U, E>) -> Result<U, E> = match self {
        Ok(value) => f(value),
        Err(error) => Err(error),
    };
    
    fn or_else(self, f: fn(E) -> Result<T, E>) -> Result<T, E> = match self {
        Ok(value) => Ok(value),
        Err(_) => f(self),
    };
}

