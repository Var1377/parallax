pub enum Result<T, E> {
    Ok(T),
    Err(E)
} 


impl<T, E> Result<T, E> {
    fn map<U>(self, f: fn(T) -> U) -> Result<U, E> = match self {
        Ok(value) => Ok(f(value)),
        Err(error) => Err(error),
    };

    fn map_err<U>(self, f: fn(E) -> U) -> Result<T, U> = match self {
        Ok(value) => Ok(value),
        Err(error) => Err(f(error)),
    };
    
    fn and_then<U>(self, f: fn(T) -> Result<U, E>) -> Result<U, E> = match self {
        Ok(value) => f(value),
        Err(error) => Err(error),
    };
    
    fn or_else<U>(self, f: fn(E) -> Result<T, U>) -> Result<T, U> = match self {
        Ok(value) => Ok(value),
        Err(_) => f(self),
    };

    fn is_ok(self) -> bool = match self {
        Ok(_) => true,
        Err(_) => false,
    };
    
    fn is_err(self) -> bool = match self {
        Ok(_) => false,
        Err(_) => true,
    };
    
    fn unwrap(self) -> T = match self {
        Ok(value) => value,
        Err(error) => panic(error),
    };

    fn unwrap_err(self) -> E = match self {
        Ok(_) => panic("unwrap_err called on Ok"),
    };
}

