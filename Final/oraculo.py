# El Oraculo semantico para ver si baila 'mija con el 'ñor

# Utilizando la siguiente forma [x][y][z]
# [x] y [y] son los tipos
# Int   -> 0
# Float -> 1
# Char  -> 2
# Bool  -> 3

# [z] seria el operador
# SUMA               -> 0
# RESTA              -> 1
# MULTIPLICACION     -> 2
# DIVISION           -> 3
# GREATER THAN       -> 4
# LESS THAN          -> 5
# EQUAL              -> 6
# NOT EQUAL          -> 7
# AND                -> 8
# OR                 -> 9
# ASIGNAR            -> 10
# EQUAL GREATER THAN -> 11
# EQUAL LESSER THAN  -> 12

# El resultado seria la asignacion del valor al cubo semantico
# Error  -> -1
# Int    -> 0
# Float  -> 1
# Char   -> 2
# Bool   -> 3

# Declaracion del oraculo
oracle = [[[0 for k in range(13)] for j in range(5)] for i in range(5)]

# Asignacion de Valores del Oraculo [INT - INT]
oracle[0][0][0] = 0  # Int - Int - Suma    = Int
oracle[0][0][1] = 0  # Int - Int - Resta   = Int
oracle[0][0][2] = 0  # Int - Int - Mult    = Int
oracle[0][0][3] = 1  # Int - Int - Div     = Float
oracle[0][0][4] = 3  # Int - Int - GTHAN   = Bool
oracle[0][0][5] = 3  # Int - Int - LTHAN   = Bool
oracle[0][0][6] = 3  # Int - Int - Equal   = Bool
oracle[0][0][7] = 3  # Int - Int - NEqual  = Bool
oracle[0][0][8] = 3  # Int - Int - AND     = Bool
oracle[0][0][9] = 3  # Int - Int - OR      = Bool
oracle[0][0][10] = 3 # Int - Int - ASIGNAR = Bool
oracle[0][0][11] = 3 # Int - Int - EGTHAN  = Bool
oracle[0][0][12] = 3 # Int - Int - ELTHAN  = Bool

# Asignacion de Valores del Oraculo [FLOAT - FLOAT]
oracle[1][1][0] = 1  # Float - Float - Suma    = Float
oracle[1][1][1] = 1  # Float - Float - Resta   = Float
oracle[1][1][2] = 1  # Float - Float - Mult    = Float
oracle[1][1][3] = 1  # Float - Float - Div     = Float
oracle[1][1][4] = 3  # Float - Float - GTHAN   = Bool
oracle[1][1][5] = 3  # Float - Float - LTHAN   = Bool
oracle[1][1][6] = 3  # Float - Float - Equal   = Bool
oracle[1][1][7] = 3  # Float - Float - NEqual  = Error
oracle[1][1][8] = 3  # Float - Float - AND     = Bool
oracle[1][1][9] = 3  # Float - Float - OR      = Bool
oracle[1][1][10] = 1 # Float - Float - ASIGNAR = Float
oracle[1][1][11] = 3 # Float - Float - EGTHAN  = Bool
oracle[1][1][12] = 3 # Float - Float - ELTHAN  = Bool

# Asignacion de Valores del Oraculo [INT - FLOAT]
oracle[0][1][0] = 1   # Int - Float - Suma    = Float
oracle[0][1][1] = 1   # Int - Float - Resta   = Float
oracle[0][1][2] = 1   # Int - Float - Mult    = Float
oracle[0][1][3] = 1   # Int - Float - Div     = Float
oracle[0][1][4] = 3   # Int - Float - GTHAN   = Bool
oracle[0][1][5] = 3   # Int - Float - LTHAN   = Bool
oracle[0][1][6] = 3   # Int - Float - Equal   = Bool
oracle[0][1][7] = 3   # Int - Float - NEqual  = Bool
oracle[0][1][8] = 3   # Int - Float - AND     = Bool
oracle[0][1][9] = 3   # Int - Float - OR      = Bool
oracle[0][1][10] = 1  # Int - Float - ASIGNAR = Float
oracle[0][1][11] = 3  # Int - Float - EGTHAN  = Bool
oracle[0][1][12] = 3  # Int - Float - ELTHAN  = Bool

# Asignacion de Valores del Oraculo [FLOAT - INT]
oracle[1][0][0] = 1   # Float - Int - Suma    = Float
oracle[1][0][1] = 1   # Float - Int - Resta   = Float
oracle[1][0][2] = 1   # Float - Int - Mult    = Float
oracle[1][0][3] = 1   # Float - Int - Div     = Float
oracle[1][0][4] = 3   # Float - Int - GTHAN   = Bool
oracle[1][0][5] = 3   # Float - Int - LTHAN   = Bool
oracle[1][0][6] = 3   # Float - Int - Equal   = Bool
oracle[1][0][7] = 3   # Float - Int - NEqual  = Bool
oracle[1][0][8] = 3   # Float - Int - AND     = Bool
oracle[1][0][9] = 3   # Float - Int - OR      = Bool
oracle[1][0][10] = 1  # Float - Int - ASIGNAR = Float
oracle[1][0][11] = 3  # Float - Int - EGTHAN  = Bool
oracle[1][0][12] = 3  # Float - Int - ELTHAN  = Bool

# Asignacion de Valores del Oraculo [CHAR - CHAR]
oracle[2][2][0] = -1   # Char - Char - Suma    = Error
oracle[2][2][1] = -1   # Char - Char - Resta   = Error
oracle[2][2][2] = -1   # Char - Char - Mult    = Error
oracle[2][2][3] = -1   # Char - Char - Div     = Error
oracle[2][2][4] = -1   # Char - Char - GTHAN   = Error
oracle[2][2][5] = -1   # Char - Char - LTHAN   = Error
oracle[2][2][6] = 3    # Char - Char - Equal   = Bool
oracle[2][2][7] = 3    # Char - Char - NEqual  = Bool
oracle[2][2][8] = -1   # Char - Char - AND     = Error
oracle[2][2][9] = -1   # Char - Char - OR      = Error
oracle[2][2][10] = 2   # Char - Char - ASIGNAR = Char
oracle[2][2][11] = -1  # Char - Char - EGTHAN  = Error
oracle[2][2][12] = -1  # Char - Char - ELTHAN  = Error

# Asignacion de Valores del Oraculo [CHAR - INT]
oracle[2][0][0] = -1   # Char - Int - Suma    = Error
oracle[2][0][1] = -1   # Char - Int - Resta   = Error
oracle[2][0][2] = -1   # Char - Int - Mult    = Error
oracle[2][0][3] = -1   # Char - Int - Div     = Error
oracle[2][0][4] = -1   # Char - Int - GTHAN   = Error
oracle[2][0][5] = -1   # Char - Int - LTHAN   = Error
oracle[2][0][6] = -1   # Char - Int - Equal   = Error
oracle[2][0][7] = -1   # Char - Int - NEqual  = Error
oracle[2][0][8] = -1   # Char - Int - AND     = Error
oracle[2][0][9] = -1   # Char - Int - OR      = Error
oracle[2][0][10] = -1  # Char - Int - ASIGNAR = Error
oracle[2][0][11] = -1  # Char - Int - EGTHAN  = Error
oracle[2][0][12] = -1  # Char - Int - ELTHAN  = Error

# Asignacion de Valores del Oraculo [CHAR - FLOAT]
oracle[2][1][0] = -1   # Char - Float - Suma    = Error
oracle[2][1][1] = -1   # Char - Float - Resta   = Error
oracle[2][1][2] = -1   # Char - Float - Mult    = Error
oracle[2][1][3] = -1   # Char - Float - Div     = Error
oracle[2][1][4] = -1   # Char - Float - GTHAN   = Error
oracle[2][1][5] = -1   # Char - Float - LTHAN   = Error
oracle[2][1][6] = -1   # Char - Float - Equal   = Error
oracle[2][1][7] = -1   # Char - Float - NEqual  = Error
oracle[2][1][8] = -1   # Char - Float - AND     = Error
oracle[2][1][9] = -1   # Char - Float - OR      = Error
oracle[2][1][10] = -1  # Char - Float - ASIGNAR = Error
oracle[2][1][11] = -1  # Char - Float - EGTHAN  = Error
oracle[2][1][12] = -1  # Char - Float - ELTHAN  = Error

# Asignacion de Valores del Oraculo [INT - CHAR]
oracle[0][2][0] = -1   # Int - Char - Suma    = Error
oracle[0][2][1] = -1   # Int - Char - Resta   = Error
oracle[0][2][2] = -1   # Int - Char - Mult    = Error
oracle[0][2][3] = -1   # Int - Char - Div     = Error
oracle[0][2][4] = -1   # Int - Char - GTHAN   = Error
oracle[0][2][5] = -1   # Int - Char - LTHAN   = Error
oracle[0][2][6] = -1   # Int - Char - Equal   = Error
oracle[0][2][7] = -1   # Int - Char - NEqual  = Error
oracle[0][2][8] = -1   # Int - Char - AND     = Error
oracle[0][2][9] = -1   # Int - Char - OR      = Error
oracle[0][2][10] = -1  # Int - Char - ASIGNAR = Error
oracle[0][2][11] = -1  # Int - Char - EGTHAN  = Error
oracle[0][2][12] = -1  # Int - Char - ELTHAN  = Error

# Asignacion de Valores del Oraculo [FLOAT - CHAR]
oracle[1][2][0] = -1   # Float - Char - Suma    = Error
oracle[1][2][1] = -1   # Float - Char - Resta   = Error
oracle[1][2][2] = -1   # Float - Char - Mult    = Error
oracle[1][2][3] = -1   # Float - Char - Div     = Error
oracle[1][2][4] = -1   # Float - Char - GTHAN   = Error
oracle[1][2][5] = -1   # Float - Char - LTHAN   = Error
oracle[1][2][6] = -1   # Float - Char - Equal   = Error
oracle[1][2][7] = -1   # Float - Char - NEqual  = Error
oracle[1][2][8] = -1   # Float - Char - AND     = Error
oracle[1][2][9] = -1   # Float - Char - OR      = Error
oracle[1][2][10] = -1  # Float - Char - ASIGNAR = Error
oracle[1][2][11] = -1  # Float - Char - EGTHAN  = Error
oracle[1][2][12] = -1  # Float - Char - ELTHAN  = Error

# Asignacion de Valores del Oraculo [BOOL - BOOL]
oracle[3][3][0] = -1   # Bool - Bool - Suma    = Error
oracle[3][3][1] = -1   # Bool - Bool - Resta   = Error
oracle[3][3][2] = -1   # Bool - Bool - Mult    = Error
oracle[3][3][3] = -1   # Bool - Bool - Div     = Error
oracle[3][3][4] = -1   # Bool - Bool - GTHAN   = Error
oracle[3][3][5] = -1   # Bool - Bool - LTHAN   = Error
oracle[3][3][6] = 3    # Bool - Bool - Equal   = Bool
oracle[3][3][7] = 3    # Bool - Bool - NEqual  = Bool
oracle[3][3][8] = 3    # Bool - Bool - AND     = Bool
oracle[3][3][9] = 3    # Bool - Bool - OR      = Bool
oracle[3][3][10] = -1  # Bool - Bool - ASIGNAR = Error
oracle[3][3][11] = -1  # Bool - Bool - EGTHAN  = Error
oracle[3][3][12] = -1  # Bool - Bool - ELTHAN  = Error

# Asignacion de Valores del Oraculo [BOOL - INT]
oracle[3][0][0] = -1   # Bool - Int - Suma    = Error
oracle[3][0][1] = -1   # Bool - Int - Resta   = Error
oracle[3][0][2] = -1   # Bool - Int - Mult    = Error
oracle[3][0][3] = -1   # Bool - Int - Div     = Error
oracle[3][0][4] = -1   # Bool - Int - GTHAN   = Error
oracle[3][0][5] = -1   # Bool - Int - LTHAN   = Error
oracle[3][0][6] = -1   # Bool - Int - Equal   = Error
oracle[3][0][7] = -1   # Bool - Int - NEqual  = Error
oracle[3][0][8] = -1   # Bool - Int - AND     = Error
oracle[3][0][9] = -1   # Bool - Int - OR      = Error
oracle[3][0][10] = -1  # Bool - Int - ASIGNAR = Error
oracle[3][0][11] = -1  # Bool - Int - EGTHAN  = Error
oracle[3][0][12] = -1  # Bool - Int - ELTHAN  = Error

# Asignacion de Valores del Oraculo [BOOL - FLOAT]
oracle[3][1][0] = -1   # Bool - Float - Suma    = Error
oracle[3][1][1] = -1   # Bool - Float - Resta   = Error
oracle[3][1][2] = -1   # Bool - Float - Mult    = Error
oracle[3][1][3] = -1   # Bool - Float - Div     = Error
oracle[3][1][4] = -1   # Bool - Float - GTHAN   = Error
oracle[3][1][5] = -1   # Bool - Float - LTHAN   = Error
oracle[3][1][6] = -1   # Bool - Float - Equal   = Error
oracle[3][1][7] = -1   # Bool - Float - NEqual  = Error
oracle[3][1][8] = -1   # Bool - Float - AND     = Error
oracle[3][1][9] = -1   # Bool - Float - OR      = Error
oracle[3][1][10] = -1  # Bool - Float - ASIGNAR = Error
oracle[3][1][11] = -1  # Bool - Float - EGTHAN  = Error
oracle[3][1][12] = -1  # Bool - Float - ELTHAN  = Error

# Asignacion de Valores del Oraculo [BOOL - CHAR]
oracle[3][2][0] = -1   # Bool - Char - Suma    = Error
oracle[3][2][1] = -1   # Bool - Char - Resta   = Error
oracle[3][2][2] = -1   # Bool - Char - Mult    = Error
oracle[3][2][3] = -1   # Bool - Char - Div     = Error
oracle[3][2][4] = -1   # Bool - Char - GTHAN   = Error
oracle[3][2][5] = -1   # Bool - Char - LTHAN   = Error
oracle[3][2][6] = -1   # Bool - Char - Equal   = Error
oracle[3][2][7] = -1   # Bool - Char - NEqual  = Error
oracle[3][2][8] = -1   # Bool - Char - AND     = Error
oracle[3][2][9] = -1   # Bool - Char - OR      = Error
oracle[3][2][10] = -1  # Bool - Char - ASIGNAR = Error
oracle[3][2][11] = -1  # Bool - Char - EGTHAN  = Error
oracle[3][2][12] = -1  # Bool - Char - ELTHAN  = Error

# Asignacion de Valores del Oraculo [INT - BOOL]
oracle[0][3][0] = -1   # Int - Bool - Suma    = Error
oracle[0][3][1] = -1   # Int - Bool - Resta   = Error
oracle[0][3][2] = -1   # Int - Bool - Mult    = Error
oracle[0][3][3] = -1   # Int - Bool - Div     = Error
oracle[0][3][4] = -1   # Int - Bool - GTHAN   = Error
oracle[0][3][5] = -1   # Int - Bool - LTHAN   = Error
oracle[0][3][6] = -1   # Int - Bool - Equal   = Error
oracle[0][3][7] = -1   # Int - Bool - NEqual  = Error
oracle[0][3][8] = -1   # Int - Bool - AND     = Error
oracle[0][3][9] = -1   # Int - Bool - OR      = Error
oracle[0][3][10] = -1  # Int - Bool - ASIGNAR = Error
oracle[0][3][11] = -1  # Int - Bool - EGTHAN  = Error
oracle[0][3][12] = -1  # Int - Bool - ELTHAN  = Error

# Asignacion de Valores del Oraculo [FLOAT - BOOL]
oracle[1][3][0] = -1   # Float - Bool - Suma    = Error
oracle[1][3][1] = -1   # Float - Bool - Resta   = Error
oracle[1][3][2] = -1   # Float - Bool - Mult    = Error
oracle[1][3][3] = -1   # Float - Bool - Div     = Error
oracle[1][3][4] = -1   # Float - Bool - GTHAN   = Error
oracle[1][3][5] = -1   # Float - Bool - LTHAN   = Error
oracle[1][3][6] = -1   # Float - Bool - Equal   = Error
oracle[1][3][7] = -1   # Float - Bool - NEqual  = Error
oracle[1][3][8] = -1   # Float - Bool - AND     = Error
oracle[1][3][9] = -1   # Float - Bool - OR      = Error
oracle[1][3][10] = -1  # Float - Bool - ASIGNAR = Error
oracle[1][3][11] = -1  # Float - Bool - EGTHAN  = Error
oracle[1][3][12] = -1  # Float - Bool - ELTHAN  = Error

# Asignacion de Valores del Oraculo [CHAR - BOOL]
oracle[2][3][0] = -1   # Char - Bool - Suma    = Error
oracle[2][3][1] = -1   # Char - Bool - Resta   = Error
oracle[2][3][2] = -1   # Char - Bool - Mult    = Error
oracle[2][3][3] = -1   # Char - Bool - Div     = Error
oracle[2][3][4] = -1   # Char - Bool - GTHAN   = Error
oracle[2][3][5] = -1   # Char - Bool - LTHAN   = Error
oracle[2][3][6] = -1   # Char - Bool - Equal   = Error
oracle[2][3][7] = -1   # Char - Bool - NEqual  = Error
oracle[2][3][8] = -1   # Char - Bool - AND     = Error
oracle[2][3][9] = -1   # Char - Bool - OR      = Error
oracle[2][3][10] = -1  # Char - Bool - ASIGNAR = Error
oracle[2][3][11] = -1  # Char - Bool - EGTHAN  = Error
oracle[2][3][12] = -1  # Char - Bool - ELTHAN  = Error


def validar(x, y, z):
    miTriada = {
        "int": 0,
        "float": 1,
        "char": 2,
        "bool": 3
    }

    misOperadores = {
        "+": 0,
        "-": 1,
        "*": 2,
        "/": 3,
        ">": 4,
        "<": 5,
        "==": 6,
        "!=": 7,
        "&": 8,
        "|": 9,
        "=": 10,
        ">=": 11,
        "<=": 12
    }

    ret = oracle[miTriada[x]][miTriada[y]][misOperadores[z]]

    return ret