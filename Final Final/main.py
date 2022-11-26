# Joel Hiram Chavez Verastegui
# A01177075
# Lenguaje de Programacion LEAD 2.0
# Última modificacion: 14 de Noviembre de 2022

#####Comentarios
# Lo mas probable es que cuando pones un int y un float la variable int
# se haga float por siempre y haya error porque no se actualiza que ahora
# es float
import oraculo
from procedures import FunctionDirectory as proc

import ply.lex
import ply.yacc

# Se inicializa la varibale proc, ya que no funciona el codigo
# ni las llamadas si no se hace esto antes
proc = proc()
######################
# Variables Globales #
######################
# El scope de la funcion actual (global o el nombre de la funcion)
global currentScope
# El tipo de la variable actual
global currentType
# Tipo de funcion actual
global currentFuncType
# Nombre de Variable actual
global currentVar
# Cantidad de Funciones
global cantFunc
# Tamanio de la variable en cuestion
global tamVar
# Operando Derecho
global rOperand
global rOperandType
# Operando Izquierdo
global lOperand
global lOperandType
# Operador
global operador
# Arreglo de Variables Temporales
global arrTemp
# Contador para Variables Temporales
global contVarTemp
# Variable Temporal para el caso de Asignacion
global currentVarTemp
# Variable para contar el numero de parametros
global numParam
# Conteo de variables para la llamadas
global contParam
# Pointer al tipo de parametro
global typeParam
# Funcion de los parametros
global funcParam

######################
##### Cuadruplos #####
######################
global cuadruplos
global contCuadruplos

######################
####### Stacks #######
######################
# Pila Operandos
global pilaO
# Pila Operadores
global pOper
# Pila Tipos
global pTipos
# Pila Saltos
global pSaltos
# Pila Operadores Booleanos
global pBool

#####################################
# Separacion de Variables (Memoria) #
#####################################
global constantInt
global arrConstantInt

global constantFloat
global arrConstantFloat

global constantChar
global arrConstantChar

global localInt
global arrLocalInt

global localFloat
global arrLocalFloat

global localChar
global arrLocalChar

global localBool
global arrLocalBool

global globalInt
global arrGlobalInt
global tempInt
global arrTempInt

global globalFloat
global arrGlobalFloat
global tempFloat
global arrTempFloat

global globalChar
global arrGlobalChar
global tempChar
global arrTempChar

global globalBool
global arrGlobalBool
global tempBool
global arrTempBool

# Conteo de variables por funciones
global contFuncInt, contFuncChar, contFuncBool, contFuncFloat
# Conteo general de las variables del programa
global conGenInt, contGenChar, contGenBool, contGenFloat
# Regreso despues de ir a una funcion
global ret
ret = []
# Pila de bases para la memoria local
global basInt, basFloat, basChar, basBool
basInt = [3000]
basFloat = [4000]
basChar = [5000]
basBool = [6000]
des = []
# Pila de desplazamiento para memoria local
global desInt, desFloat, desChar, desBool
desInt = []
desFloat = []
desChar = []
desBool = []
# Variable para guardar el return
global varRet
varRet = []
# Para el acceso a vectores
global nAcceso
nAcceso = 0

# Para los pointers
global tempPointers
global arrTempPointers
arrTempPointers = []
global currentArr
currentArr = ""

###LEXER###

# Se definen las palabras reservadas para el proyecto
reserved = {
    'program': 'PROGRAM',
    'main': 'MAIN',
    'var': 'VAR',
    'void': 'VOID',
    'int': 'INT',
    'float': 'FLOAT',
    'char': 'CHAR',
    'write': 'WRITE',
    'read': 'READ',
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'function': 'FUNCTION',
    'call': 'CALL',
    'return': 'RETURN',
    'do': 'DO',

}

# Se definen los tokens que se van a usar en el proyecto
tokens = [
    # Tokens con expresiones regulares 'sencillas'
    'PLUS',
    'MINUS',
    'MULT',
    'DIV',
    'EQUAL',
    'NEQUAL',
    'GTHAN',
    'LTHAN',
    'GEQUALTHAN',
    'LEQUALTHAN',
    'EEQUAL',
    'LPAR',
    'RPAR',
    'LBRACKET',
    'RBRACKET',
    'LSQUARE',
    'RSQUARE',
    'COLON',
    'SCOLON',
    'COMMA',
    'DOT',
    'AND',
    'OR',
    'CTEINT',
    'CTEF',
    'CTEC',
    'CSTR',
    # Tokens con expresiones regulares 'mas complejas'
    'ID',
    'COMMENT',

]


########################################
# Inicializacion de Variables Globales #
########################################
currentScope = 'global'
currentType = None
currentVar = None
currentVarTemp = None
currentFuncType = "void"
cantFunc = 2
tamVar = 1
numParam = 0
contParam = 0
contFuncInt = 0
contFuncFloat = 0
contFuncChar = 0
contFuncBool = 0
conGenInt = 0
contGenChar = 0
contGenBool = 0
contGenFloat = 0
typeParam = ""
funcParam = ""

########################################
##### Inicializacion de Cuadruplos #####
########################################
# El numero solo es una direccion
cuadruplos = [
    ["goto", "-", "-", "-"]
]
rOperand = ""
rOperandType = ""
lOperand = ""
lOperandType = ""
operador = ""
arrTemp = []
contCuadruplos = 1

################################################
# Definicion de cantidad de variables por tipo #
################################################
constantInt     = 0
constantFloat   = 1000
constantChar    = 2000
localInt        = 3000
localFloat      = 4000
localChar       = 5000
localBool       = 6000
globalInt       = 7000
globalFloat     = 8000
globalChar      = 9000
globalBool      = 10000

# Se dejan 4000 variables temporales por tipo
tempInt         = 11000
tempFloat       = 16000
tempChar        = 21000
tempBool        = 26000

# Pointers
tempPointers = 40000

#####################################################
# Inicializacion de Arreglos para Guardar Variables #
#####################################################
arrConstantInt     = []
arrConstantFloat   = []
arrConstantChar    = []
arrLocalInt        = []
arrLocalFloat      = []
arrLocalChar       = []
arrLocalBool       = []
arrGlobalInt       = []
arrGlobalFloat     = []
arrGlobalChar      = []
arrGlobalBool      = []
arrTempInt         = []
arrTempFloat       = []
arrTempChar        = []
arrTempBool        = []

########################
# Inicializacion Pilas #
########################
pilaO = []
pOper = []
pTipos = []
pSaltos = []
pBool = []

# Las expresiones regulares que se usaran en el codigo
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULT = r'\*'
t_DIV = r'\/'
t_EQUAL = r'\='
t_NEQUAL = r'\!='
t_GTHAN = r'\>'
t_LTHAN = r'\<'
t_GEQUALTHAN = r'\>='
t_LEQUALTHAN = r'\<='
t_EEQUAL = r'\=='
t_LPAR = r'\('
t_RPAR = r'\)'
t_LBRACKET = r'\{'
t_RBRACKET = r'\}'
t_LSQUARE = r'\['
t_RSQUARE = r'\]'
t_COLON = r'\:'
t_SCOLON = r'\;'
t_COMMA = r'\,'
t_DOT = r'\.'
t_AND = r'\&&'
t_OR = r'\|'
# No se pone +- aqui
t_CTEINT = r'[+-]?[0-9]+'
t_CTEF = r'[0-9]*\.[0-9]+'
t_CTEC = r'(\'[^\']\')'
t_CSTR = r'\"(.*)*\"'
t_ignore = " \t"

tokens = tokens + list(reserved.values())

#Expresiones regulares 'mas complicadas'

# Para los ID's dentro del programa
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

# Para los comentarios
def t_COMMENT(t):
    r'\[\$\$][.]*'
    t.type = reserved.get(t.value, 'COMMENT')
    pass

# Las nuevas lineas dentro del programa
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Error dentro del programa
def t_error(t):
    print("Lexical error ' {0} ' found in line ' {1} ' ".format(t.value[0], t.lineno))
    t.lexer.skip(1)

###PARSER###
def p_programa(p):
    '''programa : PROGRAM ID SCOLON vars funcion main '''

def p_main(p):
    '''main : MAIN p_n_scopeNow p_n_addFunc LPAR parametro RPAR vars p_n_empiezaMain bloque p_n_addMemory'''


def p_vars(p):
    '''vars : VAR tipo COLON vars2 SCOLON varsTemp
            | empty'''

def p_varsTemp(p):
    '''varsTemp : tipo COLON vars2 SCOLON varsTemp
                | empty'''

###############
# COMENTARIOS #
###############
# p_n_nameID sirve para tomar el nombre de la variable actual
def p_vars2(p):
    '''vars2 : ID p_n_nameID p_n_cambioTamVar p_n_saveVar
             | ID p_n_nameID p_n_cambioTamVar p_n_saveVar COMMA vars2
             | ID p_n_nameID LSQUARE CTEINT p_n_cambioTamArr RSQUARE p_n_saveVar
             | ID p_n_nameID LSQUARE CTEINT p_n_cambioTamArr RSQUARE p_n_saveVar COMMA vars2'''

###############
# COMENTARIOS #
###############
# p_n_typeNow sirve para tomar el tipo de la variable actual
def p_tipo(p):
    '''tipo : INT p_n_typeNow
            | FLOAT p_n_typeNow
            | CHAR p_n_typeNow'''

# p_n_typeNow sirve para tomar el tipo de la funcion actual
def p_tipoFunc(p):
    '''tipoFunc : INT p_n_typeNowFunc
                | FLOAT p_n_typeNowFunc
                | CHAR p_n_typeNowFunc
                | VOID p_n_typeNowFunc'''

def p_parametro(p):
    '''parametro : empty
                 | param2'''

def p_param2(p):
    '''param2 : tipo varParamTemp
              | tipo varParamTemp COMMA param2'''

# Hasta aqui le puedes dar back #

def p_varParam(p):
    '''varParam : ID p_n_nameID p_n_nAccesoNormal
                | ID p_n_nameIDArr LSQUARE p_n_arrParam RSQUARE'''
# p_n_nAccesoArreglo

def p_n_arrParam(p):
    '''p_n_arrParam : CTEINT p_n_nAccesoArreglo
                    | expresion p_n_cambioTamArr2'''


# Hasta aqui (y un poco mas) puedes darle back #

def p_varParamTemp(p):
    '''varParamTemp : ID p_n_nameID p_n_cambioTamVar p_n_saveVar p_n_tamPar
                    | ID p_n_nameID LSQUARE CTEINT p_n_cambioTamArr p_n_saveVar RSQUARE'''

def p_funcion(p):
    '''funcion : FUNCTION tipoFunc ID p_n_scopeNow p_n_addFunc LPAR parametro RPAR vars p_n_iniciaFunc p_n_addMemory bloque p_n_endFunc funcion
               | empty'''

def p_bloque(p):
    '''bloque : LBRACKET bloqueTemp RBRACKET'''

def p_bloqueTemp(p):
    '''bloqueTemp : estatuto
                  | estatuto bloqueTemp
                  | empty'''

#######################################
############# ESTATUTOS ###############
#######################################
# Se sacrifica el for
def p_estatuto(p):
    '''estatuto : asignacion
                | lectura
                | escritura
                | condicion
                | llamar
                | while
                | comentario
                | doWhile
                | retorno
                | empty'''

#######################################
# Revisar si hay que cambiar varParam #
#######################################
# Ya tiene cuadruplos temporales #
def p_asignacion(p):
    '''asignacion : varParam p_n_asignacionTemp EQUAL expresion p_n_asignacion SCOLON'''

# Ya tiene cuadruplos temporales #  
def p_lectura(p):
    '''lectura : READ LPAR lecturaTemp p_n_lectura RPAR SCOLON'''

# Ya tiene cuadruplos temporales #
def p_escritura(p):
    '''escritura : WRITE LPAR escrituraTemp RPAR SCOLON'''

def p_escrituraTemp(p):
    '''escrituraTemp : expresion p_n_escrituraExp
                     | p_escrituraTemp2 
                     | p_escrituraTemp2 COMMA escrituraTemp
                     | varTemp p_n_escrituraExp COMMA escrituraTemp
                     | expresion p_n_escrituraExp COMMA escrituraTemp'''

# Necesito p_n para guardar el string en una variable temporal
def p_escrituraTemp2(p):
    '''p_escrituraTemp2 : CSTR p_n_guardaString
                        | CSTR p_n_guardaString p_escrituraTemp2'''

# Ya tiene cuadruplos temporales #
def p_condicion(p):
    '''condicion : IF LPAR expresion p_n_revisaIF RPAR bloque p_n_acabaIF
                 | IF LPAR expresion p_n_revisaIF RPAR bloque ELSE p_n_else bloque p_n_acabaIF'''

def p_llamar(p):
    '''llamar : CALL LPAR ID p_n_nameID p_n_revisaFuncExiste p_n_generaEra LPAR llamarTemp RPAR p_n_goSub RPAR SCOLON
              | CALL LPAR ID p_n_nameID p_n_guardaVarRet EQUAL ID p_n_nameID p_n_revisaFuncExiste p_n_generaEra LPAR llamarTemp RPAR p_n_goSub RPAR SCOLON'''

def p_llamarTemp(p):
    '''llamarTemp : empty
                  | llamarTemp2'''

def p_llamarTemp2(p):
    '''llamarTemp2 : expresion p_n_revisaParam
                   | expresion p_n_revisaParam COMMA p_n_nextParam llamarTemp2'''

def p_retorno(p):
    '''retorno : RETURN SCOLON
               | RETURN LPAR expresion p_n_asignaRet RPAR SCOLON'''

# Ya tiene cuadruplos temporales #
def p_while(p):
    '''while : WHILE p_n_whileInicio LPAR expresion p_n_revisaIF RPAR bloque p_n_whileFinal'''

# SACRIFICADO #
'''
def p_for(p):
    for : FOR LPAR expresion SCOLON expresion SCOLON expresion RPAR bloque
'''
def p_comentario(p):
    '''comentario : COMMENT'''

# Ya tiene cuadruplos temporales #
def p_doWhile(p):
    '''doWhile : DO p_n_doWhileInicio bloque WHILE LPAR expresion p_n_doWhileFinal RPAR SCOLON'''

#######################################
# Revisar si hay que cambiar varParam #
#######################################
def p_lecturaTemp(p):
    '''lecturaTemp : varParam lecturaTemp
                   | empty'''

#######################################
############ EXPRESIONES ##############
#######################################
def p_expresion(p):
    '''expresion : exp '''

def p_exp(p):
    '''exp : t p_n_checkPM
           | exp PLUS p_n_addOper exp
           | exp MINUS p_n_addOper exp'''

def p_t(p):
    '''t : y p_n_checkMD
         | t MULT p_n_addOper t
         | t DIV p_n_addOper t'''

def p_y(p):
    '''y : varTemp 
         | y GTHAN p_n_addOperBool y
         | y LTHAN p_n_addOperBool y
         | y GEQUALTHAN p_n_addOperBool y
         | y LEQUALTHAN p_n_addOperBool y
         | y EEQUAL p_n_addOperBool y
         | y NEQUAL p_n_addOperBool y'''

def p_varTemp(p):
    '''varTemp : varParam p_n_addPilaO
               | CTEINT p_n_addCTEINT
               | CTEF p_n_addCTEF
               | CTEC p_n_addCTEC
               | LPAR p_n_falsoParentesis exp RPAR p_n_sacarFalsoParentesis'''

def p_empty(p):
    '''empty :'''
    pass

def p_error(p):
    global cuadruplos
    print("Error de sintaxis en el input - {}".format(p))
    """
        If there is an error, the parser will resort to this instruction to inform of it.
        :param p:
        :return:
        """
    #print(cuadruplos)
    exit()

#######################################
######### PUNTOS NEURALGICOS ##########
#######################################

# Punto neuralgico para ver el tipo actual de las variables #
def p_n_typeNow(p):
    '''p_n_typeNow : '''
    global currentType
    currentType = p[-1]

# Punto neuralgico para cambiar el tamanio de la variable global en caso de haber una variable normal
def p_n_cambioTamVar(p):
    '''p_n_cambioTamVar : '''
    global tamVar
    tamVar = 1

# Punto neuralgico para cambiar el tamanio de la variable global en caso de haber un arreglo
def p_n_cambioTamArr(p):
    '''p_n_cambioTamArr : '''
    global tamVar, tempPointers, arrTempPointers, tempInt, tempFloat, tempChar, currentType, currentScope
    global globalInt, globalChar, globalFloat
    tamVar = int(p[-1])

def p_n_cambioTamArr2(p):
    '''p_n_cambioTamArr2 : '''
    global tamVar, tempPointers, arrTempPointers, tempInt, tempFloat, tempChar, currentType, currentScope, currentVar
    global globalInt, globalChar, globalFloat
    global pilaO, pTipos, contCuadruplos, currentArr
    desVec = pilaO.pop()
    pTipos.pop()
    cuadruplos.append(["+", desVec, proc.variableTable[currentScope][currentArr].add, tempPointers])
    contCuadruplos = contCuadruplos + 1
    pilaO.append(tempPointers)
    pTipos.append(proc.variableTable[currentScope][currentVar].typeVar)
    arrTempPointers.append(0)
    tempPointers = tempPointers + 1
    currentVar = currentArr
        

    

# Punto neuralgico para tomar el nombre de la variable #
def p_n_nameID(p):
    '''p_n_nameID : '''
    global currentVar
    currentVar = p[-1]

def p_n_nameIDArr(p):
    '''p_n_nameIDArr : '''
    global currentArr, currentVar
    currentArr = p[-1]
    currentVar = currentArr

# Punto neuralgico para saber cual es el nombre de la funcion y agregarla a la tabla #
def p_n_scopeNow(p):
    '''p_n_scopeNow : '''
    global currentScope, numParam, contFuncInt, contFuncChar, contFuncBool, contFuncFloat
    currentScope = p[-1]
    numParam = 0
    contFuncInt = 0
    contFuncFloat = 0
    contFuncChar = 0
    contFuncBool = 0

# Punto neuralgico para ver el tipo actual de las funciones #
def p_n_typeNowFunc(p):
    '''p_n_typeNowFunc : '''
    global currentFuncType
    currentFuncType = p[-1]    

# Punto neuralgico para agregar la funcion en la tabla de funciones
def p_n_addFunc(p):
    '''p_n_addFunc : '''
    global currentScope, currentFuncType, varRet
    global localInt, localChar, localBool, localFloat
    global arrLocalBool, arrLocalChar, arrLocalFloat, arrLocalInt
    proc.addFunc(currentScope, currentFuncType)
    if currentFuncType == "int":
        proc.addVar(currentScope, currentScope, currentFuncType, localInt)
        varRet.append(localInt)
        localInt = localInt + 1
        arrLocalInt.append(0)
    elif currentFuncType == "float":
        proc.addVar(currentScope, currentScope, currentFuncType, localFloat)
        varRet.append(localFloat)
        localFloat = localFloat + 1
        arrLocalFloat.append(0)
    elif currentFuncType == "char":
        proc.addVar(currentScope, currentScope, currentFuncType, localChar)
        varRet.append(localChar)
        localChar = localChar + 1
        arrLocalChar.append('')
    elif currentFuncType == "bool":
        proc.addVar(currentScope, currentScope, currentFuncType, localBool)
        varRet.append(localBool)
        localBool = localBool + 1
        arrLocalBool.append(False)


def p_n_addMemory(p):
    '''p_n_addMemory : '''
    global currentScope, numParam, contFuncInt, contFuncChar, contFuncBool, contFuncFloat
    global localBool, localChar, localInt, localFloat
    proc.functionTable[currentScope].memory = len(proc.variableTable[currentScope])
    proc.functionTable[currentScope].paramNum = numParam
    proc.functionTable[currentScope].contInt = contFuncInt
    proc.functionTable[currentScope].contFloat = contFuncFloat
    proc.functionTable[currentScope].contChar = contFuncChar
    proc.functionTable[currentScope].contBool = contFuncBool
    contFuncInt = 0
    contFuncFloat = 0
    contFuncChar = 0
    contFuncBool = 0
    localInt = 3000
    localFloat = 4000
    localChar = 5000
    localBool = 6000


def p_n_tamPar(p):
    '''p_n_tamPar : '''
    global numParam
    numParam = numParam + 1

def p_n_empiezaMain(p):
    '''p_n_empiezaMain : '''
    global cuadruplos, currentScope, contCuadruplos
    proc.functionTable[currentScope].inicia = contCuadruplos
    cuadruplos[0][3] = contCuadruplos

def p_n_iniciaFunc(p):
    '''p_n_iniciaFunc : '''
    global currentScope, contCuadruplos
    proc.functionTable[currentScope].inicia = contCuadruplos

def p_n_endFunc(p):
    '''p_n_endFunc : '''
    global cuadruplos, contCuadruplos, currentScope, varRet
    cuadruplos.append(["endfunc", "", "", ""])
    proc.functionTable[currentScope].endf = contCuadruplos
    contCuadruplos = contCuadruplos + 1
    if varRet:
        varRet.pop()

def p_n_revisaFuncExiste(p):
    '''p_n_revisaFuncExiste : '''
    global currentVar
    if currentVar not in proc.functionTable:
        print("No existe la funion", currentVar)
        p_error("Function does not exist")

# Revisa la memoria #
def p_n_generaEra(p):
    '''p_n_generaEra : '''
    global cuadruplos, funcParam, currentVarTemp, typeParam, contParam, contCuadruplos, currentVar, localBool, localChar, localFloat, localInt
    global conGenInt, contGenChar, contGenBool, contGenFloat, currentScope
    ################################################
    # Cambiar esto y dejarlo en la maquina virtual #
    ################################################
    cuadruplos.append(["ERA", "", "", currentVar])
    contCuadruplos = contCuadruplos + 1
    if proc.functionTable[currentVar].typeFunc != "void":
        contParam = 1
    else:
        contParam = 0
    lst = list(proc.variableTable[currentVar])
    typeParam = proc.variableTable[currentVar][lst[contParam]].typeVar
    currentVarTemp = lst
    funcParam = currentVar
    # Cambiar esto y dejarlo en la maquina virtual
    conGenInt = conGenInt + proc.functionTable[currentVar].contInt
    contGenChar = contGenChar + proc.functionTable[currentVar].contChar
    contGenBool = contGenBool + proc.functionTable[currentVar].contBool
    contGenFloat = contGenFloat + proc.functionTable[currentVar].contFloat
        

# currentVarTemp es la lista de variables
def p_n_revisaParam(p):
    '''p_n_revisaParam : '''
    global typeParam, currentType, cuadruplos, pilaO, pTipos, currentVarTemp, contParam, funcParam, contCuadruplos
    argument = pilaO.pop()
    argumentType = pTipos.pop()
    print(currentType)
    if(currentType != argumentType):
        p_error("El tipo del argumento de la llamada no es igual al tipo de parametro")
    else:
        if proc.functionTable[funcParam].typeFunc != "void":
            cuadruplos.append(["parameter", argument, "", int(proc.variableTable[funcParam][currentVarTemp[contParam]].add)])
            contCuadruplos = contCuadruplos + 1
        else:
            cuadruplos.append(["parameter", argument, "", proc.variableTable[funcParam][currentVarTemp[contParam]].add])
            contCuadruplos = contCuadruplos + 1

def p_n_nextParam(p):
    '''p_n_nextParam : '''
    global contParam, typeParam, funcParam
    contParam = contParam + 1
    lst = list(proc.variableTable[funcParam])
    typeParam = proc.variableTable[funcParam][lst[contParam]].typeVar

def p_n_goSub(p):
    '''p_n_goSub : '''
    global cuadruplos, contCuadruplos, funcParam, varRet, pilaO, pTipos
    add = proc.functionTable[funcParam].inicia
    cuadruplos.append(["gosub", funcParam, "", add])
    contCuadruplos = contCuadruplos + 1
    fType = proc.functionTable[funcParam].typeFunc
    if fType != "void":
        argument = pilaO.pop()
        argumentType = pTipos.pop()
        vGuardada = varRet[-1]
        rValue = proc.variableTable[funcParam][funcParam].add
        cuadruplos.append(["=", argument, "", vGuardada])
        contCuadruplos = contCuadruplos + 1
    # Para saber a donde regresa en los la funcion
    proc.functionTable[funcParam].termina = contCuadruplos

def p_n_guardaVarRet(p):
    '''p_n_guardaVarRet : '''
    global varRet, currentVar, currentScope, funcParam, pilaO, pTipos, currentType
    funcParam = currentScope
    varRet.append(proc.variableTable[funcParam][currentVar].add)
    fType = proc.variableTable[currentScope][currentScope].typeVar
    if fType == "int":
        pilaO.append(3000)
    elif fType == "float":
        pilaO.append(4000)
    elif fType == "bool":
        pilaO.append(5000)
    elif fType == "char":
        pilaO.append(6000)
    pTipos.append(fType)

def p_n_asignaRet(p):
    ''' p_n_asignaRet : '''
    global currentScope, pilaO, pTipos, cuadruplos, contCuadruplos, varRet
    global tempInt, tempFloat, tempChar, tempBool
    global arrTempFloat, arrTempInt, arrTempChar, arrTempBool
    rType = pTipos.pop()
    fType = proc.functionTable[currentScope].typeFunc
    rAdd = pilaO.pop()
    if rType == fType:
        vGuardada = varRet[-1]
        if rType == "int":
            cuadruplos.append(["=", rAdd, "", vGuardada])
            pilaO.append(tempInt)
            pTipos.append("int")
            arrTempInt.append(0)
            tempInt = tempInt + 1
            contCuadruplos = contCuadruplos + 1
            cuadruplos.append(["=", rAdd, "", 3000])
            contCuadruplos = contCuadruplos + 1
        elif rType == "float":
            cuadruplos.append(["=", rAdd, "", vGuardada])
            pilaO.append(tempFloat)
            arrTempFloat.append(0.0)
            tempFloat = tempFloat + 1
            pTipos.append("float")
            contCuadruplos = contCuadruplos + 1
            cuadruplos.append(["=", rAdd, "", 4000])
            contCuadruplos = contCuadruplos + 1
        elif rType == "char":
            cuadruplos.append(["=", rAdd, "", vGuardada])
            pilaO.append(tempChar)
            arrTempChar.append('')
            tempChar = tempChar + 1
            pTipos.append("char")
            contCuadruplos = contCuadruplos + 1
            cuadruplos.append(["=", rAdd, "", 5000])
            contCuadruplos = contCuadruplos + 1
        
        '''cuadruplos.append(["=", rAdd, "", fAdd])
        contCuadruplos = contCuadruplos + 1'''
        cuadruplos.append(["return", "", "", currentScope])
        contCuadruplos = contCuadruplos + 1
        
    else:
        p_error("El retorno no es del mismo valor que el tipo de funcion")

def p_n_regresaValor(p):
    '''p_n_regresaValor : '''
    global cuadruplos, contCuadruplos, varRet
    quadGoal = cuadruplos[-1]
    cuadruplos.append(["=", quadGoal[3], "", varRet.pop()])
    contCuadruplos = contCuadruplos + 1

# Punto neuralgico para guardar las variables en sus tablas
def p_n_saveVar(p):
    '''p_n_saveVar : '''
    global currentScope, currentType, globalInt, globalFloat, globalChar, globalBool, localInt, localFloat, localChar, localBool
    global contFuncInt, contFuncChar, contFuncBool, contFuncFloat, tamVar
    global arrLocalInt, arrLocalFloat, arrLocalBool, arrLocalChar
    global arrGlobalInt, arrGlobalFloat, arrGlobalBool, arrGlobalChar
    # Se revisa primeramente el scope y luego dependiendo de la variable
    # se le asigna un espacio en la memoria virtual
    if(currentScope == "global" or currentScope == "main"):
        if (currentScope == "global" and currentVar in proc.variableTable["global"]) or (currentScope == "main" and currentVar in proc.variableTable["main"]):
            p_error("There's a variable that already exists")
        else:
            if(currentType == "int"):
                proc.addVar(currentScope, currentVar, currentType, globalInt)
                globalInt = globalInt + tamVar
                contFuncInt = contFuncInt + tamVar
                for i in range(tamVar):
                    arrGlobalInt.append(0)
            elif(currentType == "float"):
                proc.addVar(currentScope, currentVar, currentType, globalFloat)
                globalFloat = globalFloat + tamVar
                contFuncFloat = contFuncFloat + tamVar
                for i in range(tamVar):
                    arrGlobalFloat.append(0.0)
            elif(currentType == "char"):
                proc.addVar(currentScope, currentVar, currentType, globalChar)
                globalChar = globalChar + tamVar
                contFuncChar = contFuncChar + tamVar
                for i in range(tamVar):
                    arrGlobalChar.append('')
            elif(currentType == "bool"):
                proc.addVar(currentScope, currentVar, currentType, globalBool)
                globalChar = globalChar + tamVar
                contFuncBool = contFuncBool + tamVar
                for i in range(tamVar):
                    arrGlobalBool.append(False)
    else:
        if currentVar in proc.variableTable[currentScope]:
            p_error("There's a variable that already exists")
        else:
            if(currentType == "int"):
                proc.addVar(currentScope, currentVar, currentType, localInt)
                localInt = localInt + tamVar
                contFuncInt = contFuncInt + tamVar
                for i in range(tamVar):
                    arrLocalInt.append(0)
            elif(currentType == "float"):
                proc.addVar(currentScope, currentVar, currentType, localFloat)
                localFloat = localFloat + tamVar
                contFuncFloat = contFuncFloat + tamVar
                for i in range(tamVar):
                    arrLocalFloat.append(0.0)
            elif(currentType == "char"):
                proc.addVar(currentScope, currentVar, currentType, localChar)
                localChar = localChar + tamVar
                contFuncChar = contFuncChar + tamVar
                for i in range(tamVar):
                    arrLocalChar.append('')
            elif(currentType == "bool"):
                proc.addVar(currentScope, currentVar, currentType, localBool)
                localBool = localBool + tamVar
                contFuncBool = contFuncBool + tamVar
                for i in range(tamVar):
                    arrLocalBool.append(False)

def p_n_nAccesoNormal(p):
    ''' p_n_nAccesoNormal : '''
    global nAcceso
    nAcceso = 0

def p_n_nAccesoArreglo(p):
    ''' p_n_nAccesoArreglo : '''
    global nAcceso, cuadruplos
    # Pop a la expresion
    # Crea cuadruplo con 
    nAcceso = int(p[-1])


#######################################
######### PUNTOS NEURALGICOS ##########
############# ESTATUTOS ###############
#######################################

# Punto neuralgico para hacer el cuadruplo de lectura
def p_n_lectura(p):
    '''p_n_lectura : '''
    global currentVar, contCuadruplos, currentScope
    cuadruplos.append(["Read", "", "", proc.variableTable[currentScope][currentVar].add])
    
    contCuadruplos = contCuadruplos + 1

def p_n_escrituraExp(p):
    '''p_n_escrituraExp : '''
    global pilaO, pTipos, contCuadruplos
    temp = pilaO.pop()
    print("hi", temp)
    cuadruplos.append(["write", "", "", temp])
    pTipos.pop()
    contCuadruplos = contCuadruplos + 1

def p_n_guardaString(p):
    '''p_n_guardaString : '''
    global constantChar, contCuadruplos, arrConstantChar
    cuadruplos.append(["write", "", "", constantChar])
    arrConstantChar.append(p[-1])
    constantChar = constantChar + 1
    contCuadruplos = contCuadruplos + 1

def p_n_asignacion(p):
    '''p_n_asignacion : '''
    global currentVarTemp, currentScope, contVarTemp, contCuadruplos, cuadruplos
    asig = pilaO.pop()
    asigType = pTipos.pop()
    asigTypeTemp = pTipos.pop()
    currentVarTemp = pilaO.pop()
    res = oraculo.validar(asigTypeTemp, asigType, "=")
    if(res != -1):
        cuadruplos.append(["=", asig, "", currentVarTemp])
        contCuadruplos = contCuadruplos + 1
    else:
        p_error("Type Mismatch")

def p_n_asignacionTemp(p):
    '''p_n_asignacionTemp : '''
    global currentVar, currentVarTemp, currentScope, nAcceso
    currentVarTemp = currentVar
    lst = list(proc.variableTable['global'])
    if not pilaO or pilaO[-1] <40000:
        if currentVarTemp in lst:
            pilaO.append(proc.variableTable['global'][currentVarTemp].add + nAcceso)
            pTipos.append(proc.variableTable['global'][currentVarTemp].typeVar)
        else:
            pilaO.append(proc.variableTable[currentScope][currentVarTemp].add + nAcceso)
            pTipos.append(proc.variableTable[currentScope][currentVarTemp].typeVar)

# Tambien funciona para el while
def p_n_revisaIF(p):
    ''' p_n_revisaIF : '''
    global pilaO, pTipos, cuadruplos, contCuadruplos, pSaltos, arrConstantInt, arrTempBool
    global tempBool, pSaltos
    tTipo2 = pTipos.pop()
    tTipo1 = pTipos.pop()
    oTemp = pOper.pop()
    tTipo = oraculo.validar(tTipo1, tTipo2, oTemp)
    if tTipo != 3: 
        p_error("Type Mismatch, IF needs a Bool Expresion to Succeed")
    else:
        # Creo el cuadruplo a donde va a regresar
        n2 = pilaO.pop()
        n1 = pilaO.pop()
        cuadruplos.append([oTemp, n1, n2, tempBool])
        contCuadruplos = contCuadruplos + 1
        cuadruplos.append(["gotof", tempBool, "", ""])
        pSaltos.append(contCuadruplos)
        contCuadruplos = contCuadruplos + 1
        arrTempBool.append(False)
        tempBool = tempBool + 1
        

def p_n_acabaIF(p):
    '''p_n_acabaIF : '''
    global pSaltos, cuadruplos, contCuadruplos
    fin = pSaltos.pop()
    cuadruplos[fin][3] = contCuadruplos

def p_n_else(p):
    ''' p_n_else : '''
    global contCuadruplos, cuadruplos
    cuadruplos.append(["goto", "", "", ""])
    fin = pSaltos.pop()
    contCuadruplos = contCuadruplos + 1
    cuadruplos[fin][3] = contCuadruplos
    pSaltos.append(contCuadruplos-1)

def p_n_whileInicio(p):
    '''p_n_whileInicio : '''
    global pSaltos, contCuadruplos
    pSaltos.append(contCuadruplos-1)    

def p_n_whileFinal(p):
    '''p_n_whileFinal : '''
    global pSaltos, contCuadruplos, cuadruplos
    fin = pSaltos.pop()
    ret = pSaltos.pop()
    cuadruplos[fin][3] = contCuadruplos + 1
    cuadruplos.append(["goto", "", "",ret + 1])
    contCuadruplos = contCuadruplos + 1

def p_n_doWhileInicio(p):
    '''p_n_doWhileInicio : '''
    global pSaltos, contCuadruplos
    pSaltos.append(contCuadruplos) 

def p_n_doWhileFinal(p):
    '''p_n_doWhileFinal : '''
    global pilaO, pTipos, cuadruplos, contCuadruplos, pSaltos, arrConstantInt, arrTempBool
    global tempBool, pSaltos
    tTipo2 = pTipos.pop()
    tTipo1 = pTipos.pop()
    oTemp = pOper.pop()
    tTipo = oraculo.validar(tTipo1, tTipo2, oTemp)
    if tTipo != 3: 
        p_error("Type Mismatch, DO WHILE needs a Bool Expresion to Succeed")
    else:
        # Creo el cuadruplo a donde va a regresar
        n2 = pilaO.pop()
        n1 = pilaO.pop()
        cuadruplos.append([oTemp, n1, n2, tempBool])
        contCuadruplos = contCuadruplos + 1
        salto = pSaltos.pop()
        cuadruplos.append(["gotov", tempBool, "", salto])
        arrTempBool.append(False)
        contCuadruplos = contCuadruplos + 1
        tempBool = tempBool + 1

#######################################
######### PUNTOS NEURALGICOS ##########
############# EXPRESIONES #############
#######################################
def p_n_addPilaO(p):
    '''p_n_addPilaO : '''
    global pilaO, pTipos, currentScope, currentVar
    #print(currentScope, " ", currentVar)
    if not pilaO or pilaO[-1] < 40000 or pOper:
        if currentVar in proc.variableTable["global"]:
            #print(proc.variableTable["global"][currentVar].add)
            pilaO.append(proc.variableTable["global"][currentVar].add + nAcceso)
            pTipos.append(proc.variableTable["global"][currentVar].typeVar)
        elif currentVar in proc.variableTable[currentScope]:
            #print(proc.variableTable[currentScope][currentVar].add)
            pilaO.append(proc.variableTable[currentScope][currentVar].add + nAcceso)
            pTipos.append(proc.variableTable[currentScope][currentVar].typeVar)

def p_n_addOper(p):
    '''p_n_addOper : '''
    global pOper
    pOper.append(p[-1])

def p_n_addOperBoolAO(p):
    '''p_n_addOperBoolAO : '''
    global pBool
    pBool.append(p[-1])

'''def p_n_evaluaAO(p):
    p_n_evaluaAO :
    global contCuadruplos, pBool, pOper, pTipos, pilaO, tempBool
    if pBool and (pBool[-1] == "|" or pBool[-1] == "&&"):
        boo = pBool.pop()
        tTipo2 = pTipos.pop()
        tTipo1 = pTipos.pop()
        oTemp = pOper.pop()
        tTipo = oraculo.validar(tTipo1, tTipo2, oTemp)
        if tTipo != 3: 
            p_error("Type Mismatch, IF needs a Bool Expresion to Succeed")
        else:
            # Creo el cuadruplo a donde va a regresar
            n2 = pilaO.pop()
            n1 = pilaO.pop()
            cuadruplos.append([oTemp, n1, n2, tempBool])
            contCuadruplos = contCuadruplos + 1
            arrTempBool.append(False)
            tempBool = tempBool + 1
            tempBool1 = cuadruplos[contCuadruplos-2][3]
            cuadruplos.append([boo, tempBool1, tempBool-1, tempBool])
            contCuadruplos = contCuadruplos + 1
            cuadruplos.append(["gotof", tempBool, "", ""])
            arrTempBool.append(False)
            pSaltos.append(contCuadruplos)
            contCuadruplos = contCuadruplos + 1
            tempBool = tempBool + 1
    else:
        tTipo2 = pTipos.pop()
        tTipo1 = pTipos.pop()
        oTemp = pOper.pop()
        tTipo = oraculo.validar(tTipo1, tTipo2, oTemp)
        if tTipo != 3: 
            p_error("Type Mismatch, IF needs a Bool Expresion to Succeed")
        else:
            # Creo el cuadruplo a donde va a regresar
            n2 = pilaO.pop()
            n1 = pilaO.pop()
            cuadruplos.append([oTemp, n1, n2, tempBool])
            contCuadruplos = contCuadruplos + 1
            cuadruplos.append(["gotof", tempBool, "", ""])
            pSaltos.append(contCuadruplos)
            contCuadruplos = contCuadruplos + 1
            arrTempBool.append(False)
            tempBool = tempBool + 1'''


def p_n_addCTEINT(p):
    '''p_n_addCTEINT : '''
    global pilaO, pTipos, currentScope, arrConstantInt, constantInt
    arrConstantInt.append(int(p[-1]))
    pilaO.append(constantInt)
    constantInt = constantInt + 1
    pTipos.append("int")
    

def p_n_addCTEF(p):
    '''p_n_addCTEF : '''
    global pilaO, pTipos, currentScope, arrConstantFloat, constantFloat
    arrConstantFloat.append(float(p[-1]))
    pilaO.append(constantFloat)
    constantFloat = constantFloat + 1
    pTipos.append("float")

def p_n_addCTEC(p):
    '''p_n_addCTEC : '''
    global pilaO, pTipos, currentScope, arrConstantChar, constantChar
    arrConstantChar.append(p[-1])
    pilaO.append(constantChar)
    constantChar = constantChar + 1
    pTipos.append("char")


# Revisar si el stack tiene un mas o un menos
def p_n_checkPM(p):
    '''p_n_checkPM : '''
    global arrTemp, contVarTemp,  pOper, pTipos, pilaO, currentScope, currentVar, rOperand, lOperand, operador, cuadruplos, rOperandType, lOperandType
    global tempInt, tempFloat, tempChar, tempBool, contCuadruplos
    global arrTempFloat, arrTempInt, arrTempChar, arrTempBool
    #print("Si entro PM")
    if pOper and (pOper[-1] == "+" or pOper[-1] == "-"):
        rOperand = pilaO.pop()
        rOperandType = pTipos.pop()
        lOperand = pilaO.pop()
        lOperandType = pTipos.pop()
        operador = pOper.pop()
        res = oraculo.validar(rOperandType, lOperandType, operador)
        if(res != -1):
            # Que es AVAIL en el diagrama de la maestra #
            # Seria meter el arreglo temporal no? para modificarlo y luego usarlo
            # cuando se ejecuten los cuadruplos
            if res == 0:
                cuadruplos.append([operador, lOperand, rOperand, tempInt])
                pilaO.append(tempInt)
                arrTempInt.append(0)
                tempInt = tempInt + 1
                pTipos.append("int")
                contCuadruplos = contCuadruplos + 1
            elif res == 1:
                cuadruplos.append([operador, lOperand, rOperand, tempFloat])
                pilaO.append(tempFloat)
                arrTempFloat.append(0.0)
                tempFloat = tempFloat + 1
                pTipos.append("float")
                contCuadruplos = contCuadruplos + 1
            elif res == 2:
                cuadruplos.append([operador, lOperand, rOperand, tempChar])
                pilaO.append(tempChar)
                arrTempChar.append('')
                tempChar = tempChar + 1
                pTipos.append("char")
                contCuadruplos = contCuadruplos + 1
            else:
                cuadruplos.append([operador, lOperand, rOperand, tempBool])
                pilaO.append(tempBool)
                arrTempBool.append(0)
                tempBool = tempBool + 1
                pTipos.append("bool")
                contCuadruplos = contCuadruplos + 1
        else:
            p_error("Type Mismatch")

# Revisar si el stack tiene una multiplicacion o division
def p_n_checkMD(p):
    '''p_n_checkMD : '''
    global arrTemp, contVarTemp, pOper, pTipos, pilaO, currentScope, currentVar, rOperand, lOperand, operador, cuadruplos, rOperandType, lOperandType
    global tempInt, tempFloat, tempChar, tempBool, contCuadruplos
    global arrTempFloat, arrTempInt, arrTempChar, arrTempBool
    #print("Si entro MD")
    if pOper and (pOper[-1] == "*" or pOper[-1] == "/"):
        rOperand = pilaO.pop()
        rOperandType = pTipos.pop()
        lOperand = pilaO.pop()
        lOperandType = pTipos.pop()
        operador = pOper.pop()
        res = oraculo.validar(rOperandType, lOperandType, operador)
        if(res != -1):
            # Que es AVAIL en el diagrama de la maestra #
            # Seria meter el arreglo temporal no? para modificarlo y luego usarlo
            # cuando se ejecuten los cuadruplos
            # resultado = arrTemp[contVarTemp]
            if res == 0:
                cuadruplos.append([operador, lOperand, rOperand, tempInt])
                pilaO.append(tempInt)
                arrTempInt.append(0)
                tempInt = tempInt + 1
                pTipos.append("int")
                contCuadruplos = contCuadruplos + 1
            elif res == 1:
                cuadruplos.append([operador, lOperand, rOperand, tempFloat])
                pilaO.append(tempFloat)
                arrTempFloat.append(0.0)
                tempFloat = tempFloat + 1
                pTipos.append("float")
                contCuadruplos = contCuadruplos + 1
            elif res == 2:
                cuadruplos.append([operador, lOperand, rOperand, tempChar])
                pilaO.append(tempChar)
                arrTempChar.append('')
                tempChar = tempChar + 1
                pTipos.append("char")
                contCuadruplos = contCuadruplos + 1
            else:
                cuadruplos.append([operador, lOperand, rOperand, tempBool])
                pilaO.append(tempBool)
                arrTempBool.append(False)
                tempBool = tempBool + 1
                pTipos.append("bool")
                contCuadruplos = contCuadruplos + 1
        else:
            p_error("Type Mismatch")


def p_n_falsoParentesis(p):
    '''p_n_falsoParentesis : '''
    global pOper
    pOper.append("(")

def p_n_sacarFalsoParentesis(p):
    '''p_n_sacarFalsoParentesis : '''
    global pOper
    pOper.pop()

def p_n_addOperBool(p):
    '''p_n_addOperBool : '''
    global pOper
    pOper.append(p[-1])

def printFunc(currentFunc):
    print(currentFunc, "--> Parametros:", proc.printParam(currentFunc), "Memoria:", proc.printMem(currentFunc), "Inicia en Quad:", proc.functionTable[currentFunc].inicia)
    print(currentFunc, "--> Ints:", proc.functionTable[currentFunc].contInt, "floats:", proc.functionTable[currentFunc].contFloat, "char:", proc.functionTable[currentFunc].contChar, "bool:", proc.functionTable[currentFunc].contBool)


# Build Lexer
lexer = ply.lex.lex()

#Build Parser
parser = ply.yacc.yacc()

try:
    #f = open("fibonacciCiclico.txt", 'r')
    #f = open("fibonacciRecursivo.txt", 'r')
    #f = open("factorialCiclico.txt", 'r')
    #f = open("factorialRecursivo.txt", 'r')
    #f = open("ret.txt", 'r')
    #f = open("recursion.txt", 'r')
    f = open("vectores.txt", 'r')
    #f = open("recursionInf.txt", 'r')
    #f = open("test3.txt", 'r')
    #f = open("check.txt", 'r')
    
    # No funciona #
    #f = open("find.txt", 'r')

    r = f.read()
    f.close()
except FileNotFoundError:
    print("No hay archivo para probar")

parser.parse(r)
#print(proc.functionTable["main"].varKey)
#print(proc.variableTable["func2"]['l1'].val, " " , proc.variableTable["func2"]['l1'].add)
cuadruplos.append(["EndFile", "", "", ""])
'''
print("pilO:" , pilaO)
print("pTipos:" ,pTipos)
print("pOper:" ,pOper)
print("pSaltos: ", pSaltos)
print(contCuadruplos)
'''
print(cuadruplos)
print("Cantidad de Quads:", contCuadruplos)
#print(next(iter(proc.variableTable["miFuncion1"])))
################################################
# Cambiar esto y dejarlo en la maquina virtual #
################################################
#print(conGenInt)
#print(contGenFloat)
print("Código correcto")
print()
print()
print("####################################")
print("# Comienza la ejecucion del codigo #")
print("####################################")
print()

#######################################
########## MAQUINA VIRTUAL ############
#######################################
# Tengo mi arreglo de variables globales, que ya tienen valores defacto
# arrGlobalBool (10,000), arrGlobalChar (9,000), arrGlobalFloat (8,000), arrGlobalInt (7,000)
contTempQuads = 0
misOperadores = {
    "+",
    "-",
    "*",
    "/"
}
misOperadosBool = {
    ">",
    "<",
    "==",
    "!=",
    "&&",
    "|",
    ">=",
    "<="
}
# Inicializo la Memoria que tengo para las funciones
localInt        = 3000
localFloat      = 4000
localChar       = 5000
localBool       = 6000
while(contTempQuads != contCuadruplos+1):
    quadActual = cuadruplos[contTempQuads]
    need = quadActual[0]
    ar1 = quadActual[1]
    ar2 = quadActual[2]
    res = quadActual[3]
    
    ###############################
    ############ goto #############
    ###############################
    if(need == "goto"):
        contTempQuads = quadActual[3]
    
    ###############################
    ########### gotof #############
    ###############################
    elif need == "gotof":
        contTempQuads = contTempQuads + 1
        # Reviso si es una Booleana local
        if ar1 >= 6000 and ar1 < 7000:
            if arrLocalBool[ar1-6000] is False:
                contTempQuads = quadActual[3]

        # Reviso si es Booleana Global
        if ar1 >= 10000 and ar1 < 11000:
            if arrGlobalBool[ar1-10000] is False:
                contTempQuads = quadActual[3]
        
        # Reviso si es Booleana Temporal
        if ar1 >= 26000 and ar1 < 31000:
            if arrTempBool[ar1-26000] is False:
                contTempQuads = quadActual[3]
    
    ###############################
    ########### gotov #############
    ###############################
    elif need == "gotov":
        contTempQuads = contTempQuads + 1
        # Reviso si es una Booleana local
        if ar1 >= 6000 and ar1 < 7000:
            if arrLocalBool[ar1-6000] is True:
                contTempQuads = quadActual[3]

        # Reviso si es Booleana Global
        if ar1 >= 10000 and ar1 < 11000:
            if arrGlobalBool[ar1-10000] is True:
                contTempQuads = quadActual[3]
        
        # Reviso si es Booleana Temporal
        if ar1 >= 26000 and ar1 < 31000:
            if arrTempBool[ar1-26000] is True:
                contTempQuads = quadActual[3]

    ###############################
    ########### gosub #############
    ###############################
    elif need == "gosub":
        contTempQuads = contTempQuads + 1
        # Guardar a donde tengo que regresar
        ret.append(contTempQuads)

        # Tomo el nombre de la funcion a la que voy
        currentVar = ar1

        # Reviso la cantidad de variables que necesito
        conGenInt = proc.functionTable[currentVar].contInt
        contGenChar = proc.functionTable[currentVar].contChar
        contGenBool = proc.functionTable[currentVar].contBool
        contGenFloat = proc.functionTable[currentVar].contFloat

        ##### Hasta aqui pueds regresar #####
        if proc.functionTable[currentVar].typeFunc != "void":
            extra = 1;
        else:
            extra = 0;

        # Reviso si hay memoria suficiente
        if proc.functionTable[currentVar].contInt + localInt + extra >= 4000 or proc.functionTable[currentVar].contFloat + localFloat + extra >= 5000 or proc.functionTable[currentVar].contChar + localChar + extra >= 6000 or proc.functionTable[currentVar].contBool + localBool + extra >= 7000:
            p_error("No hay suficiente memoria")
        # Guardo el desplazamiento y la base de esta llamada
        else:
            basInt.append(localInt)
            desInt.append(proc.functionTable[currentVar].contInt)
            localInt = localInt + proc.functionTable[currentVar].contInt + extra
            # Genero espacios de memoria INT
            for x in range(proc.functionTable[currentVar].contInt + extra):
                arrLocalInt.append(0)
            basChar.append(localChar)
            desChar.append(proc.functionTable[currentVar].contChar)
            localChar = localChar + proc.functionTable[currentVar].contChar + extra
            # Genero espacios de memoria CHAR
            for x in range(proc.functionTable[currentVar].contChar + extra):
                arrLocalChar.append(0)
            basBool.append(localBool)
            desBool.append(proc.functionTable[currentVar].contBool)
            localBool = localBool + proc.functionTable[currentVar].contBool + extra
             # Genero espacios de memoria BOOL
            for x in range(proc.functionTable[currentVar].contBool + extra):
                arrLocalBool.append(0)
            basFloat.append(localFloat)
            desFloat.append(proc.functionTable[currentVar].contFloat)
            localFloat = localFloat + proc.functionTable[currentVar].contFloat + extra
             # Genero espacios de memoria FLOAT
            for x in range(proc.functionTable[currentVar].contFloat + extra):
                arrLocalFloat.append(0)

        
        # Ya revise memoria, ahora paso parametros
        pNum = proc.functionTable[currentVar].paramNum
        if pNum != 0:
            for x in range(pNum):
                # REVISAR RRR #
                if proc.functionTable[currentVar].typeFunc != "void":
                    quadParam = cuadruplos[contTempQuads-(pNum+1-x)]
                else:
                    quadParam = cuadruplos[contTempQuads-(pNum+1-x)]
                nu = quadParam[1]
                param = quadParam[3]
                # Asigno los parametros a las variables
                # Si el primer argumento es una constante Int
                if nu < 1000:

                    # Si a donde se asigna es una LOCAL int
                    # ["=", 0, "", 3000]
                    if param >= 3000 and param < 4000:
                        arrLocalInt[param - 6000 + basInt[-1]] = arrConstantInt[nu]

                    # Si lo que se pide es asignar una LOCAL float
                    # ["=", 0, "", 4000]
                    elif param >= 4000 and param < 5000:
                        arrLocalFloat[param-8000 + basFloat[-1]] = arrConstantInt[nu]

                    # Si a donde se asigna es una GLOBAL int
                    # ["=", 0, "", 7000]
                    elif param >= 7000 and param < 8000:
                        arrGlobalInt[param-7000] = arrConstantInt[nu]
                    
                    # Si a donde se asigna es una GLOBAL float
                    # ["=", 0, "", 8000]
                    elif param >= 8000 and param < 9000:
                        arrGlobalFloat[param-8000] = arrConstantInt[nu]

                    # Si lo que se pide es asignar un int TEMPORAL
                    # ["=", 0, "", 11000]
                    elif param >= 11000 and param < 16000:
                        arrTempInt[param-11000] = arrConstantInt[nu]

                    # Si lo que se pide es asignar un float TEMPORAL
                    # ["=", 0, "", 16000]
                    elif param >= 16000 and param < 21000:
                        arrTempFloat[param-16000] = arrConstantInt[nu]
                
                
                #############################################################
                # Si el primer argumento es una constante float
                elif nu >= 1000 and nu < 2000:
                    # Si a donde se asigna es una LOCAL int
                    # ["=", 1000, "", 3000]
                    if param >= 3000 and param < 4000:
                        arrLocalInt[param - 6000 + basInt[-1]] = arrConstantFloat[nu-1000]

                    # Si lo que se pide es asignar una LOCAL float
                    # ["=", 1000, "", 4000]
                    elif param >= 4000 and param < 5000:
                        arrLocalFloat[param-8000 + basFloat[-1]] = arrConstantFloat[nu-1000]

                    # Si a donde se asigna es una GLOBAL int
                    # ["=", 1000, "", 7000]
                    elif param >= 7000 and param < 8000:
                        arrGlobalInt[param-7000] = arrConstantFloat[nu-1000]
                    
                    # Si a donde se asigna es una GLOBAL float
                    # ["=", 1000, "", 8000]
                    elif param >= 8000 and param < 9000:
                        arrGlobalFloat[param-8000] = arrConstantFloat[nu-1000]

                    # Si lo que se pide es asignar un int TEMPORAL
                    # ["=", 1000, "", 11000]
                    elif param >= 11000 and param < 16000:
                        arrTempInt[param-11000] = arrConstantFloat[nu-1000]

                    # Si lo que se pide es asignar un float TEMPORAL
                    # ["=", 1000, "", 16000]
                    elif param >= 16000 and param < 21000:
                        arrTempFloat[param-16000] = arrConstantFloat[nu-1000]
                
                #############################################################
                # Si el primer argumento es una CONSTANTE char
                elif nu >= 2000 and nu < 3000:
                    # Si a donde se asigna es una LOCAL char
                    # ["=", 2000, "", 5000]
                    if param >= 5000 and param < 6000:
                        arrLocalChar[param-10000 + basChar[-1]] = arrConstantChar[nu-200]

                    # Si lo que se pide es asignar una GLOBAL char
                    # ["=", 2000, "", 9000]
                    elif param >= 9000 and param < 10000:
                        arrGlobalChar[param-9000] = arrConstantChar[nu-2000]

                #############################################################
                # Si el primer argumento es una LOCAL int
                if nu >= 3000 and nu < 4000:

                    # Si a donde se asigna es una LOCAL int
                    # ["=", 3000, "", 3000]
                    if param >= 3000 and param < 4000:
                        arrLocalInt[param - 6000 + basInt[-1]] = arrLocalInt[param - 6000 + basInt[-2]]

                    # Si lo que se pide es asignar una LOCAL float
                    # ["=", 3000, "", 4000]
                    elif param >= 4000 and param < 5000:
                        arrLocalFloat[param-8000 + basFloat[-1]] = arrLocalInt[param - 6000 + basInt[-2]]

                    # Si a donde se asigna es una GLOBAL int
                    # ["=", 3000, "", 7000]
                    elif param >= 7000 and param < 8000:
                        arrGlobalInt[param-7000] = arrLocalInt[param - 6000 + basInt[-2]]
                    
                    # Si a donde se asigna es una GLOBAL float
                    # ["=", 3000, "", 8000]
                    elif param >= 8000 and param < 9000:
                        arrGlobalFloat[param-8000] = arrLocalInt[param - 6000 + basInt[-2]]

                    # Si lo que se pide es asignar un int TEMPORAL
                    # ["=", 3000, "", 11000]
                    elif param >= 11000 and param < 16000:
                        arrTempInt[param-11000] = arrLocalInt[param - 6000 + basInt[-2]]

                    # Si lo que se pide es asignar un float TEMPORAL
                    # ["=", 3000, "", 16000]
                    elif param >= 16000 and param < 21000:
                        arrTempFloat[param-16000] = arrLocalInt[param - 6000 + basInt[-2]]
                
                #############################################################
                # Si el primer argumento es una LOCAL float
                if nu >= 4000 and nu < 5000:

                    # Si a donde se asigna es una LOCAL int
                    # ["=", 4000, "", 3000]
                    if param >= 3000 and param < 4000:
                        arrLocalInt[param - 6000 + basInt[-1]] = arrLocalFloat[param - 6000 + basInt[-2]]

                    # Si lo que se pide es asignar una LOCAL float
                    # ["=", 4000, "", 4000]
                    elif param >= 4000 and param < 5000:
                        arrLocalFloat[param-8000 + basFloat[-1]] = arrLocalFloat[param - 6000 + basInt[-2]]

                    # Si a donde se asigna es una GLOBAL int
                    # ["=", 4000, "", 7000]
                    elif param >= 7000 and param < 8000:
                        arrGlobalInt[param-7000] = arrLocalFloat[param - 6000 + basInt[-2]]
                    
                    # Si a donde se asigna es una GLOBAL float
                    # ["=", 4000, "", 8000]
                    elif param >= 8000 and param < 9000:
                        arrGlobalFloat[param-8000] = arrLocalFloat[param - 6000 + basInt[-2]]

                    # Si lo que se pide es asignar un int TEMPORAL
                    # ["=", 4000, "", 11000]
                    elif param >= 11000 and param < 16000:
                        arrTempInt[param-11000] = arrLocalFloat[param - 6000 + basInt[-2]]

                    # Si lo que se pide es asignar un float TEMPORAL
                    # ["=", 4000, "", 16000]
                    elif param >= 16000 and param < 21000:
                        arrTempFloat[param-16000] = arrLocalFloat[param - 6000 + basInt[-2]]
                
                #############################################################
                # Si el primer argumento es una LOCAL char
                elif nu >= 5000 and nu < 6000:
                    # Si a donde se asigna es una LOCAL char
                    # ["=", 5000, "", 5000]
                    if param >= 5000 and param < 6000:
                        arrLocalChar[param-10000 + basChar[-1]] = arrLocalChar[param-10000 + basChar[-2]]

                    # Si lo que se pide es asignar una GLOBAL char
                    # ["=", 5000, "", 9000]
                    elif param >= 9000 and param < 10000:
                        arrGlobalChar[param-9000] = arrLocalChar[param-10000 + basChar[-2]]
                
                #############################################################
                # Si el primer argumento es una GLOBAL int
                if nu >= 7000 and nu < 8000:

                    # Si a donde se asigna es una LOCAL int
                    # ["=", 7000, "", 3000]
                    if param >= 3000 and param < 4000:
                        arrLocalInt[param - 6000 + basInt[-1]] = arrGlobalInt[nu-7000]

                    # Si lo que se pide es asignar una LOCAL float
                    # ["=", 7000, "", 4000]
                    elif param >= 4000 and param < 5000:
                        arrLocalFloat[param-8000 + basFloat[-1]] = arrGlobalInt[nu-7000]

                    # Si a donde se asigna es una GLOBAL int
                    # ["=", 7000, "", 7000]
                    elif param >= 7000 and param < 8000:
                        arrGlobalInt[param-7000] = arrGlobalInt[nu-7000]
                    
                    # Si a donde se asigna es una GLOBAL float
                    # ["=", 7000, "", 8000]
                    elif param >= 8000 and param < 9000:
                        arrGlobalFloat[param-8000] = arrGlobalInt[nu-7000]

                    # Si lo que se pide es asignar un int TEMPORAL
                    # ["=", 7000, "", 11000]
                    elif param >= 11000 and param < 16000:
                        arrTempInt[param-11000] = arrGlobalInt[nu-7000]

                    # Si lo que se pide es asignar un float TEMPORAL
                    # ["=", 7000, "", 16000]
                    elif param >= 16000 and param < 21000:
                        arrTempFloat[param-16000] = arrGlobalInt[nu-7000]
                
                #############################################################
                # Si el primer argumento es una GLOBAL float
                if nu >= 8000 and nu < 9000:

                    # Si a donde se asigna es una LOCAL int
                    # ["=", 8000, "", 3000]
                    if param >= 3000 and param < 4000:
                        arrLocalInt[param - 6000 + basInt[-1]] = arrGlobalFloat[nu-8000]

                    # Si lo que se pide es asignar una LOCAL float
                    # ["=", 8000, "", 4000]
                    elif param >= 4000 and param < 5000:
                        arrLocalFloat[param-8000 + basFloat[-1]] = arrGlobalFloat[nu-8000]

                    # Si a donde se asigna es una GLOBAL int
                    # ["=", 8000, "", 7000]
                    elif param >= 7000 and param < 8000:
                        arrGlobalInt[param-7000] = arrGlobalFloat[nu-8000]
                    
                    # Si a donde se asigna es una GLOBAL float
                    # ["=", 8000, "", 8000]
                    elif param >= 8000 and param < 9000:
                        arrGlobalFloat[param-8000] = arrGlobalFloat[nu-8000]

                    # Si lo que se pide es asignar un int TEMPORAL
                    # ["=", 8000, "", 11000]
                    elif param >= 11000 and param < 16000:
                        arrTempInt[param-11000] = arrGlobalFloat[nu-8000]

                    # Si lo que se pide es asignar un float TEMPORAL
                    # ["=", 8000, "", 16000]
                    elif param >= 16000 and param < 21000:
                        arrTempFloat[param-16000] = arrGlobalFloat[nu-8000]

                #############################################################
                # Si el primer argumento es una GLOBAL char
                elif nu >= 9000 and nu < 10000:
                    # Si a donde se asigna es una LOCAL char
                    # ["=", 9000, "", 5000]
                    if param >= 9000 and param < 6000:
                        arrLocalChar[param-10000 + basChar[-1]] = arrGlobalChar[nu-9000]

                    # Si lo que se pide es asignar una GLOBAL char
                    # ["=", 9000, "", 9000]
                    elif param >= 9000 and param < 10000:
                        arrGlobalChar[param-9000] = arrGlobalChar[nu-9000]
            
                #############################################################
                # Si el primer argumento es una TEMPORAL int
                if nu >= 11000 and nu < 16000:

                    # Si a donde se asigna es una LOCAL int
                    # ["=", 11000, "", 3000]
                    if param >= 3000 and param < 4000:
                        arrLocalInt[param - 6000 + basInt[-1]] = arrTempInt[nu-11000]

                    # Si lo que se pide es asignar una LOCAL float
                    # ["=", 11000, "", 4000]
                    elif param >= 4000 and param < 5000:
                        arrLocalFloat[param-8000 + basFloat[-1]] = arrTempInt[nu-11000]

                    # Si a donde se asigna es una GLOBAL int
                    # ["=", 11000, "", 7000]
                    elif param >= 7000 and param < 8000:
                        arrGlobalInt[param-7000] = arrTempInt[nu-11000]
                    
                    # Si a donde se asigna es una GLOBAL float
                    # ["=", 11000, "", 8000]
                    elif param >= 8000 and param < 9000:
                        arrGlobalFloat[param-8000] = arrTempInt[nu-11000]

                    # Si lo que se pide es asignar un int TEMPORAL
                    # ["=", 11000, "", 11000]
                    elif param >= 11000 and param < 16000:
                        arrTempInt[param-11000] = arrTempInt[nu-11000]

                    # Si lo que se pide es asignar un float TEMPORAL
                    # ["=", 11000, "", 16000]
                    elif param >= 16000 and param < 21000:
                        arrTempFloat[param-16000] = arrTempInt[nu-11000]

                
                #############################################################
                # Si el primer argumento es una TEMPORAL float
                if nu >= 16000 and nu < 21000:

                    # Si a donde se asigna es una LOCAL int
                    # ["=", 16000, "", 3000]
                    if param >= 3000 and param < 4000:
                        arrLocalInt[param - 6000 + basInt[-1]] = arrTempFloat[nu-16000]

                    # Si lo que se pide es asignar una LOCAL float
                    # ["=", 16000, "", 4000]
                    elif param >= 4000 and param < 5000:
                        arrLocalFloat[param-8000 + basFloat[-1]] = arrTempFloat[nu-16000]

                    # Si a donde se asigna es una GLOBAL int
                    # ["=", 16000, "", 7000]
                    elif param >= 7000 and param < 8000:
                        arrGlobalInt[param-7000] = arrTempFloat[nu-16000]
                    
                    # Si a donde se asigna es una GLOBAL float
                    # ["=", 16000, "", 8000]
                    elif param >= 8000 and param < 9000:
                        arrGlobalFloat[param-8000] = arrTempFloat[nu-16000]

                    # Si lo que se pide es asignar un int TEMPORAL
                    # ["=", 16000, "", 11000]
                    elif param >= 11000 and param < 16000:
                        arrTempInt[param-11000] = arrTempFloat[nu-16000]

                    # Si lo que se pide es asignar un float TEMPORAL
                    # ["=", 16000, "", 16000]
                    elif param >= 16000 and param < 21000:
                        arrTempFloat[param-16000] = arrTempFloat[nu-16000]


                #############################################################
                # Si el primer argumento es una TEMPORAL char
                elif nu >= 21000 and nu < 26000:
                    # Si a donde se asigna es una LOCAL char
                    # ["=", 21000, "", 5000]
                    if param >= 5000 and param < 6000:
                        arrLocalChar[param-10000 + basChar[-1]] = arrTempChar[nu-21000]

                    # Si lo que se pide es asignar una GLOBAL char
                    # ["=", 5000, "", 9000]
                    elif param >= 21000 and param < 10000:
                        arrGlobalChar[param-9000] = arrTempChar[nu-21000]


        # Me voy a donde esta la funcion
        contTempQuads = quadActual[3]

    ###############################
    ########### return #############
    ###############################
    elif need == "return":
        contTempQuads = contTempQuads + 1
        quadTemp = cuadruplos[contTempQuads-1]
        funcAct = quadTemp[3]
        goto = proc.functionTable[funcAct].endf
        contTempQuads = goto
        pass

    ###############################
    ############ write ############
    ###############################
    elif need == "write":
        contTempQuads = contTempQuads + 1
        # Si lo que se pide es escribir una constante int
        if res < 1000:
            print(arrConstantInt[res])

        # Si lo que se pide es escribir una constante float
        elif res >= 1000 and res < 2000:
            print(arrConstantFloat[res-1000])

        # Si lo que me pide escribir es un char o string
        elif res >= 2000 and res < 3000:
            print(arrConstantChar[res-2000])

        # Si lo que se pide es escribir un int LOCAL
        elif res >= 3000 and res < 4000:
            print(arrLocalInt[res - 6000 + basInt[-1]])

        # Si lo que se pide es escribir un float LOCAL
        elif res >= 4000 and res < 5000:
            print(arrLocalFloat[res-8000 + basFloat[-1]])
        
        # Si lo que se pide es escribir un char LOCAL
        elif res >= 5000 and res < 6000:
            print(arrLocalChar[res-10000 + basChar[-1]])

        # Si lo que se pide es escribir un bool LOCAL
        elif res >= 6000 and res < 7000:
            print(arrLocalBool[res-12000 + basBool[-1]])
        
        # (Recordar que las variables globales
        # estan en la misma memoria que las del main)
        # Si lo que se pide es escribir un int GLOBAL
        elif res >= 7000 and res < 8000:
            print(arrGlobalInt[res-7000])
        
        # Si lo que se pide es escribir un float GLOBAL
        elif res >= 8000 and res < 9000:
            print(arrGlobalFloat[res-8000])
        
        # Si lo que se pide es escribir un char GLOBAL
        elif res >= 9000 and res < 10000:
            print(arrGlobalChar[res-9000])
        
        # Si lo que se pide es escribir un bool GLOBAL
        elif res >= 10000 and res < 11000:
            print(arrGlobalBool[res-10000])

        # Si lo que se pide es escribir un int TEMPORAL
        elif res >= 11000 and res < 16000:
            print(arrTempInt[res-11000])

        # Si lo que se pide es escribir un float TEMPORAL
        elif res >= 16000 and res < 21000:
            print(arrTempFloat[res-16000])

        # Si lo que se pide es escribir un char TEMPORAL
        elif res >= 21000 and res < 26000:
            print(arrTempChar[res-21000])

        # Si lo que se pide es escribir un bool TEMPORAL
        elif res >= 26000 and res < 31000:
            print(arrTempBool[res-26000])

        elif res >= 40000:
            pointer = arrTempPointers[res-40000]
            if pointer >= 3000 and pointer < 4000:
                print(arrLocalInt[pointer- 6000 + basInt[-1]])
            elif pointer >= 4000 and pointer < 5000:
                print(arrTempFloat[pointer-8000 + basFloat[-1]])
            elif pointer >= 7000 and pointer < 8000:
                print(arrGlobalInt[pointer-7000])
            elif pointer >= 8000 and pointer < 9000:
                print(arrGlobalFloat[pointer-8000])
    
    ###############################
    ############## = ##############
    ###############################
    elif need == "=":
        contTempQuads = contTempQuads + 1
        # Si el primer argumento es una constante Int
        if ar1 < 1000:

            # Si a donde se asigna es una LOCAL int
            # ["=", 0, "", 3000]
            if res >= 3000 and res < 4000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalInt[res - 6000 + basInt[-2]] = arrConstantInt[ar1]
                    else:
                        arrLocalInt[res - 3000] = arrConstantInt[ar1]
                else:
                    arrLocalInt[res - 6000 + basInt[-1]] = arrConstantInt[ar1]

            # Si lo que se pide es asignar una LOCAL float
            # ["=", 0, "", 4000]
            elif res >= 4000 and res < 5000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalFloat[res-8000 + basFloat[-2]] = arrConstantInt[ar1]
                    else:
                        arrLocalFloat[res-4000] = arrConstantInt[ar1]
                else:
                    arrLocalFloat[res-8000 + basFloat[-1]] = arrConstantInt[ar1]
            # Si a donde se asigna es una GLOBAL int
            # ["=", 0, "", 7000]
            elif res >= 7000 and res < 8000:
                arrGlobalInt[res-7000] = arrConstantInt[ar1]
            
            # Si a donde se asigna es una GLOBAL float
            # ["=", 0, "", 8000]
            elif res >= 8000 and res < 9000:
                arrGlobalFloat[res-8000] = arrConstantInt[ar1]

            # Si lo que se pide es asignar un int TEMPORAL
            # ["=", 0, "", 11000]
            elif res >= 11000 and res < 16000:
                arrTempInt[res-11000] = arrConstantInt[ar1]

            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 0, "", 16000]
            elif res >= 16000 and res < 21000:
                arrTempFloat[res-16000] = arrConstantInt[ar1]
            
            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 0, "", 40000]
            elif res >= 40000:
                pointer = arrTempPointers[res-40000]
                if pointer >= 3000 and pointer < 4000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalInt[pointer - 6000 + basInt[-2]] = arrConstantInt[ar1]
                        else:
                            arrLocalInt[pointer - 3000] = arrConstantInt[ar1]
                    else:
                        arrLocalInt[pointer - 6000 + basInt[-1]] = arrConstantInt[ar1]
                
                elif pointer >= 4000 and pointer < 5000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalFloat[pointer-8000 + basFloat[-2]] = arrConstantInt[ar1]
                        else:
                            arrLocalFloat[pointer-4000] = arrConstantInt[ar1]
                    else:
                        arrLocalFloat[pointer-8000 + basFloat[-1]] = arrConstantInt[ar1]
                
                elif pointer >= 7000 and pointer <8000:
                    arrGlobalInt[pointer-7000] = arrConstantInt[ar1]
                
                elif pointer >= 8000 and pointer < 9000:
                    arrGlobalFloat[res-8000] = arrConstantInt[ar1]
        
        
        #############################################################
        # Si el primer argumento es una constante float
        elif ar1 >= 1000 and ar1 < 2000:
            # Si a donde se asigna es una LOCAL int
            # ["=", 1000, "", 3000]
            if res >= 3000 and res < 4000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalInt[res - 6000 + basInt[-2]] = arrConstantFloat[ar1-1000]
                    else:
                        arrLocalInt[res - 3000] = arrConstantFloat[ar1-1000]
                else:
                    arrLocalInt[res - 6000 + basInt[-1]] = arrConstantFloat[ar1-1000]

            # Si lo que se pide es asignar una LOCAL float
            # ["=", 1000, "", 4000]
            elif res >= 4000 and res < 5000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalFloat[res-8000 + basFloat[-2]] = arrConstantFloat[ar1-1000]
                    else:
                        arrLocalFloat[res-4000] = arrConstantFloat[ar1-1000]
                else:
                    arrLocalFloat[res-8000 + basFloat[-1]] = arrConstantFloat[ar1-1000]

            # Si a donde se asigna es una GLOBAL int
            # ["=", 1000, "", 7000]
            elif res >= 7000 and res < 8000:
                arrGlobalInt[res-7000] = arrConstantFloat[ar1-1000]
            
            # Si a donde se asigna es una GLOBAL float
            # ["=", 1000, "", 8000]
            elif res >= 8000 and res < 9000:
                arrGlobalFloat[res-8000] = arrConstantFloat[ar1-1000]

            # Si lo que se pide es asignar un int TEMPORAL
            # ["=", 1000, "", 11000]
            elif res >= 11000 and res < 16000:
                arrTempInt[res-11000] = arrConstantFloat[ar1-1000]

            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 1000, "", 16000]
            elif res >= 16000 and res < 21000:
                arrTempFloat[res-16000] = arrConstantFloat[ar1-1000]

            elif res >= 40000:
                pointer = arrTempPointers[res-40000]
                if pointer >= 3000 and pointer < 4000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalInt[pointer - 6000 + basInt[-2]] = arrConstantFloat[ar1-1000]
                        else:
                            arrLocalInt[pointer - 3000] = arrConstantFloat[ar1-1000]
                    else:
                        arrLocalInt[pointer - 6000 + basInt[-1]] = arrConstantFloat[ar1-1000]
                
                elif pointer >= 4000 and pointer < 5000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalFloat[pointer-8000 + basFloat[-2]] = arrConstantFloat[ar1-1000]
                        else:
                            arrLocalFloat[pointer-4000] = arrConstantFloat[ar1-1000]
                    else:
                        arrLocalFloat[pointer-8000 + basFloat[-1]] = arrConstantFloat[ar1-1000]
                
                elif pointer >= 7000 and pointer <8000:
                    arrGlobalInt[pointer-7000] = arrConstantFloat[ar1-1000]
                
                elif pointer >= 8000 and pointer < 9000:
                    arrGlobalFloat[res-8000] = arrConstantFloat[ar1-1000]
        
        #############################################################
        # Si el primer argumento es una CONSTANTE char
        elif ar1 >= 2000 and ar1 < 3000:
            # Si a donde se asigna es una LOCAL char
            # ["=", 2000, "", 5000]
            if res >= 5000 and res < 6000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalChar[res-10000 + basChar[-2]] = arrConstantChar[ar1-2000]
                    else:
                        arrLocalChar[res-5000] = arrConstantChar[ar1-2000]
                else:
                    arrLocalChar[res-10000 + basChar[-1]] = arrConstantChar[ar1-2000]

            # Si lo que se pide es asignar una GLOBAL char
            # ["=", 2000, "", 9000]
            elif res >= 9000 and res < 10000:
                arrGlobalChar[res-9000] = arrConstantChar[ar1-2000]

        #############################################################
        # Si el primer argumento es una LOCAL int
        if ar1 >= 3000 and ar1 < 4000:

            # Si a donde se asigna es una LOCAL int
            # ["=", 3000, "", 3000]
            #############RRRR##########
            if res >= 3000 and res < 4000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalInt[res - 6000 + basInt[-2]] = arrLocalInt[ar1-6000 + basInt[-1]]
                    else:
                        arrLocalInt[res - 3000] = arrLocalInt[ar1-6000 + basInt[-1]]
                else:
                    arrLocalInt[res - 6000 + basInt[-1]] = arrLocalInt[ar1-6000 + basInt[-1]]

            # Si lo que se pide es asignar una LOCAL float
            # ["=", 3000, "", 4000]
            elif res >= 4000 and res < 5000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalFloat[res-8000 + basFloat[-2]] = arrLocalInt[ar1-6000 + basInt[-1]]
                    else:
                        arrLocalFloat[res-4000] = arrLocalInt[ar1-6000 + basInt[-1]]
                else:
                    arrLocalFloat[res-8000 + basFloat[-1]] = arrLocalInt[ar1-6000 + basInt[-1]]

            # Si a donde se asigna es una GLOBAL int
            # ["=", 3000, "", 7000]
            elif res >= 7000 and res < 8000:
                if basInt:
                    arrGlobalInt[res-7000] = arrLocalInt[ar1-6000 + basInt[-1]]
                else:
                    arrGlobalInt[res-7000] = arrLocalInt[ar1-3000]
            
            # Si a donde se asigna es una GLOBAL float
            # ["=", 3000, "", 8000]
            elif res >= 8000 and res < 9000:
                if basInt:
                    arrGlobalFloat[res-8000] = arrLocalInt[ar1-6000 + basInt[-1]]
                else:
                    arrGlobalFloat[res-8000] = arrLocalInt[ar1-3000]

            # Si lo que se pide es asignar un int TEMPORAL
            # ["=", 3000, "", 11000]
            elif res >= 11000 and res < 16000:
                if basInt:
                    arrTempInt[res-11000] = arrLocalInt[ar1-6000 + basInt[-1]]
                else:
                    arrTempInt[res-11000] = arrLocalInt[ar1-3000]

            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 3000, "", 16000]
            elif res >= 16000 and res < 21000:
                arrTempFloat[res-16000] = arrLocalInt[ar1-6000 + basInt[-1]]

            elif res >= 40000:
                pointer = arrTempPointers[res-40000]
                if pointer >= 3000 and pointer < 4000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalInt[pointer - 6000 + basInt[-2]] = arrLocalInt[ar1-6000 + basInt[-1]]
                        else:
                            arrLocalInt[pointer - 3000] = arrLocalInt[ar1-6000 + basInt[-1]]
                    else:
                        arrLocalInt[pointer - 6000 + basInt[-1]] = arrLocalInt[ar1-6000 + basInt[-1]]
                
                elif pointer >= 4000 and pointer < 5000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalFloat[pointer-8000 + basFloat[-2]] = arrLocalInt[ar1-6000 + basInt[-1]]
                        else:
                            arrLocalFloat[pointer-4000] = arrLocalInt[ar1-6000 + basInt[-1]]
                    else:
                        arrLocalFloat[pointer-8000 + basFloat[-1]] = arrLocalInt[ar1-6000 + basInt[-1]]
                
                elif pointer >= 7000 and pointer <8000:
                    arrGlobalInt[pointer-7000] = arrLocalInt[ar1-6000 + basInt[-1]]
                
                elif pointer >= 8000 and pointer < 9000:
                    arrGlobalFloat[res-8000] = arrLocalInt[ar1-6000 + basInt[-1]]
        
        #############################################################
        # Si el primer argumento es una LOCAL float
        if ar1 >= 4000 and ar1 < 5000:

            # Si a donde se asigna es una LOCAL int
            # ["=", 4000, "", 3000]
            if res >= 3000 and res < 4000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalInt[res - 6000 + basInt[-2]] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                    else:
                        arrLocalInt[res - 3000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                else:
                    arrLocalInt[res - 6000 + basInt[-1]] = arrLocalFloat[ar1-8000 + basFloat[-1]]


            # Si lo que se pide es asignar una LOCAL float
            # ["=", 4000, "", 4000]
            elif res >= 4000 and res < 5000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalFloat[res-8000 + basFloat[-2]] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                    else:
                        arrLocalFloat[res-4000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                else:
                    arrLocalFloat[res-8000 + basFloat[-1]] = arrLocalFloat[ar1-8000 + basFloat[-1]]

            # Si a donde se asigna es una GLOBAL int
            # ["=", 4000, "", 7000]
            elif res >= 7000 and res < 8000:
                if basFloat:
                    arrGlobalInt[res-7000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                else:
                    arrGlobalInt[res-7000] = arrLocalFloat[ar1-4000]
            
            # Si a donde se asigna es una GLOBAL float
            # ["=", 4000, "", 8000]
            elif res >= 8000 and res < 9000:
                if basFloat:
                    arrGlobalFloat[res-8000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                else:
                    arrGlobalFloat[res-8000] = arrLocalFloat[ar1-4000]

            # Si lo que se pide es asignar un int TEMPORAL
            # ["=", 4000, "", 11000]
            elif res >= 11000 and res < 16000:
                if basFloat:
                    arrTempInt[res-11000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                else:
                    arrTempInt[res-11000] = arrLocalFloat[ar1-4000]

            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 4000, "", 16000]
            elif res >= 16000 and res < 21000:
                if basFloat:
                    arrTempFloat[res-16000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                else:
                    arrTempFloat[res-16000] = arrLocalFloat[ar1-4000]
            
            elif res >= 40000:
                pointer = arrTempPointers[res-40000]
                if pointer >= 3000 and pointer < 4000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalInt[pointer - 6000 + basInt[-2]] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                        else:
                            arrLocalInt[pointer - 3000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                    else:
                        arrLocalInt[pointer - 6000 + basInt[-1]] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                
                elif pointer >= 4000 and pointer < 5000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalFloat[pointer-8000 + basFloat[-2]] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                        else:
                            arrLocalFloat[pointer-4000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                    else:
                        arrLocalFloat[pointer-8000 + basFloat[-1]] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                
                elif pointer >= 7000 and pointer <8000:
                    arrGlobalInt[pointer-7000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
                
                elif pointer >= 8000 and pointer < 9000:
                    arrGlobalFloat[res-8000] = arrLocalFloat[ar1-8000 + basFloat[-1]]
        
        #############################################################
        # Si el primer argumento es una LOCAL char
        elif ar1 >= 5000 and ar1 < 6000:
            # Si a donde se asigna es una LOCAL char
            # ["=", 5000, "", 5000]
            if res >= 5000 and res < 6000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalChar[res-10000 + basChar[-2]] = arrLocalChar[ar1-basChar[-1]]
                    else:
                        arrLocalChar[res-5000] = arrLocalChar[ar1-basChar[-1]]
                else:
                    arrLocalChar[res-10000 + basChar[-1]] = arrLocalChar[ar1-basChar[-1]]

            # Si lo que se pide es asignar una GLOBAL char
            # ["=", 5000, "", 9000]
            elif res >= 9000 and res < 10000:
                arrGlobalChar[res-9000] = arrLocalChar[ar1-basChar[-1]]
        
        #############################################################
        # Si el primer argumento es una GLOBAL int
        if ar1 >= 7000 and ar1 < 8000:

            # Si a donde se asigna es una LOCAL int
            # ["=", 7000, "", 3000]
            if res >= 3000 and res < 4000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalInt[res - 6000 + basInt[-2]] = arrGlobalInt[ar1-7000]
                    else:
                        arrLocalInt[res - 3000] = arrGlobalInt[ar1-7000]
                else:
                    arrLocalInt[res - 6000 + basInt[-1]] = arrGlobalInt[ar1-7000]

            # Si lo que se pide es asignar una LOCAL float
            # ["=", 7000, "", 4000]
            elif res >= 4000 and res < 5000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                            arrLocalFloat[res-8000 + basFloat[-2]] = arrGlobalInt[ar1-7000]
                    else:
                        arrLocalFloat[res-4000] = arrGlobalInt[ar1-7000]
                else:
                    arrLocalFloat[res-8000 + basFloat[-1]] = arrGlobalInt[ar1-7000]

            # Si a donde se asigna es una GLOBAL int
            # ["=", 7000, "", 7000]
            elif res >= 7000 and res < 8000:
                arrGlobalInt[res-7000] = arrGlobalInt[ar1-7000]
            
            # Si a donde se asigna es una GLOBAL float
            # ["=", 7000, "", 8000]
            elif res >= 8000 and res < 9000:
                arrGlobalFloat[res-8000] = arrGlobalInt[ar1-7000]

            # Si lo que se pide es asignar un int TEMPORAL
            # ["=", 7000, "", 11000]
            elif res >= 11000 and res < 16000:
                arrTempInt[res-11000] = arrGlobalInt[ar1-7000]

            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 7000, "", 16000]
            elif res >= 16000 and res < 21000:
                arrTempFloat[res-16000] = arrGlobalInt[ar1-7000]

            elif res >= 40000:
                pointer = arrTempPointers[res-40000]
                if pointer >= 3000 and pointer < 4000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalInt[pointer - 6000 + basInt[-2]] = arrGlobalInt[ar1-7000]
                        else:
                            arrLocalInt[pointer - 3000] = arrGlobalInt[ar1-7000]
                    else:
                        arrLocalInt[pointer - 6000 + basInt[-1]] = arrGlobalInt[ar1-7000]
                
                elif pointer >= 4000 and pointer < 5000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalFloat[pointer-8000 + basFloat[-2]] = arrGlobalInt[ar1-7000]
                        else:
                            arrLocalFloat[pointer-4000] = arrGlobalInt[ar1-7000]
                    else:
                        arrLocalFloat[pointer-8000 + basFloat[-1]] = arrGlobalInt[ar1-7000]
                
                elif pointer >= 7000 and pointer <8000:
                    arrGlobalInt[pointer-7000] = arrGlobalInt[ar1-7000]
                
                elif pointer >= 8000 and pointer < 9000:
                    arrGlobalFloat[res-8000] = arrGlobalInt[ar1-7000]
        
        #############################################################
        # Si el primer argumento es una GLOBAL float
        if ar1 >= 8000 and ar1 < 9000:

            # Si a donde se asigna es una LOCAL int
            # ["=", 8000, "", 3000]
            if res >= 3000 and res < 4000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalInt[res - 6000 + basInt[-2]] = arrGlobalFloat[ar1-8000]
                    else:
                        arrLocalInt[res - 3000] = arrGlobalFloat[ar1-8000]
                else:
                    arrLocalInt[res - 6000 + basInt[-1]] = arrGlobalFloat[ar1-8000]

            # Si lo que se pide es asignar una LOCAL float
            # ["=", 8000, "", 4000]
            elif res >= 4000 and res < 5000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                            arrLocalFloat[res-8000 + basFloat[-2]] = arrGlobalFloat[ar1-8000]
                    else:
                        arrLocalFloat[res-4000] = arrGlobalFloat[ar1-8000]
                else:
                    arrLocalFloat[res-8000 + basFloat[-1]] = arrGlobalFloat[ar1-8000]

            # Si a donde se asigna es una GLOBAL int
            # ["=", 8000, "", 7000]
            elif res >= 7000 and res < 8000:
                arrGlobalInt[res-7000] = arrGlobalFloat[ar1-8000]
            
            # Si a donde se asigna es una GLOBAL float
            # ["=", 8000, "", 8000]
            elif res >= 8000 and res < 9000:
                arrGlobalFloat[res-8000] = arrGlobalFloat[ar1-8000]

            # Si lo que se pide es asignar un int TEMPORAL
            # ["=", 8000, "", 11000]
            elif res >= 11000 and res < 16000:
                arrTempInt[res-11000] = arrGlobalFloat[ar1-8000]

            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 8000, "", 16000]
            elif res >= 16000 and res < 21000:
                arrTempFloat[res-16000] = arrGlobalFloat[ar1-8000]
            
            elif res >= 40000:
                pointer = arrTempPointers[res-40000]
                if pointer >= 3000 and pointer < 4000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalInt[pointer - 6000 + basInt[-2]] = arrGlobalFloat[ar1-8000]
                        else:
                            arrLocalInt[pointer - 3000] = arrGlobalFloat[ar1-8000]
                    else:
                        arrLocalInt[pointer - 6000 + basInt[-1]] = arrGlobalFloat[ar1-8000]
                
                elif pointer >= 4000 and pointer < 5000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalFloat[pointer-8000 + basFloat[-2]] = arrGlobalFloat[ar1-8000]
                        else:
                            arrLocalFloat[pointer-4000] = arrGlobalFloat[ar1-8000]
                    else:
                        arrLocalFloat[pointer-8000 + basFloat[-1]] = arrGlobalFloat[ar1-8000]
                
                elif pointer >= 7000 and pointer <8000:
                    arrGlobalInt[pointer-7000] = arrGlobalFloat[ar1-8000]
                
                elif pointer >= 8000 and pointer < 9000:
                    arrGlobalFloat[res-8000] = arrGlobalFloat[ar1-8000]

        #############################################################
        # Si el primer argumento es una GLOBAL char
        elif ar1 >= 9000 and ar1 < 10000:
            # Si a donde se asigna es una LOCAL char
            # ["=", 9000, "", 5000]
            if res >= 9000 and res < 6000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalChar[res-10000 + basChar[-2]] = arrGlobalChar[ar1-9000]
                    else:
                        arrLocalChar[res-5000] = arrGlobalChar[ar1-9000]
                else:
                    arrLocalChar[res-10000 + basChar[-1]] = arrGlobalChar[ar1-9000]

            # Si lo que se pide es asignar una GLOBAL char
            # ["=", 9000, "", 9000]
            elif res >= 9000 and res < 10000:
                arrGlobalChar[res-9000] = arrGlobalChar[ar1-9000]
    
        #############################################################
        # Si el primer argumento es una TEMPORAL int
        if ar1 >= 11000 and ar1 < 16000:

            # Si a donde se asigna es una LOCAL int
            # ["=", 11000, "", 3000]
            if res >= 3000 and res < 4000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalInt[res - 6000 + basInt[-2]] = arrTempInt[ar1-11000]
                    else:
                        arrLocalInt[res - 3000] = arrTempInt[ar1-11000]
                else:
                    arrLocalInt[res - 6000 + basInt[-1]] = arrTempInt[ar1-11000]

            # Si lo que se pide es asignar una LOCAL float
            # ["=", 11000, "", 4000]
            elif res >= 4000 and res < 5000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                            arrLocalFloat[res-8000 + basFloat[-2]] = arrTempInt[ar1-11000]
                    else:
                        arrLocalFloat[res-4000] = arrTempInt[ar1-11000]
                else:
                    arrLocalFloat[res-8000 + basFloat[-1]] = arrTempInt[ar1-11000]


            # Si a donde se asigna es una GLOBAL int
            # ["=", 11000, "", 7000]
            elif res >= 7000 and res < 8000:
                arrGlobalInt[res-7000] = arrTempInt[ar1-11000]
            
            # Si a donde se asigna es una GLOBAL float
            # ["=", 11000, "", 8000]
            elif res >= 8000 and res < 9000:
                arrGlobalFloat[res-8000] = arrTempInt[ar1-11000]

            # Si lo que se pide es asignar un int TEMPORAL
            # ["=", 11000, "", 11000]
            elif res >= 11000 and res < 16000:
                arrTempInt[res-11000] = arrTempInt[ar1-11000]

            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 11000, "", 16000]
            elif res >= 16000 and res < 21000:
                arrTempFloat[res-16000] = arrTempInt[ar1-11000]

            elif res >= 40000:
                pointer = arrTempPointers[res-40000]
                if pointer >= 3000 and pointer < 4000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalInt[pointer - 6000 + basInt[-2]] = arrTempInt[ar1-11000]
                        else:
                            arrLocalInt[pointer - 3000] = arrTempInt[ar1-11000]
                    else:
                        arrLocalInt[pointer - 6000 + basInt[-1]] = arrTempInt[ar1-11000]
                
                elif pointer >= 4000 and pointer < 5000:
                    if cuadruplos[contTempQuads][0] == "return":
                        if len(basInt) > 1:
                            arrLocalFloat[pointer-8000 + basFloat[-2]] = arrTempInt[ar1-11000]
                        else:
                            arrLocalFloat[pointer-4000] = arrTempInt[ar1-11000]
                    else:
                        arrLocalFloat[pointer-8000 + basFloat[-1]] = arrTempInt[ar1-11000]
                
                elif pointer >= 7000 and pointer <8000:
                    arrGlobalInt[pointer-7000] = arrTempInt[ar1-11000]
                
                elif pointer >= 8000 and pointer < 9000:
                    arrGlobalFloat[res-8000] = arrTempInt[ar1-11000]
        
        #############################################################
        # Si el primer argumento es una TEMPORAL float
        if ar1 >= 16000 and ar1 < 21000:

            # Si a donde se asigna es una LOCAL int
            # ["=", 16000, "", 3000]
            if res >= 3000 and res < 4000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalInt[res - 6000 + basInt[-2]] = arrTempFloat[ar1-16000]
                    else:
                        arrLocalInt[res - 3000] = arrTempFloat[ar1-16000]
                else:
                    arrLocalInt[res - 6000 + basInt[-1]] = arrTempFloat[ar1-16000]

            # Si lo que se pide es asignar una LOCAL float
            # ["=", 16000, "", 4000]
            elif res >= 4000 and res < 5000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                            arrLocalFloat[res-8000 + basFloat[-2]] = arrTempFloat[ar1-16000]
                    else:
                        arrLocalFloat[res-4000] = arrTempFloat[ar1-16000]
                else:
                    arrLocalFloat[res-8000 + basFloat[-1]] = arrTempFloat[ar1-16000]

            # Si a donde se asigna es una GLOBAL int
            # ["=", 16000, "", 7000]
            elif res >= 7000 and res < 8000:
                arrGlobalInt[res-7000] = arrTempFloat[ar1-16000]
            
            # Si a donde se asigna es una GLOBAL float
            # ["=", 16000, "", 8000]
            elif res >= 8000 and res < 9000:
                arrGlobalFloat[res-8000] = arrTempFloat[ar1-16000]

            # Si lo que se pide es asignar un int TEMPORAL
            # ["=", 16000, "", 11000]
            elif res >= 11000 and res < 16000:
                arrTempInt[res-11000] = arrTempFloat[ar1-16000]

            # Si lo que se pide es asignar un float TEMPORAL
            # ["=", 16000, "", 16000]
            elif res >= 16000 and res < 21000:
                arrTempFloat[res-16000] = arrTempFloat[ar1-16000]


        #############################################################
        # Si el primer argumento es una TEMPORAL char
        elif ar1 >= 21000 and ar1 < 26000:
            # Si a donde se asigna es una LOCAL char
            # ["=", 21000, "", 5000]
            if res >= 5000 and res < 6000:
                if cuadruplos[contTempQuads][0] == "return":
                    if len(basInt) > 1:
                        arrLocalChar[res-10000 + basChar[-2]] = arrTempChar[ar1-21000]
                    else:
                        arrLocalChar[res-5000] = arrTempChar[ar1-21000]
                else:
                    arrLocalChar[res-10000 + basChar[-1]] = arrTempChar[ar1-21000]

            # Si lo que se pide es asignar una GLOBAL char
            # ["=", 5000, "", 9000]
            elif res >= 21000 and res < 10000:
                arrGlobalChar[res-9000] = arrTempChar[ar1-21000]

        
    ###############################
    ########## +, -, *, / #########
    ###############################
    elif need in misOperadores:
        contTempQuads = contTempQuads + 1
        ###############################################################
        # Reviso si el primer ar es constante INT (AR1)
        ###############################################################
        if ar1 < 1000:

            # Tomo el valor del int y lo asigno a suma1
            suma1 = int(arrConstantInt[ar1])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 0, 0, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 0, 0, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 0, 0, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 0, 0, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 0, 0, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 0, 0, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+", 0, 1000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 0, 1000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 0, 1000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 0, 1000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 0, 1000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 0, 1000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 0, 3000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 0, 3000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 0, 3000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 0, 3000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 0, 3000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 0, 3000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                # Si lo que se pide es asignar un int LOCAL
                # ["+", 0, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 0, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 0, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 0, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 0, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 0, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+", 0, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 0, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 0, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 0, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 0, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 0, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+", 0, 8000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 0, 8000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 0, 8000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 0, 8000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 0, 8000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 0, 8000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+", 0, 11000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 0, 11000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 0, 11000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 0, 11000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 0, 11000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 0, 11000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+", 0, 16000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 0, 16000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 0, 16000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 0, 16000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 0, 16000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 0, 16000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
        
        ###############################################################
        # Reviso si el primer ar es constante FLOAT (AR1)
        ###############################################################
        elif ar1 >= 1000 and ar1 < 2000:
            suma1 = float(arrConstantFloat[ar1-1000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 1000, 0, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 1000, 0, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 1000, 0, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 1000, 0, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 1000, 0, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 1000, 0, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 1000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 1000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 1000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 1000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 1000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 1000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 3000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 3000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 3000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 3000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 3000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 3000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 8000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 8000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 8000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 8000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 8000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 8000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 11000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 11000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 11000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 11000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 11000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 11000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 16000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 16000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 16000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 16000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 16000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 16000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
        
        ###############################################################
        # Reviso si el primer ar es LOCAL INT (AR1)
        ###############################################################
        elif ar1 >= 3000 and ar1 < 4000:
            suma1 = int(arrLocalInt[ar1-6000 + basInt[-1]])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 1000, 0, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 1000, 0, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 1000, 0, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 1000, 0, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 1000, 0, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 1000, 0, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 1000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 1000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 1000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 1000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 1000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 1000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 3000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 3000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 3000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 3000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 3000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 3000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 8000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 8000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 8000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 8000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 8000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 8000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 11000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 11000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 11000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 11000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 11000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 11000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 16000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 16000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 16000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 16000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 16000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 16000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
        
        ###############################################################
        # Reviso si el primer ar es LOCAL FLOAT (AR1)
        ###############################################################
        elif ar1 >= 4000 and ar1 < 5000:

            suma1 = float(arrLocalFloat[ar1-8000 + basFloat[-1]])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 1000, 0, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 1000, 0, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 1000, 0, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 1000, 0, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 1000, 0, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 1000, 0, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 1000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 1000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 1000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 1000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 1000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 1000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 3000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 3000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 3000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 3000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 3000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 3000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 8000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 8000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 8000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 8000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 8000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 8000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 11000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 11000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 11000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 11000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 11000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 11000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 16000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 16000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 16000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 16000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 16000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 16000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es GLOBAL INT (AR1)
        ###############################################################
        elif ar1 >= 7000 and ar1 < 8000:

            suma1 = int(arrGlobalInt[ar1-7000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 1000, 0, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 1000, 0, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 1000, 0, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 1000, 0, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 1000, 0, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 1000, 0, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 1000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 1000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 1000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 1000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 1000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 1000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 3000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 3000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 3000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 3000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 3000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 3000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 8000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 8000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 8000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 8000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 8000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 8000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 11000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 11000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 11000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 11000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 11000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 11000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 16000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 16000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 16000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 16000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 16000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 16000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es GLOBAL FLOAT (AR1)
        ###############################################################
        elif ar1 >= 8000 and ar1 < 9000:
            suma1 = float(arrGlobalFloat[ar1-8000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 1000, 0, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 1000, 0, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 1000, 0, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 1000, 0, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 1000, 0, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 1000, 0, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 1000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 1000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 1000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 1000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 1000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 1000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 3000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 3000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 3000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 3000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 3000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 3000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 8000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 8000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 8000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 8000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 8000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 8000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 11000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 11000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 11000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 11000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 11000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 11000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 16000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 16000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 16000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 16000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 16000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 16000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es TEMP INT (AR1)
        ###############################################################
        elif ar1 >= 11000 and ar1 < 16000:

            suma1 = int(arrTempInt[ar1-11000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 1000, 0, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 1000, 0, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 1000, 0, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 1000, 0, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 1000, 0, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 1000, 0, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 1000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 1000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 1000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 1000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 1000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 1000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 3000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 3000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 3000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 3000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 3000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 3000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

                # HOLAAA AQUIIII VECTORESSSS #
                # Si lo que se pide es asignar un POINTER TEMPORAL
                # ["+", 11000, 7000, 40000]
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
                
                

            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 8000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 8000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 8000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 8000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 8000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 8000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
                
                elif res >= 40000:
                    suma2 = ar2;
                    st = "suma1" + need + "suma2"
                    arrTempPointers[res-40000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 11000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 11000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 11000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 11000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 11000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 11000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 16000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 16000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 16000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 16000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 16000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 16000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es TEMP FLOAT (AR1)
        ###############################################################
        elif ar1 >= 16000 and ar1 < 21000:
            suma1 = float(arrTempFloat[ar1-16000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])

                # Si lo que se pide es asignar un int LOCAL
                # ["+", 1000, 0, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+", 1000, 0, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+", 1000, 0, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+", 1000, 0, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+", 1000, 0, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+", 1000, 0, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 1000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 1000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 1000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 1000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 1000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 1000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 3000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 3000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 3000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 3000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 3000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 3000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 4000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 4000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 4000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 4000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 4000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 4000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 8000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 8000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 8000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 8000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 8000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 8000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 11000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 11000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 11000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 11000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 11000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 11000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                # Si lo que se pide es asignar un int LOCAL
                # ["+",1000, 16000, 3000]
                if res >= 3000 and res < 4000:
                    st = "suma1" + need + "suma2"
                    arrLocalInt[res - 6000 + basInt[-1]] = eval(st)

                # Si lo que se pide es asignar un float LOCAL
                # ["+",1000, 16000, 4000]
                elif res >= 4000 and res < 5000:
                    st = "suma1" + need + "suma2"
                    arrLocalFloat[res-8000 + basFloat[-1]] = eval(st)
            
                # (Recordar que las variables globales
                # estan en la misma memoria que las del main)
                # Si lo que se pide es asignar un int GLOBAL
                # ["+",1000, 16000, 7000]
                elif res >= 7000 and res < 8000:
                    st = "suma1" + need + "suma2"
                    arrGlobalInt[res-7000] = eval(st)
                
                # Si lo que se pide es asignar un float GLOBAL
                # ["+",1000, 16000, 8000]
                elif res >= 8000 and res < 9000:
                    st = "suma1" + need + "suma2"
                    arrGlobalFloat[res-8000] = eval(st)

                # Si lo que se pide es asignar un int TEMPORAL
                # ["+",1000, 16000, 11000]
                elif res >= 11000 and res < 16000:
                    st = "suma1" + need + "suma2"
                    arrTempInt[res-11000] = eval(st)

                # Si lo que se pide es asignar un float TEMPORAL
                # ["+",1000, 16000, 16000]
                elif res >= 16000 and res < 21000:
                    st = "suma1" + need + "suma2"
                    arrTempFloat[res-16000] = eval(st)

    #################################
    ## >, <, ==, !=, &&, |, >=, <= ##
    #################################
    elif need in misOperadosBool:
        contTempQuads = contTempQuads + 1

        ###############################################################
        # Reviso si el primer ar es CONSTANT INT (AR1)
        ###############################################################
        if ar1 < 1000:
            suma1 = int(arrConstantInt[ar1])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
        
        ###############################################################
        # Reviso si el primer ar es CONSTANT FLOAT (AR1)
        ###############################################################
        elif ar1 >= 1000 and ar1 < 2000:
            suma1 = float(arrConstantFloat[ar1-1000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
        
        ###############################################################
        # Reviso si el primer ar es LOCAL INT (AR1)
        ###############################################################
        elif ar1 >= 3000 and ar1 < 4000:
            suma1 = int(arrLocalInt[ar1-6000 + basInt[-1]])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
       
        ###############################################################
        # Reviso si el primer ar es LOCAL FLOAT (AR1)
        ###############################################################
        elif ar1 >= 4000 and ar1 < 5000:
            suma1 = float(arrLocalFloat[ar1-8000 + basFloat[-1]])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
       
        ###############################################################
        # Reviso si el primer ar es GLOBAL INT (AR1)
        ###############################################################
        elif ar1 >= 7000 and ar1 < 8000:
            suma1 = int(arrGlobalInt[ar1-7000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es GLOBAL FLOAT (AR1)
        ###############################################################
        elif ar1 >= 8000 and ar1 < 9000:
            suma1 = float(arrGlobalFloat[ar1-8000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es TEMP INT (AR1)
        ###############################################################
        elif ar1 >= 11000 and ar1 < 16000:
            suma1 = int(arrTempInt[ar1-11000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es TEMP FLOAT (AR1)
        ###############################################################
        elif ar1 >= 16000 and ar1 < 21000:
            suma1 = float(arrTempFloat[ar1-16000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es TEMP BOOL (AR1)
        ###############################################################
        elif ar1 >= 26000 and ar1 < 31000:
            suma1 = bool(arrTempBool[ar1-26000])

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

        ###############################################################
        # Reviso si el primer ar es TEMP POINTER (AR1)
        ###############################################################
        elif ar1 >= 40000:
            pointer = arrTempPointers[ar1-40000]
            if pointer >= 3000 and pointer < 4000:
                suma1 = arrLocalInt[pointer- 6000 + basInt[-1]]
            elif pointer >= 4000 and pointer < 5000:
                suma1 = arrTempFloat[pointer-8000 + basFloat[-1]]
            elif pointer >= 7000 and pointer < 8000:
                suma1 = arrGlobalInt[pointer-7000]
            elif pointer >= 8000 and pointer < 9000:
                suma1 = arrGlobalFloat[pointer-8000]

            ###############################################################
            # Reviso si el SEGUNDO ar es constante INT (AR2)
            if ar2 < 1000:
                suma2 = int(arrConstantInt[ar2])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)


            ###############################################################
            # Reviso si el segundo ar es CONSTANTE FLOAT (AR2)
            elif ar2 >= 1000 and ar2 < 2000:
                suma2 = float(arrConstantFloat[ar2-1000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            
            ###############################################################
            # Reviso si el segundo ar es LOCAL INT (AR2)
            elif ar2 >= 3000 and ar2 < 4000:
                suma2 = int(arrLocalInt[ar2-6000 + basInt[-1]])

                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es LOCAL FLOAT (AR2)
            elif ar2 >= 4000 and ar2 < 5000:
                suma2 = float(arrLocalFloat[ar2-8000 + basFloat[-1]])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            
            # (Recordar que las variables globales
            # estan en la misma memoria que las del main)
            ###############################################################
            # Reviso si el segundo ar es GLOBAL INT (AR2)
            elif ar2 >= 7000 and ar2 < 8000:
                suma2 = int(arrGlobalInt[ar2-7000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            
            ###############################################################
            # Reviso si el segundo ar es GLOBAL FLOAT (AR2)
            # ["+", 8000, 8000, 26000]
            elif ar2 >= 8000 and ar2 < 9000:
                suma2 = float(arrGlobalFloat[ar2-8000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)
            

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL INT (AR2)
            elif ar2 >= 11000 and ar2 < 16000:
                suma2 = int(arrTempInt[ar2-11000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL FLOAT (AR2)
            elif ar2 >= 16000 and ar2 < 21000:
                suma2 = float(arrTempFloat[ar2-16000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

            ###############################################################
            # Reviso si el segundo ar es TEMPORAL BOOL (AR2)
            elif ar2 >= 26000 and ar2 < 31000:
                suma2 = bool(arrTempBool[ar2-26000])
                
                #Evaluo la expresion y se la asigno a su temporal booleana
                st = "suma1" + need + "suma2"
                arrTempBool[res-26000] = eval(st)

    elif need == "Read":
        contTempQuads = contTempQuads + 1

        if res >= 3000 and res < 4000:
            arrLocalInt[res - 6000 + basInt[-1]] = int(input())
        
        elif res >= 4000 and res < 5000:
            arrLocalFloat[res-8000 + basFloat[-1]] = float(input())

        elif res >= 5000 and res < 6000:
            arrLocalChar[res-10000 + basChar[-1]] = input()

        elif res >= 7000 and res < 8000:
            arrGlobalInt[res-7000] = int(input())
        
        elif res >= 8000 and res < 9000:
            arrGlobalFloat[res-8000] = float(input())
        
        elif res >= 9000 and res < 10000:
            arrGlobalChar[res-9000] = input()
        
    elif need == "endfunc":
        go = ret.pop()
        contTempQuads = go
        basInt.pop()
        basFloat.pop()
        basChar.pop()
        basBool.pop()

    elif need == "EndFile":
        print()
        print("###################################")
        print("# Termina la ejecucion del codigo #")
        print("###################################")
        contTempQuads = contTempQuads + 1  

    else:
         contTempQuads = contTempQuads + 1
